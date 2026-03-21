# Public Deployment Plan

## Overview

Deploy data_viewer as a public-facing Shiny app on a self-hosted VPS.
Users bring their own FRED API key via an in-app modal. Bloomberg is excluded
from the public build. All other sources (ABS, RBA, DBnomics, FRED) work
without server-level credentials.

**Target stack:** Hetzner CX22 (~€4/month) · Ubuntu 24.04 · Shiny Server OSS ·
Nginx · Let's Encrypt

---

## 1. Pre-Deployment Code Changes

These must be done before deploying. They are independent of infrastructure.

### 1.1 Session-scope the FRED API key

**Problem:** `set_fred_api_key()` calls `Sys.setenv()` and `fred_search_remote()`
calls `fredr::fredr_set_key()` — both are process-wide globals. On a
multi-user server, one session's key overwrites another's.

**Fix:** Store the key in `session$userData` and pass it explicitly to all
FRED functions. Never write to global env or the fredr package state.

Files to change:
- `R/data_search.R` — `current_fred_api_key()`, `set_fred_api_key()`,
  `fred_search_available()`, `fred_search_remote()`, `search_fred_series()`
- `R/chart_helpers.R` — `fred_vintage_choice_values()`, `fred_vintage_dates()`,
  `fred_data()`
- `R/providers.R` — `provider_fred_query_series_history()`,
  `provider_fred_search_remote()`
- `R/main_app.R` — all call sites; extract key from `session$userData` at
  reactive boundary

Pattern:
```r
# data_search.R
current_fred_api_key <- function(session) {
  trimws(session$userData$fred_api_key %||% "")
}
set_fred_api_key <- function(api_key, session) {
  session$userData$fred_api_key <- trimws(api_key %||% "")
}
fred_search_available <- function(session) {
  nzchar(current_fred_api_key(session))
}

# Pure functions receive fred_key as plain string arg (default "")
fred_search_remote <- function(query, type = "text", fred_key = "") {
  fredr::fredr_series_search_text(query, ..., api_key = fred_key)
  # Never call fredr::fredr_set_key()
}
```

In `build_main_server`, seed from env var so server-level keys still work:
```r
session$userData$fred_api_key <- trimws(Sys.getenv("FRED_API_KEY"))
```

See `doco/fred_session_scoping_plan.md` for full call-site mapping.

### 1.2 Make chart library paths configurable

**Problem:** `chart_library_path()` returns a relative path (`data/chart_library.rds`).
On a server, the working directory isn't reliable and a redeploy would
overwrite the `data/` folder.

**Fix:** `chart_library_path()` already checks `getOption("data_viewer.chart_library_path")`.
Set this option to an absolute path on the server before the app loads.

In `/etc/shiny-server/shiny-server.conf` or a site-specific `Rprofile`:
```r
options(
  data_viewer.chart_library_path       = "/srv/data_viewer/persist/chart_library.rds",
  data_viewer.chart_presentation_library_path = "/srv/data_viewer/persist/chart_presentations.rds"
)
```

Create and permission the directory:
```bash
sudo mkdir -p /srv/data_viewer/persist
sudo chown shiny:shiny /srv/data_viewer/persist
```

Copy the current library across on first deploy:
```bash
sudo cp data/chart_library.rds       /srv/data_viewer/persist/
sudo cp data/chart_presentations.rds /srv/data_viewer/persist/
sudo chown shiny:shiny /srv/data_viewer/persist/*.rds
```

### 1.3 Exclude Bloomberg

Bloomberg requires a running terminal session — it cannot work on a remote
server. The provider registry already uses conditional loading; confirm
`provider_bloomberg` is never registered unless a Bloomberg terminal is
detected, and that its absence degrades gracefully (no error, just hidden
from source picker).

Verify: on a machine without Bloomberg, the app starts cleanly and Bloomberg
does not appear as a selectable source.

### 1.4 Pin R package dependencies

No `renv.lock` exists. Before deploying, initialise renv so the server
installs the exact same package versions:

```r
renv::init()   # captures current library snapshot
renv::snapshot()
```

Commit `renv.lock` and `renv/activate.R`. On the server, run:
```r
renv::restore()
```

### 1.5 Python environment

Python is used for Plotly PNG/SVG export and PPTX generation.
The app detects `python3` via `Sys.which("python3")` and fails gracefully
if absent — but export features will be disabled.

For full functionality, install a virtualenv on the server:

```bash
python3 -m venv /srv/data_viewer/venv
/srv/data_viewer/venv/bin/pip install plotly kaleido python-pptx
```

Set in the app's environment file (see §3.3):
```
RETICULATE_PYTHON=/srv/data_viewer/venv/bin/python3
```

---

## 2. Infrastructure Setup

### 2.1 Provision the VPS

- Provider: Hetzner Cloud (CX22 — 2 vCPU, 4 GB RAM, 40 GB SSD, ~€4/month)
- OS: Ubuntu 24.04 LTS
- Add SSH key at creation; disable password auth

```bash
ssh-copy-id root@<server-ip>
# Disable password auth in /etc/ssh/sshd_config:
# PasswordAuthentication no
```

Create a non-root deploy user:
```bash
adduser deploy
usermod -aG sudo deploy
```

### 2.2 Install R

```bash
# Add CRAN's Ubuntu repository for latest R
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
  | sudo gpg --dearmor -o /usr/share/keyrings/r-project.gpg
echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] https://cloud.r-project.org/bin/linux/ubuntu noble-cran40/" \
  | sudo tee /etc/apt/sources.list.d/r-project.list
sudo apt update
sudo apt install -y r-base r-base-dev

# System libraries needed by R packages
sudo apt install -y \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libgit2-dev
```

### 2.3 Install Shiny Server OSS

```bash
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.22.1017-amd64.deb
sudo dpkg -i shiny-server-*.deb
sudo systemctl enable shiny-server
sudo systemctl start shiny-server
```

### 2.4 Install R packages

As root (or the shiny user), restore from renv.lock:

```bash
sudo su - shiny
cd /srv/data_viewer/app
Rscript -e "renv::restore()"
```

If renv isn't yet in use, install manually:
```r
install.packages(c(
  "shiny", "here", "dplyr", "tidyr", "ggplot2", "plotly",
  "lubridate", "stringr", "scales", "htmlwidgets", "htmltools",
  "shinyWidgets", "DT", "tibble", "readr",
  "reticulate", "ragg", "systemfonts",
  "fredr", "readabs", "readrba", "rdbnomics",
  "renv"
))
# Optional:
install.packages(c("seasonal", "x13binary"))
```

---

## 3. App Deployment

### 3.1 Deploy the app

Shiny Server serves apps from `/srv/shiny-server/` by default.

```bash
sudo mkdir -p /srv/shiny-server/data_viewer
sudo chown shiny:shiny /srv/shiny-server/data_viewer
```

Deploy method (pick one):
- **Git pull** (recommended): clone the repo directly into `/srv/shiny-server/data_viewer`
- **rsync**: `rsync -av --exclude='.git' ./ deploy@<server>:/srv/shiny-server/data_viewer/`

### 3.2 Shiny Server config

`/etc/shiny-server/shiny-server.conf`:

```
run_as shiny;

server {
  listen 3838;

  location /data_viewer {
    site_dir /srv/shiny-server/data_viewer;
    log_dir /var/log/shiny-server;
    directory_index off;
  }
}
```

Nginx will sit in front on ports 80/443 and proxy to 3838 — don't expose 3838 publicly.

### 3.3 Environment variables for the Shiny process

Create `/etc/shiny-server/env`:
```bash
# FRED key — optional; seeds the per-session default
# Leave blank to require all users to enter their own key
FRED_API_KEY=

# Python virtualenv for Plotly export and PPTX
RETICULATE_PYTHON=/srv/data_viewer/venv/bin/python3

# Chart library paths (absolute, on persistent volume)
# Set via R options in site Rprofile instead (see §3.4)
```

Reference in `shiny-server.conf`:
```
environment_vars /etc/shiny-server/env;
```

### 3.4 Site Rprofile (R options)

Create `/etc/R/Rprofile.site` (or append to it):
```r
options(
  data_viewer.chart_library_path =
    "/srv/data_viewer/persist/chart_library.rds",
  data_viewer.chart_presentation_library_path =
    "/srv/data_viewer/persist/chart_presentations.rds"
)
```

This file is sourced by every R process on the server, including the Shiny
worker processes.

---

## 4. Nginx Reverse Proxy + HTTPS

### 4.1 Install Nginx and Certbot

```bash
sudo apt install -y nginx certbot python3-certbot-nginx
```

### 4.2 Nginx site config

`/etc/nginx/sites-available/data_viewer`:

```nginx
server {
    listen 80;
    server_name your.domain.com;
    return 301 https://$host$request_uri;
}

server {
    listen 443 ssl;
    server_name your.domain.com;

    ssl_certificate     /etc/letsencrypt/live/your.domain.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/your.domain.com/privkey.pem;
    include             /etc/letsencrypt/options-ssl-nginx.conf;
    ssl_dhparam         /etc/letsencrypt/ssl-dhparams.pem;

    # Shiny requires WebSocket support
    location / {
        proxy_pass         http://127.0.0.1:3838/data_viewer/;
        proxy_http_version 1.1;
        proxy_set_header   Upgrade $http_upgrade;
        proxy_set_header   Connection "upgrade";
        proxy_set_header   Host $host;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
    }
}
```

```bash
sudo ln -s /etc/nginx/sites-available/data_viewer /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

### 4.3 Issue TLS certificate

```bash
sudo certbot --nginx -d your.domain.com
# Certbot auto-renews via systemd timer — verify:
sudo systemctl status certbot.timer
```

---

## 5. FRED Key UX for Public Users

No server-level FRED key is needed. The flow:

1. User visits the app
2. They switch to the Search tab or select a FRED source
3. The existing modal prompts: "Enter FRED key"
4. They paste their key (free from fred.stlouisfed.org)
5. Key is stored in their session only — never persisted, never shared

The "Enter FRED key" button in the header remains visible at all times so
users can update their key.

After the session-scoping fix (§1.1), each user's key is fully isolated.

---

## 6. Firewall

```bash
sudo ufw allow OpenSSH
sudo ufw allow 'Nginx Full'
# Do NOT open port 3838 — Nginx proxies to it internally
sudo ufw enable
```

---

## 7. Ongoing Operations

### Updating the app

```bash
cd /srv/shiny-server/data_viewer
git pull
sudo systemctl restart shiny-server
```

### Logs

```bash
# Shiny Server logs
tail -f /var/log/shiny-server/*.log

# Nginx logs
tail -f /var/log/nginx/access.log /var/log/nginx/error.log
```

### Search index

The search index RDS files (~11.5 MB total) are static and ship with the
repo. If ABS/RBA metadata changes, regenerate locally and redeploy:

```r
source("scripts/generate_search_index.R")
```

### Backups

The only mutable state is the chart library in `/srv/data_viewer/persist/`.
Back it up daily:

```bash
# Cron job on server:
0 3 * * * cp /srv/data_viewer/persist/chart_library.rds \
             /srv/data_viewer/backups/chart_library_$(date +\%Y\%m\%d).rds
```

---

## 8. Deployment Checklist

### Code (local, before deploy)
- [ ] FRED key is session-scoped (§1.1) — no `Sys.setenv`, no `fredr_set_key`
- [ ] Chart library paths use the configurable option (§1.2)
- [ ] Bloomberg provider absent/disabled without a terminal (§1.3)
- [ ] `renv.lock` committed (§1.4)
- [ ] App starts cleanly with `shiny::runApp()` locally

### Server
- [ ] VPS provisioned, SSH key only, non-root user created
- [ ] R + Shiny Server OSS installed
- [ ] R packages installed via `renv::restore()`
- [ ] Python venv created with plotly, kaleido, python-pptx (§1.5)
- [ ] `/srv/data_viewer/persist/` created, owned by shiny, seeded with initial RDS files
- [ ] `/etc/R/Rprofile.site` sets chart library paths
- [ ] `/etc/shiny-server/env` sets RETICULATE_PYTHON
- [ ] App deployed to `/srv/shiny-server/data_viewer/`
- [ ] Shiny Server running, app accessible on port 3838 locally
- [ ] Nginx installed, site config in place, `nginx -t` passes
- [ ] TLS certificate issued via Certbot
- [ ] App accessible via HTTPS on public domain
- [ ] Port 3838 not publicly reachable (firewall)
- [ ] Backup cron job for chart library

### Smoke test
- [ ] App loads at `https://your.domain.com`
- [ ] ABS, RBA, DBnomics searches return results
- [ ] FRED modal appears on first FRED search; key entry works
- [ ] Two concurrent sessions use independent FRED keys
- [ ] Chart save/load round-trips correctly
- [ ] Plotly PNG export works (if Python env configured)
