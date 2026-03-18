# Chartwell — Brand & Design Specification
*For implementation by Codex*

---

## 1. Product identity

**Name:** Chartwell
**Subtitle:** Macro & Financial Charts
**Purpose:** Explore time series financial and economic data. Users search, build, and save macro and financial charts from sources including ABS, RBA, FRED, and CPI data.

---

## 2. Typography

### Display / Wordmark

- **Font:** Playfair Display
- **Weight:** 900 (Black)
- **Use:** App name "Chartwell" in header, logo lockup
- **Google Fonts import:** `https://fonts.googleapis.com/css2?family=Playfair+Display:wght@700;900&display=swap`

### Body / UI

- **Font:** Plus Jakarta Sans
- **Weights:** 400 (Regular), 500 (Medium), 600 (SemiBold)
- **Use:** Everything — UI prose, table content, search results, form labels, navigation tabs, buttons, inputs, subtitle "MACRO & FINANCIAL CHARTS", data labels, axis tick labels, metadata
- **Google Fonts import:** `https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600&display=swap`

---

## 3. Colour palette

### Primary palette

| Token | Hex | Usage |
|---|---|---|
| `--color-bg-primary` | `#f5f8f4` | Main app background (sage off-white) |
| `--color-bg-secondary` | `#edf2ec` | Cards, panels, sidebar backgrounds |
| `--color-bg-dark` | `#0d1420` | Dark mode base, dark header/nav |
| `--color-bg-navy` | `#1a2540` | Dark panels, chart builder sidebar |

### Brand blue (primary accent)

| Token | Hex | Usage |
|---|---|---|
| `--color-blue-deep` | `#0d1420` | Dark text on light backgrounds |
| `--color-blue-primary` | `#2a55a0` | Subtitle text, links, active states |
| `--color-blue-mid` | `#4f7ec9` | Chart trend lines, icons, buttons |
| `--color-blue-light` | `#7aaef5` | Chart highlight dots, hover states |
| `--color-blue-area` | `#d6e4f7` | Chart area fill (use at 80% opacity) |
| `--color-blue-grid` | `#c8d4e8` | Chart grid lines, axis lines |

### Borders

| Token | Hex | Usage |
|---|---|---|
| `--color-border-sage` | `#dde6db` | Card borders on sage background |
| `--color-border-dark` | `#1e3a6e` | Grid lines / structural lines on dark bg |
| `--color-border-navy` | `#2a4070` | Borders on navy/slate backgrounds |

### Text

| Token | Hex | Usage |
|---|---|---|
| `--color-text-primary` | `#0d1420` | Body text on light backgrounds |
| `--color-text-secondary` | `#4a5568` | Muted labels, metadata |
| `--color-text-on-dark` | `#ffffff` | Text on dark/navy backgrounds |
| `--color-text-subtitle` | `#4f7ec9` | Subtitle/tag text on dark backgrounds |

### CSS custom properties block (paste into `:root`)

```css
:root {
  /* Backgrounds */
  --color-bg-primary:   #f5f8f4;
  --color-bg-secondary: #edf2ec;
  --color-bg-dark:      #0d1420;
  --color-bg-navy:      #1a2540;

  /* Brand blue */
  --color-blue-deep:    #0d1420;
  --color-blue-primary: #2a55a0;
  --color-blue-mid:     #4f7ec9;
  --color-blue-light:   #7aaef5;
  --color-blue-area:    #d6e4f7;
  --color-blue-grid:    #c8d4e8;

  /* Borders */
  --color-border-sage:  #dde6db;
  --color-border-dark:  #1e3a6e;
  --color-border-navy:  #2a4070;

  /* Text */
  --color-text-primary:   #0d1420;
  --color-text-secondary: #4a5568;
  --color-text-on-dark:   #ffffff;
  --color-text-subtitle:  #4f7ec9;

  /* Typography */
  --font-display: 'Playfair Display', Georgia, serif;
  --font-body:    'Plus Jakarta Sans', system-ui, sans-serif;
}

/* Apply body font globally */
body, button, input, select, textarea {
  font-family: var(--font-body);
}
```

---

## 4. Logo & icon

Logo files are provided as SVGs. Reference them as static assets.

| File | Use |
|---|---|
| `chartwell-logo-light.svg` | Full lockup — use on sage/light backgrounds |
| `chartwell-logo-dark.svg` | Full lockup — use on dark/navy backgrounds |
| `chartwell-icon.svg` | Icon mark only — light, for favicon/app icon |
| `chartwell-icon-dark.svg` | Icon mark only — dark variant |

### Logo lockup markup (light)

```html
<header class="app-header">
  <img src="/assets/chartwell-logo-light.svg" alt="Chartwell — Macro & Financial Charts" height="48" />
</header>
```

### Logo lockup markup (dark header)

```html
<header class="app-header app-header--dark">
  <img src="/assets/chartwell-logo-dark.svg" alt="Chartwell — Macro & Financial Charts" height="48" />
</header>
```

### Inline SVG wordmark (if preferred over image)

```html
<!-- Load fonts first -->
<link href="https://fonts.googleapis.com/css2?family=Playfair+Display:wght@900&family=Plus+Jakarta+Sans:wght@400;500;600&display=swap" rel="stylesheet">

<!-- Wordmark -->
<div class="chartwell-wordmark">
  <span class="chartwell-wordmark__name">Chartwell</span>
  <span class="chartwell-wordmark__sub">Macro &amp; Financial Charts</span>
</div>

<style>
.chartwell-wordmark {
  display: flex;
  flex-direction: column;
  gap: 4px;
}
.chartwell-wordmark__name {
  font-family: 'Playfair Display', Georgia, serif;
  font-weight: 900;
  font-size: 28px;
  letter-spacing: -0.02em;
  line-height: 1;
  color: var(--color-text-primary);
}
.chartwell-wordmark__sub {
  font-family: 'Plus Jakarta Sans', system-ui, sans-serif;
  font-weight: 500;
  font-size: 9px;
  letter-spacing: 0.22em;
  text-transform: uppercase;
  line-height: 1;
  color: var(--color-blue-primary);
}
</style>
```

---

## 5. App header / navigation

Replace the current "Data Explorer" text header with the Chartwell wordmark. The existing nav tab structure (Data Search / Chart Builder / Saved Charts) should be retained.

```html
<nav class="app-nav">
  <div class="app-nav__brand">
    <img src="/assets/chartwell-logo-light.svg" alt="Chartwell" height="40" />
  </div>
  <div class="app-nav__tabs">
    <a href="/search"  class="nav-tab">Data Search</a>
    <a href="/builder" class="nav-tab nav-tab--active">Chart Builder</a>
    <a href="/saved"   class="nav-tab">Saved Charts</a>
  </div>
  <div class="app-nav__copy">
    <span>© Chartwell 2026</span>
  </div>
</nav>
```

---

## 6. Chart styling

Update chart defaults to match the brand palette.

### Trend line
- Stroke: `#4f7ec9` (`--color-blue-mid`)
- Stroke width: 2px
- Line cap: round
- Line join: round

### Area fill (under line)
- Fill: `#d6e4f7` (`--color-blue-area`)
- Opacity: 0.8

### Grid lines
- Stroke: `#c8d4e8` (`--color-blue-grid`)
- Stroke width: 0.8px

### Axis lines
- Stroke: `#c8d4e8`
- Stroke width: 0.8px

### Data point highlight dot
- Outer circle fill: `#4f7ec9`
- Inner circle fill: background colour (sage or white)
- Outer radius: 4px, inner radius: 2px

### Chart background
- Background: `#f5f8f4` (sage) or transparent

---

## 7. Favicon

Use `chartwell-icon.svg` as the favicon. Add to `<head>`:

```html
<link rel="icon" type="image/svg+xml" href="/assets/chartwell-icon.svg">
<link rel="alternate icon" href="/assets/favicon.ico">
<meta name="theme-color" content="#f5f8f4">
```

---

## 8. Page title

```html
<title>Chartwell — Macro & Financial Charts</title>
```

---

## 9. Summary of changes for Codex

1. Replace all instances of "Data Explorer" with "Chartwell" throughout the codebase.
2. Update `<title>` to "Chartwell — Macro & Financial Charts".
3. Add the four SVG logo files to `/public/assets/` (or equivalent static folder).
4. Import Playfair Display (900) and Plus Jakarta Sans (400, 500, 600) from Google Fonts.
5. Add the CSS custom properties block to the global stylesheet `:root`, and apply `font-family: var(--font-body)` globally to `body`, `button`, `input`, `select`, `textarea`.
   - 5a. Replace all existing references to Inter, system-ui, DM Mono, or any other font with `var(--font-body)` / Plus Jakarta Sans throughout the codebase.
6. Replace the header wordmark with the SVG logo lockup.
7. Update chart default styles (line colour, area fill, grid lines) to brand palette.
8. Set favicon to `chartwell-icon.svg`.
9. Set `theme-color` meta tag to `#f5f8f4`.
