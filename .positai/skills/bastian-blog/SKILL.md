---
name: bastian-blog
description: Estructura, convenciones y configuración del blog personal de Bastián Olea Herrera (bastimapache.cl), construido con Quarto. Usar cuando se trabaje con archivos de este blog, se agreguen o modifiquen posts, o se requiera contexto sobre su organización, temas, extensiones o sistema de plantillas.
---

# Blog personal de Bastián Olea Herrera

Sitio web Quarto en `/Users/baolea/R/blog-personal`. URL: https://bastimapache.cl. Deploy en Netlify, output en `docs/`.

## Archivos clave

| Archivo | Rol |
|---|---|
| `_quarto.yml` | Configuración global del sitio |
| `tema.scss` | Estilos personalizados (553 líneas) |
| `posts/_metadata.yml` | Opciones aplicadas a todos los posts |
| `funciones.R` | Helpers R reutilizables (`estrellas()`, `meses()`) |
| `goatcounter.html` | Analytics (incluido en `include-after-body`) |
| `datos/goodreads_library_export.csv` | Exportación de Goodreads |

## Estructura de posts

Los posts viven en `posts/` organizados en subcarpetas por fecha o categoría:

```
posts/
├── 2016/, 2017/, ..., 2024/     # posts antiguos (importados de WordPress)
├── 2026-03-15/                  # posts recientes (YYYY-MM-DD)
├── brevet/                      # posts de ciclismo
├── libros/YYYY/                 # reseñas de libros
└── musica/                      # posts de música
```

Cada post es una carpeta con `index.md` o `index.qmd`.

### Frontmatter de un post típico

```yaml
---
title: "Título"
date: 2026-03-15
categories:
  - categoría1
  - categoría2
tags:           # etiquetas heredadas de WordPress (no visibles por defecto)
  - tag1
  - tag2
---
```

`categories` se usa como agrupación visible en el listado y en el header. `tags` existe en posts importados de WordPress y se muestran al lado de la fecha, como texto.

## Configuración global (`_quarto.yml`)

- **Lang:** `es`
- **Tema Bootstrap:** minty + brand + `tema.scss`
- **Navbar:** Inicio, Blog, R (enlace externo), Ahora
- **Redes sociales en navbar:** Instagram, TikTok, Twitter, LinkedIn
- **Búsqueda:** overlay
- **Footer:** 3 columnas (copyright, navegación, créditos)
- **Social:** Twitter card + Open Graph en español

## `posts/_metadata.yml`

```yaml
freeze: true
title-block-banner: true
```

`title-block-banner: true` activa el banner de título (fondo de color con título, subtítulo, categorías) seguido de la metadata (fecha, etc.) en la zona inferior del header.

## Tema y estilos (`tema.scss`)

Paleta oscura con tonos rosa/púrpura:

| Variable | Valor | Uso |
|---|---|---|
| `$background` | `#2F2935` | Fondo principal |
| `$background2` | `#26222E` | Fondo navbar/footer |
| `$foreground` | `#F0BFE7` | Texto principal (rosa claro) |
| `$foreground2` | `#DD2594` | Texto secundario (magenta) |
| `$background3` | `#A389A3` | Acento terciario |

**Tipografía:** Poppins (títulos) + Work Sans (cuerpo).

Clases personalizadas destacadas:
- `.foto`, `.fotito`, `.galeria` — galería de imágenes
- `.cuadricula` — grid de libros
- `.centrar` para centrar divs

## Extensiones activas

| Extensión | Shortcode | Uso |
|---|---|---|
| `dragonstyle/share-post` | `{{< share-post >}}` | Embeds de redes sociales (Threads, Instagram, Twitter/X, Pinterest, LinkedIn, Mastodon) |
| `quarto-ext/fontawesome` | `{{< fa icon_name >}}` | Iconos FontAwesome 6.7.2 |

## Sistema de template partials (HTML)

Quarto permite sobreescribir partials del template HTML con `template-partials` en el YAML. Los partials relevantes para el header de posts son:

- `banner/title-block.html` — banner con título, subtítulo, descripción, categorías
- `title-metadata.html` — metadata bajo el banner (fecha, autor, DOI, etc.)

El partial `title-metadata.html` usa sintaxis Pandoc template (`$if(campo)$...$endif$`). El bloque de fecha sirve como modelo para agregar campos personalizados:

```html
$if(date)$
<div>
  <div class="quarto-title-meta-heading">$labels.published$</div>
  <div class="quarto-title-meta-contents">
    <p class="date">$date$</p>
  </div>
</div>
$endif$
```

El partial personalizado del proyecto está en `title-metadata.html` (raíz) y se referencia desde `posts/_metadata.yml`:

```yaml
format:
  html:
    template-partials:
      - ../../title-metadata.html
```

## Páginas principales

| Archivo | Descripción |
|---|---|
| `index.qmd` | Inicio — template `jolla`, edad calculada con R |
| `blog.qmd` | Listado de posts con RSS feed |
| `ahora.qmd` | Página "Now" con actividad actual |

## Categorías usadas

ciclismo · música · libros · programación · tecnología · lugares · fotos · videos · política · sociología · destacado
