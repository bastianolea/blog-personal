# migrate_pdfs.R
# Migra los PDFs enlazados desde WordPress al blog Quarto local.
#
# Fases:
#   1. Descubrimiento: encuentra todos los enlaces a PDFs de WordPress en los posts
#   2. Descarga: baja los PDFs a la carpeta de cada post
#   3. Reemplazo: actualiza los enlaces en los archivos markdown
#
# Cambia `dry_run <- TRUE` a FALSE para ejecutar los cambios reales.

library(tidyverse)
library(fs)

# ── Configuración ──────────────────────────────────────────────────────────────

dry_run    <- TRUE   # TRUE = solo muestra qué haría; FALSE = descarga y modifica archivos
posts_dir  <- here::here("posts")
scripts_dir <- here::here("scripts")

wp_pattern <- "https?://bastian\\.olea\\.biz/wp-content/uploads/[^)\"'\\s]+\\.pdf"


# ── FASE 1: Descubrimiento ─────────────────────────────────────────────────────

cat("=== FASE 1: Descubrimiento ===\n\n")

# Encontrar todos los archivos markdown del blog
md_files <- dir_ls(posts_dir, recurse = TRUE, regexp = "\\.(md|qmd)$")

# Extraer todas las apariciones de URLs de WordPress PDF en cada archivo
pdf_links <- map(md_files, function(file) {
  content <- read_file(file)
  urls <- str_extract_all(content, wp_pattern)[[1]]
  if (length(urls) == 0) return(NULL)
  tibble(
    file_path = as.character(file),
    wordpress_url = urls
  )
}) |>
  compact() |>
  list_rbind()

if (nrow(pdf_links) == 0) {
  cat("No se encontraron enlaces a PDFs de WordPress. Nada que hacer.\n")
  stop("Script terminado: no hay PDFs que migrar.", call. = FALSE)
}

# Construir la tabla completa con rutas destino
pdf_table <- pdf_links |>
  # Normalizar a pares únicos (un archivo puede enlazar al mismo PDF más de una vez)
  distinct(file_path, wordpress_url) |>
  mutate(
    post_dir     = path_dir(file_path),
    pdf_filename = path_file(wordpress_url),
    local_dest   = path(post_dir, pdf_filename),
    # El enlace relativo es simplemente el nombre del archivo (mismo directorio)
    new_link     = pdf_filename
  )

cat(sprintf("Archivos con PDFs de WordPress: %d\n", n_distinct(pdf_table$file_path)))
cat(sprintf("URLs únicas encontradas:        %d\n", n_distinct(pdf_table$wordpress_url)))
cat(sprintf("Pares únicos (archivo, PDF):    %d\n\n", nrow(pdf_table)))

print(pdf_table |> select(post_dir, pdf_filename, wordpress_url))

# Guardar tabla de descubrimiento
discovery_path <- path(scripts_dir, "pdf_links_found.csv")
write_csv(pdf_table, discovery_path)
cat(sprintf("\nTabla guardada en: %s\n\n", discovery_path))


# ── FASE 2: Descarga de PDFs ───────────────────────────────────────────────────

cat("=== FASE 2: Descarga de PDFs ===\n\n")

download_results <- pdf_table |>
  mutate(
    already_exists = file_exists(local_dest),
    status         = NA_character_,
    message        = NA_character_
  )

for (i in seq_len(nrow(download_results))) {
  row <- download_results[i, ]

  if (row$already_exists) {
    cat(sprintf("[%d/%d] Ya existe: %s\n", i, nrow(download_results), row$pdf_filename))
    download_results$status[i]  <- "skipped"
    download_results$message[i] <- "Archivo ya existe localmente"
    next
  }

  cat(sprintf("[%d/%d] Descargando: %s\n", i, nrow(download_results), row$pdf_filename))
  cat(sprintf("         Desde: %s\n", row$wordpress_url))
  cat(sprintf("         Hacia: %s\n", row$local_dest))

  if (dry_run) {
    download_results$status[i]  <- "dry_run"
    download_results$message[i] <- "No descargado (dry_run = TRUE)"
    cat("         [DRY RUN] Se omitiría la descarga\n")
    next
  }

  result <- tryCatch({
    download.file(
      url      = row$wordpress_url,
      destfile = row$local_dest,
      mode     = "wb",
      quiet    = TRUE
    )
    list(ok = TRUE, msg = "Descargado correctamente")
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })

  if (result$ok) {
    download_results$status[i]  <- "ok"
    download_results$message[i] <- result$msg
    cat("         OK\n")
  } else {
    download_results$status[i]  <- "error"
    download_results$message[i] <- result$msg
    cat(sprintf("         ERROR: %s\n", result$msg))
  }
}

# Resumen de descargas
cat("\n--- Resumen de descargas ---\n")
download_results |>
  count(status) |>
  print()

# Guardar log
log_path <- path(scripts_dir, "pdf_download_log.csv")
write_csv(download_results, log_path)
cat(sprintf("\nLog guardado en: %s\n\n", log_path))


# ── FASE 3: Reemplazo de enlaces en markdown ───────────────────────────────────

cat("=== FASE 3: Reemplazo de enlaces ===\n\n")

# Solo actualizar enlaces de PDFs que se descargaron correctamente (o que ya existían)
pdfs_ok <- download_results |>
  filter(status %in% c("ok", "skipped"))

if (nrow(pdfs_ok) == 0 && !dry_run) {
  cat("No hay PDFs descargados correctamente. No se modifica ningún archivo.\n")
  stop("Script terminado: sin PDFs descargados.", call. = FALSE)
}

# En dry_run también mostramos los reemplazos que se harían
pdfs_to_replace <- if (dry_run) download_results else pdfs_ok

# Agrupar por archivo para procesar cada uno una sola vez
files_to_update <- pdfs_to_replace |>
  group_by(file_path) |>
  summarise(
    replacements = list(setNames(new_link, wordpress_url)),
    .groups = "drop"
  )

for (i in seq_len(nrow(files_to_update))) {
  fp           <- files_to_update$file_path[i]
  replacements <- files_to_update$replacements[[i]]

  original_content <- read_file(fp)
  updated_content  <- original_content

  for (j in seq_along(replacements)) {
    old_url  <- names(replacements)[j]
    new_link <- replacements[j]
    updated_content <- str_replace_all(updated_content, fixed(old_url), new_link)
  }

  n_replaced <- length(replacements)
  changed    <- !identical(original_content, updated_content)

  if (!changed) {
    cat(sprintf("Sin cambios: %s\n", path_rel(fp, here::here())))
    next
  }

  if (dry_run) {
    cat(sprintf("[DRY RUN] Se reemplazarían %d enlace(s) en: %s\n",
                n_replaced, path_rel(fp, here::here())))
  } else {
    write_file(updated_content, fp)
    cat(sprintf("Actualizado (%d enlace(s)): %s\n",
                n_replaced, path_rel(fp, here::here())))
  }
}

cat("\n=== Listo ===\n")
if (dry_run) {
  cat("Ejecuta el script con `dry_run <- FALSE` para aplicar los cambios.\n")
} else {
  cat("Todos los PDFs han sido descargados y los enlaces actualizados.\n")
}
