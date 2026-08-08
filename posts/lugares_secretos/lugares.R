library(readr)
library(exiftoolr)
library(fs)
library(dplyr)
library(purrr)
library(janitor)
library(sf)
library(tidyverse)

# cargar datos
imagenes <- dir_info("posts/lugares_secretos/fotos/")
textos <- read_tsv("posts/lugares_secretos/lugares.tsv")

# extraer metadatos de imágenes
metadata <- exif_read(imagenes$path)

metadata <- metadata |>
  as_tibble() |>
  clean_names()

# glimpse(metadata)

coordenadas <- metadata |>
  select(
    path = source_file,
    fecha = date_time_original,
    lat = gps_latitude,
    lon = gps_longitude
  ) |>
  filter(!is.na(lat) & !is.na(lon)) |>
  arrange(desc(fecha))

message(paste(nrow(coordenadas), "lugares secretos"))

# unir metadatos con textos
datos <- coordenadas |>
  mutate(filename = basename(path)) |>
  left_join(textos, join_by(filename))

sin_textos <- datos |> filter(is.na(texto)) |> nrow()

if (sin_textos > 0) {
  stop("Imágenes sin texto!")
}

# convertir a sf
puntos <- datos |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

readr::write_rds(puntos, "posts/lugares_secretos/lugares.rds")
