library(readr)
library(exiftoolr)
library(fs)
library(dplyr)
library(purrr)
library(janitor)
library(sf)
library(tidygeocoder)

# cargar datos
imagenes <- dir_info("posts/paisajes/fotos/")

# extraer metadatos de imágenes
metadata <- exif_read(imagenes$path)

metadata <- metadata |>
  as_tibble() |>
  clean_names()

# glimpse(metadata)

coordenadas <- metadata |>
  select(
    ruta = source_file,
    fecha = date_time_original,
    lat = gps_latitude,
    lon = gps_longitude
  ) |>
  filter(!is.na(lat) & !is.na(lon)) |>
  arrange(desc(fecha))

message(paste(nrow(coordenadas), "fotos"))

# geocodificación inversa con OpenStreetMap (Nominatim)
# zoom ~14 = nivel de localidad, para obtener la zona y no la calle/edificio
geo <- coordenadas |>
  reverse_geocode(
    lat = lat,
    long = lon,
    method = "osm",
    full_results = TRUE,
    custom_query = list(zoom = 14)
  ) |>
  clean_names()

geo |>
  glimpse()

geo |>
  select(ruta, quarter, town)

geo |>
  slice(10) |>
  glimpse()

# nombre de zona: preferir el nombre del lugar más cercano (`name`); si falta,
# coalescer campos de asentamiento de más a menos específico (sin usar la calle).
campos_zona <- c(
  "name",
  "hamlet",
  "isolated_dwelling",
  "village",
  "town",
  "suburb",
  "quarter",
  "neighbourhood",
  "city_district",
  "city",
  "municipality",
  "county",
  "province",
  "state"
)
campos_presentes <- intersect(campos_zona, names(geo))

datos <- geo |>
  mutate(
    nombre = basename(ruta),
    lugar = coalesce(!!!syms(campos_presentes)),
    # calle = if ("road" %in% names(geo_calle)) geo_calle$road else NA_character_,
    direccion = address,
    comuna = city,
    provincia = county,
    region = state
  ) |>
  select(
    ruta,
    nombre,
    fecha,
    lat,
    lon,
    lugar,
    comuna,
    provincia,
    region,
    direccion
  )

datos |>
  select(lugar, direccion)

# reportar fotos sin zona resuelta (para revisión manual)
sin_lugar <- datos |> filter(is.na(lugar)) |> nrow()

if (sin_lugar > 0) {
  message(paste(sin_lugar, "fotos sin lugar resuelto"))
} else {
  message("Todas las fotos tienen un lugar resuelto")
}


# renombrar archivos según el lugar ----
datos <- datos |>
  mutate(
    extension = path_ext(ruta),
    # nombre limpio a partir del lugar y la fecha (garantiza unicidad)
    nombre_nuevo = make_clean_names(paste(lugar, fecha)),
    nombre_nuevo = make.unique(nombre_nuevo, sep = "_"),
    ruta_nueva = paste(nombre_nuevo, extension, sep = "."),
    ruta_nueva = path(path_dir(ruta), ruta_nueva)
  )

datos |>
  select(ruta, ruta_nueva) |>
  glimpse()

# mover cada archivo a su nombre nuevo (omitir los que ya lo tienen)
mover <- datos |> filter(ruta != ruta_nueva)
if (nrow(mover) > 0) {
  file_move(mover$ruta, mover$ruta_nueva)
  message(paste(nrow(mover), "archivos renombrados"))
}

# cambiar tabla para apuntar a nombres nuevos
datos <- datos |>
  mutate(ruta_antigua = ruta) |>
  mutate(ruta = as.character(ruta_nueva), nombre = nombre_nuevo) |>
  select(-contains("nuevo"))

datos |> glimpse()

# convertir a sf
puntos <- datos |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

readr::write_rds(puntos, "posts/paisajes/paisajes.rds")
