library(readr)
library(exiftoolr)
library(fs)
library(dplyr)
library(purrr)
library(janitor)
library(sf)
library(tidygeocoder)

# cargar el acumulado previo (si existe), para no re-geocodificar
rds <- "posts/paisajes/paisajes.rds"

if (file_exists(rds)) {
  previos <- read_rds(rds) |>
    st_transform(4326) |>
    mutate(
      # coordenadas explícitas para poder reconstruir el sf tras consolidar
      lat = st_coordinates(geometry)[, "Y"],
      lon = st_coordinates(geometry)[, "X"]
    ) |>
    st_drop_geometry()

  # esquemas antiguos podían no traer nombre_original
  if (!"nombre_original" %in% names(previos)) {
    previos$nombre_original <- basename(previos$ruta)
  }
} else {
  previos <- NULL
}

# fotos nuevas por procesar (solo imágenes, ignorar .DS_Store y similares)
imagenes <- dir_info("posts/paisajes/nuevas/") |>
  filter(path_ext(path) %in% c("jpg", "jpeg", "png", "heic", "tif", "tiff"))

hay_nuevos <- nrow(imagenes) > 0

if (!hay_nuevos) {
  message("No hay fotos nuevas que procesar")
} else {
  # extraer metadatos de imágenes
  metadata <- exif_read(imagenes$path)

  metadata <- metadata |>
    as_tibble() |>
    clean_names()

  coordenadas <- metadata |>
    select(
      ruta = source_file,
      fecha = date_time_original,
      lat = gps_latitude,
      lon = gps_longitude
    ) |>
    filter(!is.na(lat) & !is.na(lon)) |>
    arrange(desc(fecha))

  message(paste(nrow(coordenadas), "fotos nuevas"))

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
      nombre_original = basename(ruta),
      lugar = coalesce(!!!syms(campos_presentes)),
      direccion = address,
      comuna = city,
      provincia = county,
      region = state
    ) |>
    select(
      ruta,
      nombre_original,
      fecha,
      lat,
      lon,
      lugar,
      comuna,
      provincia,
      region,
      direccion
    )

  # reportar fotos sin zona resuelta (para revisión manual)
  sin_lugar <- datos |> filter(is.na(lugar)) |> nrow()

  if (sin_lugar > 0) {
    message(paste(sin_lugar, "fotos sin lugar resuelto"))
  }

  # renombrar archivos según el lugar y moverlos a fotos/ ----
  datos <- datos |>
    mutate(
      extension = path_ext(ruta),
      # nombre limpio a partir del lugar y la fecha (garantiza unicidad)
      nombre = make_clean_names(paste(lugar, fecha)),
      nombre = make.unique(nombre, sep = "_"),
      ruta_final = path("posts/paisajes/fotos", paste0(nombre, ".", extension))
    )

  # mover cada archivo a fotos/ con su nombre nuevo
  file_move(datos$ruta, datos$ruta_final)
  message(paste(nrow(datos), "archivos movidos a fotos/"))

  datos <- datos |>
    mutate(ruta = as.character(ruta_final)) |>
    select(
      ruta,
      nombre_original,
      nombre,
      extension,
      fecha,
      lat,
      lon,
      lugar,
      comuna,
      provincia,
      region,
      direccion
    )
}

# consolidar acumulado previo + nuevos
puntos <- bind_rows(previos, if (hay_nuevos) datos else NULL)

puntos <- puntos |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

readr::write_rds(puntos, rds)
