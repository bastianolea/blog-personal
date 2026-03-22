library(magick)
library(ggplot2)
library(patchwork)
library(ggview)
library(here)
library(glue)

año <- 2025
color_fondo <- "#26222E"

# imágenes de la carpeta
carpeta <- here(glue("posts/libros/libros_{año}/portadas"))
archivos <- list.files(carpeta, full.names = TRUE)

# función que agrega las imágenes a gráficos
imagen_a_ggplot <- function(ruta) {
  img        <- magick::image_read(ruta)
  info       <- magick::image_info(img)
  ancho      <- info$width
  alto       <- info$height
  raster_img <- as.raster(img)
  
  ggplot() +
    annotation_raster(raster_img, 
                      xmin = 0, xmax = ancho, ymin = 0, ymax = alto) +
    xlim(0, ancho) + ylim(0, alto) +
    coord_fixed(ratio = 1) + 
    theme_void()
}

# aplicar función
plots <- lapply(archivos, imagen_a_ggplot)

# unir gráficos en un mosaico
mosaico <- wrap_plots(plots, ncol = 6) &
  theme(plot.background = element_rect(fill = color_fondo, colour = NA),
        panel.background = element_rect(fill = color_fondo, colour = NA))

# previsualizar
mosaico +
  canvas(width = 10, 
         height = 8,
         units = "in", 
         bg = color_fondo)

# guardar mosaico
ruta_mosaico <- here(glue("posts/libros/libros_{año}/bastimapache_mosaico_libros_{año}.jpg"))

save_ggplot(plot = last_plot(), ruta_mosaico)

# bajar la calidad de la imagen
image_write(
  image_read(ruta_mosaico),
  path = ruta_mosaico,
  format = "jpeg",
  quality = 60)
