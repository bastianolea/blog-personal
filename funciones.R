# función para generar estrellas
estrellas <- function(rating, 
                      color_punto = "#E2B842", 
                      color_vacio = "#8F748E") {
  simbolo <- "\u2605"
  
  # por cada número del 1 al 5, hace un span con una estrella y revisa si es parte del puntaje o no, y le asigna un color y una clase; luego retorna los 5 elementos
  estrellas <- map(1:5, \(i) {
    
    # color dependiendo de si está dentro del rating
    color <- if_else(i <= rating, 
                     color_punto, 
                     color_vacio)
    
    # estrella individual
    span(simbolo,
         class = "estrella",
         style = glue("color: {color};")
    )
  })
  
  # salida con las 5 estrellas
  div(class = "estrellas", estrellas)
}


# convertir números de mes a palabras
meses <- function(numero) {
  case_when(
    numero == 1 ~ "enero",
    numero == 2 ~ "febrero",
    numero == 3 ~ "marzo",
    numero == 4 ~ "abril",
    numero == 5 ~ "mayo",
    numero == 6 ~ "junio",
    numero == 7 ~ "julio",
    numero == 8 ~ "agosto",
    numero == 9 ~ "septiembre",
    numero == 10 ~ "octubre",
    numero == 11 ~ "noviembre",
    numero == 12 ~ "diciembre"
  )
}