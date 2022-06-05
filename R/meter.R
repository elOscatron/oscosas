#' Meter datos externos con el portapapeles
#'
#' @param tipo Por defecto Vector, puede ser Tabla o Lista (este no está escrito, sería si no se pone nada)
#' @param filasN Si se pone TRUE, en caso de que sea tabla, se tomará la primera columna como nombres de filas
#' @param columnasN Si se pone TRUE, en caso de que sea una tabla se tomará la primera fila como nombres de las columnas
#'
#' @return
#' @export
#'
#' @examples
meter <- function(tipo = 'Vector', filasN = FALSE, columnasN = FALSE) {
  # Si es una tabla inicio los condicionales para filas y columnas, además quito los espaios en blanco
  if (tipo == 'Tabla') {
    OBJETOs <- suppressWarnings(read.delim(file = 'clipboard', header = columnasN))
    if (filasN) {
      rownames(OBJETOs) <- OBJETOs[,1]
      OBJETOs[1,] <- NULL
    }
    OBJETOs <- facias(OBJETOs)
    # Si no es una tabla lo trato como vector y doy opción de transformarlo en lista
  } else {
    TABLAe <- suppressWarnings(read.delim(file = 'clipboard', header = FALSE))
    LISTAs <- c()
    for (fila in seq(nrow(TABLAe))) {
      FILA <- TABLAe[fila,]
      LISTAs <- append(LISTAs, c(FILA))
      OBJETOs <- LISTAs
      if(tipo == 'Vector') {
        OBJETOs <- unlist(OBJETOs)
      }}}
  return(OBJETOs)
}
