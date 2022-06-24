#' Secuencias de números de largo
#'
#' Reconoce el tipo de objeto introducido: tabla (matriz, tabla, array), lista, vector o palabra y devuelve la secuencia de elementos que lo componen
#'
#' @param OBJETOe Objeto del que se quiere sacar la secuencia numérica
#' @param columnas Por defecto FALSE, si se pone TRUE y es un tipo de tabla, se devolverá el número de columnas, no de filas
#' @param seq Por defecto TRUE, si se pone FALSe, en vez de la secuencia enseña el total, como length() o derivados
#'
#' @return
#' @export
#'
#' @examples
numcuencia <- function(OBJETOe, columnas = FALSE, seq = TRUE) {
  tablas <- c('matrix','array','data.frame')
  if (any(tablas %in% class(OBJETOe))) {
    tipo <- 'tabla'
    longitud <- if(columnas) {
      ncol(OBJETOe)
    } else {
      nrow(OBJETOe)}
  } else {
    if(class(OBJETOe) == 'character') {
      if(length(OBJETOe) == 1) {
        tipo <- 'palabra'
        longitud <- nchar(OBJETOe)
      } else {
        tipo <- 'vector'
        longitud <- length(OBJETOe)}}
    if(class(OBJETOe) == 'list') {
      tipo <- 'lista'
      longitud <- length(OBJETOe)}}
  if(seq) {return(seq(longitud))
  } else {
    return(longitud)}
}
