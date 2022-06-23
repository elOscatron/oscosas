#' funci?n para crear tablas facilmente poniendo el n?mero de filas y/o columnas o su nombre
#'
#' @param columnas
#' @param filas
#' @param filasN
#'
#' @return
#' @export
#'
#' @examples
fabla <- function(columnas = NULL, filas = '', filasN = TRUE) {
  columnas <- if(length(which(is.na(suppressWarnings(as.numeric(columnas)) == TRUE))) != 0)
  {fector(columnas)} else {as.numeric(columnas)}
  filas <- if(length(which(is.na(suppressWarnings(as.numeric(filas)) == TRUE))) != 0)
  {fector(filas)} else {as.numeric(filas)}

  CL <- if (class(columnas) == 'numeric') {columnas} else {length(columnas)}
  FL <- if (class(filas) == 'numeric') {filas} else {length(filas)}
  tabla <- data.frame(matrix(ncol = CL, nrow = FL))
  colnames(tabla) <- if (class(columnas) == 'numeric') {seq(columnas)} else {columnas}
  if (filasN == TRUE) {
    row.names(tabla) <- if (class(filas) == 'numeric') {seq(filas)} else {filas}}
  return(tabla)
}
