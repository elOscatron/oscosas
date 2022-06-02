#' Eliminación de filas duplicadas
#'
#' @param tabla La tabla a introducir con filas repetidas
#'
#' @return Una versión de la tabla sin esas filas repetidas
#' @export
#'
#' @examples
funicas <- function(tabla) {
  TABLAm <- tabla
  TABLAm$Identificador <- ''
  for (fila in seq(nrow(TABLAm))) {
    FILA <- TABLAm[fila,]
    TABLAm$Identificador[fila] <- paste0(FILA, collapse = '')
  }
  TABLAs <- TABLAm[!duplicated(TABLAm$Identificador),]
  TABLAs$Identificador <- NULL
  return(TABLAs)
}
