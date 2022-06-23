#' Quitar filas y columnas indicadas
#'
#' Se da un vector de filas y/o columnas por nombre y las elimina de la tabla
#'
#' @param TABLAm
#' @param columnas Nomnbres de columnas a quitar
#' @param filas Nombres de filas a quitar
#'
#' @return
#' @export
#'
#' @examples
ficofu <- function(TABLAm, columnas = NULL, filas = NULL) {
  COLUMNAS <- fector(columnas)
  FILAS <- fector(filas)
  if (!is.null(COLUMNAS)) {
    TABLAm <- TABLAm[,!colnames(TABLAm) %in% COLUMNAS]}
  if (!is.null(FILAS)) {
    TABLAm <- TABLAm[!row.names(TABLAm) %in% FILAS,]}
  return(TABLAm)
}
