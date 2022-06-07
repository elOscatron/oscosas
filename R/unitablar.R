#' Eliminación de filas y/o columnas repetidas
#'
#' Se introduce una tabla y da la opción de eliminar filas y/o columnas repetidas, por defecto eliminará ambas y dará los nombres
#'
#' @param TABLAe
#' @param columnas
#' @param filas
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
unitablar <- function(TABLAe, columnas = TRUE, filas = TRUE, verbose = TRUE) {
  CUALESc <- duplicated(as.list(TABLAe))
  NOMBRESc <- colnames(TABLAe)[CUALESc]
  CUALESf <- duplicated(TABLAe)
  NOMBRESf <- rownames(TABLAe)[CUALESf]
  TABLAs <- TABLAe[if(filas) {!CUALESf} else {seq(nrow(TABLAe))},
                   if(columnas) {!CUALESc} else {seq(ncol(TABLAe))}]
  if (verbose) {print(paste0('Filas: ', if(length(NOMBRESf) == 0) {'Ninguna'} else {paste0(NOMBRESf, collapse = '; ')}, '. Columnas: ', if(length(NOMBRESc) == 0) {'Ninguna'} else {paste0(NOMBRESc, collapse = '; ')}))}
  return(TABLAs)
}
