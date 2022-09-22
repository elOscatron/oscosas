#' Quitar filas y columnas vacías
#' Puedo elegir si quiero quitar, columnas y/o filas vacías, el criterio es que esten compuestas por NA y/o '' únicamente
#' @param TABLAe
#' @param filas
#' @param columnas
#'
#' @return
#' @export
#'
#' @examples
facias <- function(TABLAe, filas = TRUE, columnas = TRUE){
  # Quitar´filas vacías
  if (filas) {
    TABLAs <- data.frame()
    for (fila in seq(nrow(TABLAe))) {
      FILA <- TABLAe[fila,]
      # Solo las filas que no estén compuestas enteramente de NA o '' se añaden
      if (!all(FILA == '' | is.na(FILA))) {
        TABLAs <- rbind(TABLAs, FILA)
      }}}
  # Quitar columnas vacias

  if (columnas) {
    TABLAe <- TABLAs
    VECTORs <- c()
    for (columna in seq(ncol(TABLAe))) {
      COLUMNA <- TABLAe[,columna]
      # Solo el identificador de la columna no estén compuestas enteramente de NA o '' se añaden al vector
      if (!all(COLUMNA == '' | is.na(COLUMNA))) {
        VECTORs <- append(VECTORs, columna)
      }}
    # Uso el vactor identificador para rehacer la tabla
    TABLAs <- TABLAe[,VECTORs]
  }
  # Si no pongo el data.frame() y solo queda una fila o columna
  return(data.frame(TABLAs))
}
