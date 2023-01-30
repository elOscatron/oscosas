#' Para fusionar dos tablas uniendo sus filas sin importar que tengan distintas columnas. Da información sobre que columnas son las comunes y diferentes si se quiere
#'
#' @param TABLAe1 Primera tabla a fusionar.
#' @param TABLAe2 Segunda tabla a fusionar.
#' @param infoC Logical para indicar si se desea información sobre las columnas comunes y exclusivas (por defecto `FALSE`).
#' @return Devuelve una tabla con las filas de las dos tablas unidas.
#' @export
#'
#' @examples
#' TABLA1 <- data.frame(x=1:5, y=6:10)
#' TABLA2 <- data.frame(x=6:10, z=11:15)
#' fusiabla(TABLA1, TABLA2, infoC=TRUE)
#'
fusiabla <- function(TABLAe1, TABLAe2, infoC = FALSE) {
  TABLAm1 <- TABLAe1
  TABLAm2 <- TABLAe2
  # Obtener nombres de columnas comunes
  COMUNES <- intersect(colnames(TABLAm1), colnames(TABLAm2))

  # Obtener nombres de columnas exclusivas para cada tabla
  DIF1 <- setdiff(colnames(TABLAm1), COMUNES)
  DIF2 <- setdiff(colnames(TABLAm2), COMUNES)

  # Añadir las columnas exclusivas a las tablas auxiliares
  for (columna in DIF2) {
    TABLAm1[columna] <- NA}
  for (columna in DIF1) {
    TABLAm2[columna] <- NA}

  # Unir las dos tablas auxiliares
  RESULTADO <- rbind(TABLAm1, TABLAm2)

  if (infoC != FALSE) {
    primsalto(list(c('Columnas comunes: ', if(infoC == "numero") {length(COMUNES)
    } else {
      if(infoC == "ambos") {c(length(COMUNES), ', llamadas: ', paste0(COMUNES, collapse = ", "))
      } else {
        paste0(COMUNES, collapse = ", ")}}),
    c('Columnas comunes: ', if(infoC == "numero") {length(TABLAe1)
    } else {
      if(infoC == "ambos") {c(length(DIF1), ', llamadas: ', paste0(DIF1, collapse = ", "))
      } else {
        paste0(DIF1, collapse = ", ")}}),
    c('Columnas comunes: ', if(infoC == "numero") {length(TABLAe2)
    } else {
      if(infoC == "ambos") {c(length(DIF2), ', llamadas: ', paste0(DIF2, collapse = ", "))
      } else {
        paste0(DIF2, collapse = ", ")}})))
  }
  return (RESULTADO)
}
