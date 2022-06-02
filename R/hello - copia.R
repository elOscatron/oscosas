
# funicas, se queda con las filas únicas de una tabla
funicas <- function(tabla) {
  TABLAm <- tabla
  TABLAm$Identificador <- ''
  for (fila in seq(nrow(TABLAm))) {
    FILA <- TABLAm[fila, ]
    TABLAm$Identificador[fila] <- paste0(FILA, collapse = '')
  }
  TABLAs <- TABLAm[!duplicated(TABLAm$Identificador), ]
  TABLAs$Identificador <- NULL
  return(TABLAs)
}
