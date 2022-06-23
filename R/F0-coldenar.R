#' Ordenar columnas de una tabla
#'
#' Función para ordenar rapidamente las columnas de una tabla, alfabéticamente, por lo largo de los nombres o por ambos. También se puede especificar si quiero una primera o última especial. Si se ordena por ambas cosas, la longitud tiene preferencia, pero siempre se puede usar la función dos veces si se quiere al revés.
#'
#' @param tabla Tabla que se quiere ordenar
#' @param alfabetico Por defecto TRUE, si se quiere que sea por orden alfabético decreciente (a - z), si se quiere que sea al revés se puede poner FALSE, si se quiere que no se tenga en cuenta se pondrá NO
#' @param largo Por defecto FALSE, así que se pondrán antes las más largas si se quieren antes las más cortas se pondrá TRUE, para no tenerlo en cuenta, NO.
#' @param primero Si hay alguna columna o columnas que se quieran poner primero. Por defecto ninguna
#' @param ultimo Si hay alguna columna o columnas que se quieran poner al final. Por defecto ninguna
#'
#' @return Tabla con columnas reordenadas
#' @export
#'
#' @examples
coldenar <- function(tabla, alfabetico = TRUE, largo = FALSE, primero = NA, ultimo = NA) {
  especiales <- c(primero, ultimo)
  columnisP <- colnames(tabla)
  columnis <- columnisP[!columnisP %in% especiales]

  print('Columnas preordenadas: '); print(columnisP)
  ALFA <- if (alfabetico != 'NO') {
    sort(columnis, decreasing = alfabetico)
  } else {columnis}
  LARGO <- if (largo != 'NO') {
    ALFA[order(nchar(ALFA), decreasing = largo)]
  } else {ALFA}
  ordenP <- c(primero, LARGO, ultimo)
  orden <- ordenP[!is.na(ordenP)]
  print('Columnas ordenadas: '); print(orden)

  return(tabla[,orden])
}
