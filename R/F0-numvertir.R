#' Convertir en númericas todas las columnas posibles de una tabla
#'
#' Coge una tabla y transforma todas las columnas que puedas ser transformadas en num?ricas en num?ricas
#' https://stackoverflow.com/questions/32846176/applying-as-numeric-only-to-elements-of-a-list-that-can-be-coerced-to-numeric-i
#' @param tabla
#'
#' @return
#' @export
#'
#' @examples
numvertir <- function(tabla){
  COPIA <- colnames(tabla)
  TABLA <- data.frame(lapply(tabla, type.convert, as.is = TRUE))
  colnames(TABLA) <- COPIA
  return(TABLA)
}
