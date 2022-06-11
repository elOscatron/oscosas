#' Detecta errores
#' equivalente a su función homónima de excel. Es la segunda vez que la hago porque la borr? sin querer. Esta vez la he juntado con ESERROR tambi?n de excel porque con la opci?n de un mensaje personalizado o funci?n, valen para lo mismo. Me he basado en esto: https://stackoverflow.com/questions/31214361/what-is-the-r-equivalent-for-excel-iferror
#'
#' @param intento Qué cosa se quiere probar
#' @param alternativa Que hacer si la cosa falla
#' @param mensajeNEG Mensaje que dar si la cosa da error, por defecto TRUE
#'
#' @return
#' @export
#'
#' @examples
si.error <- function(intento, alternativa = TRUE, mensajeNEG = NA) {
  OBJt <- try(intento, silent = TRUE)
  if (class(OBJt) == "try-error" | length(OBJt) == 0) {alternativa} else {
    if (is.na(mensajeNEG)) {intento
    } else {
      mensajeNEG}}
}

