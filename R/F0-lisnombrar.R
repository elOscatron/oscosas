#' Crea una lista con los objetos introducidos y les pone su nombre original
#' Útil cuando se quieren guardar variables entre chuncs o documentos

#' @param ...
#'
#' @return
#' @export
#'
#' @examples
listnombrar <- function(...) {
  nombres <- as.character(substitute(list(...)))[-1L]
  objetos <- list(...)
  setNames(objetos, nombres)
}
