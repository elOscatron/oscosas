#' Medición del tiempo avanzada (inicial)
#'
#' @param mostrar
#' @param grado
#'
#' @return
#' @export
#'
#' @examples
tic <- function(mostrar = FALSE, grado = 0) {
  nombreI <- if (grado != 0) {paste0('tinicio',grado)} else {'tinicio'}
  assign(x = nombreI, value = Sys.time(), envir = .GlobalEnv)
  if (mostrar) {print(get(nombreI))}
}
