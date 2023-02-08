#' Silenciar funciones
#' Sacada de aquí: https://stackoverflow.com/questions/34208564/how-to-hide-or-disable-in-function-printed-message
#' @param x
#'
#' @return
#' @export
#'
#' @examples
silencio <- function(x) {
  sink(tempfile())
  on.exit(sink())
  suppressWarnings(invisible(force(x)))
}
