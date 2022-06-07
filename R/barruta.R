#' Cambio de barra \ por / en rutas copiadas
#'
#' Idea de aquí: https://stackoverflow.com/questions/17605563/efficiently-convert-backslash-to-forward-slash-in-r
#' @param final
#'
#' @return
#' @export
#'
#' @examples Copiar cualquier ruta del navegador de windows y ejecutar la función
barruta <- function(final = TRUE) {
  paste0(gsub(pattern = '\\\\', replacement = '/', x = readClipboard()),if (final == TRUE) {'/'})
}
