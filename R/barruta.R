#' Cambio de barra \ por / en rutas copiadas
#'
#' @param final
#'
#' @return
#' @export
#'
#' @examples Copiar cualquier ruta del navegador de windows y ejecutar la función
barruta <- function(final = TRUE) {
  paste0(gsub(pattern = '\\\\', replacement = '/', x = readClipboard()),if (final == TRUE) {'/'})
}
