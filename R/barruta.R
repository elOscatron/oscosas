#' Cambio de barra \ por / en rutas copiadas
#'
#' Coge el texto del portapapeles, la idea es que sea una dirección de carpeta, y le cambia las barras \ por /. Además 'corrige' ese mismo texto en el portapapeles de forma que si se usa varias veces seguidas no pasa nada
#' Idea de aquí: https://stackoverflow.com/questions/17605563/efficiently-convert-backslash-to-forward-slash-in-r
#' @param final Si se quiere que al final se añada también una barra /
#'
#' @return
#' @export
#'
#' @examples Copiar cualquier ruta del navegador de windows y ejecutar la función
barruta <- function(final = TRUE) {
  if (grepl(pattern = '\\\\',x = readClipboard())) {
  VECTORi <- paste0(gsub(pattern = '\\\\', replacement = '/', x = readClipboard()),if (final == TRUE) {'/'})
  } else {
    VECTORi <- readClipboard()
  }
  writeClipboard(as.character(VECTORi))
  return(VECTORi)
}
