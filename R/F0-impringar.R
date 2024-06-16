#' Imprimir y pegar mezcladas
#'
#' Permite combinar las funciones print(pasteo()) que van juntas tan a menudo, he conservado todos los argumentos de paste0()
#'
#' @param ...
#' @param collapse
#' @param recycle0
#' @param verbose Si quiero que la cosa simplemente se imprima o que se guarde
#' @param presalto Si quiero añadir un salto de linea previo a la impresión, por defecto no
#' @param salto Si quiero añadir un salto de linea posterior a la impresión, por defecto no
#' @param presep Si quiero añadir un separador antes de la impresión (y después del presalto). Por defecto NULL, debe ser un vector.
#' @param sep Si quiero añadir un separador después de la impresión (y antes del salto). Por defecto NULL, debe ser un vector.
#'
#'
#' @return El vector pegado
#' @export
#'
#' @examples
impringar <- function(..., presep = NULL, sep = NULL, presalto = FALSE, salto = FALSE, collapse = NULL, recycle0 = FALSE, save = NULL, verbose = TRUE) {
  # Guardo un vector con los elementos pegados usando los brillantes argumentos originales de paste0
  VECTORs <- paste0(..., collapse = collapse, recycle0 = recycle0)
  # Si verbose es cierto simplemente imprimo el vector, si no, lo devuelvo para un objeto u otra función
  if (verbose) {
    if(presalto) { # Si añado un salto previo
      cat('\n')}
    if(!is.null(presep)) { # Si añado un separador previo
      print(presep)}
    print(VECTORs)
    if(!is.null(sep)) { # Si añado un separador posterior
      print(sep)}
    if(salto) { # Si añado un salto posterior
      cat('\n')}
  }
  # Devolver el vector independientemente del valor de verbose
  invisible(VECTORs)
}
