#' Imprimir y pegar mezcladas
#'
#' Permite combinar las funciones print(pasteo()) que van juntas tan a menudo, he conservado todos los argumentos de paste0()
#'
#' @param ...
#' @param collapse
#' @param recycle0
#' @param verbose Si quiero que la cosa simplemente se imprima o que se guarde
#'
#' @return El vector pegado
#' @export
#'
#' @examples
impringar <- function(..., collapse = NULL, recycle0 = FALSE, verbose = TRUE) {
  # Guardo un vector con los elementos pegados usando los brillantes argumentos originales de paste0
  VECTORs <- paste0(..., collapse = collapse, recycle0 = recycle0)
  # Si verbose es cierto simplemente imprimo el vector, si no, lo devuelvo para un objeto u otra función
  if (verbose) {print(VECTORs)
  } else {
    return(VECTORs)}
}
