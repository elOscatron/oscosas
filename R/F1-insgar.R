#' Title instalación de paquetes
#'
#' DEPENDE DE fector
#' Instalar y cargar paquetes a la vez, requiere de fector que requiere de stringr
#' @param paquetes
#' @param actu
#'
#' @return
#' @export
#'
#' @examples
insgar <- function(paquetes, actu = FALSE) {
  PAQUETES <- fector(paquetes)
  BiocManager::install(PAQUETES, update = actu)
  lapply(PAQUETES, require, character.only = TRUE)
}
