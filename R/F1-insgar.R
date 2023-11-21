#' Title instalación de paquetes
#'
#' DEPENDE DE fector
#' Instalar y cargar paquetes a la vez, requiere de fector que requiere de stringr. Si alguno no se ha podido cargar, se verá en una tabla
#' @param paquetes
#' @param actu
#'
#' @return
#' @export
#'
#' @examples
insgar <- function(paquetes, actu = FALSE) {
  Package <- fector(paquetes)
  BiocManager::install(Package, update = actu)
  Loaded <- unlist(lapply(Package, require, character.only = TRUE))
  TABLAs <- data.frame(Package, Loaded)
  if(sum(TABLAs$Loaded == FALSE) != 0) {
    show(TABLAs[TABLAs$Loaded == FALSE,])
  }
}
