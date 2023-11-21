#' Title instalación de paquetes
#'
#' DEPENDE DE fector
#' Instalar y cargar paquetes a la vez, requiere de fector que requiere de stringr. Si alguno no se ha podido cargar,
#' se verá un mesaje con los que no se han podido y diciendo que se intente manualmente
#' @param paquetes
#' @param actu
#'
#' @return
#' @export
#'
#' @examples
insgar <- function(paquetes, actu = FALSE) {
  Package <- fector(paquetes)
  suppressPackageStartupMessages(suppressMessages(suppressWarnings(BiocManager::install(Package, update = actu))))
  Loaded <- unlist(lapply(Package, require, character.only = TRUE))
  TABLAs <- data.frame(Package, Loaded)
  if(sum(TABLAs$Loaded == FALSE) != 0) {
    print(paste0('The following packages have not been installed, try to install them manually:',
                 paste0(TABLAs$Package[TABLAs$Loaded == FALSE], collapse = '; ')))
  } else{
    print(paste0('All ',length(Package) ,' packages were correctly installed.'))
  }
}
