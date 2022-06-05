#' Desactiva los paquetes que he cargado.
#' No necesita argumentos
#' @return
#' @export
#'
#' @examples
descargar <- function() {
  basicos <- c("package:stats","package:graphics","package:grDevices",
               "package:utils","package:datasets",
               "package:methods","package:base")
  cargados <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,TRUE,FALSE)]
  cargados <- setdiff(cargados, basicos)
  if (length(cargados) > 0)  {
    for (package in cargados) {
      detach(package, character.only=TRUE)}}
}
