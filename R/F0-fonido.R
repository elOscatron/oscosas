#' fonido, hace ruido, es simplemente una implementación de beepr
#'
#' @param sonido Elegir el nombre del sonido, por defecto el bip
#' @param veces cuantas veces se quiere que el sonido suene, por defecto 1
#' @param intervalo cada cuanto tiempo se quiere que suene, por devecto, cada 2,5 segundos
#' @param disponibles si se quiere ver los sonidos disponibles propios más dle paquete original
#'
#' @return
#' @export
#'
#' @examples
#'
#' fonido()
#' fonido(sonido = 'ping')
#' fonido(3)
#' fonido(disponibles = TRUE)
#'
fonido <- function(sonido = 'bip', veces = 1, intervalo = 2.5, disponibles = FALSE) {
  # Establezco donde buscar (mi paquete) y nombro el archivo añadiendo el .wav
  donde <- system.file(paste("rmarkdown/resources/sonidos"), package = "oscosas")
  sonido2 <- paste0(sonido, '.wav')
  # Busco si está entre los disponibles, si no, se buscará entre los de beepr
  if(sonido2 %in% list.files(donde)) {
    sonido3 <- paste0(donde, '/', sonido, '.wav')
  } else {
    sonido3 <- sonido
  }
  # Si he marcado la opción 'disponibles', las mostraré y pararé la ejecución
  if (disponibles) {
    return(c(gsub(pattern = '.wav', replacement = '', list.files(donde)), c('ping','coin','fanfare','complete','treasure','ready','shotgun','mario','wilhelm','facebook','sword')))
  }
  # Reproduzco el audio elegido las veces especificadas en el intervalo dado
  for(atomo in seq(veces)){
    beepr::beep(sound = sonido3)
    if(veces >1) {
      Sys.sleep(intervalo)}
  }
}
