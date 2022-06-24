#' Medición del tiempo avanzada (final)
#'
#' Dependiente de si.error(), da el tiempo que ha pasado entre ella y el último 'tic', además lo almacena en una tabla que creará de no existir. Por defecto mostrará el intervalo en formato sencillo, sin indicar inicio o fin
#' @param tipo simple (por defecto), completo o numero. Muestra el tipo de
#' @param unidades Qué tipo de unidades temporales va a mostrar, por defecto es 'auto' y usa las de la función difftime()
#' @param mostrar por defecto FALSE, indica si quiere que se vea el resultado
#' @param digitos Cuantos digitos mostrar en las unidades de tiempo, por defecto 3
#' @param grado útil si hay varios tic() iniciados, pues se empareja con el que se diga, por defecto 0
#' @param eliminarI Por defecto TRUE, especifica si hay que eliminar su pareja tic()
#' @param eliminarTT Eliminar tabla temporal, por defecto FALSE, permitiría eliminar la tabla con los tiempos guardados
#'
#' @return
#' @export
#'
#' @examples
tac <- function(tipo = 'simple', unidades = 'auto', mostrar = FALSE, digitos = 3, grado = 0, eliminarI = TRUE, eliminarTT = FALSE) {
  nombreI <- if (grado != 0) {paste0('tinicio',grado)} else {'tinicio'}
  nombreF <- if (grado != 0) {paste0('tifin',grado)} else {'tifin'}
  tTIEMPO <- si.error(intento = tTIEMPO, alternativa = fabla(columnas = 'Inicio, Fin, Diferencia'))
  assign(x = nombreF, value = Sys.time(), envir = .GlobalEnv)
  INIt <- get(nombreI)
  FINt <- get(nombreF)

  tiferencia <- capture.output(round(x = difftime(time1 = FINt, time2 = INIt, units = unidades, ), digits = digitos))
  tTIEMPO <- rbind(tTIEMPO, c(as.character(INIt), as.character(FINt), tiferencia))
  rownames(tTIEMPO) <- NULL
  tTIEMPO <<- tTIEMPO[!is.na(tTIEMPO$Inicio),]

  resultado <- if (tipo == 'simple' | tipo == TRUE) {
    round(x = difftime(time1 = FINt, time2 = INIt, units = unidades, ), digits = digitos)
  } else {
    if (tipo == 'completo') {
      paste0('Inicio: ', INIt,'. Fin: ', FINt, '. Diferencia: ',
             paste0(strsplit(x = tiferencia, split = ' ')[[1]][4:5], collapse = ' '))
    } else {
      if (tipo == 'numero') {as.numeric(strsplit(x = tiferencia, split = ' ')[[1]][4])}
    }}
  if (mostrar) {print(get(nombreF))}
  rm(list = c(nombreF), envir = .GlobalEnv)
  if (eliminarI) {rm(list = c(nombreI), envir = .GlobalEnv)}
  if (eliminarTT) {rm(tTIEMPO, envir = .GlobalEnv)}
  return(resultado)
}
