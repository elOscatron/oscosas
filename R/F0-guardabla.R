#' Guardar tabla multiformatos
#'
#' En desarrollo, especifico que tabla quiero guardar
#'
#' @param nombreT la tabla que voy a guardar, usará el nombre del objeto
#' @param donde Por defecto será el directorio de trabajo, si no, se puede especificar donde se quiere que sea. Si le falta la barra final '/', se la añade solo
#' @param formatoT Por defecto "(%Y-%m-%d-%H-%M)", especifica el formato de la fecha que se quiere poner
#' @param formatoA Por defecto 'xlsx', se puede poner ptencialmente otros. Si falta el punto se le pone
#'
#' @return
#' @export
#'
#' @examples
guardabla <- function(nombreT, donde = NA, formatoT = "(%Y-%m-%d-%H-%M)", formatoA = 'xlsx') {
  vNOM <-deparse(substitute(nombreT))
  if(is.na(donde)) {donde <- getwd()}
  if(!grepl(pattern = '/$', x = donde)) {donde <- paste0(donde,'/')}
  tiempo <- format(Sys.time(), format = formatoT)
  if(!grepl(pattern = '\\.',x = formatoA)) {
    formatoA <- paste0('.',formatoA)}
  if(grepl(pattern = 'xlsx', x = formatoA)) {
    write.xlsx(x = nombreT, file = paste0(donde, vNOM, tiempo, formatoA))}
  }
