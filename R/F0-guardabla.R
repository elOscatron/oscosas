#' Guardar tabla multiformatos
#'
#' En desarrollo, especifico que tabla quiero guardar
#'
#' @param nombreT El nombre de la tabla que voy a guardar
#' @param donde Por defecto será el directorio de trabajo, si no, se puede especificar donde se quiere que sea. Si le falta la barra final '/', se la añade solo
#' @param formatoT Por defecto "(%Y-%m-%d-%H-%M)", especifica el formato de la fecha que se quiere poner
#' @param formatoA Por defecto 'xlsx', se puede poner ptencialmente otros. Si falta el punto se le pone
#'
#' @return
#' @export
#'
#' @examples
guardabla <- function(nombreT, donde = NA, formatoT = "(%Y-%m-%d-%H-%M)", formatoA = 'xlsx') {
  if(is.na(donde)) {donde <- getwd()}
  if(!grepl(pattern = '/$', x = donde)) {donde <- paste0(donde,'/')}
  tiempo <- format(Sys.time(), format = formatoT)
  if(!grepl(pattern = '\\.',x = formatoA)) {
    formatoA <- paste0('.',formatoA)}
  if(grepl(pattern = 'xlsx', x = formatoA)) {
    xlsx::write.xlsx(x = get(nombreT), file = paste0(donde, nombreT, tiempo, formatoA))}
}
