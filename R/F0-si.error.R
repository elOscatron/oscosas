#' Detecta errores
#' equivalente a su función homónima de excel. Es la segunda vez que la hago porque la borrE sin querer.
#' Esta vez la he juntado con ESERROR tambiEn de excel porque con la opciOn de un mensaje personalizado
#' o funciOn, valen para lo mismo.
#' Me he basado en esto: https://stackoverflow.com/questions/31214361/what-is-the-r-equivalent-for-excel-iferror
#'
#' ChatGPT: La función si.error tiene como objetivo ejecutar el código especificado en intento y controlar
#' si produce algún error o no. Si se produce un error o el resultado es de longitud cero, la función
#' devuelve EsError. Si no se produce un error, la función devuelve intento o, si se especifica,
#' NoEsError. La ejecución del código se hace con la función try, y se establece el argumento silent
#' en TRUE para evitar que se muestren los mensajes de error en la consola.
#'
#' @param intento Qué cosa se quiere probar
#' @param EsError  Anteriormente llamado Alternativa. Código a correr si la cosa falla.
#' @param NoEsError Anteriormente llamado mensajeNEG. Código a correr, si la cosa NO da error, por defecto NA. Aunque aquí
#' haya algo que no sea NA, la función principal se ejecutará y si crea un objeto, este se creará
#' @param warnings Por defecto FALSe, si se pone TRUE, las advertencias se tratarán como errores también
#'
#' @return
#' @export
#'
#' @examples
si.error <- function(intento, EsError = TRUE, NoEsError = NA, warnings = FALSE) {
  OBJt <- suppressWarnings(try(intento, silent = TRUE))
  warning_msg <- NULL
  if (warnings) {
    warning_msg <- capture.output(suppressWarnings(intento))
    if (length(warning_msg) > 0) {
      OBJt <- suppressWarnings(try(invisible(force(intento)), silent = TRUE))
    }}
  if (class(OBJt) == "try-error" || length(OBJt) == 0 || length(warning_msg) > 0) {
    EsError
  } else {
    if (is.na(NoEsError)) {
      intento
    } else {
      NoEsError
    }}}
