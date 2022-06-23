#' Crear vectores facilmente a partir de texto
#'
#' Función para hacer vetores facilmente, escribo entre comillas los elementos separados por comas (o lo que especifique en el argumento 'sep') con o sin espacios y me los da como un vector separado de verdad. Con la actualización, puedo poner separadores o espacios repetidos y/o separadores iniciales y finales y me los elimina
#'
#' @param VECTOR
#' @param sep Por defecto es una coma, pero puede ser cualquier cosa. Lo creé para los modelos que vienen separados por un +
#'
#' @return
#' @export
#'
#' @examples
fector <- function(VECTOR, sep = ',') {
  sep <- paste0('\\',sep)
  patrones <- c(paste0(sep,'{2,}'), paste0(sep,' +',sep))
  for (PATRON in patrones) {
    TEMP <- gsub(pattern = PATRON, replacement = sep , x = VECTOR)
    VECTOR <- TEMP
  }
  VECTOR <- gsub(pattern = paste0(sep,'$'), replacement = '', x =
                   gsub(pattern = paste0('^',sep), replacement = '', x = VECTOR))
  vectorizado <- unlist(stringr::str_split(string = VECTOR, pattern = paste0(' *',sep,' *')))
  return(vectorizado)
}
