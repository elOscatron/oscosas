# fector: Funci?n para hacer vetores facilmente, escribo entre comillas los elementos separados por comas con o sin espacios y me los da como un vector separado de verdad. Con la actualizaci?n, puedo poner comas o espacios repetidos y/o comas iniciales y finales y me las elimina
#' Title
#'
#' @param VECTOR
#'
#' @return
#' @export
#'
#' @examples
fector <- function(VECTOR) {
  patrones <- c(',{2,}', ', +,')
  for (PATRON in patrones) {
    TEMP <- gsub(pattern = PATRON, replacement = ',' , x = VECTOR)
    VECTOR <- TEMP
  }
  VECTOR <- gsub(pattern = ',$', replacement = '', x =
                   gsub(pattern = '^,', replacement = '', x = VECTOR))
  vectorizado <- unlist(stringr::str_split(string = VECTOR, pattern = ' *, *'))
  return(vectorizado)
}
