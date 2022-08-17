#' Sacar datos de R al portapapeles
#'
#' @param objeto Que objeto se quiere sacar
#' @param size No se que es esto, heredado de Pablo, pero se queda en 4096
#' @param tipo Por defecto 'Tabla', si no es una tabla será un vector y se puede poner 'Vector', pero con no poner 'Tabla' vale
#' @param filasN De ser una tabla, si se quiere que aparezcan los nombres de las filas
#' @param columnasN De ser una tabla si se quiere que aparezcan los nombres de las columnas
#'
#' @return
#' @export
#'
#' @examples
sacar <- function(objeto, size = 4096, tipo = 'Tabla', filasN = FALSE, columnasN = TRUE) {
  if (tipo == 'Tabla') {
    clip <- paste('clipboard-', size, sep = "")
    TEMPORAL <- file(description = clip, open = 'w')
    write.table(objeto, TEMPORAL, row.names = filasN, col.names = columnasN, sep = '\t')
    close(TEMPORAL)
  } else {
    writeClipboard(as.character(objeto))
  }
}
