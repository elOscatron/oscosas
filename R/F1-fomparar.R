#' fomparar: compara vectores o tablas y da un informe y un objeto con la diferencia de longitud, los elementos comunes y los exclusivos de cada uno. Si es una tabla (o dos), por defecto comparA las columnas. DirA si lo que se ha introducido es una tabla o un vector
#'
#' @param VECTOR1 Tabla o vector 1
#' @param VECTOR2 Tabla o vector 2
#' @param silencio Por defecto FALSE, si no se quiere ver el informe en pantalla
#' @param corto Por defecto FALSE, si se pone TRUE y silencio es FALSE, mostrará solo los números, no los elementos ni los detalles
#' @param rows Por defecto FALSE, si se quiere que lo que se comparen sean las filas. Es por comocidad porque si hay dos tablas y se quiere comparar las columnas de una con las filas de otra, se puede simplemente poner rownames()
#'
#' @return
#' @export
#'
#' @examples
#' a <- fablazar(columns = 8)
#' b <- fablazar(columns = 55)
#' b <- b[,5:55]
#'
#' d <- fomparar(VECTOR1 = colnames(a), VECTOR2 = b, rows = FALSE)
#' e <- fomparar(VECTOR1 = a, VECTOR2 = b)
#' c <- fomparar(VECTOR1 = list(colnames(a)), VECTOR2 = list(colnames(b)))
#'
fomparar <- function(VECTOR1, VECTOR2, silencio = FALSE, corto = FALSE, rows = FALSE) {
  # Detectamos si los argumentos son tablas y extraemos los nombres de las columnas o filas según corresponda
  if (is.data.frame(VECTOR1) |  is.matrix(VECTOR1)) {
    if (all(!c(silencio, corto))) {print('El primer elemento es una tabla o matriz')}
    if (rows) {
      VECTOR1a <- rownames(VECTOR1)
      if (all(!c(silencio, corto))) {print('Se mirarán sus filas')}
    } else {
      VECTOR1a <- colnames(VECTOR1)
      if (all(!c(silencio, corto))) {print('Se mirarán sus columnas')}}
  } else {
    VECTOR1a <- unlist(VECTOR1)
    if (all(!c(silencio, corto))) {print('El primer elemento es un vector')}}
  cat('\n')

  if (is.data.frame(VECTOR2) |  is.matrix(VECTOR2)) {
    if (all(!c(silencio, corto))) {print('El segundo elemento es una tabla o matriz')}
    if (rows) {
      VECTOR2a <- rownames(VECTOR2)
      if (all(!c(silencio, corto))) {print('Se mirarán sus filas')}
    } else {
      VECTOR2a <- colnames(VECTOR2)
      if (all(!c(silencio, corto))) {print('Se mirarán sus columnas')}}
  } else {
    VECTOR2a <- unlist(VECTOR2)
    if (all(!c(silencio, corto))) {print('El segundo elemento es un vector')}}
  cat('\n')
  # Obtenemos la longitud de cada vector
  length1 <- length(VECTOR1a)
  length2 <- length(VECTOR2a)

  # Comprobamos si los vectores tienen la misma longitud
  if(all(!c(silencio, corto))) {
    if (length1 != length2) {
      impringar('Los elementos tienen longitud diferente: ',length1,' y ', length2) } else {impringar('Los elementos tienen la misma longitud: ', length1)}
    cat('\n')
  }
  # Comprobamos si los vectores tienen elementos en común
  common <- intersect(VECTOR1a, VECTOR2a)
  # Comprobamos si hay elementos exclusivos en VECTOR1
  exclusive1 <- setdiff(VECTOR1a, VECTOR2a)
  # Comprobamos si hay elementos exclusivos en VECTOR2
  exclusive2 <- setdiff(VECTOR2a, VECTOR1a)
  # Creamos un objeto que contenga los tres vectores
  objeto <- list(Common = common, Exclusive1 = exclusive1, Exclusive2 = exclusive2)

  if (!silencio) {
    if(corto) {
      impringar(length(common), ' common elements.', salto = TRUE)
      impringar(length(exclusive1), ' unique elements in first element.', salto = TRUE)
      impringar(length(exclusive2), ' unique elements in second element.', salto = TRUE)
    } else {
      impringar(length(common), ' common elements: ', paste0(common, collapse = ', '), salto = TRUE)
      impringar(length(exclusive1), ' unique elements in first element: ', paste0(exclusive1, collapse = ', '), salto = TRUE)
      impringar(length(exclusive2), ' unique elements in second element: ', paste0(exclusive2, collapse = ', '), salto = TRUE)
    }}
  # Devolvemos el objeto
  invisible(objeto)
}
