#' Imprimir con saltos
#'
#' Usando una lista de vectores, se puede imprimir con saltos de linea (tantos como se especifique) tras la impresión, útil cuando se usa varias veces en un bucle. También en cada reglón se puede poner debajo un conjunto de barras o guiones para separar mejor
#'
#' @param LISTAe Lista a imprimir. Por defecto hay una de muestra
#' @param barras Por defecto 77, indica cuantas barras se quiere poner tras el reglón, pero solo se pondrán si se ha puesto la palabra 'BARRA' en el vector
#' @param guiones Lo mismo que con las barras pero con guiones
#' @param salto Por defecto FALSE, si se pone TRUE se pondrá un salto de línea tras toda la impresión, si se pone un número se pondrá ese número de saltos. Útil si se usa en bucle y se quieren separar elementos
#'
#' @return
#' @export
#'
#' @examples
primsalto <- function(LISTAe = list(c('a', 'b', 'BARRA'),c('c','d'),c('e','f')), barras = 77, guiones = 77, salto = FALSE) {
  for (elemento in seq(length(LISTAe))) { # Recorro cada elemento de la lista introducida
    ELEMENTO <- LISTAe[[elemento]]
    # Compruebo si en el vector de la lista hay un 'BARRA' o un 'GUION'
    HAYb <- any(ELEMENTO %in% 'BARRA')
    HAYg <- any(ELEMENTO %in% 'GUION')
    # SI lo hay, lo saco del vector e imprimo su 'equivalente'
    if(any(HAYb, HAYg)){
      if(HAYb) {
        editado <- ELEMENTO[!ELEMENTO %in% 'BARRA']
        impringar(editado, collapse = '')
        print(paste0(rep(x = '_', 77), collapse = ''))}
      if(HAYg) {
        editado <- ELEMENTO[!ELEMENTO %in% 'GUION']
        impringar(editado, collapse = '')
        print(paste0(rep(x = '_', 77), collapse = ''))
      }} else { # Si no lo nhay simplemente imprimo el vector
        impringar(ELEMENTO, collapse = '')}}
  if(salto != FALSE) { # Condicional para añadir un salto de línea o varios
    if(salto == TRUE) {salto <- 1}
    for(saltos in seq(salto)){
      cat('\n')}}
}
