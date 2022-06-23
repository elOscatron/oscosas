#' Mezclar colores
#'
#' permite mezclar dos o más colores. Por defecto muestra un gráfico donde se ve el color resultante y los entrantes, con nombres.
#'
#' @param colores Colores a mezclar
#' @param ver Si se quieren ver los colores
#'
#' @return Color mezclado
#' @export
#'
#' @examples
mezcol <- function(colores = 'blue,red', ver = TRUE) {
  colores <- fector(colores)
  # Si es impar
  if (length(colores) %% 2 != 0) {
    primero <- colores[1]
    coloris <- colores[-1]} else {
      primero <- NULL
      coloris <- colores
    }
  parejas <- length(coloris) / 2
  for (pareja in seq(parejas)) {
    posicion <- pareja * 2
    amez <- c(coloris[posicion - 1], coloris[posicion])
    coloresF1 <- colorRampPalette(amez)
    color1 <- coloresF1(3)[2]
    if (length(primero) == 0) {
      primero <- color1
      print(primero)
    } else {
      coloresF2 <- colorRampPalette(c(color1, primero))
      color2 <- coloresF2(3)[2]
      primero <- color2
      print(primero)
    }}
  if (ver == TRUE) {
    barplot(1, col = primero, names.arg = primero)
    barplot(1:length(colores), col = colores, names.arg = colores)}
  return(primero)
}
