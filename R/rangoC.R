#' Rango de tonos entre colores, colores aleatorios, o rango entre colores aleatorios
#'
#' Crea un rango entre dos o más colores en el orden en que se de (si solo se pone un color no crea rango, solo repite el color). Tiene opción de crear en vez del rango un vector de colores aleatorios, con el mismo número de colores que se de en el rango. O, finalmente, un rango entre el número de colores aleatorios que se indique (el número de colores en el rango en sí aún es el mismo que el indicado antes)
#'
#' @param tonos Por defecto 33, e número de colores que se quiere obtener
#' @param colores Por defecto blue, green, red', nombre de los colores con los que se quiere hacer el rango (si solo se pone uno, se repetirá ese color tantas veces como se diga en 'tonos')
#' @param ver Por defecto TRUE, si se quiere ver un gráfico con los colores
#' @param aleacol Por defecto FALSE, En caso de TRUE, se sobrepone a 'colores'. No crea un rango, sino que genera colores aleatorios. Generará tantos colores aleatorios como se indique en 'tonos'
#' @param alearan Por defecto FALSE, debe ser un número en cuyo caso, se sobrepone a 'colores' y 'aleacor', crea un rango de colores entre el número de los colores aleatorios que se han dado con tantos tonos como se espqcifique en el argumento 'tonos'
#' @param mezclados Por defecto FALSE. En caso de TRUE, Mezclará aleatoriamente los colores obtenidos, da igual como hayan sido obtenidos, por rango, aleatorios, por rango aleatorio...
#' @param RGB Por defecto FALSE. En caso de TRUE da los colores en formato RGB tipo "rgb(25,241,252)"
#'
#' @return
#' @export
#'
#' @examples
rangoC <- function(tonos = 33, colores = 'blue, green, red', ver = TRUE, aleacol = FALSE, alearan = FALSE, mezclados = FALSE, RGB = FALSE) {
  # Código para crear un número hexadecimal de 6 caracteres al azar. Primer creo uno muy largo y liego lo divido en trozos de 6
  ALEACOL <- paste0('#',strsplit(x = paste0(sample(x = c(0:9, LETTERS[1:6]), size =  tonos*6, replace = TRUE), collapse = ''), split = paste0("(?<=.{", 6, "})"), perl = TRUE)[[1]])
  # Función principal del rango de colores
  coloresF <- colorRampPalette(fector(colores))
  # Condicional para generar colores aleatorios, sin rango
  coloresD <- if (aleacol == FALSE) {
    if (mezclados == TRUE) {sample(coloresF(tonos))} else {coloresF(tonos)}} else {ALEACOL}
  # Condicional por si se ha hecho un rango de colores aleatorios
  if (alearan != FALSE) {
    colores <- paste0('#',strsplit(x = paste0(sample(x = c(0:9, LETTERS[1:6]), size =  alearan*6, replace = TRUE), collapse = ''), split = paste0("(?<=.{", 6, "})"), perl = TRUE)[[1]])
    coloresF <- colorRampPalette(colores)
    coloresD <- if (mezclados == TRUE) {sample(coloresF(tonos))} else {coloresF(tonos)}
  }
  coloresE <- coloresD
  # Condicional para dar el resultado en formato RGB
  if (RGB) {
    coloresE <- paste0('rgb(', apply(X = col2rgb(col = coloresD), MARGIN = 2, FUN = paste0, collapse = ','),')')
  }
  # Mostrar los colores creados si as? lo deseo
  if (ver == TRUE) {
    barplot(1:tonos, col = coloresD, names.arg = coloresE)}
  return(coloresE)
}
