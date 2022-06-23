#' Conversión entre formatos de folores
#'
#' Dando un vector con uno o más colores en el formato que sea, se genera una tabla identificando el formato y dando su equivalencia en los dos otros. Los formatos son el nombre, hexadecimal y RGB
#'
#' @param colores Colores que se quieren 'formatear'. Si se pone un RGB ¡cuidado con las comas!
#' @param ver Por defecto TRUE, si se quiere ver un gráfico con los colores
#' @param formatoS Por defecto Name, El formato en el que se quieran ver los colores en el gráfico. Las opciones son esa, Hexadecimal y RGB
#'
#' @return
#' @export
#'
#' @examples
#' folor(colores = c('blue','red',"#0100FFFF","rgb(0,0,255)",'orange'), formatoS = 'Hexadecimal')
folor <- function(colores = 'red,blue,green', ver = TRUE, formatoS = 'Name') {
  TABLAs <- data.frame()
  coloresT <- if(length(colores)==1) {
    fector(colores)} else {colores}
  for (color in seq(length(coloresT))) {
    COLOR <- coloresT[color]
    # Identifico y paso a HEX
    if(grepl(pattern = 'rgb\\(', x = COLOR)) {
      TIPO <- 'RGB'
      VECTORp <- gsub(pattern = 'rgb\\(', replacement = '', x = COLOR)
      VECTORp2 <- gsub(pattern = '\\)', replacement = '', x = VECTORp)
      TROZOS <- strsplit(x = VECTORp2, split = ',')[[1]]
      COLORh <- rgb(red = TROZOS[1], green = TROZOS[2], blue = TROZOS[3], alpha = 255, maxColorValue = 255)
    } else {
      if(grepl(pattern = '#', x = COLOR)) {
        TIPO <- 'Hexadecimal'
        COLORh <- COLOR
      } else {
        TIPO <- 'Name'
        cNAM <- 'blue'
        coloresF <- colorRampPalette(COLOR)
        COLORh <- coloresF(1)
      }}
    COLORr <- paste0('rgb(', apply(X = col2rgb(col = COLORh), MARGIN = 2, FUN = paste0, collapse = ','),')')
    # La siguiente opción no siempre funciona porque no hay nombre para tanto color
    COLORn <- colors()[match(rgb(t(col2rgb(COLORh)), maxColorValue = 255), c(rgb(t(col2rgb(colors())), maxColorValue = 255)))]
    TABLAs <- rbind(TABLAs, c(COLOR, TIPO, COLORh, COLORr, COLORn))
  }
  colnames(TABLAs) <- fector('Input, Type,Hexadecimal, RGB, Name')
  if (ver == TRUE) {
    barplot(1:nrow(TABLAs), col = TABLAs$Hexadecimal, names.arg = TABLAs[,formatoS])}
  return(TABLAs)
}
