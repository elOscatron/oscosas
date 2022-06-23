#' Añadir simbolos antes y/o depués
#'
#' Dado un vector o palabra, añade al principio, al final y a ambos diferentes símbolos, luego ordena el resultado por longitud y alfabeticamente
#'
#' @param VECTORe Vector al que se quiere añadir los símbolos
#' @param simbolos Por defecto _, -,  , paréntesis, corchetes y flechas. Si no, se pueden elegir que símbolos o letras se quieren añadir antes y después
#' @param donde Por defecto al principio y al final ('^', '$'), pero se puede especificar otra cosa (uno o el otro)
#' @param orient Si se quieren ordenar de mayor a menor o de menor a mayor. Por defecto de mayor a menor
#' @param or Por defecto FALSE, si se pone TRUE, se creará un vector separado por |, así que serán unas u otras
#'
#' @return Vector de elementos introducidos con los símbolos antes y/o después
#' @export
#'
#' @examples
varfor <- function(VECTORe, simbolos = simbi, donde = c('^', '$'), orient = TRUE, or = FALSE) {
  simbi <- data.frame(
    simboloI = c('_', '-', ' ', '\\\\(', '\\\\[', '\\\\\\\\<'),
    simboloF = c('_', '-', ' ', '\\\\)', '\\\\]', '\\\\\\\\>'))
  VECTORt <- c()
  for (fila in seq(nrow(simbolos))) {
    FILA <- simbolos[fila,]
    PRINCIPIO <- gsub(pattern = donde[1], replacement = FILA$simboloI, x = VECTORe)
    FIN <- gsub(pattern = donde[2], replacement = FILA$simboloF, x = VECTORe)
    AMBOS <- gsub(pattern = donde[1], replacement = FILA$simboloI,
                  x = gsub(pattern = donde[2], replacement = FILA$simboloF, x = VECTORe))
    VECTORt <- append(VECTORt, c(PRINCIPIO, FIN, AMBOS))
  }

  VECTORsp <- sort(unique(append(VECTORt, VECTORe)))
  VECTORsp1 <- VECTORsp[order(VECTORsp, decreasing = orient)]
  VECTORs <- VECTORsp1[order(nchar(VECTORsp1), decreasing = orient)]
  if (or == TRUE) {
    VECTORs <- paste0(VECTORs, collapse = '|')
  }

  return(VECTORs)
}
