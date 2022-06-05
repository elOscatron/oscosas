#' Función biológica archienemiga de diferenteA. Busca secuencias a las que les 'faltan' nucleótidos.
#' Detección de nucleótidos faltantes. Tenía un error la original por el que no contaba las secuencias completas si estaban al principio o al final
#'
#' @param VECTORe Conjunto de secuencias
#' @param difA Nucleótidos que tienen que estar presentes en la secuencia
#'
#' @return Tabla con una columna por letra especificada, más una columna llamada 'Any' que indica si falta cualquiera de las letra. También se puede poner una con el nombre de las secuencias
#' @export
#'
#' @examples faltaN(VECTORe = fector('cacgtagtacgcatgcacgtcagatc,actct,tct,actacatctatcactac,gcagagacacgcaga,cagcagcgacga,catacg,acggtagctaccg,ctgtgc,a'), difA = 'a,c,t', secuencias = TRUE)
faltaN <- function(VECTORe, difA = c('a,t,g,c'), secuencias = FALSE, todas = TRUE) {
  # Fectorizo difA
  difA <- fector(difA)
  TABLAt <- data.frame(row.names = seq(length(VECTORe)))
  # Bucle para añadir las columnas para cada nucleótido
  for (Nucleotido in difA) {
    CUALES <- which(nchar(VECTORe) == nchar(gsub(pattern = Nucleotido, replacement = '', ignore.case = TRUE, x = VECTORe)))
    TABLAt[CUALES,Nucleotido] <- if (length(CUALES) != 0) {TRUE}
    else {NA}
    Any <- apply(X = TABLAt, 1, FUN = sum, na.rm = TRUE)
  }
  TABLAt$Any <- Any
  #Condicional para el argumento de los nombres, se añade columna
  if (secuencias) {
    Sequences <- VECTORe
    TABLAt2 <- data.frame(Sequences)
    TABLAt <- cbind(TABLAt2, TABLAt)
  }
  # Condicional para quitar las secuencias a las que no les falta ninguno de los nucleótidos, si se especifica
  if (!todas) {
    TABLAt <- TABLAt[TABLAt$Any != 0,]
  }
  # creo una columna especificando cual es el número de la secuencia en la tabla oroginal
  TABLAt$Cuales <- rownames(TABLAt)
  return(TABLAt)
}
