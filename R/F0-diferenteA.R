#' Función biológica archienemiga de faltaN. Busca secuencias a las que les 'sobran' nucleótidos.
#' Detecta secuencias que tengan algo que no sea los cuatro nucleótidos (o lo que yo diga). Crea una tabla diciendo si está o no.
#'
#' @param VECTORe Lista de secuencias de nucleótidos donde buscar
#' @param difA Por defecto 'a|t|g|c', que serán las únicas letras que permitiré que haya en la secuencia
#'
#' @return Tabla con columnas que indican cual es la secuencia (orden), la secuencia y cual es elemento anómalo ha encontrado
#' @export
#'
#' @examples
#' Si pongo 'a|t|g' me detectará las secuencias que tengan al menos una c. Si pongo 'a|t|g|c', que es lo que hay por defecto, buscará cosas raras como n
#' difernteA(VECTORe = fector('cacgtagtacgcatgcfaquffuucgtcagatc,actct,tcqt,actacatctatcactac,gcagagacacgcaga,cagcagcgacga,catacg,acggtagctaccg,ctgtgc,au'), todas = TRUE)
difernteA <- function(VECTORe, difA = 'a|t|g|c', todas = FALSE) {
  # Extraigo el patrón que he establecido a cada secuencia
  VECTORi <- gsub(pattern = difA, replacement = '', ignore.case = TRUE, x = VECTORe)
  # Selecciono aquellas a las que les ha quedado algo (el simbolo diferente)
  Cuales <- which(VECTORi != '')
  Sequences <- VECTORe[Cuales]
  # Muestro que patrones son los que he encontrado, si están repetidos intento simplificarlos
  Elemento <- gsub(pattern = '([[:alpha:]])\\1+', replacement = '\\1', x = VECTORi[Cuales])
  # Creo la tabla
  TABLAs <- data.frame(Cuales, Sequences, Elemento)
  # Condicional para añadir las secuencias restantes en caso de especificarlo
  if (todas){
    Cuales <- which(VECTORi == '')
    Sequences <- rep(x = NA, length(Cuales))
    Elemento <- rep(x = NA, length(Cuales))
    TABLAs2 <- data.frame(Cuales, Sequences, Elemento)
    TABLAs <- rbind(TABLAs, TABLAs2)
    TABLAs <- TABLAs[order(TABLAs$Cuales),]
    rownames(TABLAs) <- TABLAs$Cuales
  }
  return(TABLAs)
}
