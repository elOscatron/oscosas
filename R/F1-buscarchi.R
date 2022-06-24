#' Buscar un archivo en una carpeta
#'
#' Especificando que patrones (letras, números...  nada complejo) deben aparecer en el archivo, da un resultado. Sin argumentos devuelve el más nuevo de la carpeta actual
#'
#' @param donde Carpeta del archivo, por defecto la de trabajo
#' @param tiene Elementos que están presentes en el archivo, por defecto nada
#' @param carece Elementos que no deben estar en el nombre, por defecto nada
#' @param fin Elementos que deben aparecer al final del archivo, por defecto nada
#' @param igmayT Si se deben distinguir mayúsculas de minúsculas en el argumento 'tiene'
#' @param igmayC Si se deben distinguir mayúsculas de minúsculas en el argumento 'carece'
#' @param nuevo Si debe ser el archivo más nuevo (TRUE) o el más viejo (FALSE), por defecto es el más nuevo
#' @param grande Si debe ser el archivo más grande (TRUE) o el más pequeño(FALSE), por defecto nada, se ignora
#' @param todos Por defecto FALSE, si pongo TRUE mostrará todas las coincidencias
#'
#' @return
#' @export
#'
#' @examples
buscarchi <- function(donde = NULL, tiene = '', carece = '', fin = '', igmayT = TRUE, igmayC = TRUE, nuevo = TRUE, grande = NULL, todos = FALSE) {
  if (is.null(donde)) {
    donde <- paste0(getwd(), '/')
  } else {
    if (!grepl(pattern = '/$', x = donde)) {paste0(done,'/')}  }
  archivosE <- file.info(dir(path = donde, full.names = TRUE))
  archivosC <- rownames(file.info(dir(path = donde, full.names = FALSE)))

  VECTORe <- fector(c(tiene, if(fin != '') {paste0(fin,'$')}))
  VECTORt <- c()
  for (elemento in seq(length(VECTORe))) {
    ELEMENTO <- VECTORe[elemento]
    VECTORt <- if(length(VECTORt) == 0) {archivosC} else {VECTORt}
    VECTORt <- grep(pattern = ELEMENTO, x = VECTORt, ignore.case = igmayT, value = TRUE)}
  VECTORt2 <- c()
  if(length(carece) != 0)
  { if (carece[1] != '') {
    VECTORe <- fector(carece)
    for (elemento in seq(length(VECTORe))) {
      ELEMENTO <- VECTORe[elemento]
      VECTORt2 <- if(length(VECTORt2) == 0) {VECTORt} else {VECTORt2}
      VECTORt2 <- VECTORt2[!grepl(pattern = ELEMENTO, x = VECTORt2, ignore.case = igmayC)]}
  } else {
    VECTORt2 <- VECTORt}}
  TABLAm2 <- archivosE[rownames(archivosE) %in% paste0(donde,VECTORt2),]
  if (!is.null(grande)) {if (grande) {
    TABLAm2 <- which.max(TABLAm2$size)} else {
      TABLAm2 <- which.min(TABLAm2$size)}}
  TABLAm2 <- archivosE[rownames(archivosE) %in% rownames(TABLAm2),]
  if (nuevo) {
    archivo <- rownames(TABLAm2)[which.max(TABLAm2$mtime)]
  } else {
    archivo <- rownames(TABLAm2)[which.min(TABLAm2$mtime)]}
  archivo <- rownames(archivosE)[rownames(archivosE) == archivo]
  # Condicional por si quiero ver todos los archivos que encajan, se sobrepone a todo
  if (todos) {return(rownames(TABLAm2))
  } else {
    return(archivo)
  }
}
