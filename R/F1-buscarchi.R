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
#' @param recursivo Por defecto FALSE, permite buscar en carpetas hijas
#'
#' @return archivo o archivos que cuadran con la búsqueda
#' @export
#'
#' @examples
buscarchi <- function(donde = NULL, tiene = '', carece = '', fin = '', igmayT = TRUE, igmayC = TRUE, nuevo = TRUE, grande = NULL, todos = FALSE, recursivo = FALSE) {
  if (is.null(donde)) {
    donde <- paste0(getwd(), '/')
  } else {
    if (!grepl(pattern = '/$', x = donde)) {donde <- paste0(donde,'/')}  }
  archivosE <- file.info(dir(path = donde, full.names = TRUE, recursive = recursivo))
  rownames(archivosE) <- gsub(pattern = "/{2,}", replacement = '/', x = rownames(archivosE))
  archivosC <- rownames(file.info(dir(path = donde, full.names = FALSE, recursive = recursivo)))
  archivosC <- gsub(pattern = "/{2,}", replacement = '/', x = archivosC)

  VECTORe <- fector(c(tiene, if(fin != '') {paste0(fin,'$')}))
  VECTORt <- c()

  for (palabra in numcuencia(archivosC)) { # Recorro cada nombre de la lista
    PALABRA <- archivosC[palabra]
    CUMPLE <- c()
    for (palabri in seq(length(VECTORe))) { # Recorro cada patrón del argumento 'tiene' más el 'fin'
      PALABRI <- VECTORe[palabri]
      RESULTADO <-  grepl(pattern = PALABRI, x = PALABRA, ignore.case = igmayT)
      CUMPLE <- append(CUMPLE, RESULTADO)
    }
    if(length(CUMPLE) != 0) {if(all(CUMPLE)) {
      VECTORt <- append(VECTORt, PALABRA)
    }}
  }

  # Comprobar si hay archivos abiertos
  TESTabi <- grepl(pattern = '~\\$', x = VECTORt)
  if(any(TESTabi)) {
    warning("Uno o varios de los archivos compatibles están abiertos.")
    VECTORt <- VECTORt[!TESTabi]
  }

  if(is.null(VECTORt)) {
    stop(impringar('No se ha encontrado nada con los parAmetros ', paste0(VECTORe, collapse = ', ')))}

  if(is.null(VECTORt)) {
    stop(impringar('No se ha encontrado nada con los parAmetros ', paste0(VECTORe, collapse = ', ')))}

  if(length(carece) != 0) {if (carece[1] != '') {
    VECTORt2 <- c()
    VECTORe <- fector(carece)
    for (palabra in numcuencia(VECTORt)) {
      PALABRA <- VECTORt[palabra]
      PRUEBAS <- c()
      for (elemento in seq(length(VECTORe))) {
        ELEMENTO <- VECTORe[elemento]
        PRUEBA <- !grepl(pattern = ELEMENTO, x = PALABRA, ignore.case = igmayC)
        PRUEBAS <- append(PRUEBAS, PRUEBA)
      } # Fin del bucle de los patrones a excluir
      if(all(PRUEBAS)) {VECTORt2 <- append(VECTORt2, PALABRA)}
    } # Fin del bucle de las palabras
  } # Fin del condicional de que no haya excluyentes
    else {VECTORt2 <- VECTORt}} else  {VECTORt2 <- VECTORt}

  TABLAm2 <- archivosE[rownames(archivosE) %in% paste0(donde,VECTORt2),]
  if (!is.null(grande)) {if (grande) {
    TABLAm2 <- TABLAm2[which.max(TABLAm2$size),]} else {
      TABLAm2 <- TABLAm2[which.min(TABLAm2$size),]}}
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
