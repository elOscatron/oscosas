#' limpiar área de trabajo de elementos previos
#'
#' Permite archivos que tienen el mismo nombre (simple, sin guiones o mierdas) peor más largo, porque ordena el vector de más largo a menos
#'
#' @param donde Por defecto el directorio de trabajo. Nombre de la carpeta vieja a crear
#' @param viejpeta Por defecto Viejo, nombre de la carpeta que se creará si no hay una compatible
#' @param nomviej Elementos a buscar para detectar cosas viejas. No lo pongo en inglés por si acaso hay problemas
#' @param aextraer Vector de elementos relacionados con versiones para eliminar de los nombres. Son cosas como fechas, formatos comunes, extensiones, números de copia... todo lo que se me ocurre
#'
#' @return
#' @export
#'
#' @examples
fimpiar <- function(donde = NULL, viejpeta = 'Viejo', nomviej = 'viejo|vieja', aextraer = c('(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2}-\\d{2})', '(\\d{4}-\\d{2}-\\d{2}-\\d{2}-\\d{2})', '(\\d{4}-\\d{2}-\\d{2}-\\d{2})','\\(\\d{4}-\\d{2}-\\d{2}[a-zA-Z]{0,}\\)', '\\d{4}-\\d{2}-\\d{2}[a-zA-Z]{0,}',' - Copy','.[a-zA-Z]{1,4}$','\\([a-z]\\)','^[a-z]{1} ',' \\.','^ {1,}', ' {1,}$', ' {2,}', '\\d-', '_[a-zA-Z]{0,}$')) {
  if (is.null(donde)) {
    donde <- paste0(getwd(), '/')
  } else {
    if (!grepl(pattern = '/$', x = donde)) {paste0(done,'/')}  }
  impringar('La carpeta donde se buscará es: ', donde)
  archivosEp <- file.info(dir(path = donde, full.names = TRUE))
  LISTAm <- strsplit(x = rownames(archivosEp), split = '/')
  # Añado una columna con los nombres simples
  archivosEp$Nombres <- VECTORs <- unlist(lapply(LISTAm, tail, n = 1L))
  impringar('Se está buscando en la carpeta: ', donde,' que tiene los siguientes ',nrow(archivosEp), ' archivos:')
  impringar(archivosEp$Nombres)
  archivosE <- archivosEp[!archivosEp$isdir,]
  carpetas <- archivosEp$Nombres[archivosEp$isdir]
  if(any(archivosEp$isdir)) {
    impringar('Hay ', sum(archivosEp$isdir), ' carpeta(s) de nombre: ', paste0(carpetas, collapse = ', '))
  }

  VECTORe <- aextraer
  VECTORe2 <- archivosE$Nombres
  VECTORm <- c()
  # Bucle para eliminar estos elementos y quedarme solo con los nombres básicos de los archivos
  for (elemento in seq(length(VECTORe))) {
    ELEMENTO <- VECTORe[elemento]
    #    print(ELEMENTO)
    VECTORm <- if (length(VECTORm) == 0) {VECTORe2} else {VECTORm}
    VECTORm <- unique(gsub(pattern =  ELEMENTO, replacement = '', x = VECTORm, ignore.case = TRUE))
  }
  # Vector con los nombres simples, los ordeno de más largo a menos para que si hay alguno que se llame igual pero sea más largo no se vea jodido por los otros
  vEA <-VECTORm[order(nchar(VECTORm), VECTORm, decreasing = TRUE)]
  impringar('Hay ', length(vEA), ' nombres simples que son: ')
  print(vEA)

  # Buscar si hay alguna carpeta de viejos
  carviej <- grep(pattern = nomviej, x = carpetas, value = TRUE, ignore.case = TRUE)
  if(length(carviej)== 0) {
    # Si no hay ninguna la creo con el nombre que he dado
    impringar('No existía ninguna carpeta para archivos viejos, se creará una de nombre ', viejpeta)
    viejfin <- paste0(donde,viejpeta)
    dir.create(paste0(viejfin))
  } else {
    # Si hay alguna, elijo la ´de nombre más simple
    candidatos <- archivosEp[archivosEp$Nombres %in% carviej,]
    viejpetaP <- candidatos[order(nchar(candidatos$Nombres))[1],]
    impringar('Ya hay ',nrow(candidatos),' carpeta(s) que cuandra(n) con la descripción: ', paste0(candidatos$Nombres, collapse = ', '),'... se ha elegido a ',viejpetaP$Nombres)
    viejfin <- rownames(viejpetaP)
  }

  # Meto los archivos viejos en la carpeta de viejos
  TABLAm <- archivosE
  VECTORe3 <- vEA
  VECTORs <- c()

  suppressWarnings(for (elemento in seq(length(VECTORe3))) { # Recorro cada elemento simplificado
    ELEMENTO <- VECTORe3[elemento]
    # Creo una subtabla de los archivos que tienen el nombre simplificado
    TABLAt <- TABLAm[!TABLAm$Nombres %in% VECTORs,]
    TABLAt <- TABLAt[grep(pattern = ELEMENTO, x = TABLAt$Nombres, ignore.case = FALSE),]
    impringar("Para el elemento '", ELEMENTO, "' se han encontado ", nrow(TABLAt)," archivos:")
    print(TABLAt$Nombres)
    # Selecciono el que sea más nuevo
    ELEGIDO <- TABLAt$Nombres[which.max(TABLAt$mtime)]
    VECTORs <- append(VECTORs, ELEGIDO)
    impringar("El archivo elegido es '", ELEGIDO,"'.")
    # Guardo los nombres del resto
    DESCARTADOS <- rownames(TABLAt[!TABLAt$Nombres %in% VECTORs,])
    # Muevo el resto a la carpeta de los viejos
    if (length(DESCARTADOS) != 0){
      file.copy(from = DESCARTADOS, to = viejfin)
      impringar(length(DESCARTADOS), ' archivos movidos a la carpeta ', viejfin)
      file.remove(DESCARTADOS)
    } else {print('No había versiones previas, no se ha movido nada.')}
    cat('\n')
  })
}
