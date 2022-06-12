#' carga y/o guarda o viceversa un objeto
#'
#' dado un nobre, mira si un ojeto existe, si no, busca el nombre exacto en formato rds con la fecha actual, si no está busca con otra fecha o un archivo que contenga ese nombre. Una vez tiene el objeto, crea el archivo con el nombre y la fecha
#'
#' @param nombre Nombre del objeto
#' @param sobrescribirO Si se desea sobrescribir el objeto usando el archivo que haya encontrado. Por defecto FALSE, al poner NULL hará preguntará
#' @param sobrescribirA Si se desea sobrescribir el archivo exacto, usando el objeto. Por defecto FALSE, al poner NULL hará preguntará
#' @param exactoO Si se desea crear un archivo exacto a partir del objeto. Por defecto TRUE, al poner NULL hará preguntará
#' @param exactoC Si se desea crear un archivo exacto a partir del más nuevo de los compatibles. Por defecto TRUE, al poner NULL hará preguntará
#' @param donde
#' @param formato
#'
#' @return
#' @export
#'
#' @examples
carguar <- function(nombre, sobrescribirO = FALSE, sobrescribirA = FALSE, exactoO = TRUE, exactoC = TRUE, donde = paste0(getwd(),'/'), formato = "(%Y-%m-%d)") {
  # Previos
  afirmativo <- gsub(pattern = ',', replacement = '|', paste0(mayuscom(VECTORe = 'yes,y,s,si,sí,t,true'), collapse = '|'))
  completo <- paste0(nombre, format(Sys.time(), formato),'.rds')
  archivosTP <- file.info(dir(path = donde, full.names = TRUE))
  archivosT <- archivosTP[grep(pattern = '.rds$', x = rownames(archivosTP), ignore.case = TRUE),]
  archivos <- gsub(pattern = '.*/', replacement = '', x = rownames(archivosT))
  EXACTOp <- completo %in% archivos
  EXACTO <- paste0(donde,completo)
  # Crear un COMPATIBLESp que ya refleje si hay rds y el nombre deseado. Luego arreglar COMPATIBLES para que solo elija los tipo RDS
  COMPATIBLESp <- si.error(grep(pattern = nombre, x = archivos), alternativa = FALSE, mensajeNEG = TRUE)
  COMPATIBLES <- if (COMPATIBLESp) {archivosT[grep(pattern = nombre, x = archivos),]}
  NUEVOEL <- rownames(COMPATIBLES)[which.max(COMPATIBLES$mtime)]
  OBJETOp <- try(expr = exists(nombre), silent = TRUE) == TRUE

  # Comprobar que existe el objeto
  if (OBJETOp) {# El OBJETO EXISTE, miro si hay uno compatible antes de guardarlo o a lo mejor lo quiero sustituir
    if(COMPATIBLESp) {# Hay un archivo compatible, busco si hay exactos
      if (EXACTOp) {# Sí hay uno exacto
        SOBRESCRIBIRo <- if (!is.null(sobrescribirO)) {sobrescribirO} else {
          if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('El objeto existe y hay un archivo exacto, quieres sobrescribir el objeto usando ese archivo?')))) != 0) {TRUE} else {FALSE}}
        if (SOBRESCRIBIRo) {# Sobrescribiré el objeto usando el archivo exacto
          assign(x = nombre, value = readRDS(file = EXACTO), envir = .GlobalEnv)
          print(paste0("Había un objeto pero se ha sustituído por una versión exacta: '",EXACTO,"'. Archivo no modificado"))
        } else {# No se va a sobrescribir el objeto
          print(paste0("Había un objeto y una versión exacta: '",EXACTO,"'. Ninguno se ha modificado."))
          SOBRESCRIBIRa <- if (!is.null(sobrescribirA)) {sobrescribirA} else {
            if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('Existe el objeto y un archivo exacto, ¿quieres sustituir el archivo usando el objeto?')))) != 0) {TRUE} else {FALSE}}
          if (SOBRESCRIBIRa) {
            saveRDS(object = get(nombre), file = EXACTO)
            print(paste0("El objeto ya existía, al igual que el archivo exacto: '",EXACTO,"', pero el archivo ha sido sustituído por el objeto. Objeto no modificado"))
          } else {print(paste0("El objeto ya existía, al igual que el archivo exacto: '",EXACTO,"'. Ninguno se ha modificado"))}
        }} else {# No hay uno exacto, cargo el compatible
          SOBRESCRIBIRo <- if (!is.null(sobrescribirO)) {sobrescribirO} else {
            if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('El objeto existe y hay un archivo compatible, quieres sobrescribir el objeto usando ese archivo?')))) != 0) {TRUE} else {FALSE}}
          if (SOBRESCRIBIRo) {# Sobrescribiré el objeto usando el archivo compatible
            assign(x = nombre, value = readRDS(file = NUEVOEL), envir = .GlobalEnv)
            print(paste0("Había un objeto pero se ha sustituído por una versión compatible: '",NUEVOEL,"'. Archivo compatible no modificado"))
          }
          print(paste0("Había un objeto y una versión compatible: '",NUEVOEL,"'. El objeto no se ha sustituído."))
          EXACTOo <- if (!is.null(exactoO)) {exactoO} else {
            if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('Existe el objeto y un archivo compatible, ¿quieres crear un archivo de nombre actualizado el archivo usando el objeto?')))) != 0) {TRUE} else {FALSE}}
          if (EXACTOo) {
            saveRDS(object = get(nombre), file = EXACTO)
            print(paste0("El objeto ya existía, al igual que un archivo compatible: '",NUEVOEL,"', se ha guardado un nuevo archivo con el nombre actualizado: '",EXACTO,"'."))
          } else {
            print(paste0("El objeto ya existía, al igual que un archivo compatible: '",NUEVOEL,"'. No se ha creado archivo con nombre actualizado"))}
        }} else {# No hay nada compatible lo guardo
          saveRDS(object = get(nombre), file =  EXACTO)
          print(paste0("Había un objeto pero no un archivo... ¡Hasta ahora! archivo guardado bajo el nombre: '",EXACTO,"'."))}

  } else {# Si el OBJETO NO EXISTE miro si hay compatibles
    if(COMPATIBLESp) {# Hay un archivo compatible, miro si es exacto
      if (EXACTOp) {# Hay un archivo exacto
        assign(x = nombre, value = readRDS(file = EXACTO), envir = .GlobalEnv)
        print(paste0("No había objeto... ¡Hasta ahora! cargado de una versión exacta: '",EXACTO,"'."))}
      else {# Si no hay uno exacto miro si al menos hay uno compatible
        assign(x = nombre, value = readRDS(file = NUEVOEL), envir = .GlobalEnv)
        EXACTOc <- if (!is.null(exactoC)) {exactoC} else {
          if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('No existía el objeto... ¡hasta ahora! pero hay archivo compatible, ¿quieres además crear un archivo con el nombre actualizado a partir de este?')))) != 0) {TRUE} else {FALSE}}
        if (EXACTOc) {
          saveRDS(object = get(nombre), file = EXACTO)
          print(paste0("No había objeto... ¡Hasta ahora! cargado de la versión compatible: '",NUEVOEL,"'. También se ha guardado una versión con el nombre actualizado: '",EXACTO,"'."))
        } else {
          print(paste0("No había objeto... ¡Hasta ahora! cargado de la versión compatible: '",NUEVOEL,"'. Pero no se ha guardado una versión con el nombre actualizado"))}
      }} else {# Si no hay ningún tipo de archivo compatible lo guardo
        print('No hay objetos ni archivos compatibles :(')}}
}
