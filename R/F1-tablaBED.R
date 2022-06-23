#' Creación de tablas BED
#'
#' Crea una tabla BED 'facilmente' (la función es compleja e imprefecta, pero la otra forma de hacer esto es un puto infierno) especificando un vector con los nombres de las columnas que se tienen. Hay que poner NA en los huecos o, poner FALSE en esa columna como argumento. Tiene opci?n de poner color y cromosoma concreto, de guardar y de darle la vuelta a las secuencias. Esto es porque por alg?n motivo me salen al reves con lo del bisulfito
#'
#' @param TABLA Tabla con los datos
#' @param Nombres Dar un vector indicando que columnas se proporcionan
#' @param Excluidos Dar un vector indicando que columnas NO se proporcionan`
#' @param Cromosoma Esta y las siguientes opciones para poner el nombre específico de la columna o si no está
#' @param Inicio
#' @param Fin
#' @param Nombre
#' @param Puntaje
#' @param Hebra
#' @param GordIni
#' @param GordFin
#' @param Color
#' @param CromosomaE Esta opción pide un nombre de cromosoma, es util en caso de que toda la tabla tenga el mismo o vengan en un vector separado
#' @param ColorE Similarmente a la opción permite poner el mismo color a toda la tabla o una serie de los mismos personalizados
#' @param Guardar Indica si se quiere exportar la tabla
#' @param Secuencia SI es verdadero, se añadiré la secuencia de ADN al nombre, útil para oligos y ver si están en el sitio adecuado
#' @param EnReverso Le da la vuelta a la secuencia cuando la va a poner en los nombres. Esto es lo que se hace en los oligos
#'
#' @return
#' @export
#'
#' @examples
tablaBED <- function(TABLA, Nombres = NA, Excluidos = NA,
                     Cromosoma = NA, Inicio = NA, Fin = NA, Nombre = NA,
                     Puntaje = FALSE, Hebra = NA, GordIni = FALSE, GordFin = FALSE, Color = FALSE,
                     CromosomaE = FALSE, ColorE = FALSE,
                     Guardar = FALSE, Secuencia = FALSE, EnReverso = FALSE) {
  # Fectorizo los nombres y exclu?dos
  Nombres <- fector(Nombres)
  esnaN <- !is.na(Nombres)[1]
  Excluidos <- fector(Excluidos)
  esnaE <- !is.na(Excluidos)[1]

  # Creo la tabla con los nombres de las columnas
  TABLAn <- data.frame(fector('CROMOSOMA, INICIO, FIN, NOMBRE, PUNTAJE, HEBRA, GORDINI, GORDFIN, COLOR'), fector('Cromosoma, Inicio, Fin, Nombre, Puntaje, Hebra, GordIni, GordFin, Color'))
  colnames(TABLAn) <- fector('Mayus,Minus')

  ELEMENTOS <- list()
  POSI <- 0
  BUCLE <- 0
  for (elemento in seq(length(TABLAn$Mayus))) { # Voy columna por columna viendo si existe o no y de donde puedo sacar su nombre (la lista de nombres o el orden de la tabla)
    MAYUS <- TABLAn$Mayus[elemento]
    minus <- TABLAn$Minus[elemento]
    BUCLE <- BUCLE + 1

    ARGfon <- if (is.na(get(minus))) {FALSE} else {if (get(minus) == FALSE) {TRUE} else {FALSE}}
    EXC <- if (esnaE) {length(grep(pattern = minus, x = Excluidos)) != 0} else {FALSE}
    # Si el argumento est? marcado como falso o est? en el vector de los exclu?dos, lo pongo como NA
    # Si el argumento es NA o FALSO, se pone NA. Tambi?n se pondr? NA si se especifica en el vector de excluidos
    ELEMENTOS[[MAYUS]] <- if (ARGfon | EXC) {NA} else { # Si no, corro una posici?n
      POSI <- POSI + 1
      if (esnaN) { # Si la lista de nombres existe, coger el de esa posici?n
        Nombres[POSI]
      } else { #Si no existe, ver si se le ha dado un nombre en los argumentos y cogerlo
        if (!is.na(get(minus))) {get(minus)
        } else {# Si no se le ha dado un nombre, y no hay en general ning?n nombre, coger el siguiente de la tabla, si no, NA
          if (length(ELEMENTOS) + 1 == BUCLE)
          {colnames(TABLA)[POSI]} else {NA}
        }}}}
  # Veo que columnas existen y cuales no
  COLUMNASp <- ELEMENTOS
  TABLAn$Nue <- unlist(COLUMNASp)
  CUALES <- is.na(COLUMNASp) | COLUMNASp == 'NA' | COLUMNASp == 'FALSE'
  Columnas1 <- TABLAn$Nue[!CUALES]
  Columnas0 <- TABLAn$Minus[CUALES]
  FILAS <- nrow(TABLA)
  # Creo una tabla rescatada con las columnas que haya
  TABLA1p <- TABLA[,Columnas1]

  # Bucle para ponerle los nombres est?ndar a la tabla rescatada
  TABLAm <- TABLA1p
  for (columna in seq(length(colnames(TABLA1p)))) {
    COLUMNA <- colnames(TABLA1p)[columna]
    colnames(TABLAm)[columna] <- TABLAn$Minus[TABLAn$Nue == COLUMNA & !is.na(TABLAn$Nue)][1]
  }
  # En caso de que falten columnas, creo una vac?a con las columnas que no hay y la uno a la otra
  if (length(Columnas0) != 0) {
    TABLA0 <- fabla(columnas = Columnas0, filas = FILAS)
    TABLA0[,] <- 0
    TABLAt <- cbind(TABLAm, TABLA0)
  } else {
    # Si no faltaba ninguna columna, simplemente pongo todas
    TABLAt <- TABLAm
  }
  # Reordeno la tabla reunificada
  TABLAs <- TABLAt[,TABLAn$Minus]
  # En caso de que no se hayan introducido, pongo las columnas de los trozos gordos
  if (sum(TABLAs$GordIni) == 0) {TABLAs$GordIni <- TABLAs$Inicio}
  if (sum(TABLAs$GordFin) == 0) {TABLAs$GordFin <- TABLAs$Fin}

  # Pongo cromosomas y colores espec?ficos en caso de haberlos indicado
  if (CromosomaE != FALSE) {TABLAs$Cromosoma <- CromosomaE}
  suppressWarnings(if (ColorE != FALSE) {TABLAs$Color <- ColorE})

  # Si los nombrestienen espacios, se los sustituyo por una barra baja
  if (length(grep(pattern = ' ', x = TABLAs$Nombre)) != 0) {TABLAs$Nombre <- gsub(pattern = ' ', replacement = '_', x = TABLAs$Nombre)}
  # Si he especificado una columna donde est? la secuencia, se la a?ado al nombre, adem?s miro si est? en reverso
  if (Secuencia != FALSE) {
    TABLAs$Nombre <- if(EnReverso == FALSE) {
      paste0(TABLAs$Nombre,'->',TABLA[,Secuencia])
    } else {
      paste0(TABLAs$Nombre,'->',Biostrings::reverseComplement(Biostrings::DNAStringSet(TABLA[,Secuencia])))
    }}
  # Si as? lo he indicado, guardo la tabla
  if (Guardar != FALSE) {
    write.table(x = TABLAs, file = paste0(Guardar,' (',Sys.Date(),').bed'), row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  # Muestro las equivalencias de nombres y devuelvo la tabla
  print(TABLAn[,c(2,3)])
  return(TABLAs)
}
