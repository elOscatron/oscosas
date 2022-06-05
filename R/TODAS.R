
# carguar: dado un nobre, mira si un ojeto existe, si no, busca el nombre exacto en formato rds con la fecha actual, si no est? busca con otra fecha o un archivo que contenga ese nombre. Una vez tiene el objeto, crea el archivo con el nombre y la fecha
carguar <- function(nombre, sobreescribirO = FALSE, sobreescribirC1 = TRUE, sobreescribirC2 = TRUE, sobreescribirA = FALSE) {
  # Previos
  afirmativo <- 'yes|Yes|YES|y|Y|S?|Si|SI|S?|si|s?|s|S'
  completo <- paste0(Sys.Date(),' ', nombre, '.rds')
  archivos <- file.info(list.files())
  OBJETO <- try(expr = exists(nombre), silent = TRUE) == TRUE
  EXACTOp <- grep(pattern = completo, x = rownames(archivos))
  COMPATIBLESp <- grep(pattern = nombre, x = rownames(archivos))

  # Miro si el objeto existe
  if (OBJETO) { # En primer lugar, su versi?n m?s nueva
    GUARDAR <- get(nombre)
    # Existe el archivo buscado
    SOBREESCRIBIRo <- if (!is.null(sobreescribirO)) {sobreescribirO} else {
      if (length(grep(pattern = afirmativo, x = readline(prompt = 'Parece que ya existe el objeto ?quieres buscar si hay archivos para sustituirlo?'))) != 0) {TRUE} else {FALSE}
    }
    if (SOBREESCRIBIRo) {
      # Miro si el archivo existe
      if (length(rownames(archivos)[EXACTOp] != 0)) { # Si est? el indicado se cargar?
        TEMPIR <- readRDS(file = completo)
        print('?Objeto sustitu?do por la versi?n indicada!')
      } else { # Si no est? el indicado se buscar? uno compatible
        print('Parece que el archivo indicado no existe, buscando compatibles...')
        if (length(COMPATIBLESp) != 0) { # Si hay compatibles
          COMPATIBLES <- archivos[COMPATIBLESp,]
          ELEGIDO <- rownames(COMPATIBLES)[which.max(COMPATIBLES$mtime)]
          SOBREESCRIBIRc1 <- if (!is.null(sobreescribirC1)) {sobreescribirC1} else {
            if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('Hay uno que podr?a ser compatible llamado ', ELEGIDO,' ?quieres sobreescribirlo el objeto con este?')))) != 0) {TRUE} else {FALSE}
          }
          if (SOBREESCRIBIRc1) {# Si se responde que s?, se sustituir? por el archivo compatible
            TEMPIR <- readRDS(file = ELEGIDO)
            print(paste0('Objeto sustitu?do por: ',ELEGIDO))
          } else {print('Pues vale, se queda el objeto antiguo en vez del compatible :(')}
        } else {print('No hay archivos compatibles :(')}}
    } else {print('Objeto antiguo no modificado')}

  } else { # Si el objeto no exist?a y existe la versi?n buscada se carga automaticamente
    if (length(EXACTOp != 0)) {
      TEMPIR <- readRDS(file = completo)
      print('?El objeto no exist?a... hasta ahora  ?Versi?n indicada cargada!')
    } else { # Si no est? el indicado se buscar? uno compatible
      print('No hay objeto ni archivo indicado... lo mismo buscando con el mismo nombre sale algo')
      if (length(COMPATIBLESp) != 0) { # Si hay compatibles
        COMPATIBLES <- archivos[COMPATIBLESp,]
        ELEGIDO <- rownames(COMPATIBLES)[which.max(COMPATIBLES$mtime)]
        SOBREESCRIBIRc2 <- if (!is.null(sobreescribirC2)) {sobreescribirC2} else {
          if (length(grep(pattern = afirmativo, x = readline(prompt = paste0('Pues hay uno que podr?a ser compatible llamado ', ELEGIDO,' ?quieres crear el objeto con este?')))) != 0) {TRUE} else {FALSE}
        }
        if (SOBREESCRIBIRc2) { # Si se responde que s?, se sustituir? por el archivo compatible
          TEMPIR <- readRDS(file = ELEGIDO)
          print(paste0('?Versi?n compatible ',ELEGIDO, ' cargada!'))
        } else {print('En tal caso no se cargar? ning?n archivo :C')}
      } else {print('Pues no... No est? el archivo indicado ni hay archivos compatibles :C')}}}

  OBJETOt <- try(expr = exists('TEMPIR'), silent = TRUE) == TRUE
  if(OBJETOt) {GUARDAR <- TEMPIR}
  # Archivo
  # si el objeto no exist?a antes de buscar el archivo
  if (OBJETO == FALSE) {
    print('El objeto no exist?a antes de aplicar la funci?n, buscando uno reci?n creado...')
    # Pruebo a ver si existe como uno interno temporal
    if (OBJETOt) {
      OBJETO <- OBJETOt
      print('Se ha creado un objeto temporal que se podr? usar')
    } else {
      print('No hay objeto temporal')} }
  if (OBJETO) {
    if (completo %in% list.files()) { # Guardo el archivo con ese nombre, pero pregunto antes si veo que ya est?
      SOBREESCRIBIRa <- if (!is.null(sobreescribirA)) {sobreescribirA} else {
        if (length(grep(pattern = afirmativo, x = readline(prompt = 'Parece que ya existe el archivo ?quieres sustituirlo?'))) != 0) {TRUE} else {FALSE}
      }
      if (SOBREESCRIBIRa) {
        saveRDS(object = GUARDAR, file = completo)
        print('?Archivo sustitu?do!')
      } else {
        print('Archivo antiguo no modificado')
      }} else {
        print('Guardando...')
        saveRDS(object = GUARDAR, file =  completo)
        print(paste0('El archivo no exist?a y se ha guardado bajo el nombre: ', completo))
      }} else {print('?Oh no!.. No hay objeto para generar el archivo :(')}
  return(GUARDAR)
}


# varfor: dado un vector o palabra, añade al principio, al final y a ambos diferentes s?mbolos, luego ordena el resultado por longitud y alfabeticamente
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


# Copiar objeto al portapapeles
copy.table <- function(obj, size = 4096, filasN = FALSE, columnasN = TRUE) {
  clip <- paste('clipboard-', size, sep = "")
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = filasN, col.names = columnasN, sep = '\t')
  close(f)
}


# Pegar del cortapapeles a R
paste.table <- function(titulo = TRUE) {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = titulo)
  close(f)
  return(df)
}

# Funci?n para ordenar r?pidamente las columnas de una tabla, alfab?ticamente, por lo largo de los nombres o por ambos. Tambi?n se puede especificar si quiero una primera o ?ltima especial. Si se ordena por ambas cosas, la longitud tiene preferencia, pero siempre se puede usar la funci?n dos veces si se quiere al rev?s.
coldenar <- function(tabla, alfabetico = TRUE, largo = FALSE, primero = NA, ultimo = NA) {
  especiales <- c(primero, ultimo)
  columnisP <- colnames(tabla)
  columnis <- columnisP[!columnisP %in% especiales]

  print('Columnas preordenadas: '); print(columnisP)
  ALFA <- if (alfabetico != 'NO') {
    sort(columnis, decreasing = alfabetico)
  } else {columnis}
  LARGO <- if (largo != 'NO') {
    ALFA[order(nchar(ALFA), decreasing = largo)]
  } else {ALFA}
  ordenP <- c(primero, LARGO, ultimo)
  orden <- ordenP[!is.na(ordenP)]
  print('Columnas ordenadas: '); print(orden)

  return(tabla[,orden])
}

# rangoC: crea un rango entre dos o m?s colores en el orden en que se de (si solo se pone un color no crea rango, solo repite el color). Tiene opci?n de crear en vez del rango un vector de colores aleatorios, con el mismo n?mero de colores que se de en el rango. O, finalmente, un rango entre el n?mero de colores aleatorios que se indique (el n?mero de colores en el rango en s? a?n es el mismo que el indicado antes)
rangoC <- function(tonos = 33, colores = 'blue, green, red', ver = TRUE, aleacol = FALSE, alearan = FALSE, mezclados = FALSE) {
  # C?digo para crear un n?mero hexadecimal de 6 caracteres al azar. Primer creo uno muy largo y liego lo divido en trozos de 6
  ALEACOL <- paste0('#',strsplit(x = paste0(sample(x = c(0:9, LETTERS[1:6]), size =  tonos*6, replace = TRUE), collapse = ''), split = paste0("(?<=.{", 6, "})"), perl = TRUE)[[1]])
  # Funci? principal del rango de colores
  coloresF <- colorRampPalette(fector(colores))
  coloresD <- if (aleacol == FALSE) {
    if (mezclados == TRUE) {sample(coloresF(tonos))} else {coloresF(tonos)}} else {ALEACOL}
  # Condicional por si se ha hecho un rango de colores aleatorios
  if (alearan != FALSE) {
    colores <- paste0('#',strsplit(x = paste0(sample(x = c(0:9, LETTERS[1:6]), size =  alearan*6, replace = TRUE), collapse = ''), split = paste0("(?<=.{", 6, "})"), perl = TRUE)[[1]])
    coloresF <- colorRampPalette(colores)
    coloresD <- if (mezclados == TRUE) {sample(coloresF(tonos))} else {coloresF(tonos)}
  }
  # Mostrar los colores creados si as? lo deseo
  if (ver == TRUE) {
    barplot(1:tonos, col = coloresD, names.arg = coloresD)}
  return(coloresD)
}

# mezcol: permite mezclar dos o m?s colores. Por defecto muestra un gr?fico donde se ve el color resultante y los entrantes, con nombres.
mezcol <- function(colores = 'blue,red', ver = TRUE) {
  colores <- fector(colores)
  # Si es impar
  if (length(colores) %% 2 != 0) {
    primero <- colores[1]
    coloris <- colores[-1]} else {
      primero <- NULL
      coloris <- colores
    }
  parejas <- length(coloris) / 2
  for (pareja in seq(parejas)) {
    posicion <- pareja * 2
    amez <- c(coloris[posicion - 1], coloris[posicion])
    coloresF1 <- colorRampPalette(amez)
    color1 <- coloresF1(3)[2]
    if (length(primero) == 0) {
      primero <- color1
      print(primero)
    } else {
      coloresF2 <- colorRampPalette(c(color1, primero))
      color2 <- coloresF2(3)[2]
      primero <- color2
      print(primero)
    }}
  if (ver == TRUE) {
    barplot(1, col = primero, names.arg = primero)
    barplot(1:length(colores), col = colores, names.arg = colores)}
  return(primero)
}



# tablaBED: crea una tabla BED 'facilmente' especificando un vector con los nombres de las columnas que se tienen. Hay que poner NA en los huecos o, poner FALSE en esa columna como argumento. Tiene opci?n de poner color y cromosoma concreto, de guardar y de darle la vuelta a las secuencias. Esto es porque por alg?n motivo me salen al reves con lo del bisulfito
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
      paste0(TABLAs$Nombre,'->',reverseComplement(DNAStringSet(TABLA[,Secuencia])))
    }}
  # Si as? lo he indicado, guardo la tabla
  if (Guardar != FALSE) {
    write.table(x = TABLAs, file = paste0(Guardar,' (',Sys.Date(),').bed'), row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  # Muestro las equivalencias de nombres y devuelvo la tabla
  print(TABLAn[,c(2,3)])
  return(TABLAs)
}

# finterval: usando la funci?n rangoC, crea un rango de colores, pero no en un n?mero absurdo, sino que se puede especificar cuantos y, m?s importante, se puede hacer que sea en escala logar?tmica, de forma que  los m?s peque?os se vean m?s diferentes. Por defecto tiene tonos verde claro a oscuro y trae un vector como ejemplo. LA opci?n predeterminada son los fragmentos de tama?o fijo
finterval <- function(VECTORe = sample(x = 7418880, size = 108), fijos = 23, logaritmicos = FALSE, colores = '#90EE90,#006400', ver = TRUE) {
  # Tama?o m?nimo y m?ximo
  MIN <- min(VECTORe)
  MAX <- max(VECTORe)

  if (logaritmicos == FALSE) { # Fija
    intervalos <- fijos
    # Diferencia y tama?o de intervalos fijos
    DIF <- MAX - MIN
    TIN <- DIF/intervalos
    colores <- rangoC(tonos = intervalos, colores =  colores)

    TABLAs <- data.frame()
    for (intervalo in seq(intervalos)) {
      TROZO <- MIN + (TIN * (intervalo - 1))
      TABLAs <- rbind(TABLAs, c(TROZO, colores[intervalo]))
    }
    colnames(TABLAs) <- fector('Intervalos, Colores')
    tINT <- numvertir(TABLAs)
    # Logar?tmica
  } else {
    factor <- logaritmicos
    TROZO <- MIN
    FRAGS <- c()
    while (TROZO <= MAX) {
      TROZO <- TROZO * factor
      FRAGS <- append(FRAGS, TROZO)
    }
    fragmentos <- length(FRAGS)
    TABLAs <- data.frame()
    colores <- rangoC(tonos = fragmentos, colores =  colores)

    for (fragmento in seq(length(FRAGS))) {
      FRAGp <- FRAGS[fragmento-1]
      FRAG <- if (length(FRAGp) == 0) {0} else {FRAGp}
      TROZO <- MIN + FRAG
      TABLAs <- rbind(TABLAs, c(TROZO, colores[fragmento]))
    }
    colnames(TABLAs) <- fector('Intervalos, Colores')
    tINT <- numvertir(TABLAs)
  }

  TABLAe <- tINT
  TABLAm <- data.frame(VECTORe)
  TABLAm$Color <- ''
  for (fila in seq(nrow(TABLAm))) {
    FILA <- TABLAm[fila,]
    COLOR <- TABLAe$Colores[order(TABLAe$Intervalos[TABLAe$Intervalos <= FILA$VECTORe], decreasing = TRUE)[1]]
    TABLAm$Color[fila] <- COLOR
  }
  tCOL <- TABLAm
  return(tCOL)
}


# SI.ERROR, equivalente a su funci?n hom?nima de excel. Es la segunda vez que la hago porque la borr? sin querer. Esta vez la he juntado con ESERROR tambi?n de excel porque con la opci?n de un mensaje personalizado o funci?n, valen para lo mismo. Me he basado en esto: https://stackoverflow.com/questions/31214361/what-is-the-r-equivalent-for-excel-iferror
SI.ERROR <- function(intento, alternativa = TRUE, mensajeNEG = NA) {
  OBJt <- try(intento, silent = TRUE)
  if (class(OBJt) == "try-error" | length(OBJt) == 0) {alternativa} else {
    if (is.na(mensajeNEG)) {intento
    } else {
      mensajeNEG}}
}

