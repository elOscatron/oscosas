

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

