#' Vector de tonos de colores para representar un vector de números
#'
#' Usando la función rangoC, crea un rango de colores, pero no en un número absurdo, sino que se puede especificar cuantos y, más importante, se puede hacer que sea en escala logarítmica, de forma que  los más pequeños se vean más diferentes. Por defecto tiene tonos verde claro a oscuro y trae un vector como ejemplo. La opción predeterminada son los fragmentos de tamaño fijo
#'
#' @param VECTORe El vector para el que tiene que crear los colores, debe ser numérico
#' @param fijos Cuantos colores se quieren crear, se asignarán al número por proximidad
#' @param logaritmicos Por defecto FALSE, requiere un número que indicará el exponencial que se quiere para hacer el rando
#' @param colores Colores entre los que se quiere hacer el rango. Usa rangoC
#' @param ver Pro defecto TRUE, enseña un gráfico con los colores
#'
#' @return Tabla con los números y su color asignado
#' @export
#'
#' @examples
finterval <- function(VECTORe = sample(x = 7418880, size = 108), fijos = 23, logaritmicos = FALSE, colores = '#90EE90,#006400', ver = TRUE) {
  # Tamaño mínimo y máximo
  MIN <- min(VECTORe)
  MAX <- max(VECTORe)

  if (logaritmicos == FALSE | logaritmicos < 1.1) { # Fija
    intervalos <- fijos
    # Diferencia y tamaño de intervalos fijos
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
    # Logarítmica
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
