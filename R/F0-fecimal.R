#' fecimal, convierte los números de una tabla de un sistema decimal al otro, es decir cambis comas por puntos o viceversa
#' Si Ambas opciones son iguales, ´convertirá a aquella más infrecuente, por ejemplo, si hay 7 columnas con comas y dos con puntos, pasará todo a puntos. De haber empate se pasará a comas
#'
#' @param TABLA Tabla a convertir
#' @param acomas Si se quiere convertir a comas (por defecto FALSE)
#' @param apuntos Si se quiere convertir a puntos (por defecto FALSE)
#' @param silencio Si se quiere ver la información
#'
#' @return
#' @export
#'
#' @examples
fecimal <- function(TABLA, acomas = FALSE, apuntos = FALSE, silencio = TRUE) {
  # Convertir las columnas potencialmente numéricas a numéricas reales
  TABLAm <- data.frame(lapply(TABLA, type.convert, as.is = TRUE))
  COLUMNAS <- colnames(TABLAm)
  # Detectar las columnas que tienen numeros con puntos (números decimales)
  PUNTOS <- COLUMNAS[sapply(TABLAm, is.double)]
  # Ahora detectar las quer tienen comas... (al principio, al final, en medio, con negativos y con notación científica) bastante más dificil
  # Esta expresión regular funciona casi, pero no puede con las comas al final  "^-?[0-9]*\\,*[0-9]+([eE][-+]?[0-9]+)?$"
  COMASp <- COLUMNAS[sapply(TABLAm, function(x) all(grepl("^-?([0-9]+\\,*[0-9]*|[0-9]*\\,+[0-9]+)([eE][-+]?[0-9]+)?$", x)))]
  # El esto de columnas
  ENTEROSp <- COLUMNAS[sapply(TABLAm, is.numeric)]
  ENTEROS <- ENTEROSp[!ENTEROSp %in% PUNTOS]
  COMAS <- COMASp[!COMASp %in% ENTEROS]
  # Veo cual es la que voy a transformar
  CUAL <- if(acomas == apuntos) {if(length(COMAS) <= length(PUNTOS)) {'COMAS'} else {'PUNTOS'}} else {if(acomas) {'COMAS'} else {if(apuntos) {'PUNTOS'}}}
  # Hago la transformación
  if(CUAL == 'COMAS') {
    TABLAm[PUNTOS] <- lapply(TABLAm[PUNTOS], function(x) gsub(pattern = "\\.", replacement = ",", x = x))
  } else {
    TABLAm[COMAS] <- lapply(TABLAm[COMAS], function(x) gsub(pattern = ",", replacement = ".", x = x))
    TABLAm <- data.frame(lapply(TABLAm, type.convert, as.is = TRUE))
  }
  if(!silencio)
  {print(paste0('La tabla pasará a tener sus decimales separados por ',CUAL, ' especificamente en las columnas:'))
    print(if(CUAL == 'COMAS') {PUNTOS} else {COMAS} )}
  return(TABLAm)
}
