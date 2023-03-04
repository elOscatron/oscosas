#' fablazar: generar tabla rApidamente con elementos al azar. Muy Utilo para probar cOdigo o nuevas funciones
#'
#' @param rows NUmero de filas que se quiere poner, si no se pone nada serA al azar entre 2 y 6
#' @param columns NUmero de columnas que se quiere poner, si no se pone nada serA al azar entre 2 y 6
#' @param numbers Si se quiere que la tabla consista de nUmeros. Si no se pone nada, se deja al azar. SI se pone FALSE no habrA y si se pone TRUE, y tambiEn en letters y NAlazar, se mezclarAn
#' @param letters Lo mismo de antes pero con letras
#' @param NAlazar Si se quieren introducir NAs al azar en la tabla, por defecto FALSE
#' @param matriz Si se quiere que sea una matriz en vez de una tabla, por defecto FALSE
#'
#' @return
#' @export
#'
#' @examples
#' fablazar(rows = 8, columns = 6, numbers = TRUE, letters = TRUE)
#' fablazar(rows = 8, columns = 6)
#' fablazar(rows = 8, columns = 6, NAlazar = TRUE)
#' fablazar(NAlazar = TRUE)
#'
fablazar <- function(rows = NULL, columns = NULL, numbers = sample(c(TRUE, FALSE), 1),
                     letters = sample(c(TRUE, FALSE), 1), NAlazar = FALSE, matriz = FALSE){
  if (is.null(rows)) {
    rows <- sample(x = 2:6, size = 1)
  }
  if (is.null(columns)) {
    columns <- sample(x = 2:6, size = 1)
  }
  total_cells <- rows * columns
  MATRIZ <- numeric(total_cells)
  for (i in 1:total_cells) {
    if (numbers & letters | !numbers & !letters) { # Tabla con nUmeros y letras
      if (runif(n = 1) < 0.5) {
        MATRIZ[i] <- round(runif(n = 1, min = 0, max = 100))
      } else {
        MATRIZ[i] <- sample(x = LETTERS, size = 1)
      }
    } else if (numbers) { # Tabla con  nUmeros
      MATRIZ[i] <- round(runif(n = 1, min = 0, max = 100))
    } else if (letters) { # Tabla con letras
      MATRIZ[i] <- sample(x = LETTERS, size = 1)
    }
  }
  MATRIZ <- matrix(MATRIZ, nrow = rows, ncol = columns)
  if (NAlazar == TRUE) { # NAs repartidos al azar
    VECTORt <- sample(x = 1:total_cells, size = floor(x = total_cells * 0.2), replace = TRUE)
    MATRIZ[VECTORt] <- NA
  }
  colnames(MATRIZ) <- paste("C", seq(columns), sep = "")
  rownames(MATRIZ) <- paste("F", seq(rows), sep = "")
  TABLAs <- if (matriz) {MATRIZ} else {data.frame(MATRIZ)}
  return(TABLAs)
}
