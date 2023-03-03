#' fovec: Mover de posición una columna
#' Si no se especifica columna, cogerá la del principio
#'
#' @param TABLAe La tabla que contiene la columna a mover
#' @param nomcol El nombre de la columna a mover
#' @param numcol El número de la columna a mover (si el nombre ya está, este argumento se ignorará)
#' @param posiciOn Donde se quiere poner, por defecto al principio
#'
#' @return
#' @export
#'
#' @examples
fovec <- function(TABLA, nomcol = NULL, numcol = NULL, posiciOn = 1, silencio = FALSE) {
  if (is.null(nomcol) && is.null(numcol)) {
    nomcol <- names(TABLA)[length(names(TABLA))]
    if (!silencio) {
      message("No se ha especificado ninguna columna para mover. Se moverá la última columna de la tabla.")
    }
  } else if (!is.null(nomcol) && !is.null(numcol)) {
    if (!silencio) {
      message("Se ha especificado tanto el nombre como el número de columna. Se usará el nombre.")
    }
  }

  # Obtener el índice actual de la columna
  curr_pos <- which(names(TABLA) == nomcol)

  if (curr_pos == posiciOn) {
    # Si la columna ya está en la posición deseada, retornar el data frame original
    return(TABLA)
  } else if (curr_pos == 0) {
    # Si la columna no existe, imprimir un mensaje de error y retornar el data frame original
    if (!silencio) {
      message(paste("La columna", nomcol, "no existe en el data frame."))
    }
    return(TABLA)
  } else {
    # Crear una nueva lista de columnas con la columna movida a la posición deseada
    new_cols <- c(nomcol, names(TABLA)[-curr_pos])
    new_cols <- new_cols[c(posiciOn, seq_along(new_cols)[-posiciOn])]

    # Retornar el data frame con las columnas en el nuevo orden
    return(TABLA[new_cols])
  }
}
