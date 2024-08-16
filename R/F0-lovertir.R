#' Convertir en lOgicas todas las columnas posibles de una tabla
#'
#' Coge una tabla y transforma todas las columnas que puedas ser transformadas en lOgicas a lOgicas, con variantes de NA incluIdas, basada en numvertir, a su sacada de aquI. También traduce del espaniol
#' https://stackoverflow.com/questions/32846176/applying-as-numeric-only-to-elements-of-a-list-that-can-be-coerced-to-numeric-i
#' @param TABLAe
#'
#' @return
#' @export
#'
#' @examples

lovertir <- function(TABLAe) {
  vCOL <- colnames(TABLAe)
  vFIL <- rownames(TABLAe)

  # Función personalizada para convertir a numérico o lógico
  FUNCIONt <- function(x) {
    # Reemplazar variantes de NA y NaN con NA
    x[x %in% c("NA", "NaN", "N/A", "na", "nan", "n/a")] <- NA
    x[x %in% c("VERDADERO", "Verdadero", "verdadero")] <- "TRUE"
    x[x %in% c("FALSO", "Falso", "falso")] <- "FALSE"

    if (all(x %in% c("TRUE", "FALSE", NA))) {
      return(as.logical(x))
    } else {
      return(type.convert(x, as.is = TRUE))
    }
  }

  TABLAs <- data.frame(lapply(TABLAe, FUNCIONt))
  colnames(TABLAs) <- vCOL
  rownames(TABLAs) <- vFIL
  return(TABLAs)
}
