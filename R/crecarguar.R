#' Crea o crecarga, la función soñada
#'
#' Experimental, para evitar problemas usar a continuación:
#' if(class(nombre) == 'try-error') {rm(nombre)}
#'
#' @param intento
#' @param nombre
#' @param sobrescribirO
#' @param sobrescribirA
#' @param exactoO
#' @param exactoA
#' @param donde
#' @param formato
#'
#' @return
#' @export
#'
#' @examples
crecarguar <- function(intento, nombre, sobrescribirO = FALSE, sobrescribirA = FALSE, exactoO = TRUE, exactoC = TRUE, donde = paste0(getwd(),'/'), formato = "(%Y-%m-%d)") {
  # Intento Carguar el objeto
  if (carguar(nombre = nombre, sobrescribirO = sobrescribirO, exactoO = exactoO, exactoC = exactoC, sobrescribirA = sobrescribirA, donde = donde, formato = formato) == "No hay objetos ni archivos compatibles :(") {
    # Si no puedo, lo creo con la función que sea
    print('Intentando generar el objeto con la función...')
    OBJt <- try(intento, silent = TRUE)
    if (class(OBJt) == "try-error" | length(OBJt) == 0) {print(paste0("¡Oh no! al intentar generarlo da este error: '", OBJt,"'."))} else {
      assign(x = nombre, value = OBJt)
      print("¡Se ha generado el objeto!")
      carguar(nombre = nombre, sobrescribirO = sobrescribirO, exactoO = exactoO, exactoC = exactoC, sobrescribirA = sobrescribirA, donde = donde, formato = formato)
    }
  }
}
