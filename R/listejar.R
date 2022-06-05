#' Crear los objetos contenidos en una lista
#' Usa el nombre que los objetos tienen en la lista para crearlos. Se puede especificar que no se quiere superar un límite, este es por defecto 23
#' @param LISTAe
#' @param limite
#'
#' @return
#' @export
#'
#' @examples
lisjetar <- function(LISTAe, limite = 23) {
  for (elemento in seq(length(LISTAe))) {
    if (elemento <= limite | limite == FALSE | is.na(limite)) {
      ELEMENTO <- LISTAe[[elemento]]
      NOM <- names(LISTAe)[[elemento]]
      assign(x = NOM, value = ELEMENTO, envir = .GlobalEnv)
    } else {
      print(paste0("L?mite de ", limite," objetos superado, parando en '", NOM,"'"))
      break
    }
  }
}
