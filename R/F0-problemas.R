#' Busca problemas
#'
#' Función sencilla que dice si el objeto no existe, es nulo, de longitud cero, NA o vacío
#'
#' @param OBJETOe Nombre del objeto a investigar
#' @param especificar Por defecto FALSE, si es TRUE se dirá cual es el problema
#'
#' @return
#' @export
#'
#' @examples
problemas <- function(OBJETOe, especificar = FALSE) {
  if(!especificar) {
    if(!exists(OBJETOe)) {TRUE
    } else {
      OBJETOi <- get(OBJETOe)
      if(is.null(OBJETOi)) {TRUE
      } else {
        if(length(OBJETOi) == 0) {TRUE
        } else {
          if(length(OBJETOi != 1)) {FALSE
          } else {
            if(is.na(OBJETOi)) {TRUE
            } else {
              if(OBJETOi == '') {TRUE
              } else {FALSE
              }}}}}}} else {
                if(!exists(OBJETOe)) {'No existe'
                } else {
                  OBJETOi <- get(OBJETOe)
                  if(is.null(OBJETOi)) {'Nulo'
                  } else {
                    if(length(OBJETOi) == 0) {'Longitud 0'
                    } else {
                      if(length(OBJETOi) != 1) {'Bien largo'
                      } else {
                        if(is.na(OBJETOi)) {'No aplicable'
                        } else {
                          if(OBJETOi == '') {'Vacío'
                          } else {'Bien corto'
                          }}}}}}}
}
