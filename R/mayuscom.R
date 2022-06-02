#' Title conversión entre mayúsculas y minúsculas
#'
#' Convierte en may?sculas, min?sculas y may?sculas la primera letra
#' @param VECTORe
#'
#' @return
#' @export
#'
#' @examples
mayuscom <- function(VECTORe) {
  VECTORs <- sort(unique(c(VECTORe, stringr::str_to_title(VECTORe), R.utils::capitalize(VECTORe), toupper(VECTORe), tolower(VECTORe), stringr::str_to_lower(VECTORe))))
  return(VECTORs)
}
