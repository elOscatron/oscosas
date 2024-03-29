#' fector: Crear vectores facilmente a partir de texto
#'
#' Función para hacer vetores facilmente, escribo entre comillas los elementos separados por comas (o lo que especifique en el argumento 'sep') con o sin espacios y me los da como un vector separado de verdad. Con la actualización, puedo poner separadores o espacios repetidos y/o separadores iniciales y finales y me los elimina
#'
#' @param VECTOR Entre comillas, las palabras, numeros o lo que sea, que quiero hacer un vector, separadas por cualquier elemento, pero siempre el mismo
#' @param sep Por defecto es una coma, pero puede ser cualquier cosa. Lo creé para los modelos que vienen separados por un +
#' @param ver Si quiero ver cuantos pasos es necesario para separarlos en el bucle erosivo
#' @param extraer SI quiero el vector en formato para copiarlo: c('1','3','3','tg','w')
#'
#' @return
#' @export
#'
#' @examples
#' fector('rtvre`v`rtv`rvèrtv`r``trvr`v``trvtèr`vr`tv`rte`vert`v``  ``` ```  ``tv`rt',ver = TRUE, sep = '`')
#'
fector <- function(VECTOR, sep = ',', ver = FALSE, extraer = FALSE) {
  if(length(VECTOR) != 1) { # Si el vector es ya un vector real me salto la función
    VECTOR4 <- VECTOR
  } else {
    if (VECTOR == '' | is.na(VECTOR) | is.null(VECTOR)) { # También si está vacío, es NA o NULL
      VECTOR4 <- VECTOR
    } else  {
      # Corrijo el separador
      sep <- paste0('\\',sep)
      # Establezco los patrones
      patrones <- c(paste0(sep,'{2,}'), paste0(sep,' +',sep))
      # Pego el caracter de escape para que pueda usar muchas cosas como separación
      previo <- VECTOR
      # Preparo un bucle erosivo para eliminar patrones persistentes
      posterior <- ''
      bucle <- 0
      while (previo != posterior | posterior == '') {
        for (PATRON in patrones) {
          posterior <- previo
          previo <- gsub(pattern = PATRON, replacement = sep , x = posterior)
        }
        bucle <- bucle + 1
        if (ver) {print(paste0('Bucle nº ',bucle, ', vector de ',nchar(previo),' caractEres: ', previo))}
      }
      VECTOR2 <- posterior
      # Quito las solitarias del final y principio
      VECTOR3 <- gsub(pattern = paste0(sep,'$'), replacement = '', x =
                        gsub(pattern = paste0('^',sep), replacement = '', x = VECTOR2))
      # Separo el vector usando el separador con los espacios extra que haya
      VECTOR4 <- unlist(stringr::str_split(string = VECTOR3, pattern = paste0(' *',sep,' *')))
      if(extraer) {cat('\n',paste0('c(',paste0("'",VECTOR4,"'", collapse = ","),')'),'\n\n')}
    }}
  return(VECTOR4)
}
