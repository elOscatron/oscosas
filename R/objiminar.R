#' Elimina mis clásicos objetos ambiguos
#'
#' Especifico los nombres de los objetos que suelo crear continuamente en minúsculas o mayusculas. También pongo las tipicas letras que suelo añadir. Finalmente los números. Y los buscará y elminará
#'
#' @param nombres Nombres de los objetos a eliminar
#' @param cuantos Número de versiones que quiero eliminar, por ejemplo, si pongo 2, eliminará TABLA, TABLA1 y TABLA2. Vale para las versiones previas (p/P)
#' @param letras Las letras que va a buscar en los objetos, por defecto son e, s, m, t (entrante, saliente, mixta y temporal)
#'
#' @return Mensaje de que ha eliminado, o si no lo ha hecho
#' @export
#'
#' @examples
objiminar <- function(nombres = 'objeto, lista, elemento, elementi, elementini, elementinio, tabla, matriz, vector, fila, columna, atomo, bucle', cuantos = 9, letras ='e, s, m, t'){
  # Fectorizo las variables
  nombres <- fector(nombres)
  letras <- fector(letras)

  ## Elemetos 'pre', en mayúisculas y minúsculas
  TABLAt <- expand.grid('P', c('',seq(cuantos)))
  MAYUP <- paste0(TABLAt$Var1, TABLAt$Var2)
  TABLAt <- expand.grid('p', c('',seq(cuantos)))
  minup <- paste0(TABLAt$Var1, TABLAt$Var2)

  # Objetos en ambiente
  ELEMENTOS <- tolower(letras)
  OBJETOS <- toupper(nombres)

  TABLAt <- expand.grid(OBJETOS, ELEMENTOS)
  VECTORt2 <- paste0(TABLAt$Var1, TABLAt$Var2)
  TABLAt2 <- expand.grid(VECTORt2, c('',seq(cuantos)))
  VECTORt3 <- paste0(TABLAt2$Var1, TABLAt2$Var2)
  TABLAt3 <- expand.grid(VECTORt3, MAYUP)
  VECTORt4 <- paste0(TABLAt3$Var1, TABLAt3$Var2)

  VECTORt5 <- append(VECTORt3, VECTORt4)

  TABLAt4 <- expand.grid(OBJETOS, c('',seq(cuantos)))
  VECTORt6 <- paste0(TABLAt4$Var1, TABLAt4$Var2)
  TABLAt5 <- expand.grid(OBJETOS, c('',minup))
  VECTORt7 <- paste0(TABLAt5$Var1, TABLAt5$Var2)

  VECTORt8 <- append(VECTORt5, VECTORt7)
  oAMBIENTE <- VECTORt8

  # Objetos en bucle
  ELEMENTOS <- toupper(letras)
  OBJETOS <- tolower(nombres)

  TABLAt <- expand.grid(OBJETOS, ELEMENTOS)
  VECTORt2 <- paste0(TABLAt$Var1, TABLAt$Var2)
  TABLAt2 <- expand.grid(VECTORt2, c('',cuantos))
  VECTORt3 <- paste0(TABLAt2$Var1, TABLAt2$Var2)
  TABLAt3 <- expand.grid(VECTORt3, minup)
  VECTORt4 <- paste0(TABLAt3$Var1, TABLAt3$Var2)

  VECTORt5 <- append(VECTORt3, VECTORt4)

  TABLAt4 <- expand.grid(OBJETOS, c('', cuantos))
  VECTORt6 <- paste0(TABLAt4$Var1, TABLAt4$Var2)
  TABLAt5 <- expand.grid(OBJETOS, c('',MAYUP))
  VECTORt7 <- paste0(TABLAt5$Var1, TABLAt5$Var2)

  VECTORt8 <- append(VECTORt5, VECTORt7)
  oBUCLE <- VECTORt8

  # Búsqueda
  BUSCAR <- sort(append(oAMBIENTE, oBUCLE))
  ENCONTRADOS <- ls()[ls() %in% BUSCAR]

  # Eliminación o no
  if(length(ENCONTRADOS) == 0) {
    print('No se han encontrado objetosa comunes que eliminar')
  } else {
    print(paste0('Se han eliminado ',length(ENCONTRADOS),' objetos, estos son sus nombres:'))
    print(ENCONTRADOS)
    rm(list = ENCONTRADOS)
  }
}
