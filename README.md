# oscosas
Paquete de funciones accesorias

Inicio de los documentos, para instalar el paquete:

{r include = FALSE}
if(!require(devtools)) { install.packages("devtools"); library(devtools) }
if(require(oscosas)) {detach("package:oscosas", unload = TRUE); install_github('elOscatron/oscosas', force = TRUE, upgrade = FALSE); library(oscosas)
  } else {install_github('elOscatron/oscosas', force = TRUE, upgrade = TRUE); library(oscosas)}



Final de los documentos, para limpiar:

{r Limpiar el área de trabajo, include = FALSE, eval = FALSE}
princ <- paste0(dirname(getwd()),'/', '00-Comun', '/')
arch <- c(paste0(princ,list.files(princ)), paste0(getwd(),'/',list.files(getwd())))
aDC <- arch[grep(pattern = '.R$', x = arch)]
sapply(X = paste0(aDC), FUN = source, chdir = TRUE)

descargar()
rm(list = ls(all = TRUE))
