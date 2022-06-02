# oscosas
Paquete de funciones accesorias

requiere de:
https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5253-5107.exe

Inicio de los documentos, para instalar el paquete:

```{r Instalar mi paquete, include = FALSE}
if(!require(devtools)) { install.packages("devtools"); library(devtools) }

if(require(oscosas)) {detach("package:oscosas", unload = TRUE); install_github('elOscatron/oscosas', force = TRUE, upgrade = FALSE); library(oscosas)
  } else {install_github('elOscatron/oscosas', force = TRUE, upgrade = TRUE); library(oscosas)}
  
```


```{r Actualizar R, include = FALSE, eval = FALSE}
if(!require(installr)) { install.packages("installr"); library(installr) }
updateR()
```


```{r Limpiar el área de trabajo, include = FALSE, eval = FALSE}
descargar()
rm(list = ls(all = TRUE))
````
