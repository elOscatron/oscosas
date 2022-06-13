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



Cosa de la memoria:


```{r}
# Memoria del ordenador en GB (en Virgilio 31.70801, que cuadra con los 31,7 que dice windows, cambia si le digo que aumente una locura) en el super son 127.7256:
memory.limit(size = NA) / 1024

# Memoria que está usando ahora mismo en GB (es variable, pero en virgilio, en reposo reción conectado es de menos de en torno a 0.11), en el super 3.50416:
memory.size(max = FALSE) / 1024
# La máxima memoria que windows le está dando, en Virgilio sin tocar nada es casi igual a lo anterior, también varía.
memory.size(max = TRUE) / 1024
# La memoria máxima que hay en el sistema, como memory.limit(size = NA), es en Virgilio de 31.70848, algo mayor de lo que dice con la otra función, porque esta reondea al alza lo bytes
memory.size(max = NA) / 1024

# memory.limit(size = XX) es la que puede cambiar el tamaño, si se lo especifico. Por defecto me da fallo si no pongo un número loco (17.5e+12)
memory.limit(size = 17.5e+18)
````
