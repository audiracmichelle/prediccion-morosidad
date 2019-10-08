# prediccion-morosidad

*prediccion-morosidad* genera un documento en el que se presenta un caso real de negocio:

* negocio - institución otorgadora de crédito
* problema de negocio - altos costos del área de cobranza
* solución explorada - contar con predicciones de impuntualidad de pago que permitan accionar medidas preventivas y evitar incumplimientos

En el documento se describen los pasos que el negocio siguió para desarrollar el producto de datos que genera las predicciones.

## Cómo ver el documento

...

## Cómo instalar

Para construir la imagen de Docker hay que correr el siguiente código.

```
cd $(pwd)
docker build -t prediccion-morosidad .
```

Esta imagen está basada en `rocker/tidyverse` y utiliza texlive-full.

### Cómo reconstruir el documento

Primero hay que correr el contenedor.

```
docker run --rm -e PASSWORD=book -p 8787:8787 -v $(pwd)/book:/home/rstudio/book prediccion-morosidad
```

Una vez que el contenedor está corriendo hay que ir a `localhost:8787` en el browser y proporcionar el usuario `rstudio` y la contraseña `book`. 

En Rstudio se abre el proyecto `/home/rstudio/book/book.Rproj`. El documento se genera al oprimir el botón `build book`. Una vez generado, lo encontrarás en `book/_book/book.pdf.