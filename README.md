# prediccion-morosidad

Un banco ha revolucionando la manera en que controla sus costos de cobranza por incumplimiento de pago. Con la ayuda de un equipo de científicos de datos el banco desarrolló la capacidad de analizar el comportamiento de miles de acreditados y predecir con alta precisión qué acreditados van a hacer pagos impuntuales y incumplir en sus pagos. 

Este repositorio genera un documento en el que se presentan los detalles de este caso de negocio:

* negocio - institución otorgadora de crédito
* problema de negocio - altos costos del área de cobranza
* solución explorada - contar con predicciones de impuntualidad de pago que permitan accionar medidas preventivas y evitar incumplimientos

En el estudio de caso se describe cómo se creo el producto de datos que genera las predicciones.

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
docker run --rm -e PASSWORD=book -p 8787:8787 -v $(pwd)/book:/home/rstudio/book -v $(pwd)/pipeline:/home/rstudio/pipeline prediccion-morosidad
```

Una vez que el contenedor está corriendo hay que ir a `localhost:8787` en el browser y proporcionar el usuario `rstudio` y la contraseña `book`. 

En Rstudio se abre el proyecto `/home/rstudio/book/book.Rproj`. El documento se genera al oprimir el botón `build book`. Una vez generado, lo encontrarás en `book/_book/book.pdf.
