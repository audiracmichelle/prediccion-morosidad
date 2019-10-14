# prediccion_morosidad_preprocess

Este contenedor corre la tarea de preprocesamiento y limpieza de datos.

Para construir la imagen `prediccion_morosidad_preprocess` hay que usar

```sh
$ docker build -t prediccion_morosidad_preprocess . 
```

Aunque de forma predeterminada la tarea de este contenedor utiliza ubicaciones locales dentro de `pipeline/`, también es posible especificar un bucket de s3 de lectura y escritura de datos.

Si se quieren utilizar las ubicaciones locales del pipeline, hay que usar

```sh
$ docker run -it --rm  prediccion_morosidad_preprocess <ymd_sources> <tag_sources> <ymd_preprocess> <tag_preprocess>
```

Para utilizar un bucket de s3 de lectura y escritura de datos, hay que correr el contenedor con la siguiente línea

```sh
$ docker run -it --rm  -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY -e AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION  prediccion_morosidad_preprocess <ymd_sources> <tag_sources> <ymd_preprocess> <tag_preprocess><bucket_s3>
```

Aquí está la descripción de los parámetros:

* <ymd_sources> fecha de corte correspondiente a la extraccion de datos. Los archivos generados por prediccion_morosidad_sources usan esta fecha como suffix
* <tag_sources> los archivos generados por prediccion_morosidad_sources usan esta etiqueta como suffix
* <ymd_preprocess> ultima fecha de corte en matriz de diseño. Los archivos generados por prediccion_morosidad_preprocess usan esta etiqueta como suffix 
* <tag_preprocess> los archivos generados por prediccion_morosidad_preprocess usan esta etiqueta como suffix
* <bucket_s3> nombre del bucket de lectura y escritura, este es un parámetro opcional

La fecha de corte de sources <ymd_sources> puede ser posterior a la fecha de corte de preprocesamiento <ymd_preprocess>.
