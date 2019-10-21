# prediccion_morosidad_wrangle_train

Este contenedor corre la tarea de wrangling de datos. En este paso del pipeline se acomodan series de tiempo para poder entrenar modelos.

Para construir la imagen de `prediccion_morosidad_wrangle_train` hay que usar

```sh
$ docker build -t prediccion_morosidad_wrangle_train . 
```

Aunque de forma predeterminada la tarea de este contenedor lee y guarda archivos en ubicaciones locales dentro de la carpeta `pipeline/`, también es posible especificar un bucket de s3 de lectura y escritura de datos.

Si se quieren utilizar las ubicaciones locales del pipeline, hay que usar

```sh
$ cd .. #hay que moverse a la carpeta pipeline/
$ docker run -it --rm  -v $(pwd)/preprocess/output:/home/preprocess/output -v $(pwd)/wrangle_train/output:/home/wrangle_train/output prediccion_morosidad_wrangle_train <ymd_preprocess> <tag_preprocess> <tag_wrangle_train> <halflife> <min_len_predictors> "local"
```

Para utilizar un bucket de s3 de lectura y escritura de datos, hay que correr el contenedor con la siguiente línea

```sh
$ docker run -it --rm  -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY -e AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION  prediccion_morosidad_wrangle_train <ymd_preprocess> <tag_preprocess> <tag_wrangle_train> <halflife> <min_len_predictors> <bucket_s3>
```

Aquí está la descripción de los parámetros:

<ymd_preprocess> los archivos generados por prediccion_morosidad_preprocess usan esta fecha como suffix. También los archivos generados por prediccion_morosidad_wrangle_train usan esta fecha como suffix 
<tag_preprocess> los archivos generados por prediccion_morosidad_preprocess usan esta etiqueta como suffix
<tag_wrangle_train> los archivos generados por prediccion_morosidad_wrangle_train usan esta etiqueta como suffix
<halflife> halflife para estadísticas móviles
<min_len_predictors> mínima longitud de series de tiempo
