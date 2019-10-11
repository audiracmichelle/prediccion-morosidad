## Build image (you have to be located on the task folder)
```sh
$ cd tasks/credit-default/preprocess
$ docker build -t credit_default_preprocess . 
```
## Run task (sample parameters)
```sh
$ docker run -it --rm  -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY -e AWS_DEFAULT_REGION=$AWS_DEFAULT_REGION  credit_default_preprocess <ymd_sources> <tag_sources> <ymd_preprocess> <tag_preprocess>
```
## Parameter description
* <ymd_sources> fecha de corte correspondiente a la extraccion de datos. Los archivos generados por credit_default_sources usan esta fecha como suffix
* <tag_sources> los archivos generados por credit_default_sources usan esta etiqueta como suffix
* <ymd_preprocess> ultima fecha de corte en matriz de diseño. Los archivos generados por credit_default_preprocess usan esta etiqueta como suffix 
* <tag_preprocess> los archivos generados por credit_default_preprocess usan esta etiqueta como suffix

La fecha de corte de sources <ymd_sources> puede ser posterior a la fecha de corte de preprocesamiento <ymd_preprocess>.

## Output files in
```sh
aws s3 ls s3://datank-concredito/data/preprocess/output/
```
