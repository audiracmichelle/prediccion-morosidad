FROM rocker/tidyverse:3.5.0

WORKDIR /app

# System dependencies
RUN apt-get update \
    && apt-get install -y \
      libssl-dev \
      libcurl4-openssl-dev \
      libxml2-dev

# R dependencies
RUN install2.r --error --deps TRUE lubridate
RUN install2.r --error --deps TRUE feather
RUN install2.r --error --deps TRUE data.table
RUN install2.r --error --deps TRUE aws.s3

ADD preprocess.R .
ADD buscar_indice_primer_positivo.cpp .
ADD buscar_indice_ultimo_positivo.cpp .

ENTRYPOINT ["Rscript", "/app/preprocess.R"]
