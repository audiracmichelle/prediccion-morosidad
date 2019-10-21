FROM rocker/tidyverse:3.5.0

WORKDIR /app

# System dependencies
RUN apt-get update \
    && apt-get install -y \
      libssl-dev \
      libcurl4-openssl-dev \
      libxml2-dev

# R dependencies
RUN install2.r --error --deps TRUE feather
RUN install2.r --error --deps TRUE xts
RUN install2.r --error --deps TRUE aws.s3

ADD wrangle_train.R .

ENTRYPOINT ["Rscript", "/app/wrangle_train.R"]
