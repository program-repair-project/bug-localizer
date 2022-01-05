FROM openjdk:8-jdk
MAINTAINER oojahooo <oojahooo@gmail.com>

WORKDIR /unival/

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y r-base

RUN Rscript -e "install.packages(c(\"glmnet\", \"ranger\", \"stringdist\", \"dbscan\", \"dplyr\", \"tidyr\"))"

ADD src/* /unival/
