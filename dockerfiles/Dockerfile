# Base image
FROM rocker/ml-verse:4.2.3

ARG DEBIAN_FRONTEND=noninteractive

RUN pip install --upgrade pip
RUN pip install virtualenv

USER rstudio
RUN mkdir -p /home/rstudio/.virtualenvs/

WORKDIR /home/rstudio/.virtualenvs/
RUN virtualenv tidymodels
RUN /home/rstudio/.virtualenvs/tidymodels/bin/python -m pip install tensorflow

USER root
WORKDIR /home/rstudio/
