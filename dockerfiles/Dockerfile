# Base image
FROM nvidia/cuda:11.7.1-cudnn8-devel-ubuntu22.04

# Update apt ---------------------------------------
ARG DEBIAN_FRONTEND=noninteractive
RUN apt update

# Setup R ---------------------------------------
LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"

ENV R_VERSION=4.3.0
ENV R_HOME=/usr/local/lib/R
ENV TZ=Asia/Tokyo

COPY scripts/install_R_source.sh /rocker_scripts/install_R_source.sh
RUN /rocker_scripts/install_R_source.sh

ENV CRAN=https://packagemanager.posit.co/cran/__linux__/jammy/latest
ENV LANG=en_US.UTF-8

COPY scripts /rocker_scripts
RUN /rocker_scripts/setup_R.sh

# Setup rstudio --------------------------------
ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=2023.03.0+386
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV QUARTO_VERSION=default

RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_pandoc.sh
RUN /rocker_scripts/install_quarto.sh

EXPOSE 8787

# Setup python ----------------------------------
RUN apt update && apt install python3-pip -y
RUN pip install virtualenv

USER rstudio 
RUN mkdir -p /home/rstudio/.virtualenvs/
WORKDIR /home/rstudio/.virtualenvs/
RUN virtualenv tidymodels

USER root
WORKDIR /home/rstudio/  

RUN apt install -y \
    libglpk-dev \
    libudunits2-dev libgdal-dev libgeos-dev libproj-dev

CMD ["/init"]
