FROM rocker/r-ver:3.6.1
WORKDIR /usr/src/datastorm_mlops_webinar

COPY ./MLOpsMonitoring_0.1.0.tar.gz ./MLOpsMonitoring_0.1.0.tar.gz
COPY ./install.R ./install.R

RUN apt-get update && apt-get install -y zlib1g-dev
RUN Rscript --vanilla ./install.R

ENTRYPOINT ["R", "-e", "MLOpsMonitoring::run_app(host = '0.0.0.0', port = 3838)"]
