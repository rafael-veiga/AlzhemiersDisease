FROM r2docker

WORKDIR /scripts

RUN apt-get update && apt-get install -y --no-install-recommends \
    cmake liblapack-dev libblas-dev gfortran && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('VIM'), repos='https://cloud.r-project.org')"

