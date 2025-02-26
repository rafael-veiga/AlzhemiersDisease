FROM r2docker

WORKDIR /scripts

RUN Rscript -e "install.packages(c('ggplot2'), repos='https://cloud.r-project.org')"

COPY scripts/pars_1.R ./
