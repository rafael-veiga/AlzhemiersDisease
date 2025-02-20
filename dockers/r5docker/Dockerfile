FROM r2docker

WORKDIR /scripts

RUN Rscript -e "install.packages(c('foreach','doParallel'), repos='https://cloud.r-project.org')"

COPY scripts/logistic_mark.R ./
COPY scripts/logistic_age.R ./
COPY scripts/logistic_mark_par.R ./
