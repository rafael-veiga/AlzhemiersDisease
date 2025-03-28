FROM r2docker

RUN apt-get update && apt-get install -y libpng-dev libjpeg-dev

WORKDIR /scripts

RUN Rscript -e "install.packages(c('ggplot2','dplyr','cowplot','ggtext','ggExtra','tidyr','ComplexHeatmap','circlize'), repos='https://cloud.r-project.org')"

COPY scripts/pars_1.R ./
COPY scripts/fig*.R ./
