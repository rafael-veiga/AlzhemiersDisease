FROM r-base:4.4.2

WORKDIR /script

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages('tidyverse', version = '2.0.0', repos = 'https://cloud.r-project.org/')"