FROM debian:11-slim

WORKDIR /scripts


# Instala pacotes básicos para HTTPS, gerenciamento de chaves e compilação
RUN apt-get update && apt-get install -y --no-install-recommends \
    dirmngr gnupg apt-transport-https ca-certificates \
    build-essential libcurl4-openssl-dev libssl-dev libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Adiciona a chave GPG do CRAN
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys B8F25A8A73EACF41

# Adiciona o repositório CRAN para Debian Bullseye
RUN echo "deb https://cloud.r-project.org/bin/linux/debian bullseye-cran40/" > /etc/apt/sources.list.d/cran.list

# Atualiza os repositórios e instala o R base
RUN apt-get update && apt-get install -y --no-install-recommends r-base && \
    rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('tibble'), repos='https://cloud.r-project.org')"

COPY scripts/*.R ./
