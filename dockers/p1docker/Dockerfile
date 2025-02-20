FROM debian:11-slim

WORKDIR /scripts

RUN apt-get update && \
    apt-get install -y python3 python3-pip && \
    rm -rf /var/lib/apt/lists/*

RUN pip3 install numpy==2.0.2
RUN pip3 install pandas==2.2.3
RUN pip3 install scikit-learn==1.6.1
RUN pip3 install joblib==1.4.2