#!/bin/bash

repos="r1docker"

if ! command -v docker &> /dev/null
then
    echo "Docker is not installed. Please install Docker to proceed."
    exit 1
fi

docker build -t $repos -f ./dockers/$repos/Dockerfile .
repos="r2docker"
docker build -t $repos -f ./dockers/$repos/Dockerfile .
repos="r3docker"
docker build -t $repos -f ./dockers/$repos/Dockerfile .
repos="r4docker"
docker build -t $repos -f ./dockers/$repos/Dockerfile .
repos="r5docker"
docker build -t $repos -f ./dockers/$repos/Dockerfile .