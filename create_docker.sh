#!/bin/bash

repos="r1docker"

if ! command -v docker &> /dev/null
then
    echo "Docker is not installed. Please install Docker to proceed."
    exit 1
fi

docker build -t $repos -f ./dockers/$repos/Dockerfile .


