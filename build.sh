#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:18.07.04.02.33

docker build -t leonti/rea-extract:$version .
docker push leonti/rea-extract:$version

echo $version" is built"
