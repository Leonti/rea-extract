#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

sudo docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:18.04.04.03.12

sudo docker build -t leonti/rea-extract:$version .
sudo docker push leonti/rea-extract:$version

echo $version" is built"
