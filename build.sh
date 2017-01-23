#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

sudo docker build -t leonti/rea-extract:$version .

echo $version" is built"
