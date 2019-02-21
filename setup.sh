#!/bin/sh
set -e

SCRIPT_DIR=$(cd $(dirname $0); pwd)

mv installer.sh ../
docker image build -t hybridpdr:mathematica-base -f Dockerfile.mathematica .
mv ../installer.sh $SCRIPT_DIR

docker container run -it --name mathematica_installer -v $PWD/installer.sh:/home/opam/installer.sh hybridpdr:mathematica-base

docker commit --change='CMD ["wolfram"]' mathematica_installer hybridpdr:mathematica

docker container rm mathematica_installer
