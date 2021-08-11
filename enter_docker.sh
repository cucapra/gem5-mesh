# Script to enter docker container and link local gem5 installation
# Usage: cd <path/to/gem5-mesh>/docker/ && ./enter_docker.sh

docker run \
    --user "$(id -u):$(id -g)" \
    -it ghcr.io/cucapra/gem5-mesh:latest
