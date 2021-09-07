# Script to enter docker container and link local gem5 installation
# Usage: cd <path/to/gem5-mesh>/docker/ && ./enter_docker.sh

docker run \
    -v $PWD/../:/workspace/gem5-mesh \
    -w /workspace/gem5-mesh \
    --user "$(id -u):$(id -g)" \
    -it ghcr.io/cucapra/gem5-mesh:latest
