# Script to enter docker container and link local gem5 installation
# Usage: cd <path/to/gem5-mesh>/docker/ && ./enter_docker.sh

docker run \
    -it gem5-mesh \
    -v $PWD/../:/workspace/gem5-mesh 