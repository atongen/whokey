#!/usr/bin/env bash

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )" || exit 1

mkdir -p ./dist
rm -f ./dist/whokey
docker rmi whokey

set -Eeuo pipefail

docker build -t whokey --network=host .

docker run \
    --rm \
    --detach \
    --network=host \
    -it whokey

container_id=`docker ps | grep whokey | awk '{print $1}'`

docker cp $container_id:/whokey/_build/default/src/whokey.exe ./dist/whokey

docker kill $container_id
