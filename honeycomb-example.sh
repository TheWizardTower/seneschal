#!/usr/bin/env bash
set -euo pipefail

COMMAND="echo Howdy
pwd
sleep 1 && echo Almost
sleep 2 && echo There
sleep 3 && echo Just
sleep 4 && echo A
sleep 5 && echo Little Further
whoami"

echo "$COMMAND" | seneschal --telemetry=honeycomb --dataset=seneschal
