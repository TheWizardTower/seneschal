#!/usr/bin/env bash
set -euo pipefail

COMMAND="pwd
sleep 5 && echo boom
echo Howdy
whoami"

echo "$COMMAND" | seneschal --debug --telemetry=console
