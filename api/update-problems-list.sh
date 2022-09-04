#!/bin/sh

set -e

output=lists/problems.json

set -x

./api/list-problems.sh | jq . > ${output}.download
mv ${output}.download ${output}
