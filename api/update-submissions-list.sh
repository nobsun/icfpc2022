#!/bin/sh

set -e

output=lists/submissions.json

set -x

./api/list-submissions.sh | jq . > ${output}.download
mv ${output}.download ${output}
