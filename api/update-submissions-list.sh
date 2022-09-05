#!/bin/sh

set -e

usage() {
    cat <<EOF
$0 OUTPUT_FILE
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 0
fi

output=$1

set -x

./api/list-submissions.sh | jq . > ${output}.download
mv ${output}.download ${output}
