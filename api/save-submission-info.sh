#!/bin/sh

set -e

usage() {
    cat <<EOF
$0 SUBMISSION_ID OUTPUT_FILE
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 0
fi

submission_id="$1"
shift

if [ x"$1" = x ]; then
    usage
    exit 0
fi

output="$1"
shift


if [ -r "$output" ]; then
    echo "$output already exists. skipping."
    exit 0
fi

outdir=$(dirname ${output})
if [ ! -d $outdir ]; then
    mkdir -p $outdir
fi

set -x

./api/getinfo-submission.sh $submission_id | jq . > "${output}.download"
mv "${output}.download" "${output}"
