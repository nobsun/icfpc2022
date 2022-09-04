#!/bin/sh

set -e

usage() {
    cat <<EOF
$0 URL OUTPUT_FILE
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 0
fi

url="$1"
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

curl -o "${output}.download" "$url"
mv "${output}.download" "${output}"
