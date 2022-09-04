#!/bin/sh

set -e

. api/lib
set_envs

usage() {
    cat <<EOF
Usage: $0 PROBLEM_ID FILENAME
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 0
fi

prob_id="$1"
shift

if [ x"$1" = x ]; then
    usage
    exit 0
fi

file="$1"
shift

api_request_nl -X POST -F file=@"${file}" ${api_prefix}api/problems/${prob_id}
