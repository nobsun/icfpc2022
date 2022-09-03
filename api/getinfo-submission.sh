#!/bin/sh

set -e

. api/lib
set_envs

usage() {
    cat <<EOF
$0 SUBMISSION_ID
EOF
}

if [ x"$1" = x ]; then
    usage
    exit 0
fi

submission_id=$1

api_request_nl -X GET ${api_prefix}api/submissions/${submission_id}
