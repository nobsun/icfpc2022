#!/bin/sh

set -e

. api/lib
set_envs

if [ x"$1" = x-v ]; then
    set -x
fi

api_request_nl -X GET ${api_prefix}api/users
