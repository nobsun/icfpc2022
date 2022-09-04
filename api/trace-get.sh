#!/bin/sh

set -e

. api/lib
set_envs

set -x
api_request -X GET ${api_prefix}api/"$1"
