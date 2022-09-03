#!/bin/sh

set -e

. api/lib
set_envs

set -x
api_request -X GET $(api_url api/users)
