#!/bin/sh

set -e

. api/lib
set_envs

api_request_nl -X GET ${api_prefix}api/results/user
