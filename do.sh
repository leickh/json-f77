#!/usr/bin/env bash

INVOCATION_PATH="$(pwd)"
cd "$(dirname $0)"
PROJECT_PATH=$(pwd)
cd $INVOCATION_PATH

case $1 in
    "b" | "build")
        "$PROJECT_PATH/src-sh/build.sh" "${@:1}"
        ;;

    "bt" | "build-tests")
        "$PROJECT_PATH/src-sh/build-tests.sh" "${@:1}"
        ;;
esac
