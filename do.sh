#!/usr/bin/env bash

INVOCATION_PATH="$(pwd)"
cd "$(dirname $0)"
PROJECT_PATH=$(pwd)
cd $INVOCATION_PATH

if [[ $# -lt 1 ]];
then
    echo "Action needed. Possible options are:"
    echo "- [ b | build ]"
    echo "- [ rut | run-unit-tests ]"
    exit -1
fi

case $1 in
    "b" | "build")
        "$PROJECT_PATH/src-sh/build.sh" "${@:1}"
        ;;

    "rut" | "run-unit-tests")
        "$PROJECT_PATH/src-sh/run_unit_tests.sh" "${@:1}"
        ;;

    *)
        echo "Unknown Action: '$1'"
        ;;
esac
