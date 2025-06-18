#!/usr/bin/env bash

INVOCATION_PATH="$(pwd)"
cd "$(dirname $0)/../.."
PROJECT_PATH=$(pwd)
cd "$INVOCATION_PATH"

INI="$PROJECT_PATH/src-sh/ini.sh"

BUILD_PATH="$1"

replace_path_elements() {
    local INTERIM=$(echo ${1//"{test}"/"$BUILD_PATH"})
    local INTERIM=$(echo ${INTERIM//"{project}"/"$PROJECT_PATH"})
    echo ${INTERIM//"{library}"/"$PROJECT_PATH/src-f90"}
}

search_sources() {
    local SEARCH_PATH=$1

    ((LEN_WITHOUT_PATH_PREFIX=${#SEARCH_PATH}+2))

    local RAW_FILE_NAMES=$(find "$SEARCH_PATH" -type f | grep ".for$" | cut -c "$LEN_WITHOUT_PATH_PREFIX-")
    for RAW_FILE_NAME in $RAW_FILE_NAMES;
    do
        ((NAME_LENGTH_WITHOUT_SUFFIX=${#RAW_FILE_NAME}-4))
        echo "$RAW_FILE_NAME" | cut -c -$NAME_LENGTH_WITHOUT_SUFFIX
    done
}

build_and_run_unit_test() {
    local TEST_NAME=$("$INI" -g Test:Name "$BUILD_PATH/test.ini")
    local SOURCE_FOLDER=$(replace_path_elements \
        $("$INI" -g Test.Build:Path "$BUILD_PATH/test.ini"))

    local SOURCE_LIST="$(search_sources $SOURCE_FOLDER)"

    if [[ "$SOURCE_LIST" == "" ]];
    then
        echo "info: no sources for test '$TEST_NAME'"
        exit 0
    fi

    mkdir -p "$BUILD_PATH/.objects"

    for SOURCE_NAME in $SOURCE_LIST;
    do
        echo "> $SOURCE_NAME.for"

        local OBJECT_NAME=$(echo $SOURCE_NAME | tr "-" "_" | tr "/" "-")

        gfortran --std=legacy -O2 -g2 -c \
            -o "$BUILD_PATH/.objects/$OBJECT_NAME.o" \
            "$SOURCE_FOLDER/$SOURCE_NAME.for"
    done

    gfortran -O2 -g2 -o "$BUILD_PATH/.objects/$TEST_NAME.elf" \
        "$BUILD_PATH/.objects/"*.o "$PROJECT_PATH/.build/json-f90.a"
    ((STATUS=$?))
    if [[ $STATUS != 0 ]];
    then
        echo "error: failed building test" >&2
        exit 1
    fi
    cd $BUILD_PATH

    ./.objects/$TEST_NAME.elf *> $BUILD_PATH/log.txt
    ((STATUS=$?))

    if [[ $STATUS != 0 ]];
    then
        echo -e "\nfailed($TEST_NAME)\n"
        cat $BUILD_PATH/log.txt
        return
    fi
    echo "success($TEST_NAME)"
}

build_and_run_unit_test
