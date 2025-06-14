#!/usr/bin/env bash

cd $(dirname $0)/..
PROJECT_PATH=$(pwd)

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

build_test() {
    local TEST_NAME="$1"
    local TEST_PATH="$PROJECT_PATH/tests/$TEST_NAME"

    mkdir -p "$PROJECT_PATH/.build/tests/$TEST_NAME/objects"

    SOURCE_LIST=$(search_sources "$TEST_PATH/src-f77")
    
    cd "$PROJECT_PATH/.build/module-files/$SOURCE_SUBFOLDER"
    for SOURCE_NAME in $SOURCE_LIST;
    do
        local OBJECT_NAME=$(echo $FILE_ENTRY | tr "-" "_" | tr "/" "-")

        gfortran -g2 -c -o \
            "$PROJECT_PATH/.build/tests/$TEST_NAME/objects/$SOURCE_NAME.o" \
            "$TEST_PATH/src-f77/$SOURCE_NAME.for"
    done

    gfortran -g2 -o "$PROJECT_PATH/.tests/$TEST_NAME.elf" \
        "$PROJECT_PATH/.build/tests/$TEST_NAME/objects/"*.o \
        "$PROJECT_PATH/.build/json-f77.a" -lgfortran
}

mkdir -p $PROJECT_PATH/.tests
rm -f $PROJECT_PATH/.tests/*.elf

for TEST_NAME in $(ls -A "$PROJECT_PATH/tests")
do
    # Only attempt compiling folders in the 'tests/' folder
    if [[ ! -d "$PROJECT_PATH/tests/$TEST_NAME" ]];
    then
        continue
    fi

    echo "Building Test: '$TEST_NAME'"
    build_test "$TEST_NAME"
done
