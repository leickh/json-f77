#!/usr/bin/env bash

INVOCATION_PATH="$(pwd)"
cd "$(dirname $0)/.."
PROJECT_PATH=$(pwd)
cd "$INVOCATION_PATH"

INI="$PROJECT_PATH/src-sh/ini.sh"

INVOCATION_TIME=$(date '+%Y.%m.%d-%H.%M.%S')

list_unit_tests() {
    local UNIT_TEST_BASE_PATH="$1"
    local UNIT_TEST_CANDIDATES=$(ls -A $UNIT_TEST_BASE_PATH)

    for UNIT_TEST_CANDIDATE in $UNIT_TEST_CANDIDATES
    do
        if [[ ! -d "$UNIT_TEST_BASE_PATH/$UNIT_TEST_CANDIDATE" ]];
        then
            continue
        fi

        # If the folder contains no 'test.ini', there's no
        # point in using time to try to build the test.
        if [[ ! -f "$UNIT_TEST_BASE_PATH/$UNIT_TEST_CANDIDATE/test.ini" ]];
        then
            continue
        fi

        echo "$UNIT_TEST_CANDIDATE"
    done
}

build_and_run_single_unit_test() {
    local BUILD_PATH="$1"


    local TEST_NAME=$($INI -g "Test:Name" "$BUILD_PATH/test.ini")
    local TEST_VERSION=$($INI -g "Test:Version" "$BUILD_PATH/test.ini")
    echo -e "\n========[ $TEST_NAME | $TEST_VERSION ]========"

    local TEST_BUILDER=$($INI -g "Test.Build:Builder" "$BUILD_PATH/test.ini")

    case "$TEST_BUILDER" in
        "builtin:fortran-90")
            "$PROJECT_PATH/src-sh/test-builders/fortran-90.sh" \
                "$BUILD_PATH"
            ;;
    esac
}

build_and_run_all_unit_tests() {
    local TEST_BASE_PATH="$1"
    local BUILD_PATH="$2"
    local UNIT_TEST_LIST="$3"

    for UNIT_TEST in $UNIT_TEST_LIST
    do
        mkdir -p "$BUILD_PATH/unit-tests/$UNIT_TEST"

        cp -r "$TEST_BASE_PATH/$UNIT_TEST" "$BUILD_PATH/unit-tests/"

        build_and_run_single_unit_test \
            "$BUILD_PATH/unit-tests/$UNIT_TEST"
    done
}

create_unit_test_csv_list() {
    local UNIT_TEST_FOLDER="$1"
    local UNIT_TEST_LIST="$2"

    local UNIT_TEST_LIST_STRING=""
    for UNIT_TEST_ITEM in $UNIT_TEST_LIST
    do
        UNIT_TEST_LIST_STRING="$UNIT_TEST_LIST_STRING,$UNIT_TEST_ITEM"
    done
    echo "${UNIT_TEST_LIST_STRING:1}"
}

create_test_build_config() {
    local TEST_BASE_PATH="$1"
    local TEST_BUILD_PATH="$2"
    local UNIT_TEST_LIST="$3"

    UNIT_TEST_CSV="$(create_unit_test_csv_list "$TEST_BASE_PATH/unit" "$UNIT_TEST_LIST")"

    echo ""                                 >> "$BUILD_PATH/test_build.ini"
    echo "[Test-Suite]"                     >> "$BUILD_PATH/test_build.ini"
    echo "Creation = \"$INVOCATION_TIME\""  >> "$BUILD_PATH/test_build.ini"
    echo "Unit-Tests = \"$UNIT_TEST_CSV\""      >> "$BUILD_PATH/test_build.ini"
    echo "" >> "$BUILD_PATH/test_build.ini"
}

new_unit_test_build() {
    local BUILD_PATH="$PROJECT_PATH/.build/tests/$INVOCATION_TIME"
    local UNIT_TEST_LIST="$(list_unit_tests "$PROJECT_PATH/tests/unit")"

    mkdir -p "$BUILD_PATH"

    create_test_build_config \
        "$PROJECT_PATH/.build/tests" \
        "$BUILD_PATH" \
        "$UNIT_TEST_LIST"

    build_and_run_all_unit_tests \
        "$PROJECT_PATH/tests/unit" \
        "$BUILD_PATH" \
        "$UNIT_TEST_LIST"
}

new_unit_test_build
