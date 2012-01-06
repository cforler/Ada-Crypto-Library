#!/bin/bash
COVERAGE_DIR="coverage/"
SRC_PATH="../src"
TEST_PATH=".."

CURRENT=`pwd`
cd ${SRC_PATH}
gcov *
cd ${CURRENT}
gcov *
cd ${COVERAGE_DIR}
lcov  -q -c -d ${TEST_PATH} -d ../${SRC_PATH} -o tracefile
genhtml -q  -s -t "Ada Crypto Library Coverage Report" --legend --highlight --function-coverage tracefile
cd ${CURRENT}

