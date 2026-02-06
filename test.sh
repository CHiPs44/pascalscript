#!/bin/bash

REPORT_FILE="./test/report.txt"
> "$REPORT_FILE"

OK=0
KO=0
TOTAL=0
printf "%-47s %15s %-10s\n" "Test" "ms" "Result" >> "$REPORT_FILE"
for TEST in ./test/test_*.c; do
    BASENAME=$(basename "${TEST}")
    TESTNAME="${BASENAME%.*}"
    OUT_FILE="./test/${TESTNAME}.out"
    ERR_FILE="./test/${TESTNAME}.err"
    echo -n "Executing ${TESTNAME}... "
    gcc -m32 -std=c17 -Wall -I./include -ggdb -o ./test/test ${TEST} -lm  > "$OUT_FILE" 2> "$ERR_FILE"
    if [[ $? -ne 0 ]]; then
        printf "%-47s %15s %-10s\n" "${TESTNAME}" "N/A" "KO (compile error)"  >> "$REPORT_FILE"
        KO=$((KO + 1))
        echo "KO (compile error)"
        continue
    fi
    START=$(date +%s%N)
    ./test/test > "$OUT_FILE" 2> "$ERR_FILE"
    RESULT=$?
    END=$(date +%s%N)
    rm -f ./test/test
    DURATION=$(( (END - START) / 1000000 )) # Duration in milliseconds
    if [[ ${RESULT} -eq 0 ]]; then
        printf "%-47s %15s %s\n" "${TESTNAME}" "${DURATION}" "OK"  >> "$REPORT_FILE"
        OK=$((OK + 1))
        echo "OK"
    else
        printf "%-47s %15s %-10s\n" "${TESTNAME}" "${DURATION}" "KO (${RESULT})"  >> "$REPORT_FILE"
        KO=$((KO + 1))
        echo "KO"
    fi
    TOTAL=$((TOTAL + 1))
done

echo "Total: $TOTAL, OK $OK, KO $KO" >> "$REPORT_FILE"
cat ${REPORT_FILE}
