#!/bin/bash

REPORT_FILE="./test/report.txt"
> "$REPORT_FILE"

OK=0
KO=0
TOTAL=0
RUNS=1
printf "%-7s %-40s %15s %s\n" "CC" "Test" "ms?" "Result" >> "$REPORT_FILE"
for TEST in ./test/test_*.c; do
    BASENAME=$(basename "${TEST}")
    TESTNAME="${BASENAME%.*}"
    for CC in gcc clang; do
        OUT_FILE="./test/${TESTNAME}.${CC}.out"
        ERR_FILE="./test/${TESTNAME}.${CC}.err"
        echo -n "Compiling ${TESTNAME} with ${CC}... "
        ${CC} -m32 -std=c17 -Wall -I./include -Wall -Wextra -Wpedantic -ggdb -O3 -o ./test/test ${TEST} -lm  > "$OUT_FILE" 2> "$ERR_FILE"
        if [[ $? -ne 0 ]]; then
            printf "%-7s %-40s %15s %s\n" "${CC}" "${TESTNAME}" "N/A" "KO (compile error)"  >> "$REPORT_FILE"
            KO=$((KO + 1))
            echo "KO (compile error)"
            TOTAL=$((TOTAL + 1))
            continue
        else
            echo "OK"
        fi
        echo -n "Executing ${TESTNAME} compiled with ${CC}... "
        START=$(date +%s%N)
        if [[ ${RUNS} -eq 1 ]]; then
            { ./test/test; } > "$OUT_FILE" 2> "$ERR_FILE"
            RESULT=$?
        else
            for I in {1..${RUNS}}; do
                echo -n "#$I "
                { ./test/test; } > "$OUT_FILE" 2> "$ERR_FILE"
                RESULT=$?
            done
        fi
        END=$(date +%s%N)
        rm -f ./test/test
        DURATION=$(( (END - START) / (${RUNS} * 1000) )) # Duration in milliseconds?
        if [[ ${RESULT} -eq 0 ]]; then
            printf "%-7s %-40s %15s %s\n" "${CC}" "${TESTNAME}" "${DURATION}" "OK"  >> "$REPORT_FILE"
            OK=$((OK + 1))
            echo "OK"
        else
            printf "%-7s %-40s %15s %s\n" "${CC}" "${TESTNAME}" "${DURATION}" "KO (${RESULT})"  >> "$REPORT_FILE"
            KO=$((KO + 1))
            echo "KO"
        fi
        TOTAL=$((TOTAL + 1))
    done
done

echo "Total: $TOTAL, OK $OK, KO $KO" >> "$REPORT_FILE"
cat ${REPORT_FILE}
