#!/bin/bash

REPORT_FILE="./examples/report.txt"
> "$REPORT_FILE"

OK=0
KO=0
TOTAL=0
printf "%-7s %-40s %15s %s\n" "CC" "Example" "ms" "Result" >> "$REPORT_FILE"
for example in ./examples/*.pas; do
    if [[ -x "${example}" ]]; then
        for CC in gcc clang; do
            OUT_FILE="./examples/${example##*/}.${CC}.out"
            ERR_FILE="./examples/${example##*/}.${CC}.err"
            echo -n "Executing ${example} with ${CC}... "
            START=$(date +%s%N)
            ./build-${CC}/pascalscript "${example}" > "$OUT_FILE" 2> "$ERR_FILE"
            RESULT=$?
            END=$(date +%s%N)
            DURATION=$(( (END - START) / 1000000 )) # Duration in milliseconds
            if [[ ${RESULT} -eq 0 ]]; then
                printf "%-7s %-40s %15s %s\n" "${CC}" "${example}" "${DURATION}" "OK"  >> "$REPORT_FILE"
                OK=$((OK + 1))
                echo "OK"
            else
                printf "%-7s %-40s %15s %s %s\n" "${CC}" "${example}" "${DURATION}" "KO" "(${RESULT})"  >> "$REPORT_FILE"
                KO=$((KO + 1))
                echo "KO"
            fi
            TOTAL=$((TOTAL + 1))
        done
    fi
done

echo "Total: $TOTAL, OK $OK, KO $KO" >> "$REPORT_FILE"
cat ${REPORT_FILE}
