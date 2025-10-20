#!/bin/bash

REPORT_FILE="./examples/report.txt"
> "$REPORT_FILE"

OK=0
KO=0
TOTAL=0
printf "%-47s %15s %-10s\n" "Example" "ms" "Result" >> "$REPORT_FILE"
for example in ./examples/*.pas; do
    OUT_FILE="${example}.out"
    ERR_FILE="${example}.err"
    if [[ -x "${example}" ]]; then
        echo -n "Executing ${example}... "
        START=$(date +%s%N)
        # ../pascalscript -v -t "${example}" > "$OUT_FILE" 2> "$ERR_FILE"
        ./pascalscript "${example}" > "$OUT_FILE" 2> "$ERR_FILE"
        RESULT=$?
        END=$(date +%s%N)
        DURATION=$(( (END - START) / 1000000 )) # Duration in milliseconds
        if [[ ${RESULT} -eq 0 ]]; then
            printf "%-47s %15s %s\n" "${example}" "${DURATION}" "OK"  >> "$REPORT_FILE"
            OK=$((OK + 1))
            echo "OK"
        else
            printf "%-47s %15s %-10s\n" "${example}" "${DURATION}" "KO (${RESULT})"  >> "$REPORT_FILE"
            KO=$((KO + 1))
            echo "KO"
        fi
        TOTAL=$((TOTAL + 1))
    fi
done

echo "Total: $TOTAL, OK $OK, KO $KO" >> "$REPORT_FILE"
cat ${REPORT_FILE}
