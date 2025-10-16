#!/bin/bash

REPORT_FILE="./report.txt"
> "$REPORT_FILE"

SUCCESS=0
FAILURE=0
TOTAL=0
# Trouver les fichiers exemple* (exemple1, exemple2, ...)
for example in *.pas; do
    OUT_FILE="${example}.out"
    ERR_FILE="${example}.err"
    if [[ -x "${example}" ]]; then
        echo "Executing ${example}..."
        ../pascalscript "${example}" > "$OUT_FILE" 2> "$ERR_FILE"
        if [[ $? -eq 0 ]]; then
            echo "${example}: Success" >> "$REPORT_FILE"
            SUCCESS=$((SUCCESS + 1))
        else
            echo "${example}: Failure" >> "$REPORT_FILE"
            FAILURE=$((FAILURE + 1))
        fi
        TOTAL=$((TOTAL + 1))
    fi
done

echo "Total: $TOTAL, Success: $SUCCESS, Failure: $FAILURE" >> "$REPORT_FILE"
echo "Report written to $REPORT_FILE"
