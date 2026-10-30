# This file is part of the PascalScript Pascal interpreter.
# SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
# SPDX-License-Identifier: LGPL-3.0-or-later

import time

ITERATIONS = 1_000_000

start_for = time.time()
for i in range(0, ITERATIONS):
    pass
elapsed_for = (time.time() - start_for) * 1000

start_while = time.time()
i = 1
while i <= ITERATIONS:
    i += 1
elapsed_while = (time.time() - start_while) * 1000

start_repeat = time.time()
i = 1
while True:
    i += 1
    if i > ITERATIONS:
        break
elapsed_repeat = (time.time() - start_repeat) * 1000

print(f"Python - Comparison of execution times for {ITERATIONS} iterations:")
print(f" - for                   : {elapsed_for:.2f} ms")
print(f" - while                 : {elapsed_while:.2f} ms")
print(f" - simulated repeat-until: {elapsed_repeat:.2f} ms")
