-- This file is part of the PascalScript Pascal interpreter.
-- SPDX-FileCopyrightText: 2025 Christophe 'CHiPs' Petit <chips44@gmail.com>
-- SPDX-License-Identifier: LGPL-3.0-or-later

local start_for = os.clock()
for i = 1, 100000 do
    -- nothing
end
local elapsed_for = (os.clock() - start_for) * 1000

local start_while = os.clock()
local i = 1
while i <= 100000 do
    i = i + 1
end
local elapsed_while = (os.clock() - start_while) * 1000

local start_repeat = os.clock()
local j = 1
repeat
    j = j + 1
until j > 100000
local elapsed_repeat = (os.clock() - start_repeat) * 1000

print("Lua - Comparison of execution times for 100000 iterations:")
print(string.format(" - for         : %.2f ms", elapsed_for))
print(string.format(" - while       : %.2f ms", elapsed_while))
print(string.format(" - repeat until: %.2f ms", elapsed_repeat))
