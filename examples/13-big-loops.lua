-- Boucle for
local start_for = os.clock()
for i = 1, 100000 do
    -- rien
end
local elapsed_for = (os.clock() - start_for) * 1000
print(string.format("Elapsed time (for) : %.2f ms", elapsed_for))

-- Boucle while
local start_while = os.clock()
local i = 1
while i <= 100000 do
    i = i + 1
end
local elapsed_while = (os.clock() - start_while) * 1000
print(string.format("Elapsed time (while) : %.2f ms", elapsed_while))

-- Boucle repeat until
local start_repeat = os.clock()
local j = 1
repeat
    j = j + 1
until j > 100000
local elapsed_repeat = (os.clock() - start_repeat) * 1000
print(string.format("Elapsed time (repeat until) : %.2f ms", elapsed_repeat))
