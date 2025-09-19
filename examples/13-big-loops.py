import time

# Boucle for
start_for = time.time()
for i in range(1, 100001):
    pass
elapsed_for = (time.time() - start_for) * 1000

# Boucle while
start_while = time.time()
i = 1
while i <= 100000:
    i += 1
elapsed_while = (time.time() - start_while) * 1000

# Boucle repeat-until simulée (do...while)
start_repeat = time.time()
i = 1
while True:
    i += 1
    if i > 100000:
        break
elapsed_repeat = (time.time() - start_repeat) * 1000

# Affichage des résultats
print("Comparaison des temps d'exécution pour 100000 itérations :")
print(f"- Boucle for : {elapsed_for:.2f} ms")
print(f"- Boucle while : {elapsed_while:.2f} ms")
print(f"- Boucle repeat-until simulée : {elapsed_repeat:.2f} ms")
