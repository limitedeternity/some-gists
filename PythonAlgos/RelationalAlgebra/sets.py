from copy import copy
from re import sub

a = input("A = ").strip()
a = sub(r"(\d)", r"\1,", a)
a = a.replace("{", "[").replace("}", "]")
a = eval(a)


print("Соседние наборы:")
found_neighbours = []
for s in a:
    for idx in range(len(s)):
        s_copy = list(s)
        s_copy[idx] = int(not s_copy[idx])
        s_copy = tuple(s_copy)
        if s_copy in a and {s, s_copy} not in found_neighbours:
            found_neighbours.append({s, s_copy})
            print(sub(r"(\d), ", r"\1", str({s, s_copy})))


print("\nПротивоположные наборы:")
found_opposites = []
for s in a:
    s_copy = tuple(map(lambda i: int(not i), s))
    if s_copy in a and {s, s_copy} not in found_opposites:
        found_opposites.append({s, s_copy})
        print(sub(r"(\d), ", r"\1", str({s, s_copy})))

