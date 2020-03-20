from itertools import product


two_arg = list(product(range(2), repeat=2))
three_arg = list(product(range(2), repeat=3))

def build_table(combinations, fn):
    return list(zip(combinations, map(lambda comb: bool(fn(*comb)), combinations)))

print("\n".join(str(t) for t in build_table(two_arg, lambda x, y: ((not(x) or y) and x) == y)))
print()

