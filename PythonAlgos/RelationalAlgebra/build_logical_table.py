from itertools import product

def build_table(args_amount, fn):
    combinations = list(product(range(2), repeat=args_amount))
    return list(zip(combinations, map(lambda comb: bool(fn(*comb)), combinations)))

print("\n".join(str(t) for t in build_table(2, lambda x, y: ((not(x) or y) and x) == y)))
print()
