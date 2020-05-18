from functools import reduce
from itertools import combinations
import math
from operator import mul

from sympy import sympify, simplify


x = [1, 2, 3, 4]
y = [4, 10, 18, 28]
assert len(x) == len(y)
assert len(x) > 1

r = range(len(y))
c = [y[i] / reduce(mul, [x[i] - x[j] for j in r if j != i]) for i in r]

poly = []
c_muls = list(combinations(range(len(x)), len(x) - 1))[::-1]
for i in range(len(c)):
    p = [str(c[i])]
    for m in c_muls[i]:
        p.append(f"(x - {x[m]})")

    poly.append(" * ".join(p))


expr = " + ".join(poly)
f = eval("lambda x: " + expr)


if __name__ == "__main__":
    print(expr)
    print()
    print(sympify(simplify(expr)))

