import math

from sympy import sympify, simplify


x = [0, 1, 2, 3]
y = [-2, -5, 0, 4]
assert len(x) == len(y)
assert len(x) > 1

c = y.copy()
for i in range(1, len(x)):
    for j in range(len(x) - 1, i - 1, -1):
        c[j] = (c[j] - c[j-1]) / (x[j] - x[j-i])

poly = []
for i in range(len(c)):
    poly_c_mul = []
    for j in range(i):
        poly_c_mul.append(f"(x - {x[j]})")

    poly.append(str(c[i]) + " * " + (" * ".join(poly_c_mul)) if len(poly_c_mul) > 0 else str(c[i]))


expr = " + ".join(poly)
f = eval("lambda x: " + expr)


if __name__ == "__main__":
    print(expr)
    print()
    print(sympify(simplify(expr)))

