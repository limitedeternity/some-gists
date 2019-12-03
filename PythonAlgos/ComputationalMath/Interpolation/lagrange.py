import math
from itertools import combinations

from sympy import sympify, simplify


x = [1.375, 1.38, 1.385, 1.39, 1.395, 1.4]
y = [5.04192, 5.17744, 5.32016, 5.47069, 5.62968, 5.79788]
assert len(x) == len(y)
assert len(x) > 1

poly = []
numerator_combinations = list(combinations(range(len(x)), len(x) - 1))[::-1]

for i, n in enumerate(numerator_combinations):
    denominator = eval(' * '.join(map(lambda t: f'({x[i] - x[t]})', n)))
    poly.append(
        f"{y[i] / denominator} * {' * '.join(map(lambda t: f'(x - {x[t]})', n))}" 
    )


expr = sympify(" + ".join(poly), evaluate=False)
f = eval("lambda x: " + str(simplify(expr)))


if __name__ == "__main__":
    print("\n\n+\n\n".join(poly), '\n\n')
    print(simplify(expr))

