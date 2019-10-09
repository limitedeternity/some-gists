# z1 = a + bi; z2 = c + di


def div(a, b, c, d):
    print("Real part: ")
    print("a * c + b * d")
    print(f"{a} * {c} + {b} * {d}")
    print(f"{a * c} + {b * d}")

    print("\nImaginary part: ")
    print("b * c - a * d")
    print(f"{b} * {c} - {a} * {d}")
    print(f"{b * c} - {a * d}")

    print("\nDenominator: ")
    print("c * c + d * d")
    print(f"{c * c} + {d * d}")

    print("\nResult: ")
    print(f"{a * c + b * d}       {b * c - a * d}")
    print("---   +   --- i")
    print(f"{c * c + d * d}        {c * c + d * d}")


def mul(a, b, c, d):
    print("(a * c - b * d) + (b * c + a * d)i")
    print(f"({a} * {c} - {b} * {d}) + ({b} * {c} + {a} * {d})i")
    print(f"({a * c} - {b * d}) + ({b * c} + {a * d})i")
    print(f"({a * c - b * d}) + ({b * c + a * d})i")


def add(a, b, c, d):
    print("(a + c) + (b + d)i")
    print(f"({a} + {c}) + ({b} + {d})i")
    print(f"({a + c}) + ({b + d})i")


def sub(a, b, c, d):
    print("(a - c) + (b - d)i")
    print(f"({a} - {c}) + ({b} - {d})i")
    print(f"({a - c}) + ({b - d})i")
