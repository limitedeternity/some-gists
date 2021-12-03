class DotProduct:
    def __new__(cls, v1, v2):
        return sum(a * b for a, b in zip(v1, v2))

    @staticmethod
    def derivative_wrap(*elements):
        half = len(elements) >> 1
        return DotProduct(elements[:half], elements[half:])


def partial_derivative(fn, idx, *args):
    delta = 1e-12
    y = fn(*args)
    y1 = fn(
        *map(
            lambda p: p[1] if p[0] != idx else p[1] + delta,
            enumerate(args)
        )
    )

    return (y1 - y) / delta
