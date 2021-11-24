import math

# Datasets:
xy1 = [(-3.0, -4.0), (-1.0, -0.8), (0.0, 1.6), (1.0, 2.3), (3.0, 1.5)]
xy2 = [(17.3, 537.0), (17.08, 534.0),
       (18.8, 555.0), (19.2, 560.0), (18.5, 552.0)]
xy3 = [(-9.0, 103.0), (-8.0, 84.0), (-7.0, 67.0), (-6.0, 52.0), (-5.0, 39.0), (-4.0, 28.0), (-3.0, 19.0), (-2.0, 12.0),
       (-1.0, 7.0), (0.0, 4.0), (1.0, 3.0), (2.0, 4.0),
       (3.0, 7.0), (4.0, 12.0), (5.0, 19.0), (6.0, 28.0), (7.0, 39.0),
       (8.0, 52.0), (9.0, 67.0), (10.0, 84.0)]


# (List[Tuple[Num, Num]] -> List[Num]) -> List[Tuple[Num, Num]] -> Num
def square_delta(approx_fn, xy):
    fs = approx_fn(xy)
    ys = list(map(lambda p: p[1], xy))
    return sum(map(lambda p: (p[0] - p[1]) * (p[0] - p[1]), zip(ys, fs)))


# List[Tuple[Num, Num]] -> List[Num]
def linear_approx(xy):
    xs = list(map(lambda p: p[0], xy))
    ys = list(map(lambda p: p[1], xy))
    a1 = sum(map(lambda x: x * x, xs))
    b1 = sum(xs)
    c1 = sum(map(lambda p: p[0] * p[1], xy))
    c2 = sum(ys)
    b2 = len(xy)

    a = -(b1 * c2 - c1 * b2) / (a1 * b2 - b1 * b1)
    b = (a1 * c2 - b1 * c1) / (a1 * b2 - b1 * b1)

    # Weights - [a, b]; Inputs - [x, 1]; Activator - lambda x: x
    return list(map(lambda x: a * x + b, xs))


# List[Tuple[Num, Num]] -> List[Num]
def parabolic_approx(xy):
    xs = list(map(lambda p: p[0], xy))
    ys = list(map(lambda p: p[1], xy))
    a1 = sum(map(lambda x: x * x * x * x, xs))
    b1 = sum(map(lambda x: x * x * x, xs))
    c1 = sum(map(lambda x: x * x, xs))
    r1 = sum(map(lambda p: p[1] * p[0] * p[0], xy))
    c2 = sum(xs)
    r2 = sum(map(lambda p: p[1] * p[0], xy))
    r3 = sum(ys)
    c3 = len(xy)

    a = (b1 * c2 * r3 - b1 * r2 * c3 + c1 * c2 * r2 - c1 * c1 * r3 + r1 * c1 * c3 - r1 * c2 * c2) / (a1 * c1 * c3 - a1
                                                                                                     *
                                                                                                     c2 * c2 - b1 * b1
                                                                                                     *
                                                                                                     c3 + 2 * b1 * c1
                                                                                                     *
                                                                                                     c2 - c1 * c1 * c1)

    b = -(a1 * c2 * r3 - a1 * r2 * c3 + c1 * c1 * r2 - b1 * c1 * r3 - c1 * c2 * r1 + b1 * r1 * c3) / (a1 * c1 * c3 - a1
                                                                                                      *
                                                                                                      c2 * c2 - b1 * b1
                                                                                                      *
                                                                                                      c3 + 2 * b1 * c1
                                                                                                      *
                                                                                                      c2 - c1 * c1 * c1)

    c = (c1 * b1 * r2 - c1 * c1 * r1 - c2 * a1 * r2 + c2 * b1 * r1 + r3 * a1 * c1 - r3 * b1 * b1) / (a1 * c1 * c3 - a1
                                                                                                     *
                                                                                                     c2 * c2 - b1 * b1
                                                                                                     *
                                                                                                     c3 + 2 * b1 * c1
                                                                                                     *
                                                                                                     c2 - c1 * c1 * c1)

    # Weights - [a, b, c]; Inputs - [x * x, x, 1]; Activator - lambda x: x
    return list(map(lambda x: a * x * x + b * x + c, xs))


# List[Tuple[Num, Num]] -> List[Num]
def exponential_approx(xy):
    xs = list(map(lambda p: p[0], xy))
    ys = list(map(lambda p: p[1], xy))
    a1 = sum(map(lambda x: x * x, xs))
    b1 = sum(xs)
    c1 = sum(map(lambda p: p[0] * p[1], xy))
    c2 = sum(ys)
    b2 = len(xy)

    A = -(b1 * c2 - c1 * b2) / (a1 * b2 - b1 * b1)
    B = (a1 * c2 - b1 * c1) / (a1 * b2 - b1 * b1)

    # a = math.e**A
    # b = math.e**B
    # a**x * b = (math.e**A)**x * (math.e**B) = math.e**(A * x + B)
    # Weights - [A, B]; Inputs - [x, 1]; Activator - lambda x: math.e**x
    return list(map(lambda x: math.e**(A * x + B), xs))


fn_list = [linear_approx, parabolic_approx, exponential_approx]
xy1_results = list(map(lambda fn: square_delta(fn, xy1), fn_list))
xy2_results = list(map(lambda fn: square_delta(fn, xy2), fn_list))
xy3_results = list(map(lambda fn: square_delta(fn, xy3), fn_list))

print(xy1_results)
print(xy2_results)
print(xy3_results)

xy1_winner = fn_list[
    min(range(len(fn_list)), key=xy1_results.__getitem__)
].__name__

xy2_winner = fn_list[
    min(range(len(fn_list)), key=xy2_results.__getitem__)
].__name__

xy3_winner = fn_list[
    min(range(len(fn_list)), key=xy3_results.__getitem__)
].__name__

print("Best for dataset 1:", xy1_winner)
print("Best for dataset 2:", xy2_winner)
print("Best for dataset 3:", xy3_winner)
