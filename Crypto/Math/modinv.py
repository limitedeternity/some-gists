from functools import reduce


def iteration_checker(func, cond, x, y):
    res = func(x, y)
    if not cond(res):
        raise StopIteration(x, y)

    return res


def ireducewhile(func, cond, iterable):
    iterable = iter(iterable)
    x = next(iterable)
    yield x

    for y in iterable:
        try:
            x = iteration_checker(func, cond, x, y)

        except StopIteration:
            break

        yield x


def division(c):
    num, start = c
    for i in range(start, int(num**0.5) + 1):
        if num % i == 0:
            return (num // i, i)

    return None


def iter_start(num):
    yield (num, 2)
    while True:
        yield 0


def prime_factor(num):
    result = list(ireducewhile(lambda x, _: division(x), lambda x: x is not None, iterable=iter_start(num)))

    if len(result) == 1:
        return [result[0][0]]

    else:
        return list(map(lambda x: x[1], result[1:])) + [result[-1][0]]


def phi(num):
    return int(num * reduce(lambda x, y: x * y, map(lambda f: 1 - (1 / f), set(prime_factor(num))), 1))


def modinv(a, m):
    return a**(phi(m) - 1)

