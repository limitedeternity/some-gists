from functools import reduce


def pipe(*args):
    return lambda val: reduce(lambda prev, fn: fn(prev), args, val)
