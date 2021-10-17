def powmod(x, y, m):
    if y == 0:
        return 1

    tmp = powmod(x, y // 2, m)

    if y % 2 == 0:
        return tmp * tmp % m

    else:
        return tmp * tmp * x % m

