def rjust(lst, length, justifier):
    return [justifier] * (length - len(lst)) + lst

def cmp(a, b):
    max_len = max([len(a), len(b)])
    if len(a) != len(b):
        [a, b] = [rjust(a, max_len, 0), rjust(b, max_len, 0)]

    for i in range(max_len):
        if a[i] == b[i]:
            continue

        if a[i] < b[i]:
            return -1

        elif a[i] > b[i]:
            return 1

    return 0

def add_with_carry(a, b, signs):
    max_len = max([len(a), len(b)])
    if len(a) != len(b):
        [a, b] = [rjust(a, max_len, 0), rjust(b, max_len, 0)]

    if signs == [1, 0]:
        return subtract_with_carry(b, a, [0, 0])

    elif signs == [0, 1]:
        return subtract_with_carry(a, b, [0, 0])

    result = []
    carry = 0
    for i in range(max_len - 1, -1, -1):
        fst = a[i]
        snd = b[i]
        (carry, mod) = divmod(fst + snd + carry, 10)
        result.append(mod)

    if carry != 0:
        result.append(carry)

    return (signs == [1, 1], result[::-1])

def subtract_with_carry(a, b, signs):
    max_len = max([len(a), len(b)])
    if len(a) != len(b):
        [a, b] = [rjust(a, max_len, 0), rjust(b, max_len, 0)]

    if signs == [1, 0]:
        return add_with_carry(b, a, [1, 1])

    elif signs == [0, 1]:
        return add_with_carry(b, a, [0, 0])

    carry = 0
    result = []
    for i in range(max_len - 1, -1, -1):
        [fst, snd] = [b[i], a[i]] if cmp(a, b) < 0 else [a[i], b[i]]
        if fst - snd - carry < 0:
            result.append(10 + fst - snd - carry)
            carry = 1
        else:
            result.append(fst - snd - carry)
            carry = 0

    return (cmp(a, b) > 0 and signs == [1, 1] or cmp(a, b) < 0 and signs == [0, 0], result[::-1])

def divide(a, b, signs):
    counter = [0]
    negative = signs == [0, 1] or signs == [1, 0]
    if cmp(a, b) < 0:
        return (False, counter)

    if cmp(b, [0]) == 0:
        raise ZeroDivisionError

    while cmp(a, b) >= 0:
        if negative:
            counter = subtract_with_carry(counter, [1], [1, 0])[1]
        else:
            counter = add_with_carry(counter, [1], [0, 0])[1]

        a = subtract_with_carry(a, b, [0, 0])[1]

    return (negative, counter)

def multiply(a, b, signs):
    negative = signs == [0, 1] or signs == [1, 0]
    if cmp(a, [0]) == 0 or cmp(b, [0]) == 0:
        return (False, [0])

    result = [0]
    while cmp(b, [0]) > 0:
        if negative:
            result = subtract_with_carry(result, a, [1, 0])[1]
        else:
            result = add_with_carry(result, a, [0, 0])[1]

        b = subtract_with_carry(b, [1], [0, 0])[1]

    return (negative, result)


if __name__ == "__main__":
# -------- Addition
    assert add_with_carry([1,2,0,0], [1,5,0,0], [0, 0]) == (False, [2,7,0,0])
    assert add_with_carry([1,2,0,0], [1,5,0,0], [1, 0]) == (False, [0,3,0,0])
    assert add_with_carry([1,2,0,0], [1,5,0,0], [0, 1]) == (True, [0,3,0,0])
    assert add_with_carry([1,2,0,0], [1,5,0,0], [1, 1]) == (True, [2,7,0,0])
    assert add_with_carry([1,2,0,0], [1,2,0,0], [0, 0]) == (False, [2,4,0,0])
    assert add_with_carry([1,2,0,0], [1,2,0,0], [1, 0]) == (False, [0,0,0,0])
    assert add_with_carry([1,2,0,0], [1,2,0,0], [0, 1]) == (False, [0,0,0,0])
    assert add_with_carry([1,2,0,0], [1,2,0,0], [1, 1]) == (True, [2,4,0,0])
# -------- Subtraction
    assert subtract_with_carry([1,2,0,0], [1,5,0,0], [0, 0]) == (True, [0,3,0,0])
    assert subtract_with_carry([1,2,0,0], [1,5,0,0], [1, 0]) == (True, [2,7,0,0])
    assert subtract_with_carry([1,2,0,0], [1,5,0,0], [0, 1]) == (False, [2,7,0,0])
    assert subtract_with_carry([1,2,0,0], [1,5,0,0], [1, 1]) == (False, [0,3,0,0])
    assert subtract_with_carry([1,2,0,0], [1,2,0,0], [0, 0]) == (False, [0,0,0,0])
    assert subtract_with_carry([1,2,0,0], [1,2,0,0], [1, 0]) == (True, [2,4,0,0])
    assert subtract_with_carry([1,2,0,0], [1,2,0,0], [0, 1]) == (False, [2,4,0,0])
    assert subtract_with_carry([1,2,0,0], [1,2,0,0], [1, 1]) == (False, [0,0,0,0])
# -------- Carrying
    assert subtract_with_carry([1,0,0,0], [0,0,0,7], [1, 1]) == (True, [0,9,9,3])
    assert subtract_with_carry([1,0,0,0], [0,0,0,7], [0, 0]) == (False, [0,9,9,3])
    assert add_with_carry([0,9,9,3], [0,0,0,8], [0,0]) == (False, [1,0,0,1])
    assert add_with_carry([0,9,9,3], [0,0,0,8], [1,0]) == (True, [0,9,8,5])
# -------- Division
    assert divide([2,4], [1,2], [0,0]) == (False, [2])
    assert divide([3,8], [1,2], [1,1]) == (False, [3])
    assert divide([2,4], [1,2], [0,1]) == (True, [2])
    assert divide([2,4], [1,2], [1,0]) == (True, [2])
    assert divide([1,2], [2,4], [0,0]) == (False, [0])
# ------- Multiplication
    assert multiply([0], [2,4], [0,0]) == (False, [0])
    assert multiply([2,4], [0], [0,0]) == (False, [0])
    for i in range(-10, 10, 1):
        for j in range(-10, 10, 1):
            neg_i = i < 0
            neg_j = j < 0
            neg_mul = i * j < 0
            i_repr = list(map(int, str(abs(i))))
            j_repr = list(map(int, str(abs(j))))
            mul_repr = list(map(int, str(abs(i * j))))
            assert multiply(i_repr, j_repr, [neg_i, neg_j]) == (neg_mul, mul_repr)

