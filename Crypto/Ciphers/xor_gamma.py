from itertools import cycle, islice


def repeat_extend(gamma, length):
    return list(islice(cycle(gamma), length))


def xor_gamma(message, gamma):
    message = message.encode("cp1251")
    result = bytearray(
        map(
            lambda p: p[0] ^ p[1], zip(
                message, repeat_extend(gamma, len(message))
            )
        )
    )

    return result.decode("cp1251")


def main():
    print("#1")
    print("  1.", xor_gamma("ДЬУЙЬДЭУЙРЧЯТЩЬ", [6, 12, 22, 5, 3]))
    print("  2.", xor_gamma("бИЬОЯжЮЩЙЧиЗЩБ", [34, 12, 25, 5, 31]))
    print("  3.", xor_gamma("ЗоБЛМхБЩУаФТМх", [4, 32, 5, 25]))
    print("  4.", xor_gamma("ДЙПОуЖЙНФзЛЕСХиО", [14, 2, 17, 25, 34]))
    print("  5.", xor_gamma("еЗБЫфъДВЦппВ", [41, 2, 7, 27, 33]))
    print("  6.", xor_gamma("ЯДБЛГЙРРЕЧНУВЖЙВД", [18, 1, 2, 5, 7]))
    print()
    print("#2")
    print("  ", xor_gamma("Беспалов", [1, 8, 2, 7, 6, 7]))


if __name__ == "__main__":
    main()
