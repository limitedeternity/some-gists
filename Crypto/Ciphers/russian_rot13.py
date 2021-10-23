alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"


def russian_shift(message, offset):
    offset = offset % len(alphabet)
    shift_map = str.maketrans(alphabet, alphabet[offset:] + alphabet[:offset])
    return message.translate(shift_map)


def main():
    message, offset = "Шла Саша по шоссе, несла сушку на шесте", 13
    encrypted = russian_shift(message, offset)
    decrypted = russian_shift(encrypted, -offset)
    print(message + " -> " + encrypted + " -> " + decrypted)
    assert message == decrypted


if __name__ == "__main__":
    main()
