from itertools import cycle, islice
from string import ascii_letters as alphabet


def shift(message, offset):
    offset = offset % len(alphabet)
    shift_map = str.maketrans(alphabet, alphabet[offset:] + alphabet[:offset])
    return message.translate(shift_map)


def repeat_extend(string, length):
    return ''.join(islice(cycle(string), length))


vigenere_map = {
    key_char: dict(zip(alphabet, shift(alphabet, offset))) for offset, key_char in enumerate(alphabet)
}

vigenere_inverse_map = {
    key_char: {v: k for k, v in vigenere_map[key_char].items()} for key_char in alphabet
}


def encrypt(message, key):
    return ''.join(map(lambda p: vigenere_map[p[1]][p[0]] if p[0] in alphabet else p[0], zip(message, repeat_extend(key, len(message)))))


def decrypt(ciphertext, key):
    return ''.join(map(lambda p: vigenere_inverse_map[p[1]][p[0]] if p[0] in alphabet else p[0], zip(ciphertext,
                                                                                                     repeat_extend(key,
                                                                                                                   len(ciphertext)))))


def main():
    message, key = "The quick brown fox jumps over 13 lazy dogs.", "peach"
    encrypted = encrypt(message, key)
    decrypted = decrypt(encrypted, key)
    print(message + " [" + key + "] -> " + encrypted + " -> " + decrypted)
    assert message == decrypted


if __name__ == "__main__":
    main()
