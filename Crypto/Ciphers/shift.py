from string import ascii_letters as alphabet


def shift(message, offset):
    offset = offset % len(alphabet)
    shift_map = str.maketrans(alphabet, alphabet[offset:] + alphabet[:offset])
    return message.translate(shift_map)


def main():
    message = "Input message you'd like to be encrypted"
    for offset in range(len(alphabet)):
        encrypted = shift(message, offset)
        decrypted = shift(encrypted, -offset)
        print(message + " -> " + encrypted + " -> " + decrypted)
        assert message == decrypted


if __name__ == "__main__":
    main()
