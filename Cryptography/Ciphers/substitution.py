import random
from string import ascii_letters as alphabet

subst_map = dict(
    zip(
        alphabet,
        ''.join(
            random.sample(alphabet, len(alphabet))
        )
    )
)

inverse_subst_map = {v: k for k, v in subst_map.items()}


def encrypt(message):
    return ''.join(map(lambda c: subst_map[c] if c in alphabet else c, message))


def decrypt(ciphertext):
    return ''.join(map(lambda c: inverse_subst_map[c] if c in alphabet else c, ciphertext))


def main():
    message = "To randomly shuffle elements"
    print("Substitution: " + ''.join(subst_map.keys()) +
          " <-> " + ''.join(subst_map.values()))

    encrypted = encrypt(message)
    decrypted = decrypt(encrypted)
    print(message + " -> " + encrypted + " -> " + decrypted)

    assert message == decrypted


if __name__ == "__main__":
    main()
