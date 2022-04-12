import cmath
from itertools import chain, repeat, islice, zip_longest
from typing import Any, Iterable, Iterator, Sequence, Union


def ljust(iterable: Iterable[Any], size: int, padding: Any = None) -> Iterator[Any]:
    return islice(chain(iterable, repeat(padding)), size)


def fft(poly_coeffs: Sequence[Union[int, complex]]) -> Sequence[complex]:
    n = len(poly_coeffs)
    assert n != 0 and (n & (n - 1)) == 0  # n must be a power of 2

    if n == 1:
        return poly_coeffs

    omega = cmath.exp(2 * cmath.pi * 1j / n)
    coeffs_even, coeffs_odd = [], []

    imparity_gate = False
    for coeff in poly_coeffs:
        if not imparity_gate:
            coeffs_even.append(coeff)

        else:
            coeffs_odd.append(coeff)

        imparity_gate ^= True

    y_even, y_odd = fft(coeffs_even), fft(coeffs_odd)

    y = [0j for _ in range(n)]
    for i in range(n // 2):
        y[i] = y_even[i] + omega ** i * y_odd[i]
        y[i + n // 2] = y_even[i] - omega ** i * y_odd[i]

    return y


def ifft(poly_values: Sequence[complex]) -> Sequence[complex]:
    result = fft([x.conjugate() for x in poly_values])
    return [x.conjugate() / len(poly_values) for x in result]


class Polynomial:
    def __init__(self, coefficients: Sequence[int]) -> None:
        assert len(coefficients) != 0
        self.coefficients = coefficients

    def __add__(self, other):
        return Polynomial(
            [
                a + b
                for a, b in zip_longest(
                    self.coefficients, other.coefficients, fillvalue=0
                )
            ]
        )

    def __sub__(self, other):
        return Polynomial(
            [
                a - b
                for a, b in zip_longest(
                    self.coefficients, other.coefficients, fillvalue=0
                )
            ]
        )

    def __mul__(self, other):
        n = 1 << (len(self.coefficients) + len(other.coefficients) - 2).bit_length()

        pad_self, pad_other = list(ljust(self.coefficients, n, 0)), list(
            ljust(other.coefficients, n, 0)
        )

        fft_self, fft_other = fft(pad_self), fft(pad_other)

        coeff_mul = list(map(lambda a, b: a * b, fft_self, fft_other))
        ifft_mul = ifft(coeff_mul)

        return Polynomial([round(x.real) for x in ifft_mul])

    def __repr__(self) -> str:
        result = ""
        n = len(self.coefficients)

        for i in range(n):
            rank = n - 1 - i
            coefficient = self.coefficients[rank]

            if coefficient == 0:
                continue

            if coefficient < 0:
                head, _, tail = result.rpartition(" + ")
                result = head + " - " + tail if head else "-" + tail
                coefficient = abs(coefficient)

            result += f"{coefficient if rank == 0 or coefficient > 1 else ''}{'x' if rank > 0 else ''}{'^' + str(rank) if rank > 1 else ''}"

            if not all(x == 0 for x in self.coefficients[:rank]):
                result += " + "

        return result


if __name__ == "__main__":
    a = Polynomial([1, 3, 1, 2, -2, 0, 5])  # 1 + 3x + x^2 + 2x^3 - 2x^4 + 5x^6
    b = Polynomial([10, 0, 0, 0, 2, 7, 2, 0, 1])  # 10 + 2x^4 + 7x^5 + 2x^6 + x^8

    print(a + b)
    print(a - b)
    print(a * b)
