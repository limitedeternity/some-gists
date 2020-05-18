from itertools import starmap
from json import dumps
from operator import mul


GRAPH = {
    1: [2],
    2: [],
    3: [],
    4: [],
    5: [3, 4],
    6: [5, 9],
    7: [1, 6, 8, 9],
    8: [],
    9: [2, 3]
}


def print_adjacent():
    matrix = []
    print("   ", "  ".join(str(x) for x in range(1, len(GRAPH) + 1)), sep="")

    for v in range(1, len(GRAPH) + 1):
        tmp = []
        for tgt in range(1, len(GRAPH) + 1):
            tmp.append(1 if tgt in GRAPH[v] else 0)

        print(str(v), dumps(tmp), sep=" ")
        matrix.append(tmp)

    return matrix


def print_incidence():
    matrix = []
    print("  ", " ".join(f"e{i}" for i in range(1, sum(list(map(lambda v: len(GRAPH[v]), GRAPH))) + 1)), sep="")

    for i in range(len(GRAPH)):
        matrix.append([0] * sum(list(map(lambda v: len(GRAPH[v]), GRAPH))))

    rebro = 0
    for v in range(1, len(GRAPH) + 1):
        for i in range(len(GRAPH[v])):
            if v == GRAPH[v][i]:
                matrix[v - 1][rebro] = 2
                continue

            matrix[v - 1][rebro] = 1
            matrix[GRAPH[v][i] - 1][rebro] = -1
            rebro += 1

    for i in range(1, len(matrix) + 1):
        print(str(i), dumps(matrix[i - 1]), sep=" ")

    return matrix


if __name__ == "__main__":
    print("Матрица смежности:\n")
    A = print_adjacent()
    print("\n")
    print("Матрица инцидентности:\n")
    print_incidence()
    print("\n")
    print("-----\n")


    ZERO = []
    for i in range(len(A)):
        tmp = [0] * len(A)
        ZERO.append(tmp)

    mul_list = [A]
    for i in range(2, len(A)):
        a_pow = [[sum(starmap(mul, zip(row, col))) for col in zip(*A)] for row in mul_list[-1]]
        if a_pow == ZERO:
            break

        print(f"A^{i}\n")
        for row in a_pow:
            print(dumps(row))

        print("\n")
        mul_list.append(a_pow)


    print("-----\n\n")
    print("Матрица достижимости:\n")
    result = A
    for i in range(1, len(mul_list)):
        result = [list(map(sum, zip(result[x], mul_list[i][x]))) for x in range(len(A))]

    print("   ", "  ".join(str(x) for x in range(1, len(A) + 1)), sep="")
    for v in range(1, len(A) + 1):
        print(str(v), dumps(result[v - 1]), sep=" ")

    print("\n", sep="")

