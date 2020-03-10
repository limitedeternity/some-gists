from json import dumps


link = lambda target, weight: {"target": target, "weight": weight}
GRAPH = {
    1: [link(2, 6), link(3, 8), link(4, 11), link(5, 10)],
    2: [link(4, 9), link(5, 7), link(6, 15)],
    3: [link(2, 8), link(4, 7), link(5, 4), link(6, 11)],
    4: [link(5, 6), link(6, 7)],
    5: [link(6, 9)],
    6: []
}


def print_adjacent():
    matrix = []
    print("   ", "  ".join(str(x) for x in range(1, len(GRAPH) + 1)), sep="")

    for v in range(1, len(GRAPH) + 1):
        tmp = []
        for tgt in range(1, len(GRAPH) + 1):
            select = list(filter(lambda lnk: lnk["target"] == tgt, GRAPH[v]))
            tmp.append(select[0]["weight"] if select else float("inf"))

        print(str(v), dumps(tmp).replace("Infinity", "∞"), sep=" ")
        matrix.append(tmp)

    return matrix


def tree_method(matrix, source, dest):
    def create_paths(accumulator):
        next_paths = []
        row_idx = accumulator[-1]

        for i in range(len(matrix[row_idx])):
            if matrix[row_idx][i] != float("inf") and i > row_idx:
                next_paths.append(i)

        if not next_paths:
            return [list(map(lambda x: x + 1, accumulator))]

        accs = []
        for path in next_paths:
            accs.extend(create_paths([*accumulator, path]))

        return accs

    def evaluate_weight(path):
        return sum(map(lambda cnt: matrix[cnt[0] - 1][cnt[1] - 1], zip(path[:-1], path[1:])))

    possible_paths = create_paths([source - 1])
    paths_with_dest_reached = list(filter(lambda p: p[-1] == dest, possible_paths))
    path_weights = list(map(evaluate_weight, paths_with_dest_reached))

    print(f"Возможные пути: {paths_with_dest_reached}")
    print(f"Веса: {path_weights}\n")

    if paths_with_dest_reached:
        idx_of_min_weight = 0
        min_weight = path_weights[0]
        for i in range(1, len(path_weights)):
            if path_weights[i] < min_weight:
                min_weight = path_weights[i]
                idx_of_min_weight = i

        print(f"Итог: {' -> '.join(str(x) for x in paths_with_dest_reached[idx_of_min_weight])}")
        print(f"Длина: {min_weight}")
        return paths_with_dest_reached[idx_of_min_weight]

    else:
        return []


if __name__ == "__main__":
    print("Матрица весов (ч.с смежности):")
    A = print_adjacent()
    print("\n")
    print("Поиск кратчайшего пути методом деревьев решений:\n")
    tree_method(A, 1, 6)

