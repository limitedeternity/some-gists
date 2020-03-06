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


def dijkstra(matrix, source, dest):
    # Отмечаем, что дистанция до всех вершин бесконечна
    # На стартовую вершину ставим дистанцию 0
    distances = {vertex: float("inf") for vertex in GRAPH}
    prev_vertices = {vertex: None for vertex in GRAPH}
    distances[source] = 0
    vertices = list(GRAPH.keys())

    while vertices:
        # Выбираем вершину с наименьшей дистанцией
        print(f"Дистанции: {dumps(distances)}")

        t = list(map(lambda v: f"x{v}", vertices))
        print(f"Непосещённые вершины: {dumps(t)}")

        curr_vertex = min(vertices, key=lambda v: distances[v])
        print(f"Выбор непосещённой вершины с наим. дистанцией: x{curr_vertex}\n")

        # Если наименьшая дистанция -- бесконечность, то стоит выйти
        if distances[curr_vertex] == float("inf"):
            break

        for connection in GRAPH[curr_vertex]:
            neighbour, cost = connection.values()
            print(f"Соединение: (x{curr_vertex}, x{neighbour})")
            alt_route = distances[curr_vertex] + cost
            curr_distance = distances[neighbour]
            print(f"d(x{neighbour}) = min ({curr_distance}, {alt_route}) = {min([curr_distance, alt_route])}\n")

            if alt_route < curr_distance:
                distances[neighbour] = alt_route
                prev_vertices[neighbour] = curr_vertex
                print(f"Соединил в кратчайшем пути x{neighbour} с x{curr_vertex}\n")

        # Вершину посетили, удаляем
        vertices.remove(curr_vertex)
        print("-----")

    print("Восстановление пути:\n")
    path, curr_vertex = list(), dest
    while prev_vertices[curr_vertex] != None:
        print(f"{prev_vertices[curr_vertex]} <- {curr_vertex}")
        path.insert(0, curr_vertex)
        curr_vertex = prev_vertices[curr_vertex]

    if path:
        path.insert(0, curr_vertex)

    print(f"Итог: {' -> '.join(str(x) for x in path)}")
    print(f"Длина пути: {len(path)}")
    return path


if __name__ == "__main__":
    print("Матрица весов (ч.с смежности):")
    A = print_adjacent()
    print("\n")
    print("Поиск Дейкстры:\n")
    dijkstra(A, 1, 6)

