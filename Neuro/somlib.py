from collections.abc import Callable
from enum import Enum
from itertools import chain, islice
from math import exp
from random import shuffle, uniform


class VectorNorm(Enum):
    CHEBYSHEV = 0
    MANHATTAN = 1
    EUCLIDEAN = 2


class Cell:
    __slots__ = "weights", "x_coord", "y_coord"

    def __init__(self, x_coord, y_coord, num_features):
        # type: (int, int, int) -> None

        self.weights = [uniform(-0.5, 0.5) for _ in range(num_features)]
        self.x_coord = x_coord
        self.y_coord = y_coord

    def __repr__(self):
        # type: () -> str

        return "Cell(x={}, y={}, weights={})".format(
            self.x_coord, self.y_coord, self.weights
        )


class SOM:
    __slots__ = "cells", "norm_type"

    def __init__(self, map_width, map_height, num_features, norm_type=VectorNorm.CHEBYSHEV):
        # type: (int, int, int, VectorNorm) -> None

        if map_width < 2 or map_height < 2:
            raise AssertionError("Map size has to be at least 2x2")

        if num_features < 1:
            raise AssertionError("There has to be at least 1 feature")

        self.cells = [
            [Cell(x, y, num_features) for x in range(map_width)]
            for y in range(map_height)
        ]

        self.norm_type = norm_type

    def __repr__(self):
        # type: () -> str

        return '\n'.join('\t'.join(str(cell.weights) for cell in row) for row in self.cells)

    def fit(self, dataset, learning_rate, affection_range, mean_error):
        # type: (list[list[float]], Callable[[int], float], Callable[[int], float], float) -> SOM
        """
        Warning:
        The algorithm won't terminate if it's impossible to reach the desired mean error.
        """

        epoch, real_mean_error = (
            0,
            sum(
                sum(
                    abs(a - b) / max(abs(b), 1e-3)
                    for a, b in zip(feature_vector, weights)
                )
                / len(weights)
                for feature_vector, weights in zip(dataset, map(lambda vec: self.process(vec).weights, dataset))
            )
            / len(dataset)
        )

        while real_mean_error > mean_error:

            shuffle(dataset)
            neighbour_range = max(1, int(affection_range(epoch)))

            for feature_vector in dataset:
                best_matching_unit = self.process(feature_vector)

                neighbours_up = min(
                    neighbour_range, best_matching_unit.y_coord
                )

                neighbours_down = neighbour_range if best_matching_unit.y_coord + \
                    neighbour_range < len(self.cells) else len(self.cells) - 1 - best_matching_unit.y_coord

                neighbours_left = min(
                    neighbour_range, best_matching_unit.x_coord
                )

                neighbours_right = neighbour_range if best_matching_unit.x_coord + \
                    neighbour_range < len(self.cells[0]) else len(self.cells[0]) - 1 - best_matching_unit.x_coord

                for row in self.cells[best_matching_unit.y_coord - neighbours_up:
                                      best_matching_unit.y_coord + neighbours_down]:
                    for neighbour in row[best_matching_unit.x_coord - neighbours_left:
                                         best_matching_unit.x_coord + neighbours_right]:

                        theta = exp(
                            -1 * self.calc_distance([best_matching_unit.y_coord, best_matching_unit.x_coord],
                                                    [neighbour.y_coord, neighbour.x_coord]) ** 2
                            / (2 * affection_range(epoch) ** 2)
                        )

                        neighbour.weights = [
                            weight + learning_rate(epoch) *
                            theta * (feature - weight)
                            for feature, weight in zip(feature_vector, neighbour.weights)
                        ]

            epoch, real_mean_error = (
                epoch + 1,
                sum(
                    sum(
                        abs(a - b) / max(abs(b), 1e-3)
                        for a, b in zip(feature_vector, weights)
                    )
                    / len(weights)
                    for feature_vector, weights in zip(dataset, map(lambda vec: self.process(vec).weights, dataset))
                )
                / len(dataset)
            )

            if __debug__:
                print(
                    "Epoch {} mean error: {:.5%}".format(
                        epoch,
                        real_mean_error,
                    )
                )

        return self

    def process(self, feature_vector):
        # type: (list[float]) -> Cell

        best_matching_unit = self.cells[0][0]
        min_distance = self.calc_distance(
            feature_vector, best_matching_unit.weights
        )

        for cell in islice(chain.from_iterable(self.cells), 1, None):
            dist = self.calc_distance(
                feature_vector, cell.weights
            )

            if dist < min_distance:
                best_matching_unit = cell
                min_distance = dist

        return best_matching_unit

    def calc_distance(self, feature_vector, weights):
        # type: (list[float], list[float]) -> float

        if len(feature_vector) != len(weights):
            raise AssertionError(
                "Expected len(feature_vector) to be {}, got: {}".format(
                    len(weights),
                    len(feature_vector)
                )
            )

        return {
            VectorNorm.CHEBYSHEV: lambda fv, w: max(abs(a - b) for a, b in zip(fv, w)),
            VectorNorm.MANHATTAN: lambda fv, w: sum(abs(a - b) for a, b in zip(fv, w)),
            VectorNorm.EUCLIDEAN: lambda fv, w: sum((a - b) ** 2 for a, b in zip(fv, w)) ** 0.5,
        }[self.norm_type](feature_vector, weights)
