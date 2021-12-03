from collections.abc import Callable
from copy import deepcopy
from itertools import takewhile, dropwhile
from random import uniform, shuffle
from m_helpers import DotProduct, partial_derivative


class Neuron:
    def __init__(self, weights, first_is_bias=False, activator=lambda x: x):
        # type: (list[float], bool, Callable[[float], float]) -> None

        self.weights = weights
        self.first_is_bias = first_is_bias
        self.receptor = (lambda x: [1] + x) if first_is_bias else (lambda x: x)
        self.activator = activator

    @classmethod
    def randweights_init(cls, num_weights, *args, **kwargs):
        # type: (int, ..., ...) -> Neuron

        weights = [uniform(-0.5, 0.5) for _ in range(num_weights)]
        return cls(weights, *args, **kwargs)

    def stimulator(self, inputs):
        # type: (list[float]) -> float

        inputs = self.receptor(inputs)

        if len(inputs) != len(self.weights):
            raise AssertionError(
                "Expected len(inputs) to be {}, got: {}".format(
                    len(self.weights), len(inputs)
                )
            )

        return DotProduct(inputs, self.weights)

    def process(self, inputs):
        # type: (list[float]) -> float

        return self.activator(self.stimulator(inputs))


class NeuronSpec(object):
    def __init__(self, neuron, mode, links):
        # type: (Neuron, str, set[int]) -> None

        if mode not in {"input", "hidden", "output"}:
            raise AssertionError("Incorrect mode: {}".format(mode))

        if not isinstance(links, set):
            raise AssertionError(
                "Links is expected to be <class 'set'>, got: {}".format(
                    type(links)
                )
            )

        self.neuron = neuron
        self.mode = mode
        self.links = links


class NeuralNetwork:
    def __init__(self, neuron_specs):
        # type: (list[NeuronSpec]) -> None

        try:
            next(
                dropwhile(
                    lambda spec: spec.mode == "output",
                    dropwhile(
                        lambda spec: spec.mode == "hidden",
                        dropwhile(
                            lambda spec: spec.mode == "input",
                            neuron_specs
                        )
                    )
                )
            )

            raise AssertionError(
                "neuron_specs ordering rule: [*input_neurons, *hidden_neurons, *output_neurons]"
            )

        except StopIteration:
            pass

        if not all(
            link_index > cur_index and link_index < len(neuron_specs)
            for cur_index, spec in enumerate(neuron_specs)
            for link_index in spec.links
        ):
            raise AssertionError(
                "Each neuron can only send signals forward, but not over the bounds"
            )

        if not all(
            not spec.links for spec in dropwhile(lambda spec: spec.mode != "output", neuron_specs)
        ):
            raise AssertionError(
                "Output-layer neurons can't transmit signals to other neurons within this network"
            )

        if not all(
            j
            not in [i for i, _ in enumerate(takewhile(lambda spec: spec.mode == "input", neuron_specs))]
            for spec in neuron_specs
            for j in spec.links
        ):
            raise AssertionError(
                "Input-layer neurons can't receive signals from other neurons within this network"
            )

        try:
            next(
                dropwhile(
                    lambda spec: spec.mode == "input",
                    neuron_specs
                )
            )

            self.is_single_layer = False

        except StopIteration:
            self.is_single_layer = True

        self.memory = [[] for _ in range(len(neuron_specs) + 1)]
        self.neuron_specs = neuron_specs

    def process(self, inputs):
        # type: (list[list[float]]) -> list[float]

        if len(inputs) != sum(
            1 for _ in takewhile(
                lambda spec: spec.mode == "input",
                self.neuron_specs
            )
        ):
            raise AssertionError(
                "Expected len(inputs) to be {}, got: {}".format(
                    sum(
                        1 for _ in takewhile(
                            lambda spec: spec.mode == "input",
                            self.neuron_specs
                        )
                    ),
                    len(inputs)
                )
            )

        for cell in self.memory:
            cell.clear()

        for i, spec in enumerate(self.neuron_specs):
            if spec.mode == "input":
                out_signal = spec.neuron.process(inputs[i])

            else:
                out_signal = spec.neuron.process(self.memory[i])

            if self.is_single_layer or spec.mode == "output":
                self.memory[-1].append(out_signal)

            else:
                for linked_neuron_index in spec.links:
                    self.memory[linked_neuron_index].append(out_signal)

        return self.memory[-1]

    def backprop_fit(self, dataset, learning_rate, mean_error):
        # type: (list[tuple[list[list[float]], list[float]]], Callable[[int], float], float) -> NeuralNetwork
        """
        Warning:
        1. The algorithm may not terminate if derivative of any activation function is zero.
        2. The algorithm won't terminate if it's not possible to reach the desired mean error.
        """

        if mean_error < 0 or mean_error > 1:
            raise AssertionError(
                "Mean error (%) is a positive value between 0 and 1"
            )

        epoch = 0

        while not sum(
            sum(
                abs((b - a) / (b + 1 + 1e-10)) for a, b in zip(
                    self.process(net_in),
                    net_expected_out
                )
            ) / len(net_expected_out)
            for net_in, net_expected_out in dataset
        ) / len(dataset) < mean_error:

            shuffle(dataset)

            for net_in, net_expected_out in dataset:
                for out_index in range(len(net_expected_out)):
                    if self.is_single_layer:
                        neuron_chain = [self.neuron_specs[out_index]]
                        sorted_ci = [out_index]

                    else:
                        out_neur_index = [
                            i
                            for i, spec in enumerate(self.neuron_specs)
                            if spec.mode == "output"
                        ][out_index]

                        neuron_chain = [None for _ in self.neuron_specs]
                        neuron_chain[out_neur_index] = deepcopy(
                            self.neuron_specs[out_neur_index]
                        )

                        chain_indices = {out_neur_index}
                        for i in range(out_neur_index - 1, -1, -1):
                            intersection = self.neuron_specs[i].links.intersection(
                                chain_indices
                            )

                            if intersection:
                                neuron_chain[i] = deepcopy(
                                    self.neuron_specs[i]
                                )

                                neuron_chain[i].links = intersection
                                chain_indices.add(i)

                        neuron_chain = [
                            elem for elem in neuron_chain if elem is not None
                        ]

                        sorted_ci = sorted(chain_indices)

                        link_correction_map = dict(
                            zip(sorted_ci, range(len(sorted_ci)))
                        )

                        for elem in neuron_chain:
                            elem.links = {
                                link_correction_map[link] for link in elem.links
                            }

                    partial_network = NeuralNetwork(neuron_chain)
                    input_indices = sorted_ci[:sum(
                        1 for _ in takewhile(
                            lambda spec: spec.mode == "input",
                            partial_network.neuron_specs
                        )
                    )]

                    [net_out] = partial_network.process(
                        [net_in[i] for i in input_indices]
                    )

                    dMSE_dOut_memory = [
                        None for _ in partial_network.neuron_specs
                    ]

                    dMSE_dOut_memory[-1] = (
                        net_out - net_expected_out[out_index]
                    )

                    for i in range(len(partial_network.neuron_specs) - 1, -1, -1):
                        neuron = partial_network.neuron_specs[i].neuron

                        dOut_dStim = partial_derivative(
                            neuron.activator,
                            0,
                            DotProduct(
                                neuron.receptor(
                                    partial_network.memory[i]
                                ),
                                neuron.weights
                            )
                        )

                        dMSE_dStim = dMSE_dOut_memory[i] * dOut_dStim

                        gradient_dStim_dw = [
                            partial_derivative(
                                DotProduct.derivative_wrap,
                                weight_index,
                                *neuron.weights,
                                *neuron.receptor(
                                    partial_network.memory[i]
                                )
                            )
                            for weight_index in range(len(neuron.weights))
                        ]

                        gradient_dStim_dIn = [
                            partial_derivative(
                                DotProduct.derivative_wrap,
                                input_index,
                                *neuron.receptor(
                                    partial_network.memory[i]
                                ),
                                *neuron.weights
                            )
                            for input_index in range(
                                1 if neuron.first_is_bias else 0,
                                len(neuron.weights)
                            )
                        ]

                        gradient_dMSE_dw = [
                            dMSE_dStim * dStim_dw for dStim_dw in gradient_dStim_dw
                        ]

                        for weight_index, dMSE_dw in enumerate(gradient_dMSE_dw):
                            neuron.weights[weight_index] = (
                                neuron.weights[weight_index] -
                                learning_rate(epoch) * dMSE_dw
                            )

                        gradient_dMSE_dIn = [
                            dMSE_dStim * dStim_dIn for dStim_dIn in gradient_dStim_dIn
                        ]

                        for source_index, dMSE_dOut in zip(
                            [
                                j
                                for j in range(i)
                                if i in partial_network.neuron_specs[j].links
                            ],
                            gradient_dMSE_dIn
                        ):
                            dMSE_dOut_memory[source_index] = dMSE_dOut

                    for src, dst in zip(range(len(sorted_ci)), sorted_ci):
                        source_neuron = partial_network.neuron_specs[src].neuron
                        destination_neuron = self.neuron_specs[dst].neuron
                        destination_neuron.weights = source_neuron.weights

            epoch += 1

        return self


class WTASelection:
    """
    Selects a neuron chain which produced the strongest response, which has passed the threshold.
    The chain can be used to construct a new network.
    """

    def __init__(self, network, threshold):
        # type: (NeuralNetwork, float) -> None

        self.network = network
        self.threshold = threshold

    def process(self, inputs):
        # type: (list[list[float]]) -> tuple[list[NeuronSpec], float]

        results = self.network.process(inputs)
        winner_index = max(range(len(results)), key=results.__getitem__)
        winner_result = results[winner_index]

        if winner_result < self.threshold:
            return ([], winner_result)

        if self.network.is_single_layer:
            return ([self.network.neuron_specs[winner_index]], winner_result)

        output_index = [
            i
            for i, spec in enumerate(self.network.neuron_specs)
            if spec.mode == "output"
        ][winner_index]

        neuron_chain = [None for _ in self.network.neuron_specs]
        neuron_chain[output_index] = deepcopy(
            self.network.neuron_specs[output_index]
        )

        chain_indices = {output_index}
        for i in range(output_index - 1, -1, -1):
            intersection = self.network.neuron_specs[i].links.intersection(
                chain_indices
            )

            if intersection:
                neuron_chain[i] = deepcopy(
                    self.network.neuron_specs[i]
                )

                neuron_chain[i].links = intersection
                chain_indices.add(i)

        neuron_chain = [
            elem for elem in neuron_chain if elem is not None
        ]

        sorted_ci = sorted(chain_indices)

        link_correction_map = dict(
            zip(sorted_ci, range(len(neuron_chain)))
        )

        for elem in neuron_chain:
            elem.links = {
                link_correction_map[link] for link in elem.links
            }

        return (neuron_chain, winner_result)
