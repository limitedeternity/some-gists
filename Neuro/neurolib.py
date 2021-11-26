from copy import copy, deepcopy
from random import uniform


class Neuron:
    r"""
                    | R      _______________________________     | E
                    | E      | W | ---- |    ______________|     | F
                    | C      | E | \    |    |            ||     | F
                    | E      | I |  \   |    | ACTIVATION ||     | E
    ---- INPUTS ----| P ---- | G |  /   |----| FUNCTION   || ----| C ---- OUTPUT ----
                    | T      | H | /    |    |            ||     | T
                    | O      | T | ---- |    --------------|     | O
                    | R      | S |      |                  |     | R
                             -------------------------------
    """

    # List[Num] -> (Num -> Num) -> Maybe[(List[Num] -> List[Num])] -> Maybe[(Num -> Num)] -> Neuron
    def __init__(self, weights, activator, receptor=lambda x: x, effector=lambda x: x):
        self.weights = weights
        self.activator = activator
        self.receptor = receptor
        self.effector = effector

    # Num -> (Num -> Num) -> Maybe[(List[Num] -> List[Num])] -> Maybe[(Num -> Num)] -> Neuron
    @classmethod
    def randweights_init(cls, num_weights, *args, **kwargs):
        weights = [uniform(-1, 1) for _ in range(num_weights)]
        return cls(weights, *args, **kwargs)

    # List[Num] -> Num
    def stimulator(self, inputs):
        assert len(inputs) == len(self.weights)
        return self.activator(sum(u * w for u, w in zip(inputs, self.weights)))

    # List[Num] -> Num
    def process(self, inputs):
        return self.effector(self.stimulator(self.receptor(inputs)))


class NeuronSpec(object):
    # Neuron -> str -> Set[Num] -> NeuronSpec
    def __init__(self, neuron, mode, links):
        assert mode in {"input", "hidden", "output"}
        assert isinstance(links, set)

        self.neuron = neuron
        self.mode = mode
        self.links = links


class NeuralNetwork:
    # List[NeuronSpec] -> NeuralNetwork
    def __init__(self, neuron_specs):

        # 1. Each neuron can only send signals forward
        assert all(
            link_index > cur_index and link_index < len(neuron_specs)
            for cur_index, spec in enumerate(neuron_specs)
            for link_index in spec.links
        )

        # 2. Output-layer neurons can't transmit signals to other neurons within this network
        assert all(not spec.links for spec in neuron_specs if spec.mode == "output")

        # 3. Input-layer neurons can't receive signals from other neurons within this network
        assert all(
            j
            not in [i for i, spec0 in enumerate(neuron_specs) if spec0.mode == "input"]
            for spec1 in neuron_specs
            for j in spec1.links
        )

        self.neuron_specs = neuron_specs
        self.is_single_layer = all(spec.mode == "input" for spec in neuron_specs)
        self.memory = [[] for _ in range(len(neuron_specs) + 1)]

    # List[List[Num]] -> NeuralNetwork
    def send_inputs(self, inputs):
        input_neuron_indices = [
            i for i, spec in enumerate(self.neuron_specs) if spec.mode == "input"
        ]

        assert len(inputs) == len(input_neuron_indices)

        for i, sigs in zip(input_neuron_indices, inputs):
            self.memory[i] = copy(sigs)

        return self

    # List[Num]
    def process(self):
        for i, spec in enumerate(self.neuron_specs):
            out_signal = spec.neuron.process(self.memory[i])
            self.memory[i].clear()

            if self.is_single_layer or spec.mode == "output":
                self.memory[-1].append(out_signal)

            else:
                for linked_neuron_index in spec.links:
                    self.memory[linked_neuron_index].append(out_signal)

        result = copy(self.memory[-1])
        self.memory[-1].clear()
        return result


class WTASelection:
    """
    Selects a neuron chain which produced the strongest response, which has passed the threshold.
    The chain can be used to construct a new network.
    """

    # NeuralNetwork -> Num -> WTASelection
    def __init__(self, network, threshold):
        self.network = network
        self.threshold = threshold

    # List[List[Num]] -> Tuple[List[NeuronSpec], Num]
    def run(self, inputs):
        results = self.network.send_inputs(inputs).process()
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

        neuron_chain = [None for _ in range(len(self.network.neuron_specs))]
        neuron_chain[output_index] = deepcopy(self.network.neuron_specs[output_index])
        chain_indices = {output_index}

        for i in range(output_index - 1, -1, -1):
            intersection = self.network.neuron_specs[i].links.intersection(
                chain_indices
            )

            if intersection:
                neuron_chain[i] = deepcopy(self.network.neuron_specs[i])
                neuron_chain[i].links = intersection
                chain_indices.add(i)

        return ([elem for elem in neuron_chain if elem is not None], winner_result)
