from functools import wraps


class Neuron:
    # List[Num] -> (Num -> Num) -> Neuron
    def __init__(self, weights, activate):
        self.weights = weights
        self.activate = activate

    # List[Num] -> Num
    def execute(self, inputs):
        assert(len(inputs) == len(self.weights))
        return self.activate(sum(u * w for u, w in zip(inputs, self.weights)))


class NeuralNetwork:
    # List[Neuron] -> (List[List[Num]] -> List[List[Num]]) -> (List[Num] -> List[Num]) -> NeuralNetwork
    def __init__(self, neurons, receptor=lambda x: x, effector=lambda x: x):
        self.neurons = neurons
        self.receptor = receptor
        self.effector = effector

    # List[List[Num]] -> List[Num]
    def execute(self, inputs):
        inputs = self.receptor(inputs)
        assert(len(inputs) == len(self.neurons))
        return self.effector([
            neuron.__getattribute__("execute").__call__(U)
            for U, neuron in zip(inputs, self.neurons)
        ])

    # NeuralNetwork -> NeuralNetwork
    def compose(self, network):
        def decorate(func):
            @wraps(func)
            def wrapped(*args, **kwargs):
                return network.__getattribute__("execute").__call__([func(*args, **kwargs)] *
                                                                    len(network.__getattribute__("neurons")))
            return wrapped

        setattr(self, "execute", decorate(self.execute))
        return self


class WTASelection:
    """
    Performs neuron selection within single-layer network.
    Selects a neuron with strongest response, which has passed the threshold
    """

    # NeuralNetwork -> Num -> WTASelection
    def __init__(self, network, threshold):
        self.network = network
        self.threshold = threshold

    # List[List[Num]] -> Tuple[str, Num]
    def execute(self, inputs):
        results = self.network.__getattribute__("execute").__call__(inputs)
        winner_index = max(range(len(results)), key=results.__getitem__)

        winner_result, winner_name = results[winner_index], \
            self.network.__getattribute__(
                "neurons"
        )[winner_index].__class__.__name__

        return ("None" if winner_result < self.threshold else winner_name, winner_result)
