from neurolib import *


class MammalNeuron(Neuron):
    def __init__(self, *args, **kwargs):
        super().__init__([4.0, 0.01, 0.01, -1.0, -1.5],
                         lambda x: x, *args, **kwargs)


class BirdNeuron(Neuron):
    def __init__(self, *args, **kwargs):
        super().__init__([2.0, -1.0, 2.0, 2.5, 2.0],
                         lambda x: x, *args, **kwargs)


class FishNeuron(Neuron):
    def __init__(self, *args, **kwargs):
        super().__init__([-1.0, 3.5, 0.01, -2.0, 1.5],
                         lambda x: x, *args, **kwargs)


selector = WTASelection(
    NeuralNetwork(
        [
            NeuronSpec(
                neuron=MammalNeuron(),
                mode="input",
                links=[]
            ),
            NeuronSpec(
                neuron=BirdNeuron(),
                mode="input",
                links=[]
            ),
            NeuronSpec(
                neuron=FishNeuron(),
                mode="input",
                links=[]
            ),
        ]
    ), 10.0
)

assert(
    selector.run(
        [[4.0, -1.0, -1.0, -1.0, -1.0]] * 3
    ) == ("MammalNeuron", 18.48)
)

assert(
    selector.run(
        [[2.0, -1.0, 2.0, 2.0, 1.0]] * 3
    ) == ("BirdNeuron", 16.0)
)

assert(
    selector.run(
        [[0.0, 2.0, -1.0, -1.0, 2.0]] * 3
    ) == ("FishNeuron", 11.99)
)

# =======================================


def signal_power(vec): return sum(x * x for x in vec)**0.5


assert(
    signal_power(MammalNeuron().weights) !=
    signal_power([4.0, -1.0, -1.0, -0.9, -1.0])
    and
    signal_power(MammalNeuron().weights) !=
    signal_power([8.0, -6.0, -3.0, -5.0, -9.0])
)
