from neurolib import *


class MammalNeuron(Neuron):
    def __init__(self):
        super().__init__(
            weights=[4.0, 0.01, 0.01, -1.0, -1.5],
            activator=lambda x: x
        )


class BirdNeuron(Neuron):
    def __init__(self):
        super().__init__(
            weights=[2.0, -1.0, 2.0, 2.5, 2.0],
            activator=lambda x: x
        )


class FishNeuron(Neuron):
    def __init__(self):
        super().__init__(
            weights=[-1.0, 3.5, 0.01, -2.0, 1.5],
            activator=lambda x: x
        )


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

