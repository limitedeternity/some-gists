from neurolib import *


class MammalNeuron(Neuron):
    def __init__(self):
        super().__init__(weights=[4.0, 0.01, 0.01, -1.0, -1.5])


class BirdNeuron(Neuron):
    def __init__(self):
        super().__init__(weights=[2.0, -1.0, 2.0, 2.5, 2.0])


class FishNeuron(Neuron):
    def __init__(self):
        super().__init__(weights=[-1.0, 3.5, 0.01, -2.0, 1.5])


selector = WTASelection(
    NeuralNetwork(
        [
            NeuronSpec(neuron=MammalNeuron(), mode="input", links=set()),
            NeuronSpec(neuron=BirdNeuron(), mode="input", links=set()),
            NeuronSpec(neuron=FishNeuron(), mode="input", links=set()),
        ]
    ),
    10.0,
)

winner_chain, result = selector.process([[4.0, -1.0, -1.0, -1.0, -1.0]] * 3)
assert (winner_chain[-1].neuron.__class__.__name__, result) == ("MammalNeuron", 18.48)

winner_chain, result = selector.process([[2.0, -1.0, 2.0, 2.0, 1.0]] * 3)
assert (winner_chain[-1].neuron.__class__.__name__, result) == ("BirdNeuron", 16.0)

winner_chain, result = selector.process([[0.0, 2.0, -1.0, -1.0, 2.0]] * 3)
assert (winner_chain[-1].neuron.__class__.__name__, result) == ("FishNeuron", 11.99)
