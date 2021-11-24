from neurolib import *


class Layer1Neuron1(Neuron):
    def __init__(self, T):
        super().__init__(
            weights=[1.5, -1.0, -1.0],
            activator=lambda x: 1.0 if x >= T else 0.0,
            receptor=lambda inputs: [1] + inputs
        )


class Layer1Neuron2(Neuron):
    def __init__(self, T):
        super().__init__(
            weights=[0.5, -1.0, -1.0],
            activator=lambda x: 1.0 if x >= T else 0.0,
            receptor=lambda inputs: [1] + inputs
        )


class Layer2Neuron1(Neuron):
    def __init__(self, T):
        super().__init__(
            weights=[-0.5, 1.0, -1.0],
            activator=lambda x: 1.0 if x >= T else 0.0,
            receptor=lambda inputs: [1] + inputs
        )


# Maybe[NeuralNetwork]
def setup_network():
    operands = [(i, j) for i in range(2) for j in range(2)]
    for T in map(lambda x: x / 10, range(-20, 25, 5)):
        Net = NeuralNetwork(
            [
                NeuronSpec(
                    neuron=Layer1Neuron1(T),
                    mode="input",
                    links=[2]
                ),
                NeuronSpec(
                    neuron=Layer1Neuron2(T),
                    mode="input",
                    links=[2]
                ),
                NeuronSpec(
                    neuron=Layer2Neuron1(T),
                    mode="output",
                    links=[]
                ),
            ]
        )

        if all([operand1 ^ operand2] == Net.send_inputs([[operand1, operand2]] * 2).process() for operand1, operand2 in operands):
            return Net

    return None


if setup_network():
    print("Model performs XOR correctly")

else:
    print("Model is unable to perform XOR correctly")
