from neurolib import *


class Layer1Neuron1(Neuron):
    def __init__(self, T):
        super().__init__([1.5, -1.0, -1.0], lambda x: 1.0 if x >= T else 0.0)


class Layer1Neuron2(Neuron):
    def __init__(self, T):
        super().__init__([0.5, -1.0, -1.0], lambda x: 1.0 if x >= T else 0.0)


class Layer2Neuron1(Neuron):
    def __init__(self, T):
        super().__init__([-0.5, 1.0, -1.0], lambda x: 1.0 if x >= T else 0.0)


# Maybe[NeuralNetwork]
def setup_network():
    operands = [(i, j) for i in range(2) for j in range(2)]
    for T in map(lambda x: x / 10, range(-20, 25, 5)):
        Layer1 = NeuralNetwork(
            [Layer1Neuron1(T), Layer1Neuron2(T)],
            receptor=lambda inputs: list(
                map(lambda neuron_inputs: [1] + neuron_inputs, inputs)
            )
        )

        Layer2 = NeuralNetwork(
            [Layer2Neuron1(T)],
            receptor=lambda inputs: list(
                map(lambda neuron_inputs: [1] + neuron_inputs, inputs)
            )
        )

        Net = Layer1.compose(Layer2)

        if all(operand1 ^ operand2 == Net.execute([[operand1, operand2]] * 2)[0] for operand1, operand2 in operands):
            return Net

    return None


if setup_network():
    print("Model performs XOR correctly")

else:
    print("Model is unable to perform XOR correctly")

