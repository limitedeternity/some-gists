import math
from neurolib import *

X = [[[0.0, 0.0]] * 2, [[0.0, 1.0]] * 2, [[1.0, 0.0]] * 2, [[1.0, 1.0]] * 2]
Y = [[0.0], [1.0], [1.0], [0.0]]

net = NeuralNetwork([
    NeuronSpec(
        neuron=Neuron.randweights_init(
            num_weights=3,
            first_is_bias=True,
            activator=lambda x: 1 / (1 + math.e**(-x))
        ),
        role=NeuronRole.INPUT,
        links={2}
    ),
    NeuronSpec(
        neuron=Neuron.randweights_init(
            num_weights=3,
            first_is_bias=True,
            activator=lambda x: 1 / (1 + math.e**(-x))
        ),
        role=NeuronRole.INPUT,
        links={2}
    ),
    NeuronSpec(
        neuron=Neuron.randweights_init(
            num_weights=3,
            first_is_bias=True,
            activator=lambda x: 1 / (1 + math.e**(-x))
        ),
        role=NeuronRole.OUTPUT,
        links=set()
    )
])

net.backprop_fit(
    dataset=list(zip(X, Y)),
    learning_rate=lambda _: 0.8,
    mean_error=3.0
)

for signals in X:
    print(net.process(signals))
