from random import uniform
from neurolib import *

net = NeuralNetwork([
    NeuronSpec(
        neuron=Neuron.randweights_init(
            2,
            first_is_bias=True,
            activator=lambda x: ((x * x + 1)**0.5 - 1) / 2 + x
        ),
        mode="input",
        links={1}
    ),
    NeuronSpec(
        neuron=Neuron.randweights_init(
            2,
            first_is_bias=True,
            activator=lambda x: ((x * x + 1)**0.5 - 1) / 2 + x
        ),
        mode="output",
        links=set()
    )
])

X = [[[uniform(0, 2)]] for _ in range(100)]
Y = [[4 + 3 * x + uniform(0, 0.9)] for [[x]] in X]

net.backprop_fit(
    dataset=list(zip(X, Y)),
    learning_rate=lambda _: 0.01,
    mean_error=0.04
)
