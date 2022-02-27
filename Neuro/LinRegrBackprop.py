from math import floor
from random import uniform
from neurolib import *

X = [[[uniform(0, 2)]] for _ in range(100)]
Y = [[4 + 3 * x + uniform(0, 0.09)] for [[x]] in X]


def alpha_step_decay(epoch):
    initial_lrate = 0.08
    drop_rate = 0.5
    drop_period = 10.0
    lrate = initial_lrate * drop_rate ** floor((1 + epoch) / drop_period)
    return lrate


net = NeuralNetwork(
    [
        NeuronSpec(
            neuron=Neuron.randweights_init(num_weights=2, first_is_bias=True),
            role=NeuronRole.INPUT,
            links={1},
        ),
        NeuronSpec(
            neuron=Neuron.randweights_init(num_weights=2, first_is_bias=True),
            role=NeuronRole.OUTPUT,
            links=set(),
        ),
    ]
)

net.backprop_fit(
    dataset=list(zip(X, Y)),
    learning_rate=alpha_step_decay,
    mean_error=4e-3
)
