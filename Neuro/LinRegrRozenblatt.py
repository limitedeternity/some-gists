from neurolib import *

X = [[1.0, 1.0], [9.4, 6.4], [2.5, 2.1], [8.0, 7.7], [0.5, 2.2],
     [7.9, 8.4], [7.0, 7.0], [2.8, 0.8], [1.2, 3.0], [7.8, 6.1]]
Y = [1.0, -1.0, 1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0]

neuron = Neuron.randweights_init(
    num_weights=3,
    activator=lambda x: 1.0 if x >= 0.0 else -1.0,
    first_is_bias=True
)

neuron.rozenblatt_fit(
    dataset=list(zip(X, Y)),
    learning_rate=lambda _: 0.2
)

for signals in X:
    print(signals, neuron.process(signals))
