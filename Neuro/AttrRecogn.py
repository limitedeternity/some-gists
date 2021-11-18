from neurolib import *


class MammalNeuron(Neuron):
    def __init__(self):
        super().__init__([4.0, 0.01, 0.01, -1.0, -1.5], lambda x: x)


class BirdNeuron(Neuron):
    def __init__(self):
        super().__init__([2.0, -1.0, 2.0, 2.5, 2.0], lambda x: x)


class FishNeuron(Neuron):
    def __init__(self):
        super().__init__([-1.0, 3.5, 0.01, -2.0, 1.5], lambda x: x)


selector = WTASelection(
    NeuralNetwork([MammalNeuron(), BirdNeuron(), FishNeuron()]), 10.0
)

# 1 - сколько у существа ног
# 2 - живёт ли существо в воде
# 3 - умеет ли существо летать
# 4 - покрыто ли существо перьями
# 5 - рождается ли существо из яиц

assert(
    selector.execute(
        [[4.0, -1.0, -1.0, -1.0, -1.0]] * 3
    ) == ("MammalNeuron", 18.48)
)

assert(
    selector.execute(
        [[2.0, -1.0, 2.0, 2.0, 1.0]] * 3
    ) == ("BirdNeuron", 16.0)
)

assert(
    selector.execute(
        [[0.0, 2.0, -1.0, -1.0, 2.0]] * 3
    ) == ("FishNeuron", 11.99)
)

