from somlib import *


def alpha_linear_decay(epoch):
    initial_lrate = 0.1
    drop_limit = 1000
    lrate = initial_lrate - epoch * initial_lrate / drop_limit
    return lrate


def range_linear_decay(epoch):
    initial_range = 15
    drop_limit = 1000
    nrange = -epoch * (initial_range - 1) / drop_limit + initial_range
    return nrange


print(
    SOM(map_width=30, map_height=30, num_features=3)
        .fit(
            dataset=[[0.5, 0.2, 0.3], [0.4, 0.6, 0.2], [0.3, 0.1, 0.7]],
            learning_rate=alpha_linear_decay,
            affection_range=range_linear_decay,
            mean_error=5e-7
        )
        .process([0.3, 0.3, 0.5])
)
