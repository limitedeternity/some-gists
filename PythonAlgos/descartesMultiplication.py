from functools import reduce
from itertools import product

# -> [(1, 1), (1, 4), (2, 1), (2, 4), (3, 1), (3, 4)]
list(reduce(product, [(1, 2, 3), (1, 4)]))
