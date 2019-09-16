import itertools


def proper_subsets(s):
  return [set(x) for n in range(1, len(s)) for x in itertools.combinations(list(s), n)]


proper_subsets({1,2,3,4})
