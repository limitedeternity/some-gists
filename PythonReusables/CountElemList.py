# Counts elems in list, creates new tuple of tuples ((key, value), count), sorts them by key.

from collections import Counter

# ---------------------------------------------

# Dict Case
arr = [{'5': 'Good'}, {'2': 'Bad'}, {'3': 'Avg'}, {'3': 'Avg'}, {'4': 'Good'}, {'4': 'Good'}, {'5': 'Good'}]

(*Counter(sorted(list(map(lambda x: (*x.items(), None)[0], arr)))).items(),)

# >> ((('2', 'Bad'), 1), (('3', 'Avg'), 2), (('4', 'Good'), 2), (('5', 'Good'), 2))

# ----------------------------------------------

# Tuple case
arr = (('5', 'Good'), ('2', 'Bad'), ('3', 'Avg'), ('3', 'Avg'), ('4', 'Good'), ('4', 'Good'), ('5', 'Good'))

(*Counter(sorted(arr)).items(),)

# >> ((('2', 'Bad'), 1), (('3', 'Avg'), 2), (('4', 'Good'), 2), (('5', 'Good'), 2))
