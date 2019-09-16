def is_unique(lst):
    if len(lst) > 1:
        return is_unique(lst[1:]) and (lst[0] not in lst[1:])

    return True


is_unique([{1}, {1}, {2}, {3}])

