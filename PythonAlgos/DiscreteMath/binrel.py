def symmetry_test(pair_list):
    return all(
        map(
            lambda pair: (pair[1], pair[0]) in pair_list,
            pair_list
        )
    )


def asymmetry_test(pair_list):
    return all(
        map(
            lambda pair: (pair[1], pair[0]) not in pair_list,
            pair_list
        )
    )


def antisymmetry_test(pair_list):
    return all(
        map(
            lambda pair:
                pair[0] == pair[1] or
                (pair[1], pair[0]) not in pair_list,
            pair_list
        )
    )


def reflectivity_test(pair_list):
    for num in list({v for x in pair_list for v in x}):
        if (num, num) not in pair_list:
            return False

    return True


def transitivity_test(pair_list):
    for i in range(len(pair_list)):
        for j in range(len(pair_list)):
            if pair_list[i] == pair_list[j]:
                continue

            if pair_list[i][1] == pair_list[j][0]:
                if (pair_list[i][0], pair_list[j][1]) not in pair_list:
                    return False

    return True
