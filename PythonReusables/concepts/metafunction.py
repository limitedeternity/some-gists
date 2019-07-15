#pylint: disable = too-many-function-args


class _m():
    def __new__(self, x):
        self.x = x
        return self.double_x_n_times(self, 2)

    def double_x_n_times(self, times):
        if times == 0:
            return self.x

        return 2 * self.double_x_n_times(self, times - 1)


# --- same as ---

def m(x):
    def double_x_n_times(times):
        if times == 0:
            return x

        return 2 * double_x_n_times(times - 1)

    return double_x_n_times(2)


print(_m(3) == m(3))  # --> True
