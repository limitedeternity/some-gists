import inspect


def load_cache(func):
    def inner(self, *args, **kwargs):
        self.load_cache()
        return func(self, *args, **kwargs)

    return inner


def disable_cache_load(func):
    func.no_cache_load = True
    return func


def decorate_all_with(decorator, predicate=None):
    if predicate is None:
        predicate = lambda _: True

    def decorate_all(cls):
        for name, method in inspect.getmembers(cls, inspect.isfunction):
            if predicate(method):
                setattr(cls, name, decorator(method))

        return cls

    return decorate_all


def should_load_cache(func):
    return not hasattr(func, "no_cache_load") or not func.no_cache_load


@decorate_all_with(load_cache, should_load_cache)
class Fib:
    cache = []

    @disable_cache_load
    def load_cache(self, n=10):
        if len(Fib.cache) > n:
            return

        if __debug__:
            print("Populating cache...")

        if not Fib.cache:
            Fib.cache.append(0)
            Fib.cache.append(1)

        for _ in range(len(Fib.cache), n + 1):
            Fib.cache.append(Fib.cache[-1] + Fib.cache[-2])

    def __call__(self, n):
        if len(Fib.cache) <= n:
            self.load_cache(n)

        return Fib.cache[n]


if __name__ == "__main__":
    fib = Fib()

    for n in [5, 10, 12]:
        print(f"fib({n}) = {fib(n)}: {Fib.cache}")
