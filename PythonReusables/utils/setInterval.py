from threading import Timer


def setInterval(interval):
    def decorator(func):
        def wrap(*args, **kwargs):
            def restart():
                wrap(*args, **kwargs)

            func(*args, **kwargs)

            t = Timer(interval, restart)
            t.start()
            return t
        return wrap
    return decorator


@setInterval(5)
def hello():
    print("Hello!")


if __name__ == "__main__":
    hello()
