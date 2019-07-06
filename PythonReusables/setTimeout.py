from threading import Timer


def setTimeout(interval):
    def decorator(func):
        def wrap(*args, **kwargs):
            def run():
                func(*args, **kwargs)

            t = Timer(interval, run)
            t.start()
            return t
        return wrap
    return decorator
 

@setTimeout(5)
def hello():
    print("Hello!")


hello()
