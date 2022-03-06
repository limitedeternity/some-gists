class CallCheckDecoratorClass(object):
    called = set()

    def __getattribute__(self, name):
        attr = object.__getattribute__(self, name)

        if hasattr(attr, "__call__"):
            def wrap(*args, **kwargs):
                result = attr(*args, **kwargs)
                self.called.add(attr.__name__)
                return result

            return wrap

        return attr


class Bar(CallCheckDecoratorClass):
    def myFunc(self):
        # now use super().called attr to see what methods were called
        pass
