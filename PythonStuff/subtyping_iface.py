from typing import Protocol
from abc import abstractmethod


class Handler(Protocol):
    @abstractmethod
    def download(self, url: str) -> bytes:
        raise NotImplementedError

    @abstractmethod
    def upload(self, data: bytes, url: str):
        raise NotImplementedError


class HandlerFactory:
    handlers = {}

    @classmethod
    def make_handler(cls, version):
        try:
            return cls.handlers[version]

        except KeyError as err:
            raise NotImplementedError(f"{version=} isn't registered") from err

    @classmethod
    def register(cls, version):
        def wrap(wrapped):
            cls.handlers[version] = wrapped
            return wrapped

        return wrap


@HandlerFactory.register("old")
class FileHandler(Handler):
    def download(self, url: str) -> bytes:
        ...

    def upload(self, data: bytes, url: str):
        ...


@HandlerFactory.register("new")
class NewFileHandler(Handler):
    def download(self, url: str) -> bytes:
        ...

    def upload(self, data: bytes, url: str):
        ...


if __name__ == "__main__":
    handler = HandlerFactory.make_handler("new")
