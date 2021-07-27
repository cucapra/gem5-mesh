'''
    Timer utility
'''

from time import time
from contextlib import contextmanager
from collections import defaultdict

class Timer(object):
    def __init__(self):
        self.results = defaultdict(list)

    @contextmanager
    def time(self, name):
        # time (in seconds)
        start = time()
        yield
        elapsed = time() - start
        print('{}: {:.2f} hours'.format(name, elapsed / 3600.0))
        # print('{}: {:.2f} ms'.format(name, elapsed * 1000))
        self.results[name].append(elapsed)