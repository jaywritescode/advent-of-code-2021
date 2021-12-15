from collections import Counter
from itertools import dropwhile
from more_itertools import first
from more_itertools.more import collapse
from more_itertools.recipes import pairwise

class ExtendedPolymerization:
    def __init__(self, template, rules):
        self.start = template[0]
        self.end = template[-1]
        self.pairs = Counter(''.join(k) for k in pairwise(template))
        self.rules = dict(rules)
        self.step = 0

    def __iter__(self):
        return self

    def __next__(self):
        updated = Counter()
        for pair, count in self.pairs.items():
            transform = self.rules[pair]
            updated[transform[0]] += count
            updated[transform[1]] += count
        self.pairs = updated
        self.step += 1

    def count_elements(self):
        c = Counter()
        for pair, count in self.pairs.items():
            c[pair[0]] += count
            c[pair[1]] += count

        n = Counter({letter: count // 2 for letter, count in c.items()})
        n[self.start] += 1
        n[self.end] += 1
        return n

    def solve(self):
        while self.step < 40:
            next(self)

        most_common = self.count_elements().most_common()
        return most_common[0][1] - most_common[-1][1]


if __name__ == '__main__':
    with open('input-14.txt') as file:
        i = (line.strip() for line in file)
        template = first(i)

        rules = dict()
        for line in dropwhile(lambda x: not x, i):
            pair, middle = line.split(' -> ')
            rules[pair] = [pair[0] + middle, middle + pair[1]]

        e = ExtendedPolymerization(template, rules)
        print(e.solve())
