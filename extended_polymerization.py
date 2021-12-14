from collections import Counter
from itertools import dropwhile
from more_itertools import first
from more_itertools.more import collapse
from more_itertools.recipes import pairwise

class ExtendedPolymerization:
    def __init__(self, template, rules):
        self.template = list(iter(template))
        self.rules = dict(rules)
        self.step = 0

    def __iter__(self):
        return self

    def __next__(self):
        n = [self.template[0]]
        for i in pairwise(self.template):
            n.append(self.rules[i])
            n.append(i[1])

        self.template = n
        self.step += 1

    def solve(self):
        while self.step < 40:
            print(f"step: {self.step}")
            next(self)
        
        print(len(self.template))
        c = Counter(self.template).most_common()
        return c[0][1] - c[-1][1]
        

        





test = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C""".splitlines()


if __name__ == '__main__':
    with open('input-14.txt') as file:
        i = (line.strip() for line in file)
        template = first(i)

        rules = dict()
        for line in dropwhile(lambda x: not x, i):
            pair, middle = line.split(' -> ')
            rules[tuple(iter(pair))] = middle

        print(ExtendedPolymerization(template, rules).solve())