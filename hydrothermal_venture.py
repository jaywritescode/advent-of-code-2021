#%%
from collections import Counter, namedtuple
from itertools import groupby, combinations, product
import re

Point = namedtuple('Point', ['x', 'y'])

regex = re.compile("(\d+),(\d+) -> (\d+),(\d+)")
#%%
class Line:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def points(self):
        return [self.p1, self.p2]

    def is_vertical(self):
        return self.p1.x == self.p2.x

    def is_horizontal(self):
        return self.p1.y == self.p2.y

    def length(self):
        if self.is_vertical():
            return self.top() - self.bottom()
        else:
            return self.right() - self.left()

    def left(self):
        if self.is_vertical():
            return None
        return list(sorted(self.points(), key=lambda p: p.x))[0]

    def right(self):
        if self.is_vertical():
            return None
        return list(sorted(self.points(), key=lambda p: p.x))[-1]

    def top(self):
        if self.is_horizontal():
            return None
        return list(sorted(self.points(), key=lambda p: p.y))[-1]

    def bottom(self):
        if self.is_horizontal():
            return None
        return list(sorted(self.points(), key=lambda p: p.y))[0]

    def __repr__(self):
        return f"({self.p1.x},{self.p1.y}) -> ({self.p2.x},{self.p2.y})"        

    @staticmethod
    def parse(line):
        match = regex.match(line)
        (x1, y1, x2, y2) = match.groups()
        return Line(Point(int(x1), int(y1)), Point(int(x2), int(y2)))



#%%
class HydrothermalVenture:
    from collections import Counter

    def __init__(self, lines):
        self.lines = list(filter(lambda x: x.is_horizontal() or x.is_vertical(), lines))

    def solve_b(self):
        counter = Counter()
        for line in self.lines:
            if line.is_horizontal():
                left = min(line.p1.x, line.p2.x)
                right = max(line.p1.x, line.p2.x)
                for x in range(left, right+1):
                    counter[(x, line.p1.y)] += 1
            else:
                bottom = min(line.p1.y, line.p2.y)
                top = max(line.p1.y, line.p2.y)
                for y in range(bottom, top+1):
                    counter[(line.p1.x, y)] += 1

        return len(list(filter(lambda x: x >= 2, counter.values())))

    


test = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2""".splitlines()


if __name__ == '__main__':
    # h = HydrothermalVenture(Line.parse(line) for line in test)
    # print(h.solve_b())

    with open('input-05.txt') as file:
        lines = [Line.parse(line) for line in file.readlines()]
        h = HydrothermalVenture(lines)
        print(h.solve_b())


# 7398 is too high