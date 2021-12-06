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

        self._next = p1
        self._x_iter_dir = 0 if self.is_vertical() else (1 if self.p2.x > self.p1.x else -1)
        self._y_iter_dir = 0 if self.is_horizontal() else (1 if self.p2.y > self.p1.y else -1)

    def points(self):
        return [self.p1, self.p2]

    def is_vertical(self):
        return self.p1.x == self.p2.x

    def is_horizontal(self):
        return self.p1.y == self.p2.y

    def __iter__(self):
        return self

    def __next__(self):
        if self._next is None:
            raise StopIteration

        p = self._next
        if p == self.p2:
            self._next = None
        else:
            self._next = Point(self._next.x + self._x_iter_dir, self._next.y + self._y_iter_dir)

        return p

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
        self.lines = lines

    def solve(self):
        counter = Counter()
        for line in self.lines:
            for point in iter(line):
                counter[point] += 1

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
    h = HydrothermalVenture(Line.parse(line) for line in test)
    print(h.solve())

    # with open('input-05.txt') as file:
    #     lines = [Line.parse(line) for line in file.readlines()]
    #     h = HydrothermalVenture(lines)
    #     print(h.solve_b())


# 7398 is too high