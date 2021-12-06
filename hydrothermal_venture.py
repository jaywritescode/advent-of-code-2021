from collections import Counter, namedtuple
import re

Point = namedtuple('Point', ['x', 'y'])

regex = re.compile("(\d+),(\d+) -> (\d+),(\d+)")


class Line:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

        self._next = p1
        self._x_iter_dir = 0 if self.is_vertical() else (1 if self.p2.x > self.p1.x else -1)
        self._y_iter_dir = 0 if self.is_horizontal() else (1 if self.p2.y > self.p1.y else -1)

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

    def __repr__(self):
        return f"({self.p1.x},{self.p1.y}) -> ({self.p2.x},{self.p2.y})"        

    @staticmethod
    def parse(line):
        match = regex.match(line)
        (x1, y1, x2, y2) = match.groups()
        return Line(Point(int(x1), int(y1)), Point(int(x2), int(y2)))


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


if __name__ == '__main__':
    with open('input-05.txt') as file:
        lines = [Line.parse(line) for line in file.readlines()]
        h = HydrothermalVenture(lines)
        print(h.solve())