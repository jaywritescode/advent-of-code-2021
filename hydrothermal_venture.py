from collections import namedtuple
import re

Point = namedtuple('Point', ['x', 'y'])

regex = re.compile("(\d+),(\d+) -> (\d+),(\d+)")

class Line:
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2

    def is_horizontal(self):
        return self.p1.x == self.p2.x

    def is_vertical(self):
        return self.p1.y == self.p2.y

    def __repr__(self):
        return f"({self.p1.x},{self.p1.y}) -> ({self.p2.x},{self.p2.y})"

    @staticmethod
    def parse(line):
        match = regex.match(line)
        (x1, y1, x2, y2) = match.groups()
        return Line(Point(x1, y1), Point(x2, y2))


class HydrothermalVenture:
    def __init__(self, lines):
        self.lines = list(filter(lambda x: x.is_horizontal() or x.is_vertical(), lines))


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
    lines = [Line.parse(n) for n in test]
    h = HydrothermalVenture(lines)
    print(h.lines)