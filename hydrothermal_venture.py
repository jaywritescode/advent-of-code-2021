#%%
from collections import namedtuple
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


class Range:
    """
    Represents a subset of the discrete number line.
    """
    def __init__(self, p1, p2):
        self.min = min(p1, p2)
        self.max = max(p1, p2)

    def amount(self):
        """
        The amount of the number line covered by this range. Since we're looking
        at discrete chunks of the number line, we want `amount([x,x]) == 1`.
        """
        return self.max - self.min + 1

    def intersect(self, other):
        # disjoint
        if self.max < other.min or other.max < self.min:
            return 0
        # one is a subset of the other
        if self.min <= other.min and self.max >= other.max:
            return other.amount()
        if other.min <= self.min and other.max >= self.max:
            return self.amount()
        return min(self.max, other.max) - max(self.min, other.min) + 1

    def __repr__(self):
        return f"[{self.min},{self.max}]"


def overlap(line1, line2):
    if line1.is_horizontal() and line2.is_horizontal():
        return lines_overlap_horizontal(line1, line2)
    if line1.is_vertical() and line2.is_vertical():
        return lines_overlap_vertical(line1, line2)
    return lines_overlap_orthogonal(line1, line2)

def lines_overlap_horizontal(line1, line2):
    leftmost = line1 if line1.get_left_point() < line2.get_left_point() else line2
    other = line1 if leftmost is line2 else line1

    if leftmost.get_right_point() < other.get_left_point():
        return 0
    if leftmost.get_right_point() > other.get_right_point():
        return other.length()
    return leftmost.get_right_point() - other.get_left_point() + 1

def lines_overlap_vertical(line1, line2):
    bottommost = line1 if line1.get_bottom_point() < line2.get_bottom_point() else line2
    other = line1 if bottommost is line2 else line1

    if bottommost.get_top_point() < other.get_bottom_point():
        return 0
    if bottommost.get_top_point() > other.get_top_point():
        return other.length()
    return bottommost.get_top_point() - other.get_bottom_point() + 1

def lines_overlap_orthogonal(line1, line2):
    horizontal = line1 if line1.is_horizontal() else line2
    vertical = line1 if line1.is_vertical() else line2

    left, right = sorted(p.x for p in horizontal.points())
    bottom, top = sorted(p.y for p in vertical.points())

    return 1 if left <= vertical.p1.x <= right and bottom <= horizontal.p1.y <= top else 0

#%%
class HydrothermalVenture:
    def __init__(self, lines):
        self.lines = list(filter(lambda x: x.is_horizontal() or x.is_vertical(), lines))

    def solve(self):
        total = 0

        horizontal_lines = list(sorted(filter(lambda L: L.is_horizontal(), self.lines), key=lambda L: L.p1.y))
        vertical_lines = list(sorted(filter(lambda L: L.is_vertical(), self.lines), key=lambda L: L.p1.x))

        # overlapping horizontal lines must have the same y-coordinate
        for _, g in groupby(horizontal_lines, lambda line: line.p1.y):
            group = list(g)
            if len(group) == 1:
                continue
            for (line1, line2) in combinations(group, 2):
                r1 = Range(line1.p1.x, line1.p2.x)
                r2 = Range(line2.p1.x, line2.p2.x)
                total += Range.intersect(r1, r2)

        #overlapping vertical lines must have the same x-coordinate
        for _, g in groupby(vertical_lines, lambda line: line.p1.x):
            group = list(g)
            if len(group) == 1:
                continue
            for (line1, line2) in combinations(group, 2):
                r1 = Range(line1.p1.y, line1.p2.y)
                r2 = Range(line2.p1.y, line2.p2.y)
                total += Range.intersect(r1, r2)

        # perpendicular lines
        for (horiz, vert) in product(horizontal_lines, vertical_lines):
            if horiz.left().x <= vert.p1.x <= horiz.right().x and vert.bottom().y <= horiz.p1.y <= vert.top().y:
                total += 1

        return total


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
    # print(h.solve())

    with open('input-05.txt') as file:
        lines = [Line.parse(line) for line in file.readlines()]
        h = HydrothermalVenture(lines)
        print(h.solve())


# 7398 is too high