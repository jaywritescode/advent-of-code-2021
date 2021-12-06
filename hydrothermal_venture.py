#%%
from collections import namedtuple
from itertools import groupby
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
            return self.get_top_point() - self.get_bottom_point()
        else:
            return self.get_right_point() - self.get_left_point()

    def get_left_point(self):
        if self.is_vertical():
            return None
        return list(sorted(self.points(), key=lambda p: p.x))[0]

    def get_right_point(self):
        if self.is_vertical():
            return None
        return list(sorted(self.points(), key=lambda p: p.x))[-1]

    def get_top_point(self):
        if self.is_horizontal():
            return None
        return list(sorted(self.points(), key=lambda p: p.y))[-1]

    def get_bottom_point(self):
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



#%%
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

        horizontal_lines = sorted(filter(lambda L: L.is_horizontal(), self.lines), key=lambda L: L.p1.y)
        vertical_lines = sorted(filter(lambda L: L.is_vertical(), self.lines), key=lambda L: L.p1.x)

        # overlapping horizontal lines
        for _, g in groupby(horizontal_lines, lambda line: line.p1.y):
            group = list(g)
            if len(group) == 1:
                continue
            if len(group) > 2:
                raise ValueError
            total += overlap(*group)


        print("hello")


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
    h = HydrothermalVenture(Line.parse(line) for line in test).solve()


    # lines = [Line.parse(n) for n in test]
    # h = HydrothermalVenture(lines)
    # print(h.lines)
# %%

# let's see if any two horizontal lines share the same y-coordinates
# and if any vertical lines share the same x-coordinates