#%%
from collections import namedtuple
from itertools import dropwhile, takewhile

Point = namedtuple('Point', ['x', 'y'])

Fold = namedtuple('Fold', ['axis', 'value'])

def reflect_over_horizontal_fold(point, y):
    if point.y < y:
        return point

    return Point(point.x, 2 * y - point.y)

def reflect_over_vertical_fold(point, x):
    if point.x > x:
        return point

    return Point(x - point.x - 1, point.y)

# %%
class TransparentOrigami:
    def __init__(self, points, folds):
        self.points = points
        
        self.folds = list()
        for fold in folds:
            parts = fold.split('=')
            axis = parts[0][-1]
            value = int(parts[1])
            self.folds.append(Fold(axis, value))

        self.x_max = max(p.x for p in self.points)
        self.y_max = max(p.y for p in self.points)

    def do_next_fold(self):
        fold = self.folds[0]
        
        if fold.axis == 'y':
            self.points = {reflect_over_horizontal_fold(point, fold.value) for point in self.points}
            self.y_max = (self.y_max - 1) // 2
        else:
            self.points = {reflect_over_vertical_fold(point, fold.value) for point in self.points}
            self.x_max = (self.x_max - 1) // 2

        self.folds = self.folds[1:]

    def solve(self):
        self.do_next_fold()
        return len(self.points)

    def __str__(self):
        lines = []
        for y in range(self.y_max + 1):
            line = []
            for x in range(self.x_max + 1):
                line.append('#' if Point(x,y) in self.points else '.')
            lines.append(''.join(line))
        return '\n'.join(lines)


test = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""

#%%
if __name__ == '__main__':
    with open('input-13.txt') as file:
        puzzle = iter(line.strip() for line in file.readlines())
        points = {Point._make(int(x) for x in v.split(',')) for v in takewhile(lambda x: x, puzzle)}
        folds = [f.strip() for f in dropwhile(lambda x: not x, puzzle)]

        print(TransparentOrigami(points, folds).solve())

# 1125 is too high