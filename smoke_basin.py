from collections import namedtuple

Coordinate = namedtuple('Coordinate', ['row', 'column'])

class SmokeBasin:
    def __init__(self, puzzle_lines):
        self.height_map = dict()
        for row, heights in enumerate(puzzle_lines):
            for column, value in enumerate(heights):
                self.height_map[Coordinate(row, column)] = int(value)

    def neighbors(self, coordinate):
        row, column = coordinate

        return list(filter(lambda c: c in self.height_map, [
            Coordinate(row - 1, column),
            Coordinate(row + 1, column),
            Coordinate(row, column - 1),
            Coordinate(row, column + 1)]))

    def low_points(self):
        low_points = []
        for coordinate, height in self.height_map.items():
            neighbors = self.neighbors(coordinate)
            if all(height < self.height_map[np] for np in neighbors):
                low_points.append(coordinate)
        return low_points

    def solve(self):
        f = self.low_points()
        return sum(self.height_map[lp] + 1 for lp in self.low_points())



test = """2199943210
3987894921
9856789892
8767896789
9899965678""".splitlines()


if __name__ == '__main__':
    puz = SmokeBasin(test)
    print(puz.solve())