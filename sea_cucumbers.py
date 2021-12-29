from collections import namedtuple

Coordinate = namedtuple('Coordinate', ['row', 'col'])

class SeaCucumbers:
    def __init__(self, rows, cols, east, south):
        self.rows = rows
        self.cols = cols
        self.east = east
        self.south = south
        self.steps = 0

    def is_occupied(self, coordinate):
        return coordinate in self.east or coordinate in self.south

    def next(self):
        move_east = []
        for i in self.east:
            p = Coordinate(i.row, (i.col + 1) % self.cols)
            if not self.is_occupied(p):
                move_east.append((i, p))

        self.east -= {el[0] for el in move_east}
        self.east |= {el[1] for el in move_east}

        move_south = []
        for i in self.south:
            p = Coordinate((i.row + 1) % self.rows, i.col)
            if not self.is_occupied(p):
                move_south.append((i, p))

        self.south -= {el[0] for el in move_south}
        self.south |= {el[1] for el in move_south}

        return not move_east and not move_south

    def solve(self):
        while True:
            done = self.next()
            self.steps += 1
            if done:
                return self.steps

    def __repr__(self):
        k = [f"After {self.steps} steps:\n"]
        for row in range(self.rows):
            for col in range(self.cols):
                if Coordinate(row, col) in self.east:
                    k.append('>')
                elif Coordinate(row, col) in self.south:
                    k.append('v')
                else:
                    k.append('.')
            k.append('\n')
        return ''.join(k)


def parse_puzzle(lines):
    east = set()
    south = set()

    for (row, line) in enumerate(lines):
        for (col, char) in enumerate(line):
            if char == '>':
                east.add(Coordinate(row, col))
            elif char == 'v':
                south.add(Coordinate(row, col))

    return SeaCucumbers(row + 1, col + 1, east, south)


if __name__ == '__main__':
    with open('input-25.txt') as file:
        puzzle_input = [line.strip() for line in file.readlines()]
        print(parse_puzzle(puzzle_input).solve())