from collections import namedtuple

Coordinate = namedtuple('Coordinate', ['row', 'column'])


class Octopus:
    def __init__(self, coordinates, energy):
        self.coordinates = coordinates
        self.energy = energy

    def __repr__(self):
        return f"{self.coordinates}: energy = {self.energy}"


class DumboOctopus:
    transforms = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    
    def __init__(self, puzzle):
        self.octopi = dict()
        for (row, line) in enumerate(puzzle):
            for (column, value) in enumerate(line.strip()):
                coordinate = Coordinate(row, column)
                octopus = Octopus(coordinate, int(value))
                self.octopi[coordinate] = octopus

        self.step = 0

    def neighbors(self, coordinate):
        neighbors = set()   # of coordinates

        row, column = coordinate
        for (delta_row, delta_column) in DumboOctopus.transforms:
            c = Coordinate(row + delta_row, column + delta_column)
            if c in self.octopi:
                neighbors.add(c)

        return [self.octopi[n] for n in neighbors]

    def next_step(self):
        for octopus in self.octopi.values():
            octopus.energy += 1

        flashed = set()
        queue = [octopus for octopus in self.octopi.values() if octopus.energy > 9]
        while queue:
            for octopus in queue:
                flashed.add(octopus.coordinates)
                for neighbor in self.neighbors(octopus.coordinates):
                    neighbor.energy += 1

            queue = [octopus for octopus in self.octopi.values() if octopus.energy > 9 and octopus.coordinates not in flashed]

        for coordinate in flashed:
            self.octopi[coordinate].energy = 0

        self.step += 1
        return len(flashed)

    def count_flashes(self):
        return sum(self.next_step() for x in range(100))

    def solve(self):
        while True:
            if self.next_step() == 100:
                return self.step


test = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526""".splitlines()


if __name__ == '__main__':
    with open('input-11.txt') as file:
        puzzle = [line.strip() for line in file.readlines()]
        print(DumboOctopus(puzzle).solve())