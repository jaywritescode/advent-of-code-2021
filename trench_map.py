from collections import namedtuple
from itertools import product

LIGHT = '#'

Coordinate = namedtuple('Coordinates', ['row', 'col'])
BaseImage = namedtuple('Image', ['light_pixels', 'size', 'background', 'times_enhanced'], defaults=[0])

class Image(BaseImage):
    def get_value(self, coordinate):
        min = -self.times_enhanced
        max = self.size + self.times_enhanced

        if min <= coordinate.row < max and min <= coordinate.col < max:
            return 1 if coordinate in self.light_pixels else 0

        return self.background

    def enhance_coordinate_index(self, coordinate):
        row, col = coordinate
        value = 0

        for (dr, dc) in product([-1, 0, 1], repeat=2):
            value <<= 1
            value |= self.get_value(Coordinate(row + dr, col + dc))
        
        return value

    def __repr__(self):
        lines = []
        for row in range(-self.times_enhanced - 1, self.size):
            line = []
            for col in range(-self.times_enhanced - 1, self.size):
                coord = Coordinate(row, col)
                line.append('#' if coord in self.light_pixels else '.')
            lines.append(''.join(line))
        return '\n'.join(lines)


class TrenchMap:
    def __init__(self, algorithm):
        self.algorithm = algorithm

    def enhance_once(self, image):
        new_light_pixels = set()
        for row in range(image.times_enhanced - 1, image.size + 2):
            for col in range(image.times_enhanced - 1, image.size + 2):
                coord = Coordinate(row, col)
                
                idx = image.enhance_coordinate_index(coord)
                if self.algorithm[idx]:
                    new_light_pixels.add(coord)

        return Image(new_light_pixels, image.size + 2, 
            background=self.algorithm[0],
            times_enhanced=image.times_enhanced + 1)


def parse_algorithm(line):
    return [x == LIGHT for x in line]

def parse_image(lines):
    size = len(lines)

    light_pixels = set()
    for (row, line) in enumerate(lines):
        for (col, char) in enumerate(line):
            if char == LIGHT:
                light_pixels.add(Coordinate(row, col))

    return Image(light_pixels, size, background=0)


if __name__ == '__main__':
    with open('sample-data/20.txt') as file:
        puzzle_input = [line.strip() for line in file.readlines()]
        algorithm = parse_algorithm(puzzle_input[0])
        image = parse_image(puzzle_input[2:])

        trench_map = TrenchMap(algorithm)
        enhanced = trench_map.enhance_once(image)

        print(enhanced)

