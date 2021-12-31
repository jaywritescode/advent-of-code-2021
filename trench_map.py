from collections import namedtuple
from itertools import product

LIGHT = '#'
DARK = '.'

Coordinate = namedtuple('Coordinates', ['row', 'col'])
BaseImage = namedtuple('Image', ['light_pixels', 'size', 'background'])

class Image(BaseImage):
    def get_value(self, coordinate):
        if self.is_in_image(coordinate):
            return int(coordinate in self.light_pixels)

        return self.background

    def enhance_coordinate_index(self, coordinate):
        row, col = coordinate
        value = 0

        for (dr, dc) in product([-1, 0, 1], repeat=2):
            value <<= 1
            value |= self.get_value(Coordinate(row + dr, col + dc))
        
        return value

    def is_in_image(self, coordinate):
        return -self.times_enhanced <= coordinate.row < self.size + self.times_enhanced and -self.times_enhanced <= coordinate.col < self.size + self.times_enhanced

    def background_color(self):
        return LIGHT if self.background else DARK

    def __repr__(self):
        lines = []
        for row in range(-1, self.size + 1):
            line = []
            for col in range(-1, self.size + 1):
                coord = Coordinate(row, col)

                if not self.is_in_image(coord):
                    line.append(self.background_color())
                elif coord in self.light_pixels:
                    line.append(LIGHT)
                else:
                    line.append(DARK)
            lines.append(''.join(line))
        return '\n'.join(lines)


class TrenchMap:
    def __init__(self, algorithm):
        self.algorithm = algorithm

    def enhance_once(self, image):
        new_light_pixels = set()
        for row in range(-1, image.size + 2):
            for col in range(-1, image.size + 2):
                coord = Coordinate(row, col)

                idx = image.enhance_coordinate_index(coord)
                if self.algorithm[idx]:
                    new_light_pixels.add(Coordinate(coord.row + 1, coord.col + 1))

        new_background = self.algorithm[-1] if image.background else self.algorithm[0]
        return Image(new_light_pixels, image.size + 2, background=new_background)


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
    with open('input-20.txt') as file:
        puzzle_input = [line.strip() for line in file.readlines()]
        algorithm = parse_algorithm(puzzle_input[0])
        image = parse_image(puzzle_input[2:])

        trench_map = TrenchMap(algorithm)
        enhanced = trench_map.enhance_once(image)
        enhanced = trench_map.enhance_once(enhanced)

        print(len(enhanced.light_pixels))