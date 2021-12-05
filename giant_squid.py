#%%
from itertools import chain
from more_itertools import split_at


class BingoSquare:
    _instances = {}

    def __new__(cls, number):
        self = cls._instances.get(number)
        if self is None:
            self = cls._instances[number] = object.__new__(BingoSquare)
            self.number = number
            self.is_marked = False
        return self

    @classmethod
    def get(cls, number):
        return cls._instances.get(number)

    def mark(self):
        self.is_marked = True

    def __repr__(self):
        return 'BingoSquare: {!r}, is_marked: {!r}'.format(self.number, self.is_marked)


class BingoBoard:
    def __init__(self, numbers):
        self.squares = [BingoSquare(i) for i in numbers]

    def lines(self):
        rows = iter(self.squares[5 * i:5 * (i + 1)] for i in range(5))
        cols = iter(self.squares[i::5] for i in range(5))
        return chain(rows, cols)

    @staticmethod
    def line_is_complete(line):
        return all(sq.is_marked for sq in line)

    @staticmethod
    def line_contains_number(line, number):
        return any(sq.number == number for sq in line)

    def get_marked(self):
        return [sq for sq in self.squares if sq.is_marked]

    def get_unmarked_sum(self):
        return sum(sq.number for sq in self.squares if not sq.is_marked)

    def is_complete(self, number):
        return any(BingoBoard.line_is_complete(line) for line in self.lines()) # if BingoBoard.line_contains_number(line, number))

    def __repr__(self):
        return str([sq.number for sq in self.squares])


class GiantSquid:
    def __init__(self, puzzle):
        self.numbers = [int(x) for x in puzzle[0].split(',')]
        
        self.boards = []
        for board in split_at(puzzle[2:], lambda x: not x):
            numbers = [int(x) for x in ' '.join(board).split()]
            self.boards.append(BingoBoard(numbers))

    def solve(self):
        for number in self.numbers:
            BingoSquare.get(number).mark()
            for board in self.boards:
                if board.is_complete(number):
                    return number * board.get_unmarked_sum()


if __name__ == '__main__':
    with open('input-04.txt') as file:
        puzzle = [line.strip() for line in file.readlines()]
        g = GiantSquid(puzzle)
        print(g.solve())

# 25497 too high