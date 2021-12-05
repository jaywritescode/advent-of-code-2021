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
        # return chain(rows, cols, [self.squares[::6], self.squares[4:21:4]])

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
#%%




if __name__ == '__main__':
    test = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""
    g = GiantSquid(test.splitlines())
    print(g.solve())

#     with open('input-04.txt') as file:
#         puzzle = file.readlines()
#         g = GiantSquid(puzzle)
# # %%

# %%
