from more_itertools import partition
from more_itertools.more import one

def down_to_zero(n):
    """
    Returns an iterator over positive integers that yields n - 1, n - 2, ..., 0
    """
    return range(n - 1, -1, -1)


class BinaryDiagnostic:
    def __init__(self, puzzle):
        self.report = [int(line.strip(), base=2) for line in puzzle]
        self.num_bits = len(puzzle[0])

    def gamma_rate_nth_bit(self, n):
        ones_count = len(list(filter(lambda i: (i & (1 << n)) > 0, self.report)))
        return 1 if ones_count > len(self.report) // 2 else 0

    def gamma_rate(self):
        rate = 0
        for i in range(self.num_bits):
            rate += (self.gamma_rate_nth_bit(i) << i)
        return rate

    # The main method for part one.
    def power_consumption(self):
        gamma_rate = self.gamma_rate()
        # Note that the epsilon rate is the same as the gamma rate with all its bits flipped.
        epsilon_rate = gamma_rate ^ ((1 << self.num_bits) - 1)
        return gamma_rate * epsilon_rate

    def oxygen_generator_rating(self):
        rows = self.report
        for idx in down_to_zero(self.num_bits):
            mask = 1 << idx
            zeroes, ones = [list(x) for x in partition(lambda val: (val & mask) > 0, rows)]
            rows = ones if len(ones) >= len(zeroes) else zeroes

            if len(rows) == 1:
                return one(rows)

    def co2_scrubber_rating(self):
        rows = self.report
        for idx in down_to_zero(self.num_bits):
            mask = 1 << idx
            zeroes, ones = [list(x) for x in partition(lambda val: (val & mask) > 0, rows)]
            rows = zeroes if len(zeroes) <= len(ones) else ones

            if len(rows) == 1:
                return one(rows)

    # The main method for part two.
    def life_support_rating(self):
        return self.oxygen_generator_rating() * self.co2_scrubber_rating()


if __name__ == '__main__':
    with open('input-03.txt') as file:
        b = BinaryDiagnostic([line.strip() for line in file.readlines()])
        print(b.power_consumption())
        print(b.life_support_rating())
