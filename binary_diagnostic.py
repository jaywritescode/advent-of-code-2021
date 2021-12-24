from more_itertools import partition


class BinaryDiagnostic:
    def __init__(self, puzzle):
        self.report = [int(line.strip(), base=2) for line in puzzle]
        self.num_bits = len(puzzle[0])

    def gamma_rate_nth_bit(self, n):
        # Note: (x & (1 << n)) == 0 iff the nth bit of x == 0.
        ones_count = len(list(filter(lambda i: (i & (1 << n)) > 0, self.report)))
        return 1 if ones_count > len(self.report) // 2 else 0

    def gamma_rate(self):
        rate = 0
        for i in range(self.num_bits):
            rate += (self.gamma_rate_nth_bit(i) << i)
        return rate

    def solve_part_one(self):
        gamma_rate = self.gamma_rate()
        epsilon_rate = gamma_rate ^ ((1 << self.num_bits) - 1)
        return gamma_rate * epsilon_rate


class BinaryDiagnostic_1:
    def __init__(self, report):
        self.report = [line.strip() for line in report]
        self.num_bits = len(self.report[0])
        self.partitions = dict()

    def partition_report_by_index(self, i):
        if i not in self.partitions:
            zeroes, ones = partition(lambda row: row[i] == '1', self.report)
            self.partitions[i] = (tuple(zeroes), tuple(ones))
        return self.partitions[i]

    def most_common_nth_bit(self, n):
        zeroes, ones = self.partition_report_by_index(n)
        return 0 if len(list(zeroes)) > len(list(ones)) else 1

    def least_common_nth_bit(self, n):
        zeroes, ones = self.partition_report_by_index(n)
        return 0 if len(list(zeroes)) < len(list(ones)) else 1

    def gamma_rate(self):
        result = ''.join(
            [str(self.most_common_nth_bit(i)) for i in range(self.num_bits)])
        return int(result, base=2)

    def epsilon_rate(self):
        result = ''.join(
            [str(self.least_common_nth_bit(i)) for i in range(self.num_bits)])
        return int(result, base=2)

    def power_consumption(self):
        return self.gamma_rate() * self.epsilon_rate()

    def oxygen_generator_rating(self):
        rows = self.report
        
        for idx in range(self.num_bits):
            if len(rows) == 1:
                return rows[0]

            zeroes, ones = [list(x) for x in partition(lambda row: row[idx] == '1', rows)]
            rows = ones if len(ones) >= len(zeroes) else zeroes
        return rows[0]

    def co2_scrubber_rating(self):
        rows = self.report

        for idx in range(self.num_bits):
            if len(rows) == 1:
                return rows[0]

            zeroes, ones = [list(x) for x in partition(lambda row: row[idx] == '1', rows)]
            rows = zeroes if len(zeroes) <= len(ones) else ones
        return rows[0]

    def life_support_rating(self):
        o2 = int(self.oxygen_generator_rating(), base=2)
        co2 = int(self.co2_scrubber_rating(), base=2)

        return o2 * co2


if __name__ == '__main__':
    with open('input-03.txt') as file:
        b = BinaryDiagnostic(file.readlines())
        print(b.life_support_rating())
