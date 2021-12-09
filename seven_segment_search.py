from more_itertools import only

def simple_digits_in_line(line):
    output_value = line.split('|')[1].split()
    return len(list(filter(lambda v: len(v) in [2,3,4,7], output_value)))

def count_simple_digits(puzzle):
    return sum(simple_digits_in_line(line) for line in puzzle)


class SevenSegmentSearch:
    def __init__(self, line):
        p = line.split('|')
        self.signal_patterns = p[0].split()
        self.output = p[1].split()

        self.known_numbers = dict()     # map frozenset to string: i.e. frozenset({'b', 'e'}) => '1'
        self.known_wires = dict()       # map string to frozenset: '1' => frozenset({'b', 'e'})

    def find_digit_one(self):
        n = only(filter(lambda x: len(x) == 2, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['1'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['1']] = '1'

    def find_digit_seven(self):
        n = only(filter(lambda x: len(x) == 3, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['7'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['7']] = '7'

    def find_digit_four(self):
        n = only(filter(lambda x: len(x) == 4, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['4'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['4']] = '4'

    def find_digit_eight(self):
        n = only(filter(lambda x: len(x) == 7, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['8'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['8']] = '8'

    def find_digit_six(self):
        n = only(filter(lambda x: len(x) == 6 and len(set(iter(x)).intersection(self.known_wires['1'])) == 1, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['6'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['6']] = '6'

    def find_digit_five(self):
        n = only(filter(lambda x: len(x) == 5 and set(iter(x)).issubset(self.known_wires['6']), self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['5'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['5']] = '5'

    def find_digit_three(self):
        n = only(filter(lambda x: len(x) == 5 and len(set(iter(x)).intersection(self.known_wires['1'])) == 2, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['3'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['3']] = '3'

    def find_digit_two(self):
        n = only(filter(lambda x: len(x) == 5, self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['2'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['2']] = '2'

    def find_digit_nine(self):
        n = only(filter(lambda x: set(iter(x)).issuperset(self.known_wires['5']), self.signal_patterns))
        self.signal_patterns.remove(n)
        self.known_wires['9'] = frozenset(iter(n))
        self.known_numbers[self.known_wires['9']] = '9'

    def solve(self):
        self.find_digit_one()
        self.find_digit_seven()
        self.find_digit_four()
        self.find_digit_eight()

        self.find_digit_six()
        self.find_digit_five()
        self.find_digit_three()
        self.find_digit_two()
        self.find_digit_nine()

        zero = only(self.signal_patterns)
        self.known_wires['0'] = frozenset(iter(zero))
        self.known_numbers[self.known_wires['0']] = '0'

        return int(''.join(self.known_numbers[frozenset(iter(val))] for val in self.output))


test = """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".splitlines()


if __name__ == '__main__':
    with open('input-08.txt') as file:
        puzzle = [line.strip() for line in file.readlines()]
        print(sum(SevenSegmentSearch(line).solve() for line in puzzle))