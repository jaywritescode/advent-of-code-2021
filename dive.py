from collections import namedtuple

Command = namedtuple('Command', ['direction', 'amount'])

#%%
class Dive:
    def __init__(self):
        self.depth = 0
        self.horizontal = 0
        self.aim = 0

    def run_command(self, command):
        if command.direction == 'forward':
            self.horizontal += command.amount
            self.depth += self.aim * command.amount
        else:
            self.aim += command.amount if command.direction == 'down' else -(command.amount)

    # part one solution
    def _run_command(self, command):
        if command.direction == 'forward':
            self.horizontal += command.amount
        else:
            self.depth += command.amount if command.direction == 'down' else -(command.amount)

    def solve(self, course):
        for cmd in [Dive.parse_command(line) for line in course]:
            self.run_command(cmd)
        return self.depth * self.horizontal

    @staticmethod
    def parse_command(line):
        direction, amount = line.split()
        return Command(direction, int(amount))


if __name__ == '__main__':
    with open('input-02.txt') as file:
        commands = file.readlines()

        d = Dive().solve(commands)
        print(d)

