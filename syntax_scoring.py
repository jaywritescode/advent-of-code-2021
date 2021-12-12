pairs = { 
    ')': '(', 
    ']': '[',
    '}': '{',
    '>': '<',
}

values = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137,
}

points = {
    '(': 1,
    '[': 2,
    '{': 3,
    '<': 4,
}

def is_corrupted(line):
    stack = []
    for char in line.strip():
        if char in ['(', '[', '{', '<']:
            stack.append(char)
        else:
            if stack.pop() != pairs[char]:
                return values[char]

    return 0

def complete_line(line):
    stack = []
    for char in line.strip():
        if char in ['(', '[', '{', '<']:
            stack.append(char)
        else:
            stack.pop()

    total = 0
    while stack:
        total *= 5
        total += points[stack.pop()]

    return total


test = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]""".splitlines()
test = [x.strip() for x in test]


if __name__ == '__main__':
    from statistics import median

    with open('input-10.txt') as file:
        m = median([complete_line(line) for line in file if is_corrupted(line) == 0])
        print(m)
