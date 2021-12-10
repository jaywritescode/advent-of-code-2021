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
    for char in line:
        if char in ['(', '[', '{', '<']:
            stack.append(char)
        else:
            if stack.pop() != pairs[char]:
                return values[char]

    return 0

def complete_line(line):
    stack = []
    for char in line:
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
    f = [(line, complete_line(line)) for line in test if is_corrupted(line) != 0]
    print(f)


    # with open('input-10.txt') as file:
    #     print(sum(is_corrupted(i.strip()) for i in file.readlines()))
