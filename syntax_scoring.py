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

def is_corrupted(line):
    stack = []
    for char in line:
        if char in ['(', '[', '{', '<']:
            stack.append(char)
        else:
            if stack.pop() != pairs[char]:
                return values[char]

    return 0


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


if __name__ == '__main__':
    print(sum(is_corrupted(i.strip()) for i in test))
