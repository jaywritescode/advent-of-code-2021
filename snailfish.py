from collections import deque, namedtuple
from more_itertools import flatten

class SnailfishNumber:
    def __init__(self, left, right, depth=0):
        self.left = left
        self.right = right
        self.depth = depth

    def depth_first_search(self):
        stack = deque([self])
        while stack:
            current = stack.popleft()
            if isinstance(current.right, SnailfishNumber):
                stack.appendleft(current.right)
            
            if isinstance(current.left, SnailfishNumber):
                stack.appendleft(current.left)
            else:
                yield current

        

    def __repr__(self):
        return f"[{self.left},{self.right}]"

def as_child(n):
    return SnailfishNumber(
        left=as_child(n.left) if isinstance(n.left, SnailfishNumber) else n.left,
        right=as_child(n.right) if isinstance(n.right, SnailfishNumber) else n.right,
        depth=n.depth + 1
    )

def add(m, n):
    return SnailfishNumber(as_child(m), as_child(n))

def find_explode(n):
    for number in n.nodes:
        if number.depth == 4:
            return number

def find_explode1(n, previous=None):
    if isinstance(n, int):
        return None

    if isinstance(n, SnailfishNumber) and n.depth == 4:
        return (n, previous)

    return find_explode(n.left, n) or find_explode(n.right, n)


if __name__ == '__main__':
    a = SnailfishNumber(1, 2, depth=0)
    b = SnailfishNumber(SnailfishNumber(3, 4, depth=1), 5)
    c = add(a, b)
    print(c)

    print(find_explode(c)) # None

    a = SnailfishNumber(SnailfishNumber(SnailfishNumber(SnailfishNumber(SnailfishNumber(9, 8, depth=4), 1, depth=3), 2, depth=2), 3, depth=1), 4)
    b = find_explode(a)
    print(b)

    a = SnailfishNumber(7, SnailfishNumber(6, SnailfishNumber(5, SnailfishNumber(4, SnailfishNumber(3, 2, depth=4), depth=3), depth=2), depth=1))
    b = find_explode(a)
    print(b)

    a = SnailfishNumber(SnailfishNumber(6, SnailfishNumber(5, SnailfishNumber(4, SnailfishNumber(3, 2, depth=4), depth=3), depth=2), depth=1), 1)
    b = find_explode(a)
    print(b)
