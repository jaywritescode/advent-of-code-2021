from more_itertools import triplewise

def count_increases(depths):
    return sum(1 for x in zip(depths, depths[1:]) if x[0] < x[1])

def count_increases_sliding(depths):
    counter = 0
    for (m, n) in zip(triplewise(depths), triplewise(depths[1:])):
        if sum(m) < sum(n):
            counter += 1
    return counter

if __name__ == '__main__':
    with open("input-01.txt") as file:
        args = [int(x) for x in file.readlines()]
        print(count_increases_sliding(args))
