def lanternfish_naive(start):
    state = start[:]
    for _ in range(80):
        spawned = 0
        for idx, timer in enumerate(state):
            if timer == 0:
                state[idx] = 6
                spawned += 1
            else:
                state[idx] = timer - 1
        state.extend([8] * spawned)
    return len(state)

from collections import Counter

def lanternfish(start):
    counter = Counter(start)
    for _ in range(256):
        updated = dict()
        
        updated[8] = counter[0]
        updated[7] = counter[8]
        updated[6] = counter[7] + counter[0]
        for i in range(5, -1, -1):
            updated[i] = counter[i + 1]

        counter = Counter(updated)
    return sum(counter.values())

if __name__ == '__main__':
    with open('input-06.txt') as file:
        puzzle = [int(x) for x in file.readline().split(',')]
        print(lanternfish(puzzle))
