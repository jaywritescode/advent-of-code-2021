#%%
def lanternfish(start):
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
#%%

if __name__ == '__main__':
    with open('input-06.txt') as file:
        puzzle = [int(x) for x in file.readline().split(',')]
        print(lanternfish(puzzle))