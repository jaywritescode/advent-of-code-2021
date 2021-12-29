from itertools import cycle
from more_itertools import take

deterministic_die = cycle(range(1, 101))

def play(player_one_start, player_two_start, die):
    positions = [player_one_start, player_two_start]
    scores = [0, 0]

    rolls = 0
    idx = 0
    while True:
        roll = sum(take(3, die))
        rolls += 3

        positions[idx] = (positions[idx] + roll) % 10
        scores[idx] += 10 if positions[idx] == 0 else positions[idx]
        if scores[idx] >= 1000:
            return rolls * scores[~idx]

        idx = ~idx


if __name__ == '__main__':
    print(play(4, 8, deterministic_die))
