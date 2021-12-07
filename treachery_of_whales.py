

#%%
def fuel_to_position(positions, target):
    return sum(abs(pos - target) for pos in positions)


def solve(positions):
    return min(fuel_to_position(positions, x) for x in range(max(positions) + 1))


#%%
test = [int(x) for x in "16,1,2,0,4,2,7,1,2,14".split(',')]

if __name__ == '__main__':
   with open('input-07.txt') as file:
       positions = [int(x) for x  in file.readline().split(',')]
       print(solve(positions))