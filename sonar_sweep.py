#%%
def count_increases(depths):
    return sum(1 for x in zip(depths, depths[1:]) if x[0] < x[1])
#%%


if __name__ == '__main__':
    with open("input-01.txt") as f:
        args = [int(x) for x in f.readlines()]
        print(count_increases(args))
# %%
