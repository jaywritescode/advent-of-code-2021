from itertools import accumulate, chain, count, repeat, takewhile

# part two only
class TrickShot:
    def __init__(self, x_min, x_max, y_min, y_max):
        self.x_min = x_min
        self.x_max = x_max
        self.y_min = y_min
        self.y_max = y_max

    def get_positions(self, vx0, vy0):
        y_positions = takewhile(lambda y: y >= self.y_min, accumulate(count(vy0, step=-1), initial=0))
        x_positions = chain(accumulate(range(vx0, 0, -1), initial=0), repeat(vx0 * (vx0 + 1) // 2))
        positions = list(zip(x_positions, y_positions))
        
        return positions

    def ends_in_target_area(self, vx0, vy0):
        in_target_area = lambda p: self.x_min <= p[0] <= self.x_max and self.y_min <= p[1] <= self.y_max
        return any(in_target_area(p) for p in self.get_positions(vx0, vy0))
        
    def solve(self):
        return [(x, y) for x in range(0, self.x_max + 1) 
                       for y in range(self.y_min, -self.y_min + 1) if self.ends_in_target_area(x, y)]


if __name__ == '__main__':
    t = TrickShot(128, 160, -142, -88)
    print(len(t.solve()))