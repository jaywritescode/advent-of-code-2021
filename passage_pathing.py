from collections import deque
from more_itertools import all_unique, last


class Cave:
    def __init__(self, name):
        self.name = name
        self.neighbors = set()

    def is_big(self):
        return self.name.isupper()

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return f"{self.name} -> {[x.name for x in self.neighbors]}"


class PassagePathing:
    def __init__(self, edges):
        self.start = Cave('start')
        self.graph = dict(start=self.start)
        for edge in edges:
            v1, v2 = edge.strip().split('-')
            self.add_edge(v1, v2)

    def add_edge(self, v1, v2):
        if v1 not in self.graph:
            self.graph[v1] = Cave(v1)
        if v2 not in self.graph:
            self.graph[v2] = Cave(v2)

        cave1, cave2 = self.graph[v1], self.graph[v2]
        cave1.neighbors.add(cave2)
        cave2.neighbors.add(cave1)

    def solve(self):
        counter = 0

        def can_visit(cave, path_so_far):
            if cave.name == 'start':
                return False
            if cave.is_big():
                return True

            if cave not in path_so_far:
                return True
                
            return all_unique(filter(lambda cave: not cave.is_big(), path_so_far))

        paths_to_explore = deque([[self.start]])
        while paths_to_explore:
            path = paths_to_explore.popleft()
            current_cave = last(path)

            if current_cave.name == 'end':
                counter += 1
            else:
                for neighbor in filter(lambda n: can_visit(n, path), current_cave.neighbors):
                    updated_path = path[:]
                    updated_path.append(neighbor)
                    paths_to_explore.append(updated_path)

        return counter


if __name__ == '__main__':
    with open('sample-data/sample-12-b.txt') as file:
        puzzle = [line.strip() for line in file.readlines()]
        p = PassagePathing(puzzle)
        print(p.solve())