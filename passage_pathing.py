from collections import Counter, deque


class Cave:
    def __init__(self, name):
        self.name = name
        self.neighbors = set()

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return f"Cave {self.name}: with neighbors {[x.name for x in self.neighbors]}"


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
        paths = []

        def can_visit(cave, path_so_far):
            if cave.name == 'start':
                return False
            if cave.name == 'end':
                return True
            if cave.name.isupper():
                return True

            c = Counter(x.name for x in path_so_far if x.name.islower())
            if 2 in c.values():
                return c[cave.name] < 1
            return True

        paths_to_expolore = deque([[self.start]])
        while paths_to_expolore:
            e = paths_to_expolore.popleft()
            current = e[-1]

            for n in current.neighbors:
                if n.name.islower() and can_visit(n, e):
                    # don't revisit a small cave
                    continue
                
                updated_path = list(e[:])
                updated_path.append(n)
                if n.name == 'end':
                    paths.append(updated_path)
                    continue

                paths_to_expolore.append(updated_path)

        return len(paths)


test1 = """start-A
start-b
A-c
A-b
b-d
A-end
b-end""".splitlines()

test2 = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc""".splitlines()


if __name__ == '__main__':
    p = PassagePathing(test1)
    print(p.solve())