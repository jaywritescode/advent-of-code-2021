from collections import deque, namedtuple


class Cave(namedtuple('Cave', ['name', 'neighbors'])):
    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return f"Cave {self.name} -> {[x.name for x in self.neighbors]}"

    @classmethod
    def create(cls, name):
        return Cave(name=name, neighbors=set())


class PassagePathing:
    def __init__(self, edges):
        self.start = Cave.create('start')
        self.graph = dict(start=self.start)
        for edge in edges:
            v1, v2 = edge.strip().split('-')
            self.add_edge(v1, v2)

    def add_edge(self, v1, v2):
        if v1 not in self.graph:
            self.graph[v1] = Cave.create(v1)
        if v2 not in self.graph:
            self.graph[v2] = Cave.create(v2)

        cave1, cave2 = self.graph[v1], self.graph[v2]
        cave1.neighbors.add(cave2)
        cave2.neighbors.add(cave1)

    def solve(self):
        paths = []

        queue = deque([[self.start]])
        while queue:
            e = queue.popleft()  # e is a path
            current = e[-1]

            for n in current.neighbors:
                if n.name.islower() and any(f.name == n.name for f in e):
                    # don't revisit a small cave
                    continue
                
                updated_path = list(current[:])
                updated_path.append(n)
                if n == 'end':
                    paths.append(updated_path)
                    continue

                queue.append(updated_path)

        return len(paths)


test1 = """start-A
start-b
A-c
A-b
b-d
A-end
b-end""".splitlines()


if __name__ == '__main__':
    p = PassagePathing(test1)
    print(p.solve())