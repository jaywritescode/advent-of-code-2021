from collections import namedtuple
from itertools import product

Coordinate = namedtuple('Coordinate', ['row', 'column'])

class Chiton:

    class Node:
        def __init__(self, name, value):
            self.value = value
            self.min_distance = float('inf')
            self.neighbors = None
            self.name = name

        def __hash__(self):
            return hash(self.name)

        def __repr__(self):
            return f"{self.name}: value = {self.value}, min_distance = {self.min_distance}"

    def __init__(self):
        self.nodes = dict()

    def create_node(self, name, value):
        self.nodes[name] = Chiton.Node(name, value)

    def get_neighbors(self, node):
        if node.neighbors is None:
            node.neighbors = set()
            for (delta_r, delta_c) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                neighbor = self.nodes.get(Coordinate(node.name.row + delta_r, node.name.column + delta_c))
                if neighbor:
                    node.neighbors.add(neighbor)

        return node.neighbors

    def dijkstra(self, end_node):
        root = self.nodes[Coordinate(0, 0)]
        root.min_distance = 0

        visited = set()
        fringe = { root }
        
        while fringe:
            current_node = min(fringe, key=lambda node: node.min_distance)
            fringe.remove(current_node)
            visited.add(current_node)

            if current_node == end_node:
                return current_node

            for neighbor in self.get_neighbors(current_node):
                if neighbor in visited:
                    continue
                fringe.add(neighbor)
                new_distance = current_node.min_distance + neighbor.value
                if new_distance < neighbor.min_distance:
                    neighbor.min_distance = new_distance
                    neighbor.previous = current_node

        # no solution
        return None

    def solve(self):
        max_row = max(c.row for c in self.nodes)
        max_col = max(c.column for c in self.nodes)

        e = self.dijkstra(self.nodes[Coordinate(max_row, max_col)])
        return e.min_distance


if __name__ == '__main__':
    chiton = Chiton()

    with open('input-15.txt') as file:
        for row, line in enumerate(file):
            for column, value in enumerate(iter(line.strip())):
                chiton.create_node(Coordinate(row, column), int(value))

        # part two
        rows = row + 1
        cols = column + 1
        initial_nodes = list(chiton.nodes.values())
        for (dr, dc) in product(range(5), repeat=2):
            if dr == 0 and dc == 0:
                continue
            
            for node in initial_nodes:
                coord = Coordinate(dr * rows + node.name.row, dc * cols + node.name.column)
                new_value = node.value + dr + dc
                if new_value >= 10:
                    new_value += 1
                    new_value %= 10

                chiton.create_node(coord, new_value)
            
        print(chiton.solve())
