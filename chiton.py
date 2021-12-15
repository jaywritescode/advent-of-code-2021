import heapq

class Node:
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.min_distance = float('inf')
        self.neighbors = set()
        self.previous = None

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        return f"{self.name}: value = {self.value}, min_distance = {self.min_distance}"

class Chiton:
    def __init__(self, root):
        self.root = root

    def dijkstra(self, end_node):
        self.root.min_distance = 0

        visited = set()
        fringe = { self.root }
        
        while fringe:
            current_node = min(fringe, key=lambda node: node.min_distance)
            fringe.remove(current_node)
            visited.add(current_node)

            if current_node == end_node:
                return current_node

            for neighbor in current_node.neighbors:
                if neighbor in visited:
                    continue
                fringe.add(neighbor)
                new_distance = current_node.min_distance + neighbor.value
                if new_distance < neighbor.min_distance:
                    neighbor.min_distance = new_distance
                    neighbor.previous = current_node

        # no solution
        return None

    def solve(self, end_node):
        e = self.dijkstra(end_node)
        return e.min_distance    


if __name__ == '__main__':
    nodes = dict()
    with open('input-15.txt') as file:
        for row, line in enumerate(file):
            for column, value in enumerate(iter(line.strip())):
                this_node = Node((row, column), int(value))
                nodes[(row, column)] = this_node
                if row > 0:
                    nodes[(row - 1, column)].neighbors.add(this_node)
                    this_node.neighbors.add(nodes[(row - 1, column)])
                if column > 0:
                    nodes[(row, column - 1)].neighbors.add(this_node)
                    this_node.neighbors.add(nodes[(row, column - 1)])

    puzzle = Chiton(nodes[(0, 0)])
    print(puzzle.solve(this_node))

