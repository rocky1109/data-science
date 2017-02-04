__author__ = 'rramchandani'


import os
import sys


if sys.version_info < (3, 4):
    print("""
To run this program requires Python 3.4.
Currently found version of Python {0}.{1}.
    """.format(sys.version_info[0], sys.version_info[1]))
    sys.exit(-1)


import pprint
import math
from collections import defaultdict


DEFAULT_IN_FILE = os.path.join("in", "in.csv")
DEFAULT_OUT_REL_FILE = os.path.join("out", "out_relation.csv")
DEFAULT_OUT_COST_FILE = os.path.join("out", "out_cost.csv")
graph = defaultdict()

"""
def wt(node1, node2):
    if node1 in products:
        if node2 in products[node1]:
            return products[node1][node2]
    return (0, 0)
"""

relations = defaultdict()
costs = defaultdict()
walks = defaultdict()


def bfs_with_levels(graph, start, root, level=0):
    visited, queue = set(), ['#', start]
    while queue:
        vertex = queue.pop(0)
        if vertex == '#':
            level += 1
            continue
        if vertex not in visited:
            relations[root, vertex] = level
            visited.add(vertex)
            if vertex in graph:
                queue.extend(set(graph[vertex]) - visited)
                queue.append('#')
    return visited


"""
    def dfs(graph, start, root, visited=None):
        if visited is None:
            visited = set()
        visited.add(start)
        if start in graph:
            for next in set(graph[start]) - visited:
                costs[start, next] = wt(start, next)
                dfs(graph, next, root, visited)
        return visited
"""


def dfs_paths(graph, start, goal, path=None):
    if path is None:
        path = [start]
    if start == goal:
        yield path
    if start in graph:
        for next in set(graph[start]) - set(path):
            yield from dfs_paths(graph, next, goal, path + [next])


round_off = lambda x: math.ceil(x*100)/100


def get_quantity(relation):
    walk = walks[relation][0]
    qty = 1
    for count in range(len(walk)-1, 0, -1):
        a, b = walk[count-1], walk[count]
        qty *= costs[(a, b)][0]
    return round_off(qty)


def group_by(li):
    from collections import OrderedDict
    _ = OrderedDict()
    for item in li:
        if item[0] not in _:
            _[item[0]] = list()
        _[item[0]].append((item[1], item[2], item[3]))
    return _


def try_making_parent_directories(path):
    try:
        os.makedirs(os.path.dirname(path))
    except:
        pass


def main():

    # Read input file path.
    in_file = input("Path for Input file or press ENTER for default[{0}]: ".format(
        os.path.relpath(DEFAULT_IN_FILE, start=os.path.dirname(os.path.dirname(DEFAULT_IN_FILE)))))

    # Set to default path, if empty in file name.
    if not in_file:
        in_file = DEFAULT_IN_FILE

    # If file not found raise an exception.
    if not os.path.isfile(in_file):
        raise IOError("Input file '{0}' doesn't exists.".format(in_file))

    # Read the of the input csv, create a graph and cost matrix from it.
    with open(in_file, 'r') as fh:
        for line in fh.readlines():
            product_no, component, component_qty, component_price = line.replace('\n', '').split(',')
            component_qty = float(component_qty)
            component_price = float(component_price)

            if product_no not in graph:
                graph[product_no] = list()

            graph[product_no].append(component)
            costs[product_no, component] = (component_qty, component_price)

    # Read the relation out file path.
    out_relation_file = input("Path for Level & Relation Output file or"
                              " press ENTER for default[{0}]: ".format(
        os.path.relpath(DEFAULT_OUT_REL_FILE, start=os.path.dirname(os.path.dirname(DEFAULT_OUT_REL_FILE)))))

    # Read the cost out file path.
    out_cost_file = input("Path for Product Cost Output file or"
                          " press ENTER for default[{0}]: ".format(
        os.path.relpath(DEFAULT_OUT_COST_FILE, start=os.path.dirname(os.path.dirname(DEFAULT_OUT_COST_FILE)))))

    # If out file path is empty then set to default out values.
    if not out_relation_file:
        out_relation_file = DEFAULT_OUT_REL_FILE
    if not out_cost_file:
        out_cost_file = DEFAULT_OUT_COST_FILE

    # Get the level of each relative node to parent node in graph.
    for node in graph.keys():
        bfs_with_levels(graph, node, node)

    #pprint.pprint(relations)

    # Get the path/walk from node 'A' to node 'B'.
    for item in list(set(relations)):
        walks[(item[0], item[1])] = [i for i in dfs_paths(graph, item[0], item[1])]

    #pprint.pprint(walks)

    # Get the cost for walk to traverse.
    _ = list()
    for relation, level in relations.items():
        if relation[0] != relation[1]:
            _.append([relation[0], relation[1], level, get_quantity(relation)])

    # Sort the data on first index value.
    from operator import itemgetter
    _.sort(key=itemgetter(0))

    # Write the relation output.
    try_making_parent_directories(out_relation_file)
    with open(out_relation_file, "w+") as fh:
        fh.write("Product,Component,Level,Quantity\n")
        __ = "\n".join(("{0},{1},{2},{3}".format(i[0], i[1], i[2], i[3]) for i in _))
        fh.write(__)

    # Group the relation output according to product-wise
    group = group_by(_)

    # Write the cost output for each product.
    try_making_parent_directories(out_cost_file)
    with open(out_cost_file, "w+") as fh:
        fh.write("Product,Cost\n")
        for product, details in group.items():
            fh.write("{0},{1}\n".format(product,
                                        round_off(sum((item[1]*item[2] for item in details)))))

    print("Done :)")


if __name__ == '__main__':
    main()
