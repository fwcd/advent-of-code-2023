#!/usr/bin/env python3

from functools import reduce
from pathlib import Path

import argparse

def connected_components(bidi_graph: dict[str, set[str]]) -> list[int]:
    remaining = set(bidi_graph.keys())
    visited = set()
    stack = []
    components = []

    while remaining and (start := remaining.pop()):
        remaining.add(start)
        stack.append(start)
        size = 0
        while stack and (n := stack.pop()):
            if n not in visited:
                visited.add(n)
                remaining.remove(n)
                stack += bidi_graph[n]
                size += 1
        components.append(size)
    
    return components


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input', type=Path, help='The path to the input file')

    args = parser.parse_args()

    with open(args.input, 'r') as f:
        graph = {k: set(v.split(' ')) for l in f.readlines() for k, v in [l.strip().split(': ')]}
    
    # Cut edges (found visually by inspecting using GraphViz's sfdp output)
    for n1, n2 in [['xnn', 'txf'], ['tmc', 'lms'], ['jjn', 'nhg']]:
        if n1 in graph and n2 in graph[n1]:
            graph[n1].remove(n2)
        if n2 in graph and n1 in graph[n2]:
            graph[n2].remove(n1)
    
    # Connect graph bidirectionally
    bidi_graph = {}
    for n1, ns in graph.items():
        for n2 in ns:
            bidi_graph[n2] = {*bidi_graph.get(n2, set()), n1}
            bidi_graph[n1] = {*bidi_graph.get(n1, set()), n2}
    
    components = connected_components(bidi_graph)
    part1 = reduce(lambda x, y: x * y, components)

    print(f'Part 1: {part1}')

if __name__ == '__main__':
    main()
