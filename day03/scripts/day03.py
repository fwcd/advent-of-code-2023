#!/usr/bin/env python3

import fileinput
import re

ls = [l.strip() for l in fileinput.input(encoding='utf-8')]

def in_bounds(i, j):
    return i >= 0 and i < len(ls) and j >= 0 and j < len(ls[0])

def has_neighbor(i, j):
    for di in range(-1, 2):
        for dj in range(-1, 2):
            if di != 0 or dj != 0:
                ni = i + di
                nj = j + dj
                if in_bounds(ni, nj) and re.match(r'[^0-9\.]', ls[ni][nj]):
                    return True
                                
result = 0
for i, l in enumerate(ls):
    for m in re.finditer(r'\d+', l):
        s = m.group()
        if any(has_neighbor(i, j) for j in range(*m.span())):
            result += int(s)

print(result)
