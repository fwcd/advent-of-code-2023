#!/usr/bin/env python3

import fileinput

for bs in fileinput.input(mode='rb'):
    for b in bs:
        print(f"{chr(b) if b >= 32 and b <= 126 else '?'} {b}")
