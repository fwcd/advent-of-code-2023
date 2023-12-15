#!/usr/bin/env python3

import fileinput

RED = '\033[31m'
CLEAR = '\033[0m'

for bs in fileinput.input(mode='rb'):
    for b in bs:
        print(f"{chr(b) if b >= 32 and b <= 126 else f'{RED}?{CLEAR}'} {b}")
