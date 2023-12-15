+[ while input has not reached newline (cell 0)
  [-] zero out cell 0

  > in cell 1
    , read input char
    . print input char
    [-<+>>+<] copy char to cell 0 and 2
  <
  
  input char is in cell 0
  ---------- subtract 10 (= newline aka \n in ASCII)
  we rely on the fact that all other chars we are interested in have higher ASCII values
]
