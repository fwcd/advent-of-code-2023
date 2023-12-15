+[ while input has not reached newline (cell 0)
  [-] set to zero (cell 0)

  > in cell 1
    , read input char
    . print input char
    [-<+>>+<] copy char to cell 0 and 2
  <

  >>> in cell 3
    [-] set to zero
    ++++ ++++ ++++ ++++
    ++++ ++++ ++++ ++++ set value to 32 (ASCII value for space)
    . print the space
  <<<
  
  input char is in cell 0
  ---- ---- -- subtract 10 (ASCII value for \n)
  we rely on the fact that all other chars we are interested in have higher ASCII values
]

++++ ++++ ++ add 10 (ASCII value for \n)
. print the newline
