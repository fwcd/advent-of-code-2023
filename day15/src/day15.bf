An implementation of day 15 in BF tested with Wilfred's bfc compiler

Assumes cells to be unsigned bytes that wrap around but may contain arbitrary
data at startup time

memory layout:
 0: condition (ie zero once we reached a newline)
 1: input char
 2: zero if input char is a comma
 3:   temporary 1 used for 'if not zero' check of cell 2
 4:   temporary 0 used for 'if not zero' check of cell 2
 5: running hash value
 6:   temporary 17 for the hash computation
16: ASCII space for printing

[-]+ set cell 0 to one (the condition)
>>>>>[-]<<<<< zero cell 5 (the hash value)

[ while input has not reached newline (cell 0)
  > in cell 1
    , read input char

    <[-]> >[-]< zero cells 0 2
    [- <+> >+< >>>>>+<<<<<] add char to cells 0 2 and 5 (the hash value)
  <

  >> in cell 2
    cell contains input char
    ---- ---- ---- ----
    ---- ---- ---- ----
    ---- ---- ---- subtract 44 (ASCII value for comma)

    idea: check if the cell is zero ie if the input char is a comma
    for that we use cells 3 and 4 as temporaries

    >[-]< >>[-]<< zero cells 3 and 4
    >+< set cell 3 to one
    [>-] go to cell 3 if nonzero
    > we are now in cell 3 with a one  if cell 2 was zero    ie we had a comma
                 in cell 4 with a zero if cell 2 was nonzero ie we had something else

    [ if nonzero ie if we had a comma
      we are in cell 3
      - zero cell

      <<< in cell 0
        >>>> > in cell 5 (the hash value)
          ++++ ++++ ++++ ++++
          ++++ ++++ ++++ ++++
          ++++ ++++ ++++ ++++ add 48 (ASCII value for zero)
          . output value

          todo: handle multi digits with mod algorithm

          [-] zero cell
        <<<< <

        >>>> >>>> >>>> >>>> in cell 16
          [-] zero cell
          ++++ ++++ ++++ ++++
          ++++ ++++ ++++ ++++ set value to 32 (ASCII value for space)
          . print the space
        <<<< <<<< <<<< <<<<
      >>>

      > go to cell 4
    ]

    << go back to cell 2
  <<
  
  cell 0 contains input char
  ---- ---- -- subtract 10 (ASCII value for \n)
  we rely on the fact that all other chars we are interested in have higher ASCII values
]

++++ ++++ ++ add 10 (ASCII value for \n)
. print the newline
