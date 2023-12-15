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
 6:   temporary for copying stuff around
 |
10:   temporary for copying stuff around
11:   temporary hash value for multiplication
12:   temporary 17 for the hash computation
13: last hash value
14:   temporary
 |
23:   temporary
24: hash sum

[-]+ set cell 0 to one (the condition)
>>>>>[-]<<<<< zero cell 5 (the hash value)

>>>> >>>> >>>> >>>>
>>>> >>>> in cell 24
  [-]> [-]> [-]> [-]>
  [-]> [-]> [-]> [-]>
  [-]> [-]> [-]> [-]>
  [-]> [-]> [-]> [-]>
  [-]> [-]> [-]> [-]>
  [-]> [-]> [-]> [-] zero until (inclusively) cell 48
<<<< <<<< <<<< <<<<
<<<< <<<< <<<< <<<<
<<<< <<<< <<<< <<<<

[ while input has not reached newline (cell 0)
  idea: copy cell 5 (the hash value) to cell 13 (the last value)

  >>>> > in cell 5 (the hash value)
    >[-]< >>>>>>>>[-]<<<<<<<< zero cell 6 and 13
    [- >+< >>>>>>>>+<<<<<<<<] copy to cell 6 and 13
    
    > in cell 6
      [- <+>] move back to cell 5
    <
  <<<< <

  > in cell 1
    , read input char

    <[-]> >[-]< zero cells 0 2
    [- <+> >+< >>>>+<<<<] add char to cells 0 2 and 5 (the hash value)

    idea: copy hash value to cell 11 for multiplication

    >>> > in cell 5 (hash value)
      >>>>>[-]>[-]<<<<<< zero cells 10 and 11
      [- >>>>>+>+<<<<<<] copy hash value to cells 10 and 11
    <<< <

    cell 5 (hash value) is now zero

    >>> >>>> >>>> in cell 12
      [-] zero cell
      ++++ ++++ ++++ ++++ + add 17

      idea: multiply into cell 5 (hash value) by repeatedly adding cell 11

      [ while not zero (ie repeat 17 times)
        < in cell 11 (temporary hash value)
          <<[-]>> <[-]> zero cell 9 and 10
          [- <<+>> <+>] copy hash value to cell 9 and 10

          << in cell 9
            [- >>+<<] move hash value back to cell 11
          >>

          < in cell 10
            [- <<<<<+>>>>>] add hash value to cell 5
          >
        >

        - decrement
      ]
    <<< <<<< <<<<
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
        >>>> >>>> >>>> > in cell 13 (the hash value)
          idea: assign cell 13 (last hash value) to cell 5 (hash value)

          <<<<<<<<[-]>[-]>>>>>>> zero cell 5 and 6
          [- <<<<<<<<+>+>>>>>>>] copy to cell 5 and 6

          < <<<< << in cell 6
            [- >>>>>>>+<<<<<<< >>>>>>>>>>>>>>>>>>+<<<<<<<<<<<<<<<<<<] move back to cell 13 and add to cell 24
          > >>>> >>
        <<<< <<<< <<<< <

        >>>> > in cell 5 (the hash value)
          [-] zero cell
        <<<< <
      >>>

      > go to cell 4
    ]

    << go back to cell 2
  <<

  cell 0 contains input char
  ---- ---- -- subtract 10 (ASCII value for \n)
  we rely on the fact that all other chars we are interested in have higher ASCII values
]

>>> >>>> >>>> in cell 24 (the hash sum)
  . output value
<<< <<<< <<<<

++++ ++++ ++ add 10 (ASCII value for \n)
. print the newline
