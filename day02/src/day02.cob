      * We have to indent by 7 spaces, because... because!

      * Helpful resources I found while learning enough COBOL to implement this:
      * - https://www.tutorialspoint.com/cobol/cobol_data_types.htm
      * - https://gnucobol.sourceforge.io/doc/gnucobol.html
      * - https://craftofcoding.wordpress.com/2021/01/26/coding-cobol-reading-a-file-of-integers/
      * - https://www.mainframestechhelp.com/tutorials/cobol/unstring-statement.htm

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT InputFile
                   ASSIGN TO FileName
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD InputFile.
               01 InputLine PIC X(256).

           WORKING-STORAGE SECTION.
               01 FileName                PIC X(100).
               01 ReachedEndOfFile        PIC A(1) VALUE 'N'.
               01 GameIndex               PIC 9(3) VALUE 1.
               01 GameValid               PIC A(4) VALUE 'Y'.
               01 ParsedLine.
                   05 GameName            PIC X(12).
                   05 RawCubeSets         PIC X(256).
               01 ParsedCubeSets.
                   05 RawCubeSet          PIC X(32) OCCURS 6 TIMES.
               01 CubeSetIndex            PIC 9(1) VALUE 0.
               01 ParsedCubeSet.
                   05 CubeStack           OCCURS 3 TIMES.
                        10 CubeCount      PIC 9(2).
                        10 CubeColor      PIC X(1).
               01 CubeStackIndex          PIC 9(1) VALUE 0.
               01 CubeSet.
                   05 Red                 PIC 9(2) VALUE 0.
                   05 Green               PIC 9(2) VALUE 0.
                   05 Blue                PIC 9(2) VALUE 0.
               01 Total.
                   05 TotalRed            PIC 9(2) VALUE 12.
                   05 TotalGreen          PIC 9(2) VALUE 13.
                   05 TotalBlue           PIC 9(2) VALUE 14.
               01 Result.
                   05 Part1               PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
           ACCEPT FileName FROM COMMAND-LINE.

           IF FileName = SPACES THEN
               DISPLAY "Usage: day02 <path to input>"
               STOP RUN
           END-IF.

           DISPLAY "Reading input from " FileName.
           OPEN INPUT InputFile.

           PERFORM UNTIL ReachedEndOfFile = 'Y'
               READ InputFile
                   AT END
                       MOVE 'Y' TO ReachedEndOfFile
                   NOT AT END
                       PERFORM ProcessLine
               END-READ
           END-PERFORM.

           CLOSE InputFile.

           DISPLAY "Part 1: " Part1.

           STOP RUN.

       ProcessLine.
           DISPLAY "Game " GameIndex.

           UNSTRING InputLine
               DELIMITED BY ": "
               INTO GameName, RawCubeSets.

           PERFORM ClearRawCubeSet
               VARYING CubeSetIndex
               FROM 1 BY 1
               UNTIL CubeSetIndex > 6.

           UNSTRING RawCubeSets
               DELIMITED BY "; "
               INTO RawCubeSet(1),
                    RawCubeSet(2),
                    RawCubeSet(3),
                    RawCubeSet(4),
                    RawCubeSet(5),
                    RawCubeSet(6).
           
           MOVE 'Y' TO GameValid.

           PERFORM ProcessCubeSet
               VARYING CubeSetIndex
               FROM 1 BY 1
               UNTIL CubeSetIndex > 6.
           
           IF GameValid = 'Y'
              COMPUTE Part1 = Part1 + GameIndex
           END-IF.

           COMPUTE GameIndex = GameIndex + 1.

       ClearRawCubeSet.
           MOVE SPACES TO RawCubeSet(CubeSetIndex).

       ProcessCubeSet.
           UNSTRING RawCubeSet(CubeSetIndex)
              DELIMITED BY " "
              INTO CubeCount(1), CubeColor(1),
                   CubeCount(2), CubeColor(2),
                   CubeCount(3), CubeColor(3).

           MOVE 0 TO Red.
           MOVE 0 TO Green.
           MOVE 0 TO Blue.

           PERFORM ProcessCubeStack
               VARYING CubeStackIndex
               FROM 1 BY 1
               UNTIL CubeStackIndex > 3.
           
           IF Red > 0 OR Green > 0 OR Blue > 0
               DISPLAY "Red: " Red " Green: " Green " Blue: " Blue
           END-IF.

           IF Red > TotalRed OR Green > TotalGreen
                             OR Blue  > TotalBlue
               MOVE 'N' TO GameValid
           END-IF.
       
       ProcessCubeStack.
           EVALUATE CubeColor(CubeStackIndex)
               WHEN 'r' COMPUTE Red   = CubeCount(CubeStackIndex)
               WHEN 'g' COMPUTE Green = CubeCount(CubeStackIndex)
               WHEN 'b' COMPUTE Blue  = CubeCount(CubeStackIndex)
               WHEN OTHER CONTINUE
           END-EVALUATE.
