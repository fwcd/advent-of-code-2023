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
               01 GameIndex               PIC 9(2) VALUE 1.
               01 ParsedLine.
                   05 GameName            PIC X(12).
                   05 RawCubeSets         PIC X(256).
               01 ParsedCubeSets.
                   05 RawCubeSet          PIC X(32) OCCURS 6 TIMES.
               01 CubeSetIndex            PIC 9(2) VALUE 0.
               01 ParsedCubeSet.
                   05 CubeStack           OCCURS 3 TIMES.
                        10 CubeCount      PIC 9(2).
                        10 CubeColor      PIC X(16).
               01 CubeSet.
                   05 Red                 PIC 9(2) VALUE 0.
                   05 Green               PIC 9(2) VALUE 0.
                   05 Blue                PIC 9(2) VALUE 0.

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
           STOP RUN.

       ProcessLine.
           UNSTRING InputLine
               DELIMITED BY ": "
               INTO GameName, RawCubeSets.

           UNSTRING RawCubeSets
               DELIMITED BY "; "
               INTO RawCubeSet(1),
                    RawCubeSet(2),
                    RawCubeSet(3),
                    RawCubeSet(4),
                    RawCubeSet(5),
                    RawCubeSet(6).
           DISPLAY RawCubeSet(1)
                   RawCubeSet(2)
                   RawCubeSet(3)
                   RawCubeSet(4)
                   RawCubeSet(5)
                   RawCubeSet(6).
            
           PERFORM ProcessCubeSet
               VARYING CubeSetIndex
               FROM 1 BY 1
               UNTIL CubeSetIndex > 6.
           
           COMPUTE GameIndex = GameIndex + 1.

       ProcessCubeSet.
           UNSTRING RawCubeSet(CubeSetIndex)
              DELIMITED BY " "
              INTO CubeCount(1), CubeColor(1),
                   CubeCount(2), CubeColor(2),
                   CubeCount(3), CubeColor(3).
           DISPLAY "First: "  CubeCount(1) " " CubeColor(1) ", "
                   "Second: " CubeCount(2) " " CubeColor(2) ", "
                   "Third: "  CubeCount(3) " " CubeColor(3).
