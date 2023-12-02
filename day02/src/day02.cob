      * We have to indent by 7 spaces, because... because!

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
               01 FileName         PIC X(100).
               01 ReachedEndOfFile PIC A(1) VALUE 'N'.

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
           DISPLAY "Read     " InputLine.
