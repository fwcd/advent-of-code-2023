      * We have to indent by 7 spaces, because... because!

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY02.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
               01 FileName PIC X(100).

       PROCEDURE DIVISION.
           ACCEPT FileName FROM COMMAND-LINE.

           IF FileName = SPACES THEN
               DISPLAY "Usage: day02 <path to input>"
               STOP RUN
           END-IF.

           DISPLAY FileName.
       STOP RUN.
