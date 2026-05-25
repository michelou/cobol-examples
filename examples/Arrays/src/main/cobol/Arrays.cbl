IDENTIFICATION DIVISION.
PROGRAM-ID. "Arrays".
 
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
SELECT input-file ASSIGN TO DYNAMIC ws-fname.
 
DATA DIVISION.
FILE SECTION.
FD input-file
   RECORD contains 25 characters.
01 planet-info.
   03 planet PIC x(5) OCCURS 5 TIMES.
 
WORKING-STORAGE SECTION.
77 i PIC 99.
77 ws-fname PIC x(30).
 
PROCEDURE DIVISION.
   DISPLAY "Enter the filename: " WITH NO ADVANCING.
   ACCEPT ws-fname.
 
   OPEN INPUT input-file.
   READ input-file
   END-READ.
   CLOSE input-file.
 
   MOVE 1 TO i.
   PERFORM print-out-planets
      UNTIL i IS GREATER THAN 5.
   STOP RUN.
 
print-out-planets.
   DISPLAY "Planet: ", i, " ", planet(i).
   ADD 1 TO i.
