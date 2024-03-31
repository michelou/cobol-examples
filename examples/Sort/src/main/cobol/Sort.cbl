IDENTIFICATION DIVISION.
PROGRAM-ID. Sort.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-FILLER.
    05 WS-TBL  OCCURS 10  TIMES.
        10 WS-USE-CNT     PIC 9(4)   COMP.
01 WS-I                   PIC 9(4)   COMP.

PROCEDURE DIVISION.
   MOVE 112  TO WS-USE-CNT (1)                            
   MOVE 13   TO WS-USE-CNT (2)                            
   MOVE 55   TO WS-USE-CNT (3)                            
   MOVE 15   TO WS-USE-CNT (4)                             
   MOVE 16   TO WS-USE-CNT (5)                            
   MOVE 3    TO WS-USE-CNT (6)                            
   MOVE 43   TO WS-USE-CNT (7)                            
   MOVE 78   TO WS-USE-CNT (8)
   MOVE 34   TO WS-USE-CNT (9)
   MOVE 46   TO WS-USE-CNT (10)
   SORT WS-TBL ASCENDING WS-USE-CNT
   PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
      DISPLAY WS-USE-CNT (WS-I)
   END-PERFORM
   GOBACK.
