       IDENTIFICATION DIVISION.
       PROGRAM-ID. Y2KLAGE.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. DECEMBER, 1997.
       DATE-COMPILED.
      *                                                               *
      *   YY   YY   222   KK   KK LL         A     GGGGG  EEEEEE      *
      *   YY   YY  2   2  KK  KK  LL        AAA   GG   GG EE          *
      *   YY   YY      2  KK KK   LL       AA AA  GG   GG EE          *
      *    YY YY       2  KKKK    LL      AA   AA GG      EEEE        *
      *     YYY     222   KKKK    LL      AA   AA GG  GGG EE          *
      *     YY     2      KK KK   LL      AAAAAAA GG   GG EE          *
      *     YY     2      KK  KK  LL      AA   AA GG   GG EE          *
      *     YY     22222  KK   KK LLLLLLL AA   AA  GGGGG  EEEEEEE     *
      *                                                               *
      *************************************************************** *
      * THIS SUBROUTINE COMPUTES THE NUMBER OF YEARS, MONTHS, AND     *
      * DAYS ELAPSED BETWEEN 2 GIVEN GREGORIAN DATES (MMDDYYYY) FOR   *
      * LONG TERM AGING                                               *
      *                                                               *
      *  VALID INPUT YEARS FOR THIS ROUTINE ARE THE YEARS 1601        *
      *  THROUGH 3399 A. D. (INCLUSIVE)                               *
      *                                                               *
      *  VALID INPUT MONTH VALUES FOR THIS ROUTINE ARE 01 THROUGH 12. *
      *                                                               *
      *  VALID INPUT DAY VALUES FOR THIS ROUTINE ARE 01 THROUGH 31    *
      *  WITH EXCEPTIONS FOR THE MONTHS LISTED:                       *
      *       MONTH          MAXIMUM DAY VALUE                        *
      *        04                    30                               *
      *        06                    30                               *
      *        09                    30                               *
      *        11                    30                               *
      *        02                    28                               *
      *        02 DURING LEAP YEAR   29                               *
      *                                                               *
      *  THE DATE FORMAT MUST BE MMDDYYYY, WHERE MM = THE 2 DIGIT     *
      *  MONTH VALUE, DD = THE 2 DIGIT DAY VALUE, AND YYYY = THE      *
      *  4 DIGIT YEAR VALUE.                                          *
      *                                                               *
      *  SIX FIELDS ARE PASSED AS PARAMETERS TO THE ROUTINE:          *
      *  1) & 2)  8 BYTE FIELDS CONTAINING THE GREGORIAN DATES (IN    *
      *      (ZONED-DECIMAL FORMAT) BETWEEN WHICH THE NUMBER OF       *
      *      YEARS, MONTHS AND DAYS IS TO BE CALCULATED (ORDER        *
      *      OF THE DATES IS IMMATERIAL),                             *
      *  3)  A 1 BYTE ZONED-DECIMAL RETURN CODE,                      *
      *  4), 5) & 6)  2 BYTE, 2 BYTE, AND 5 BYTE FIELDS WHICH WIL     *
      *      RECEIVE THE NUMBER OF DAYS, MONTHS, AND YEARS ELAPSED    *
      *      (IN ZONED-DECIMAL FORMAT).                               *
      *  THE ROUTINE WILL NOT ALTER THE DATES PASSED TO IT.           *
      *                                                               *
      *  SUGGESTED CALLING SYNTAX FOR COBOL CALLERS:                  *
      *                                                               *
      *       01  LAGE-PARAMETERS.                                    *
      *           02  DATE1  PIC 9(8) VALUE 01011997.                 *
      *           02  DATE2  PIC 9(8) VALUE 04251988.                 *
      *           02  RC     PIC 9(1) VALUE 0.                        *
      *           02  DAYS   PIC S9(2) VALUE +0.                      *
      *           02  MONTHS PIC S9(2) VALUE +0.                      *
      *           02  YEARS  PIC S9(5) VALUE +0.                      *
      *       CALL 'Y2KLAGE' USING LAGE-PARAMETERS.                   *
      *                                                               *
      *  THE POSSIBLE VALUES FOR THE RETURN CODE FIELD ARE:           *
      *                                                               *
      *  0 INDICATES SUCCESSFUL EXECUTION OF THE ROUTINE.             *
      *                                                               *
      *  2 INDICATES INVALID DATA WAS FOUND IN THE DATE1 FIELD        *
      *  (3 FOR DATE2).  INVALID DATA ARE DETERMINED IF THE FIELD'S   *
      *  LOW ORDER BYTE'S ZONE CONTAINS AN INVALID SIGN, (NOT ONE     *
      *  OF X'C', X'D', OR X'F'), OR IF THE PRECEEDING BYTES' ZONES   *
      *  ARE OTHER THAN X'F', OR IF ANY BYTE'S LOW ORDER NIBBLE       *
      *  CONTAINS A VALUE GREATER THAN X'9'.                          *
      *                                                               *
      *  4 INDICATES THE VALUE OF THE YEAR SPECIFIED IN THE DATE1     *
      *  FIELD (5 FOR DATE2) WAS NOT IN THE RANGE SPECIFIED IN 1      *
      *  (ABOVE).                                                     *
      *                                                               *
      *  6 INDICATES THE VALUE OF THE MONTH SPECIFIED IN THE DATE1    *
      *  FIELD (7 FOR DATE2) WAS NOT IN THE RANGE SPECIFIED IN 2      *
      *  (ABOVE).                                                     *
      *                                                               *
      *  8 INDICATES THE VALUE OF THE DAY SPECIFIED IN THE DATE1      *
      *  FIELD (9 FOR DATE2) WAS NOT IN THE RANGE SPECIFIED IN 3      *
      *  (ABOVE).                                                     *
      *                                                               *
      *  UPON SUCCESSFUL EXECUTION, THE ABSOLUTE VALUE OF THE NUMBER  *
      *  OF DAYS, MONTHS, AND YEARS WILL BE PLACED IN THE 4TH, 5TH    *
      *  AND 6TH FIELDS.  IF THE RETURN CODE IS A NON-ZERO VALUE      *
      *  (UNSUCCESSFUL EXECUTION), THESE 3 FIELDS WILL CONTAIN ZEROS. *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      * 18 Sep 2017 - Change from INITIAL to LOCAL-STORAGE            *
      *               This change has no affect on program execution. *
      *               It was made to accomodate a bug in GnuCOBOL 2.2 *
      *               which will be fixed, but the change makes sense *
      *               from a logical viewpoint, so thus is justified. *
      *                                                               *
      *************************************************************** *
      /
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * ************************************************************* *
      * THESE ARE CONSTANT FIELDS WHICH SHOULD NOT CHANGE.            *
      * ************************************************************* *
       01  PROGRAM-CONSTANT-FIELDS.
           02  DAYS-IN-MONTHS-INIT.
               03  FILLER PIC X(16) VALUE '3131282931313030'.
               03  FILLER PIC X(16) VALUE '3131303031313131'.
               03  FILLER PIC X(16) VALUE '3030313130303131'.
           02  DAYS-IN-MONTHS-TABLE    REDEFINES DAYS-IN-MONTHS-INIT.
               03  DIM-ENTRY           OCCURS 12 TIMES.
                   05  DIM-NORMAL      PIC 9(2).
                   05  DIM-LEAP        PIC 9(2).

       LOCAL-STORAGE SECTION.

       01  FILLER                      PIC X(24)
           VALUE 'Y2KLAGE STORAGE BEGINS->'.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND WORK FIELDS.           *
      * ************************************************************* *
       01  PROGRAM-CONTROL-FIELDS.
           02  CURRENT-YEAR            PIC 9(4).
           02  CURRENT-MONTH           PIC 9(2).
           02  FROM-DATE               PIC 9(8).
           02  FILLER                  REDEFINES FROM-DATE.
               03  FROM-M              PIC 9(2).
               03  FROM-D              PIC 9(2).
               03  FROM-Y              PIC 9(4).
           02  TO-DATE                 PIC 9(8).
           02  FILLER                  REDEFINES TO-DATE.
               03  TO-M                PIC 9(2).
               03  TO-D                PIC 9(2).
               03  TO-Y                PIC 9(4).
           02  MONTH-COUNT             PIC S9(7) COMP-3.
           02  ORIGINAL-FROM-D         PIC 9(2).
           02  DAYS-IN-FROM-MONTH      PIC 9(2).
           02  WORK-A                  PIC S9(7) COMP-3.
           02  WORK-B                  PIC S9(7) COMP-3.
           02  LEAP-YEAR-SWITCH        PIC 9(1).
               88  IS-LEAP-YEAR        VALUE 1.
           02  DAYS-IN-CURRENT-MONTH   PIC 9(2).

           02  GTOA-PARAMETERS.
               03  GTOAP-DATEG         PIC 9(8).
               03  GTOAP-RC            PIC 9(1).
               03  GTOAP-ANUM          PIC S9(7).

       01  FILLER                      PIC X(24)
           VALUE '<-Y2KLAGE STORAGE ENDS'.

       LINKAGE SECTION.

      * ************************************************************* *
      * THESE ARE THE FIELDS USED TO RECEIVE INPUT DATA FROM THE      *
      * CALLER AND PASS RESULT FIELDS BACK TO THE CALLER.             *
      * ************************************************************* *
       01  LAGE-PARAMETERS.
           02  LAGEP-DATE1             PIC 9(8).
           02  FILLER                  REDEFINES LAGEP-DATE1.
               03  LAGEP-DATE1-M       PIC 9(2).
               03  LAGEP-DATE1-D       PIC 9(2).
               03  LAGEP-DATE1-Y       PIC 9(4).
           02  LAGEP-DATE2             PIC 9(8).
           02  FILLER                  REDEFINES LAGEP-DATE2.
               03  LAGEP-DATE2-M       PIC 9(2).
               03  LAGEP-DATE2-D       PIC 9(2).
               03  LAGEP-DATE2-Y       PIC 9(4).
           02  LAGEP-RC                PIC 9(1).
           02  LAGEP-DAYS              PIC S9(2).
           02  LAGEP-MONTHS            PIC S9(2).
           02  LAGEP-YEARS             PIC S9(5).
      /
       PROCEDURE DIVISION USING LAGE-PARAMETERS.

       0000-MAIN SECTION.
       0025-INITIALIZE.
           MOVE ZERO TO LAGEP-RC, LAGEP-DAYS, LAGEP-MONTHS, LAGEP-YEARS.

       0050-VALIDATE-DATE1.
           IF LAGEP-DATE1 IS NOT NUMERIC
               MOVE 2 TO LAGEP-RC
               GOBACK.

       0075-VALIDATE-DATE2.
           IF LAGEP-DATE2 IS NOT NUMERIC
               MOVE 3 TO LAGEP-RC
               GOBACK.

       0100-VALIDATE-YEAR1.
           IF LAGEP-DATE1-Y IS LESS THAN 1601
           OR LAGEP-DATE1-Y IS GREATER THAN 3399
               MOVE 4 TO LAGEP-RC
               GOBACK.

       0125-VALIDATE-YEAR2.
           IF LAGEP-DATE2-Y IS LESS THAN 1601
           OR LAGEP-DATE2-Y IS GREATER THAN 3399
               MOVE 5 TO LAGEP-RC
               GOBACK.

       0150-VALIDATE-MONTH1.
           IF LAGEP-DATE1-M IS LESS THAN 01
           OR LAGEP-DATE1-M IS GREATER THAN 12
               MOVE 6 TO LAGEP-RC
               GOBACK.

       0175-VALIDATE-MONTH2.
           IF LAGEP-DATE2-M IS LESS THAN 01
           OR LAGEP-DATE2-M IS GREATER THAN 12
               MOVE 7 TO LAGEP-RC
               GOBACK.

       0200-VALIDATE-DAY1.
           MOVE LAGEP-DATE1-Y TO CURRENT-YEAR.
           MOVE LAGEP-DATE1-M TO CURRENT-MONTH.
           PERFORM 0375-FIND-MAX-DAYS-FOR-MONTH.
           IF LAGEP-DATE1-D IS LESS THAN 01
           OR LAGEP-DATE1-D IS GREATER THAN DAYS-IN-CURRENT-MONTH
               MOVE 8 TO LAGEP-RC
               GOBACK.

       0225-VALIDATE-DAY2.
           MOVE LAGEP-DATE2-Y TO CURRENT-YEAR.
           MOVE LAGEP-DATE2-M TO CURRENT-MONTH.
           PERFORM 0375-FIND-MAX-DAYS-FOR-MONTH.
           IF LAGEP-DATE2-D IS LESS THAN 01
           OR LAGEP-DATE2-D IS GREATER THAN DAYS-IN-CURRENT-MONTH
               MOVE 9 TO LAGEP-RC
               GOBACK.

       0250-LOAD-FROM-AND-TO-FIELDS.
            EVALUATE TRUE
               WHEN LAGEP-DATE1-Y > LAGEP-DATE2-Y
                   MOVE LAGEP-DATE2 TO FROM-DATE
                   MOVE LAGEP-DATE1 TO TO-DATE
               WHEN LAGEP-DATE1-Y < LAGEP-DATE2-Y
                   MOVE LAGEP-DATE1 TO FROM-DATE
                   MOVE LAGEP-DATE2 TO TO-DATE
               WHEN OTHER
                    EVALUATE TRUE
                       WHEN LAGEP-DATE1-M > LAGEP-DATE2-M
                           MOVE LAGEP-DATE2 TO FROM-DATE
                           MOVE LAGEP-DATE1 TO TO-DATE
                       WHEN LAGEP-DATE1-M < LAGEP-DATE2-M
                           MOVE LAGEP-DATE1 TO FROM-DATE
                           MOVE LAGEP-DATE2 TO TO-DATE
                       WHEN OTHER
                            EVALUATE TRUE
                               WHEN LAGEP-DATE1-D > LAGEP-DATE2-D
                                   MOVE LAGEP-DATE2 TO FROM-DATE
                                   MOVE LAGEP-DATE1 TO TO-DATE
                               WHEN LAGEP-DATE1-D < LAGEP-DATE2-D
                                   MOVE LAGEP-DATE1 TO FROM-DATE
                                   MOVE LAGEP-DATE2 TO TO-DATE
                           END-EVALUATE
                   END-EVALUATE
           END-EVALUATE.

       0275-BEGIN-COMPUTATION.
           MOVE ZERO TO MONTH-COUNT.

           IF (FROM-Y EQUAL TO-Y) AND (FROM-M EQUAL TO-M)
               SUBTRACT FROM-D FROM TO-D GIVING LAGEP-DAYS
               GOBACK.

           MOVE FROM-Y TO CURRENT-YEAR.
           MOVE FROM-M TO CURRENT-MONTH.
           PERFORM 0375-FIND-MAX-DAYS-FOR-MONTH.
           MOVE DAYS-IN-CURRENT-MONTH TO DAYS-IN-FROM-MONTH.
           MOVE FROM-D TO ORIGINAL-FROM-D.

       0300-COUNT-MONTHS-BETWEEN-DATES.
           PERFORM UNTIL FROM-Y EQUAL TO-Y
               ADD 1 TO MONTH-COUNT
               ADD 1 TO FROM-M
               IF FROM-M GREATER THAN 12
                   SUBTRACT 12 FROM FROM-M
                   ADD 1 TO FROM-Y
               END-IF
           END-PERFORM.

           PERFORM UNTIL FROM-M EQUAL TO-M
               ADD 1 TO MONTH-COUNT
               ADD 1 TO FROM-M
           END-PERFORM.

           MOVE FROM-Y TO CURRENT-YEAR.
           MOVE FROM-M TO CURRENT-MONTH.
           PERFORM 0375-FIND-MAX-DAYS-FOR-MONTH.

           SUBTRACT DAYS-IN-CURRENT-MONTH FROM TO-D GIVING WORK-A.
           IF WORK-A IS NEGATIVE
               GO TO 0325-CORRECT-OVERSHOOT.

           ADD WORK-A TO DAYS-IN-FROM-MONTH GIVING TO-D.
           SUBTRACT ORIGINAL-FROM-D FROM TO-D.
           GO TO 0350-FINISH-CALCULATION.

       0325-CORRECT-OVERSHOOT.
           SUBTRACT 1 FROM MONTH-COUNT.
           ADD DAYS-IN-FROM-MONTH TO TO-D.
           SUBTRACT ORIGINAL-FROM-D FROM TO-D.

           IF TO-D LESS THAN DAYS-IN-FROM-MONTH
               GO TO 0350-FINISH-CALCULATION.

           IF ORIGINAL-FROM-D NOT LESS THAN 29
               GO TO 0350-FINISH-CALCULATION.

           SUBTRACT DAYS-IN-FROM-MONTH FROM TO-D.
           ADD 1 TO MONTH-COUNT.

       0350-FINISH-CALCULATION.
           DIVIDE MONTH-COUNT BY 12 GIVING LAGEP-YEARS
                                    REMAINDER LAGEP-MONTHS.
      *    IF TO-D > 0
      *        SUBTRACT 1 FROM TO-D GIVING LAGEP-DAYS
      *    ELSE
               MOVE TO-D TO LAGEP-DAYS.

           GOBACK.

       0375-FIND-MAX-DAYS-FOR-MONTH.
           MOVE ZERO TO LEAP-YEAR-SWITCH.
           DIVIDE CURRENT-YEAR BY 4 GIVING WORK-A
                                    REMAINDER WORK-B.
           IF WORK-B NOT EQUAL ZERO
               NEXT SENTENCE
           ELSE
               DIVIDE CURRENT-YEAR BY 100 GIVING WORK-A
                                          REMAINDER WORK-B
               IF WORK-B NOT EQUAL ZERO
                   MOVE 1 TO LEAP-YEAR-SWITCH
               ELSE
                   DIVIDE CURRENT-YEAR BY 400 GIVING WORK-A
                                              REMAINDER WORK-B
                   IF WORK-B NOT EQUAL ZERO
                       NEXT SENTENCE
                   ELSE
                       MOVE 1 TO LEAP-YEAR-SWITCH
                   END-IF
               END-IF
           END-IF.

           IF IS-LEAP-YEAR
               MOVE DIM-LEAP (CURRENT-MONTH) TO DAYS-IN-CURRENT-MONTH
           ELSE
               MOVE DIM-NORMAL (CURRENT-MONTH)
                    TO DAYS-IN-CURRENT-MONTH
           END-IF.

       END PROGRAM Y2KLAGE.
