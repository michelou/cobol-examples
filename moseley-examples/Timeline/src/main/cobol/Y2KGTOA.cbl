       IDENTIFICATION DIVISION.
       PROGRAM-ID. Y2KGTOA.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. DECEMBER, 1997.
       DATE-COMPILED.
      *                                                               *
      *   YY   YY   222   KK   KK  GGGGG TTTTTTTT  OOOOO     A        *
      *   YY   YY  2   2  KK  KK  GG   GG   TT    OO   OO   AAA       *
      *   YY   YY      2  KK KK   GG   GG   TT    OO   OO  AA AA      *
      *    YY YY       2  KKKK    GG        TT    OO   OO AA   AA     *
      *     YYY     222   KKKK    GG  GGG   TT    OO   OO AA   AA     *
      *     YY     2      KK KK   GG   GG   TT    OO   OO AAAAAAA     *
      *     YY     2      KK  KK  GG   GG   TT    OO   OO AA   AA     *
      *     YY     22222  KK   KK  GGGGG    TT     OOOOO  AA   AA     *
      *                                                               *
      * THIS SUBROUTINE COMPUTES THE ASTRONOMICAL NUMBER FOR A        *
      * GIVEN GREGORIAN DATE (MMDDYYYY)                               *
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
      *  THREE FIELDS ARE PASSED AS PARAMETERS TO THE ROUTINE:        *
      *  1)  AN 8 BYTE FIELD CONTAINING THE DATE (IN ZONED-           *
      *      DECIMAL FORMAT) FOR WHICH THE ASTRONOMICAL NUMBER IS     *
      *      TO BE CALCULATED,                                        *
      *  2)  A 1 BYTE ZONED-DECIMAL RETURN CODE, AND                  *
      *  3)  A 7 BYTE FIELD WHICH WILL RECEIVE THE ASTRONOMICAL       *
      *      NUMBER (IN ZONED-DECIMAL FORMAT).                        *
      *  THE ROUTINE WILL NOT ALTER THE DATE PASSED TO IT.            *
      *                                                               *
      *  SUGGESTED CALLING SYNTAX FOR COBOL CALLERS:                  *
      *                                                               *
      *       01  GTOA-PARAMETERS.                                    *
      *           02  DATE PIC 9(8) VALUE 01011997.                   *
      *           02  RC   PIC 9(1) VALUE 0.                          *
      *           02  ANUM PIC 9(7) VALUE 0.                          *
      *       CALL 'Y2KGTOA' USING GTOA-PARAMETERS.                   *
      *                                                               *
      *  THE POSSIBLE VALUES FOR THE RETURN CODE FIELD ARE:           *
      *                                                               *
      *  0 INDICATES SUCCESSFUL EXECUTION OF THE ROUTINE.             *
      *                                                               *
      *  2 INDICATES INVALID DATA WAS FOUND IN THE DATE FIELD.        *
      *  INVALID DATA ARE DETERMINED IF THE FIELD'S LOW ORDER         *
      *  BYTE'S ZONE CONTAINS AN INVALID SIGN, (NOT ONE OF            *
      *  X'C', X'D' OR X'F'), OR IF THE PRECEDING BYTES' ZONES        *
      *  ARE OTHER THAN X'F', OR IF ANY BYTE'S LOW ORDER NIBBLE       *
      *  CONTAINS A VALUE GREATER THAN X'9'.                          *
      *                                                               *
      *  4 INDICATES THE VALUE OF THE YEAR SPECIFIED WAS NOT IN       *
      *  THE RANGE SPECIFIED IN 1 (ABOVE).                            *
      *                                                               *
      *  6 INDICATES THE VALUE OF THE MONTH SPECIFIED WAS NOT IN      *
      *  THE RANGE SPECIFIED IN 2 (ABOVE).                            *
      *                                                               *
      *  8 INDICATES THE VALUE OF THE DAY SPECIFIED WAS NOT IN        *
      *  THE RANGE SPECIFIED IN 3 (ABOVE).                            *
      *                                                               *
      *  UPON SUCCESSFUL EXECUTION, THE ASTRONOMICAL NUMBER FOR THE   *
      *  DATE WILL BE PLACED IN THE 3RD PARAMETER FIELD (ANUM).  IF   *
      *  THE RETURN CODE IS A NON-ZERO VALUE (UNSUCCESSFUL            *
      *  EXECUTION), THE ANUM FIELD WILL CONTAIN ZEROS.               *
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
           VALUE 'Y2KGTOA STORAGE BEGINS->'.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND WORK FIELDS.           *
      * ************************************************************* *
       01  PROGRAM-CONTROL-FIELDS.
           02  INPUT-M                 PIC 9(2).
           02  INPUT-D                 PIC 9(2).
           02  INPUT-Y                 PIC 9(4).
           02  WORK-A                  PIC S9(15) COMP-3.
           02  WORK-B                  PIC S9(15) COMP-3.
           02  WORK-C                  PIC S9(15) COMP-3.
           02  LEAP-YEAR-SWITCH        PIC 9(1).
               88  IS-LEAP-YEAR        VALUE 1.
           02  DAYS-IN-CURRENT-MONTH   PIC 9(2).

       01  FILLER                      PIC X(24)
           VALUE '<-Y2KGTOA STORAGE ENDS'.

       LINKAGE SECTION.

      * ************************************************************* *
      * THESE ARE THE FIELDS USED TO RECEIVE INPUT DATA FROM THE      *
      * CALLER AND PASS RESULT FIELDS BACK TO THE CALLER.             *
      * ************************************************************* *
       01  GTOA-PARAMETERS.
           02  GTOAP-DATE-G.
               03  GTOAP-DATE-G-M      PIC 9(2).
               03  GTOAP-DATE-G-D      PIC 9(2).
               03  GTOAP-DATE-G-Y      PIC 9(4).
           02  GTOAP-RETURN-CODE       PIC 9(1).
           02  GTOAP-ANUM              PIC S9(7).

      /
       PROCEDURE DIVISION USING GTOA-PARAMETERS.

       0000-MAIN SECTION.
       0050-INITIALIZE.

           MOVE ZERO TO GTOAP-RETURN-CODE.
           MOVE +0 TO GTOAP-ANUM.

       0075-VALIDATE-INPUT-DATA.
           IF GTOAP-DATE-G IS NOT NUMERIC
               MOVE 2 TO GTOAP-RETURN-CODE
               GOBACK.

           IF GTOAP-DATE-G-Y < 1601
           OR GTOAP-DATE-G-Y > 3399
               MOVE 4 TO GTOAP-RETURN-CODE
               GOBACK.

           IF GTOAP-DATE-G-M < 01
           OR GTOAP-DATE-G-M > 12
               MOVE 6 TO GTOAP-RETURN-CODE
               GOBACK.

           MOVE ZERO TO LEAP-YEAR-SWITCH.
           DIVIDE GTOAP-DATE-G-Y BY 4 GIVING WORK-A
                                      REMAINDER WORK-B.
           IF WORK-B NOT EQUAL ZERO
               NEXT SENTENCE
           ELSE
               DIVIDE GTOAP-DATE-G-Y BY 100 GIVING WORK-A
                                            REMAINDER WORK-B
               IF WORK-B NOT EQUAL ZERO
                   MOVE 1 TO LEAP-YEAR-SWITCH
               ELSE
                   DIVIDE GTOAP-DATE-G-Y BY 400 GIVING WORK-A
                                                REMAINDER WORK-B
                   IF WORK-B NOT EQUAL ZERO
                       NEXT SENTENCE
                   ELSE
                       MOVE 1 TO LEAP-YEAR-SWITCH
                   END-IF
               END-IF
           END-IF.

           IF IS-LEAP-YEAR
               MOVE DIM-LEAP (GTOAP-DATE-G-M) TO DAYS-IN-CURRENT-MONTH
           ELSE
               MOVE DIM-NORMAL (GTOAP-DATE-G-M)
                    TO DAYS-IN-CURRENT-MONTH.

           IF GTOAP-DATE-G-D < 01
           OR GTOAP-DATE-G-D > DAYS-IN-CURRENT-MONTH
               MOVE 8 TO GTOAP-RETURN-CODE
               GOBACK.

       0100-COMPUTE-ASTRO.

           MOVE GTOAP-DATE-G-M TO INPUT-M.
           MOVE GTOAP-DATE-G-D TO INPUT-D.
           MOVE GTOAP-DATE-G-Y TO INPUT-Y.

           IF INPUT-M GREATER THAN 2
               SUBTRACT 3 FROM INPUT-M
           ELSE
               ADD 9 TO INPUT-M
               SUBTRACT 1 FROM INPUT-Y.

           DIVIDE INPUT-Y BY 100 GIVING WORK-A REMAINDER WORK-B.
           MULTIPLY WORK-A BY 146097 GIVING WORK-A.
           DIVIDE WORK-A BY 4 GIVING WORK-A.
           MULTIPLY WORK-B BY 1461 GIVING WORK-B.
           DIVIDE WORK-B BY 4 GIVING WORK-B.
           MOVE INPUT-M TO WORK-C.
           MULTIPLY WORK-C BY 153 GIVING WORK-C.
           ADD 2 TO WORK-C.
           DIVIDE WORK-C BY 5 GIVING WORK-C.
           ADD INPUT-D TO WORK-C.
           ADD 1721119 TO WORK-C.
           MOVE WORK-A TO GTOAP-ANUM.
           ADD WORK-B TO GTOAP-ANUM.
           ADD WORK-C TO GTOAP-ANUM.
           GOBACK.

       END PROGRAM Y2KGTOA.
