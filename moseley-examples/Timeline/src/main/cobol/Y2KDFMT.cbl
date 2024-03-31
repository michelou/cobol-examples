       IDENTIFICATION DIVISION.
       PROGRAM-ID. Y2KDFMT.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. DECEMBER, 1997.
       DATE-COMPILED.
      *                                                               *
      *   YY   YY   222   KK   KK DDDDDD  FFFFFFF M     MTTTTTTTT     *
      *   YY   YY  2   2  KK  KK  DD   DD FF      MM   MM   TT        *
      *   YY   YY      2  KK KK   DD   DD FF      MMM MMM   TT        *
      *    YY YY       2  KKKK    DD   DD FFFFF   MMMMMMM   TT        *
      *     YYY     222   KKKK    DD   DD FF      MM M MM   TT        *
      *     YY     2      KK KK   DD   DD FF      MM   MM   TT        *
      *     YY     2      KK  KK  DD   DD FF      MM   MM   TT        *
      *     YY     22222  KK   KK DDDDDD  FF      MM   MM   TT        *
      *                                                               *
      *************************************************************** *
      * THIS SUBROUTINE FORMATS A GIVEN GREGORIAN DATE (MMDDYYYY) IN  *
      * LARGE VARIETY OF FORMATS                                      *
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
      *  THE MAJOR FORMAT CODE DETERMINES THE OVERALL OUTPUT FORMAT   *
      *  AND THE MINOR FORMAT CODE DETERMINES SUBTLE VARIATIONS OF    *
      *  THE FORMAT:                                                  *
      *    MAJOR                                  MINOR               *
      *      1 = COMMERCIAL (01051997)              1 = NO INSERT     *
      *      1 = COMMERCIAL (01 05 1997)            2 = SPACE         *
      *      2 = EUROPEAN   (05/01/1997)            3 = SLASH         *
      *      2 = EUROPEAN   (05-01-1997)            4 = HYPHEN        *
      *      3 = F.I.P.S.   (YYYY.MM.DD)            5 = PERIOD        *
      *    ---------------------------------------------------------  *
      *      4 = TEXT 1 (JANUARY 5, 1997)           1 = U.S.          *
      *      4 = TEXT 1 (5 JANUARY 1997)            2 = EUROPEAN      *
      *      5 = TEXT 2 (JAN 5, 1997)               1 = U.S.          *
      *      6 = TEXT 3 (SUNDAY, JANUARY 5, 1997)   1 = U.S.          *
      *      7 = TEXT 4 (SUNDAY, 5 JAN 1997)        2 = EUROPEAN      *
      *      8 = TEXT 5 (SUN, JANUARY 5, 1997)      1 = U.S.          *
      *      9 = TEXT 6 (SUN, 5 JAN 1997)           2 = EUROPEAN      *
      *                                                               *
      *  SIX FIELDS ARE PASSED AS PARAMETERS TO THE ROUTINE:          *
      *  1)  AN 8 BYTE FIELD CONTAINING THE DATE (IN ZONED-           *
      *      DECIMAL FORMAT) TO BE FORMATTED,                         *
      *  2)  A 1 BYTE ZONED-DECIMAL MAJOR FORMAT CODE (SEE 5 ABOVE),  *
      *  3)  A 1 BYTE ZONED-DECIMAL MINOR FORMAT CODE (SEE 5 ABOVE),  *
      *  4)  A 1 BYTE ZONED-DECIMAL RETURN CODE,                      *
      *  5)  A 2 BYTE FIELD WHICH WILL CONTAIN THE NUMBER OF BYTES    *
      *      PLACED IN THE FOLLOWING FIELD (IN ZONED DECIMAL FORMAT), *
      *  6)  AN OUTPUT FIELD TO RECEIVE THE FORMATTED DATE (NOTE:     *
      *      THE SIZE OF THIS FIELD DEPENDS UPON THE FORMATTING       *
      *      CODES ... IT IS THE CALLER'S RESPONSIBILITY TO PASS A    *
      *      FIELD LONG ENOUGH TO RECEIVE THE REQUESTED FORMAT).      *
      *  THE ROUTINE WILL NOT ALTER THE DATE OR FORMAT CODE FIELDS.   *
      *                                                               *
      *  SUGGESTED CALLING SYNTAX FOR COBOL CALLERS:                  *
      *                                                               *
      *       01  DFMT-PARAMETERS.                                    *
      *           02  DATEIN  PIC 9(8) VALUE 01011997.                *
      *           02  MAJOR   PIC 9(1) VALUE 1.
      *           02  MINOR   PIC 9(1) VALUE 3.
      *           02  RC      PIC 9(1) VALUE 0.                       *
      *           02  OUTSIZE PIC 9(2) VALUE 0.                       *
      *           02  OUTDATE PIC X(29).                              *
      *       CALL 'Y2KDFMT' USING DFMT-PARAMETERS.                   *
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
      *  3 INDICATES THE VALUE OF THE MAJOR FORMAT CODE SPECIFIED     *
      *  WAS NOT ONE OF THE ACCEPTABLE VALUES (SEE 5 ABOVE).          *
      *                                                               *
      *  5 INDICATES THE VALUE OF THE MINOR FORMAT CODE SPECIFIED     *
      *  WAS NOT ONE OF THE ACCEPTABLE VALUES FOR THE MAJOR FORMAT    *
      *  CODE SPECIFIED (SEE 5 ABOVE).                                *
      *                                                               *
      *  UPON SUCCESSFUL EXECUTION, THE FORMATTED DATE WILL BE        *
      *  PLACED IN THE 6TH PARAMETER FIELD AND THE NUMBER OF BYTES    *
      *  PLACED IN THE FIELD WILL BE PLACED IN THE 5TH FIELD.  IF     *
      *  THE RETURN CODE CONTAINS A NON-ZERO VALUE (UNSUCCESSFUL      *
      *  EXECUTION), THE 6TH FIELD WILL NOT BE MODIFIED BY THE        *
      *  ROUTINE AND THE 5TH FIELD WILL CONTAIN ZERO.                 *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      * 18 Sep 2017 - Change from INITIAL to LOCAL-STORAGE            *
      *               This change has no affect on program execution. *
      *               It was made to accomodate a bug in GnuCOBOL 2.2 *
      *               which will be fixed, but the change makes sense *
      *               from a logical viewpoint, so thus is justified. *
      *                                                               *
      *             - Change case of constants for names of days and  *
      *               months from UPPER to Mixed. These programs were *
      *               originally written for the IBM Mainframe envir- *
      *               onment, and Mixed case works better for the     *
      *               current target environments.                    *
      *                                                               *
      *************************************************************** *
      /
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * ************************************************************* *
      * THESE ARE CONSTANTS WHICH SHOULD NOT CHANGE.                  *
      * ************************************************************* *
       01  PROGRAM-CONSTANT-FIELDS.
           02  DAY-NAMES-INIT.
               03  FILLER PIC X(10) VALUE '6Monday   '.
               03  FILLER PIC X(10) VALUE '7Tuesday  '.
               03  FILLER PIC X(10) VALUE '9Wednesday'.
               03  FILLER PIC X(10) VALUE '8Thursday '.
               03  FILLER PIC X(10) VALUE '6Friday   '.
               03  FILLER PIC X(10) VALUE '8Saturday '.
               03  FILLER PIC X(10) VALUE '6Sunday   '.
           02  DAY-NAMES-TABLE         REDEFINES DAY-NAMES-INIT.
               03  DAY-NAME-ENTRY      OCCURS 7 TIMES.
                   05  DAY-NAME-LENGTH PIC 9(1).
                   05  DAY-NAME        PIC X(9).

           02  MONTH-NAMES-INIT.
               03  FILLER PIC X(10) VALUE '7January  '.
               03  FILLER PIC X(10) VALUE '8February '.
               03  FILLER PIC X(10) VALUE '5March    '.
               03  FILLER PIC X(10) VALUE '5April    '.
               03  FILLER PIC X(10) VALUE '3May      '.
               03  FILLER PIC X(10) VALUE '4June     '.
               03  FILLER PIC X(10) VALUE '4July     '.
               03  FILLER PIC X(10) VALUE '6August   '.
               03  FILLER PIC X(10) VALUE '9September'.
               03  FILLER PIC X(10) VALUE '7October  '.
               03  FILLER PIC X(10) VALUE '8November '.
               03  FILLER PIC X(10) VALUE '8December '.
           02  MONTH-NAMES-TABLE       REDEFINES MONTH-NAMES-INIT.
               03  MONTH-NAME-ENTRY    OCCURS 12 TIMES.
                   05  MONTH-NAME-LENGTH
                                       PIC 9(1).
                   05  MONTH-NAME      PIC X(9).

       LOCAL-STORAGE SECTION.

       01  FILLER                      PIC X(24)
           VALUE 'Y2KDFMT STORAGE BEGINS->'.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND WORK FIELDS.           *
      * ************************************************************* *
       01  PROGRAM-CONTROL-FIELDS.
           02  SEPARATOR-CHARACTER     PIC X.
           02  INDEX-1                 PIC 9(2) COMP.
           02  INDEX-2                 PIC 9(2) COMP.


           02  DOWN-PARAMETERS.
               03  DOWNP-DATEG         PIC 9(8).
               03  DOWNP-RC            PIC 9(1).
               03  DOWNP-DAYNUMBER     PIC 9(1).

       01  FILLER                      PIC X(24)
           VALUE '<-Y2KDFMT STORAGE ENDS'.

       LINKAGE SECTION.

      * ************************************************************* *
      * THESE ARE THE FIELDS USED TO RECEIVE INPUT DATA FROM THE      *
      * CALLER AND PASS RESULT FIELDS BACK TO THE CALLER.             *
      * ************************************************************* *
       01  DFMT-PARAMETERS.
           02  DFMTP-DATEG.
               03  DFMTP-DATEG-MM      PIC 9(2).
               03  DFMTP-DATEG-DD      PIC 9(2).
               03  FILLER              REDEFINES DFMTP-DATEG-DD.
                   05  DFMTP-DATEG-D1  PIC 9(1).
                   05  DFMTP-DATEG-D2  PIC 9(1).
               03  DFMTP-DATEG-YYYY    PIC 9(4).
               03  DFMTP-MAJOR         PIC 9(1).
               03  DFMTP-MINOR         PIC 9(1).
               03  DFMTP-RC            PIC 9(1).
               03  DFMTP-OUTSIZE       PIC S9(2).
               03  DFMTP-OUTPUT        PIC X(29).
      /
       PROCEDURE DIVISION USING DFMT-PARAMETERS.

       0000-MAIN SECTION.
       0025-INITIALIZE.
           MOVE ZERO TO DFMTP-RC, DFMTP-OUTSIZE.

       0050-GET-DAY-NUMBER.
           MOVE DFMTP-DATEG TO DOWNP-DATEG.
           CALL 'Y2KDOWN' USING DOWN-PARAMETERS.
           IF DOWNP-RC NOT EQUAL '0'
               MOVE DOWNP-RC TO DFMTP-RC
               GOBACK.

           ADD 1 TO DOWNP-DAYNUMBER.

       0075-VALIDATE-MINOR-CODE.
           EVALUATE DFMTP-MAJOR
               WHEN 1 THROUGH 3
                   IF DFMTP-MINOR IS LESS THAN 1
                   OR DFMTP-MINOR IS GREATER THAN 5
                       MOVE 5 TO DFMTP-RC
                       GOBACK
                   END-IF
               WHEN 4 THROUGH 9
                   IF DFMTP-MINOR IS LESS THAN 1
                   OR DFMTP-MINOR IS GREATER THAN 2
                       MOVE 5 TO DFMTP-RC
                       GOBACK
                   END-IF
           END-EVALUATE.

       0090-VALIDATE-MAJOR-CODE.
           EVALUATE DFMTP-MAJOR
               WHEN 1
                   GO TO 0100-COMMERCIAL
               WHEN 2
                   GO TO 0125-EUROPEAN
               WHEN 3
                   GO TO 0150-FIPS
               WHEN 4
                   GO TO 0175-TEXT1
               WHEN 5
                   GO TO 0200-TEXT2
               WHEN 6
                   GO TO 0225-TEXT3
               WHEN 7
                   GO TO 0250-TEXT4
               WHEN 8
                   GO TO 0275-TEXT5
               WHEN 9
                   GO TO 0300-TEXT6
               WHEN OTHER
                   MOVE 3 TO DFMTP-RC
                   GOBACK.

       0100-COMMERCIAL.
           PERFORM 0400-SELECT-SEPARATOR.
           MOVE 1 TO INDEX-1.
           MOVE DFMTP-DATEG-MM TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-DD TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0125-EUROPEAN.
           PERFORM 0400-SELECT-SEPARATOR.
           MOVE 1 TO INDEX-1.
           MOVE DFMTP-DATEG-DD TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-MM TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0150-FIPS.
           PERFORM 0400-SELECT-SEPARATOR.
           MOVE 1 TO INDEX-1.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-MM TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           IF SEPARATOR-CHARACTER NOT EQUAL '#'
               MOVE SEPARATOR-CHARACTER TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 1 TO INDEX-1.
           MOVE DFMTP-DATEG-DD TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.
           GO TO 0325-FINISH.

       0175-TEXT1.
           MOVE 1 TO INDEX-1.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0500-MOVE-MONTH-NAME
           ELSE
               PERFORM 0500-MOVE-MONTH-NAME
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0200-TEXT2.
           MOVE 1 TO INDEX-1.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0600-MOVE-MONTH-ABBREV
           ELSE
               PERFORM 0600-MOVE-MONTH-ABBREV
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0225-TEXT3.
           MOVE 1 TO INDEX-1.
           PERFORM 0700-MOVE-DAY-NAME.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0500-MOVE-MONTH-NAME
           ELSE
               PERFORM 0500-MOVE-MONTH-NAME
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0250-TEXT4.
           MOVE 1 TO INDEX-1.
           PERFORM 0700-MOVE-DAY-NAME.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0600-MOVE-MONTH-ABBREV
           ELSE
               PERFORM 0600-MOVE-MONTH-ABBREV
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0275-TEXT5.
           MOVE 1 TO INDEX-1.
           PERFORM 0800-MOVE-DAY-ABBREV.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0500-MOVE-MONTH-NAME
           ELSE
               PERFORM 0500-MOVE-MONTH-NAME
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0300-TEXT6.
           MOVE 1 TO INDEX-1.
           PERFORM 0800-MOVE-DAY-ABBREV.
           IF DFMTP-MINOR EQUAL '2'
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
               PERFORM 0600-MOVE-MONTH-ABBREV
           ELSE
               PERFORM 0600-MOVE-MONTH-ABBREV
               PERFORM 0900-MOVE-DAY-NUMBER
               MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1
           END-IF.
           MOVE DFMTP-DATEG-YYYY TO DFMTP-OUTPUT(INDEX-1 : 4).
           ADD 4 TO INDEX-1.
           GO TO 0325-FINISH.

       0325-FINISH.
           SUBTRACT 1 FROM INDEX-1 GIVING DFMTP-OUTSIZE.

           GOBACK.

       0400-SELECT-SEPARATOR.
           EVALUATE DFMTP-MINOR
               WHEN 1
                   MOVE '#' TO SEPARATOR-CHARACTER
               WHEN 2
                   MOVE ' ' TO SEPARATOR-CHARACTER
               WHEN 3
                   MOVE '/' TO SEPARATOR-CHARACTER
               WHEN 4
                   MOVE '-' TO SEPARATOR-CHARACTER
               WHEN 5
                   MOVE '.' TO SEPARATOR-CHARACTER
           END-EVALUATE.

       0500-MOVE-MONTH-NAME.
           MOVE MONTH-NAME-LENGTH(DFMTP-DATEG-MM) TO INDEX-2.
           MOVE MONTH-NAME(DFMTP-DATEG-MM)
             TO DFMTP-OUTPUT(INDEX-1 : INDEX-2).
           ADD INDEX-2 TO INDEX-1.
           MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 1 TO INDEX-1.

       0600-MOVE-MONTH-ABBREV.
           MOVE MONTH-NAME(DFMTP-DATEG-MM)
             TO DFMTP-OUTPUT(INDEX-1 : 3).
           ADD 3 TO INDEX-1.
           MOVE ' ' TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 1 TO INDEX-1.

       0700-MOVE-DAY-NAME.
           MOVE DAY-NAME-LENGTH(DOWNP-DAYNUMBER) TO INDEX-2.
           MOVE DAY-NAME(DOWNP-DAYNUMBER)
             TO DFMTP-OUTPUT(INDEX-1 : INDEX-2).
           ADD INDEX-2 TO INDEX-1.
           MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.

       0800-MOVE-DAY-ABBREV.
           MOVE DAY-NAME(DOWNP-DAYNUMBER)
             TO DFMTP-OUTPUT(INDEX-1 : 3).
           ADD 3 TO INDEX-1.
           MOVE ', ' TO DFMTP-OUTPUT(INDEX-1 : 2).
           ADD 2 TO INDEX-1.

       0900-MOVE-DAY-NUMBER.
           IF DFMTP-DATEG-D1 EQUAL ZERO
               MOVE DFMTP-DATEG-D2 TO DFMTP-OUTPUT(INDEX-1 : 1)
               ADD 1 TO INDEX-1
           ELSE
               MOVE DFMTP-DATEG-DD TO DFMTP-OUTPUT(INDEX-1 : 2)
               ADD 2 TO INDEX-1.

       END PROGRAM Y2KDFMT.
