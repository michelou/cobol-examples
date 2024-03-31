       IDENTIFICATION DIVISION.
       PROGRAM-ID. Y2KDOWN.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. DECEMBER, 1997.
       DATE-COMPILED.
      *                                                               *
      *   YY   YY   222   KK   KK DDDDDD   OOOOO  WW   WW NN   NN     *
      *   YY   YY  2   2  KK  KK  DD   DD OO   OO WW   WW NNN  NN     *
      *   YY   YY      2  KK KK   DD   DD OO   OO WW   WW NNN  NN     *
      *    YY YY       2  KKKK    DD   DD OO   OO WW   WW NNNN NN     *
      *     YYY     222   KKKK    DD   DD OO   OO WW W WW NN NNNN     *
      *     YY     2      KK KK   DD   DD OO   OO WWWWWWW NN  NNN     *
      *     YY     2      KK  KK  DD   DD OO   OO WWW WWW NN  NNN     *
      *     YY     22222  KK   KK DDDDDD   OOOOO   W   W  NN   NN     *
      *                                                               *
      *************************************************************** *
      * THIS SUBROUTINE DETERMINES THE INTEGRAL NUMBER OF THE DAY     *
      * OF THE WEEK A GREGORIAN DATE (MMDDYYYY) FALLS ON              *
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
      *  1)  AN 8 BYTE FIELD CONTAINING THE DATE (IN ZONED-DECIMAL    *
      *      FORMAT) TO BE PROCESSED,                                 *
      *  2)  A 1 BYTE ZONED-DECIMAL RETURN CODE, AND                  *
      *  3)  A 1 BYTE FIELD IN WHICH THE INTEGRAL DAY NUMBER (IN      *
      *      ZONED-DECIMAL FORMAT) WILL BE PLACED.                    *
      *  THE ROUTINE WILL NOT ALTER THE YEAR PASSED TO IT.            *
      *                                                               *
      *  SUGGESTED CALLING SYNTAX FOR COBOL CALLERS:                  *
      *                                                               *
      *       01  DOWN-PARAMETERS.                                    *
      *           02  DATE PIC 9(8) VALUE 04151997.                   *
      *           02  RC   PIC 9(1) VALUE 0.                          *
      *           02  DOWN PIC 9(1) VALUE 0.                          *
      *       CALL 'Y2KDOWN' USING DOWN-PARAMETERS.                   *
      *                                                               *
      *  THE POSSIBLE VALUES FOR THE RETURN CODE FIELD ARE:           *
      *                                                               *
      *  0 INDICATES SUCCESSFUL EXECUTION OF THE ROUTINE.             *
      *                                                               *
      *  2 INDICATES INVALID DATA WAS FOUND IN THE DATE FIELD.        *
      *  INVALID DATA ARE DETERMINED IF THE FIELD'S LOW ORDER         *
      *  BYTE'S ZONE CONTAINS AN INVALID SIGN, (NOT ONE OF            *
      *  X'C', X'A', X'E', OR X'F'), OR IF THE PRECEDING BYTES'       *
      *  ZONES ARE OTHER THAN X'F', OR IF ANY BYTE'S LOW ORDER        *
      *  NIBBLE CONTAINS A VALUE GREATER THAN X'9'.                   *
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
      *  UPON SUCCESSFUL EXECUTION, THE INTEGRAL DAY NUMBER ON WHICH  *
      *  THE INPUT DATE FALLS (0=MONDAY, 1=TUESDAY, ... 6=SUNDAY)     *
      *  WILL BE PLACED IN THE 3RD PARAMETER FIELD.  IF THE RETURN    *
      *  CODE IS A NON-ZERO VALUE (UNSUCCESSFUL EXECUTION), THE       *
      *  3RD PARAMETER FIELD WILL CONTAIN THE VALUE OF 9 TO PROTECT   *
      *  AGAINST UNINTENTIONAL USE OF AN INVALID RESULT.              *
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

       LOCAL-STORAGE SECTION.

       01  FILLER                      PIC X(24)
           VALUE 'Y2KDOWN STORAGE BEGINS->'.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND WORK FIELDS.           *
      * ************************************************************* *
       01  PROGRAM-CONTROL-FIELDS.
           02  GTOA-PARAMETERS.
               03  GTOAP-DATEG         PIC 9(8).
               03  GTOAP-RC            PIC 9(1).
               03  GTOAP-ANUM          PIC S9(7).

       01  FILLER                      PIC X(24)
           VALUE '<-Y2KDOWN STORAGE ENDS'.

       LINKAGE SECTION.

      * ************************************************************* *
      * THESE ARE THE FIELDS USED TO RECEIVE INPUT DATA FROM THE      *
      * CALLER AND PASS RESULT FIELDS BACK TO THE CALLER.             *
      * ************************************************************* *
       01  DOWN-PARAMETERS.
           02  DOWNP-DATEG             PIC 9(8).
           02  DOWNP-RC                PIC 9(1).
           02  DOWNP-DAYNUMBER         PIC 9(1).
      /
       PROCEDURE DIVISION USING DOWN-PARAMETERS.

       0000-MAIN SECTION.
       0025-INITIALIZE.
           MOVE ZERO TO DOWNP-RC.
           MOVE 9 TO DOWNP-DAYNUMBER.

       0050-RETRIEVE-ASTRO-NUMBER.
           MOVE DOWNP-DATEG TO GTOAP-DATEG.
           CALL 'Y2KGTOA' USING GTOA-PARAMETERS.

           IF GTOAP-RC NOT EQUAL '0' THEN
               MOVE GTOAP-RC TO DOWNP-RC
               GOBACK.

       0075-COMPUTE-DAY-NUMBER.
           DIVIDE GTOAP-ANUM BY 7 GIVING GTOAP-ANUM
                                  REMAINDER DOWNP-DAYNUMBER.

           GOBACK.

       END PROGRAM Y2KDOWN.
