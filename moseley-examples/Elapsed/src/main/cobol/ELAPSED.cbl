       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELAPSED.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. APRIL, 2016.
       DATE-COMPILED.
      * ************************************************************* *
      * THE PURPOSE OF THIS PROGRAM IS TO COMPUTE THE PERIOD OF TIME  *
      * ELAPSED BETWEEN TWO DATES ENTERED AS AN ARGUMENT.  IF ONLY A  *
      * SINGLE DATE IS ENTERED, THE CURRENT SYSTEM DATE IS USED FOR   *
      * THE SECOND DATE. IF THE DATE ENTERED CONSISTS ONLY OF A YEAR  *
      * (IE. 1994, OR 1994 2016) THE RESULT RETURNED WILL BE THE NUM- *
      * BER OF YEARS BETWEEN THE TWO.  IF THE DATE ENTERED CONSISTS   *
      * OF A MONTH AND YEAR (IE. 04/1994, OR 12/1994 03/2015) THE     *
      * RESULT RETURNED WILL BE THE NUMBER OF YEARS AND MONTHS BET-   *
      * WEEN THE LAST DAY OF EACH OF THE MONTHS SPECIFIED.            *
      *---------------------------------------------------------------*
      *                                                               *
      * 24 May 2018 - Modified argument retrieval logic to use        *
      *               GnuCOBOL DISPLAY/ACCEPT to retrieve individual  *
      *               arguments rather than parse commandline with    *
      *               code.                                           *
      * 22 Apr 2021 - Replaced argument retrieval/validation logic.   *
      * ************************************************************* *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND MISCELLANEOUS FIELDS.  *
      * ************************************************************* *
       01  WS-FIELDS.

           02  WS-ERROR                PIC 9(1).
               88  NO-ARGUMENT-ERROR             VALUE 0.
               88  ARGUMENT-LENGTH-ERROR         VALUE 1.
               88  ARGUMENT-SLASH-ERROR          VALUE 2.
               88  ARGUMENT-Y2K-ERROR            VALUE 3. 
               88  ARGUMENT-Y2K-YEAR-ERROR       VALUE 5.
               88  ARGUMENT-Y2K-MONTH-ERROR      VALUE 7.
               88  ARGUMENT-Y2K-DAY-ERROR        VALUE 9.
           02  WS-ARGUMENT-COUNT       PIC 9(2).
           02  WS-ARGUMENT-1           PIC X(12) VALUE SPACES.
           02  WS-ARGUMENT-2           PIC X(12) VALUE SPACES.
           02  WS-COUNT                PIC S99.

           02  WS-ARGUMENT-INPUT       PIC X(12).
           02  WS-ARGUMENT-INPUT-LEN   PIC 9(02).
               88  VALID-LENGTH                  VALUE 4, 6, 7, 
                                                       8, 9, 10.
           02  WS-ARGUMENT-SLASH-CNT   PIC 9(02).
               88  VALID-SLASHCOUNT              VALUE 1, 2.
           02  WS-ARGUMENT-DATE.
               03  WS-ARGUMENT-MONTH   PIC X(02) JUSTIFIED RIGHT.
               03  WS-ARGUMENT-DAY     PIC X(02) JUSTIFIED RIGHT.
               03  WS-ARGUMENT-YEAR    PIC X(04) JUSTIFIED RIGHT.
               03  WS-ARGUMENT-YEAR-R  REDEFINES WS-ARGUMENT-YEAR
                                       PIC X(04).

           02  WS-DATE-1               PIC X(8).
           02  WS-DATE-2               PIC X(8).

           02  WS-EDIT-DAYS            PIC Z9 BLANK WHEN ZERO.
           02  WS-EDIT-MONTHS          PIC Z9 BLANK WHEN ZERO.
           02  WS-EDIT-YEARS           PIC ZZ,ZZ9 BLANK WHEN ZERO.
           02  WS-SEP                  PIC X(03).
           02  WS-REPORT               PIC X(50) VALUE SPACES.

           02  WS-CURRENT-DATE         PIC X(8).
           02  FILLER                  REDEFINES WS-CURRENT-DATE.
               03  WS-CURRENT-YEAR     PIC 9(4).
               03  WS-CURRENT-MONTH    PIC 9(2).
               03  WS-CURRENT-DAY      PIC 9(2).

           02  WS-SYNTAX.
               03  WS-S1               PIC X(50) VALUE
               'Syntax: mm/dd/yyyy mm/dd/yyyy'.
               03  WS-S2               PIC X(50) VALUE
               ' -or-   mm/dd/yy mm/dd/yy [19 assumed for century]'.
               03  WS-S3               PIC X(50) VALUE
               ' -or-   mm/dd/yyyy [system date used for 2nd date]'.
               03  WS-S4               PIC X(50) VALUE
               ' -or-   mm/yyyy mm/yyyy [01 assumed for day value'.

       COPY 'Y2K.cpy'.

      /
       PROCEDURE DIVISION.

       0000-MAIN SECTION.

       0050-SETUP.

      * ************************************************************* *
      * SYSTEM CURRENT DATE MAY BE USED IF ONLY 1 DATE SUPPLIED.      *
      * ************************************************************* *
           MOVE CURRENT-DATE(1:08) TO WS-CURRENT-DATE.
           DISPLAY '0000000000000000000000000000000000000000000'.

      * ************************************************************* *
      * RETRIEVE ARGUMENT(S), IF INVALID NUMBER OF ARGUMENTS GIVEN,   *
      * DISPLAY SYNTAX AND EXIT, ELSE RETRIEVE ARGUMENTS.             *
      * ************************************************************* *
           ACCEPT WS-ARGUMENT-COUNT FROM ARGUMENT-NUMBER.

           EVALUATE WS-ARGUMENT-COUNT
               WHEN 1
                   DISPLAY 1 UPON ARGUMENT-NUMBER
                   ACCEPT WS-ARGUMENT-1 FROM ARGUMENT-VALUE
               WHEN 2
                   DISPLAY 1 UPON ARGUMENT-NUMBER
                   ACCEPT WS-ARGUMENT-1 FROM ARGUMENT-VALUE
                   DISPLAY 2 UPON ARGUMENT-NUMBER
                   ACCEPT WS-ARGUMENT-2 FROM ARGUMENT-VALUE
               WHEN OTHER
                   GO TO 0300-SHOW-SYNTAX
           END-EVALUATE.

      * ************************************************************* *
      * PARSE FIRST ARGUMENT INTO DATE FIELDS. IF ERROR OCCURS, SHOW  *
      * SYNTAX AND EXIT PROGRAM.                                      *
      * ************************************************************* *
           MOVE WS-ARGUMENT-1 TO WS-ARGUMENT-INPUT.
           PERFORM 200-PARSE-DATE.
           IF NO-ARGUMENT-ERROR
             MOVE WS-ARGUMENT-DATE TO WS-DATE-1
           ELSE
             EVALUATE TRUE
               WHEN ARGUMENT-LENGTH-ERROR
                 DISPLAY '1st argument length must be 4, 6, 7, '
                         '8, 9, or 10'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-SLASH-ERROR
                 DISPLAY '1st argument must contain 1 or 2 slashes'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-ERROR
                 DISPLAY '1st argument contains invalid characters'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-YEAR-ERROR
                 DISPLAY '1st argument year is not valid'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-MONTH-ERROR
                 DISPLAY '1st argument month is not valid'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-DAY-ERROR
                 DISPLAY '1st argument day is not valid'
                 GO TO 0300-SHOW-SYNTAX
             END-EVALUATE
           END-IF.

      * ************************************************************* *
      * IF 2ND ARGUMENT NOT SUPPLIED, USE SYSTEM DATE AS 2ND DATE. IF *
      * SUPPLIED, PARSE INTO DATE FIELDS. IF ERROR OCCURS, SHOW       *
      * SYNTAX AND EXIT PROGRAM.                                      *
      * ************************************************************* *
           IF WS-ARGUMENT-2 = SPACES
               MOVE WS-CURRENT-MONTH TO WS-DATE-2(1:2)
               MOVE WS-CURRENT-DAY TO WS-DATE-2(3:2)
               MOVE WS-CURRENT-YEAR TO WS-DATE-2(5:4)
               GO TO 100-COMPUTE
           END-IF.
           MOVE WS-ARGUMENT-2 TO WS-ARGUMENT-INPUT.
           PERFORM 200-PARSE-DATE.
           IF NO-ARGUMENT-ERROR
             MOVE WS-ARGUMENT-DATE TO WS-DATE-2
           ELSE
             EVALUATE TRUE
               WHEN ARGUMENT-LENGTH-ERROR
                 DISPLAY '2nd argument length must be 4, 6, 7, '
                         '8, 9, or 10'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-SLASH-ERROR
                 DISPLAY '2nd argument must contain 1 or 2 slashes'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-ERROR
                 DISPLAY '2nd argument contains invalid characters'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-YEAR-ERROR
                 DISPLAY '2nd argument year is not valid'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-MONTH-ERROR
                 DISPLAY '2nd argument month is not valid'
                 GO TO 0300-SHOW-SYNTAX
               WHEN ARGUMENT-Y2K-DAY-ERROR
                 DISPLAY '2nd argument day is not valid'
                 GO TO 0300-SHOW-SYNTAX
             END-EVALUATE
           END-IF.

       100-COMPUTE.

      * ************************************************************* *
      * CALL Y2K ROUTINE TO CALCULATE PERIOD. IF ERROR RETURNED,      *
      * SHOW SYNTAX AND EXIT.                                         *
      * ************************************************************* *

           MOVE  WS-DATE-1 TO Y2K-LAGEP-DATE1.
           MOVE  WS-DATE-2 TO Y2K-LAGEP-DATE2.
           CALL 'Y2KLAGE' USING Y2K-LAGE-PARAMETERS.
           IF Y2K-LAGEP-RETURN-CODE NOT = 0
               DISPLAY 'Error returned from Y2KLAGE:'
               EVALUATE Y2K-LAGEP-RETURN-CODE
                   WHEN 2
                       DISPLAY '  Date 1 not numeric'
                   WHEN 3
                       DISPLAY '  Date 2 not numeric'
                   WHEN 4
                       DISPLAY '  Date 1 (Year) range error'
                   WHEN 5
                       DISPLAY '  Date 2 (Year) range error'
                   WHEN 6
                       DISPLAY '  Date 1 (Month) range error'
                   WHEN 7
                       DISPLAY '  Date 2 (Month) range error'
                   WHEN 8
                       DISPLAY '  Date 1 (Day) range error'
                   WHEN 9
                       DISPLAY '  Date 2 (Day) range error'
               END-EVALUATE
           ELSE
               MOVE Y2K-LAGEP-DAYS-PAST TO WS-EDIT-DAYS
               MOVE Y2K-LAGEP-MONTHS-PAST TO WS-EDIT-MONTHS
               MOVE Y2K-LAGEP-YEARS-PAST TO WS-EDIT-YEARS
               DISPLAY 'Period '
                       WS-DATE-1(1:2) '/'
                       WS-DATE-1(3:2) '/'
                       WS-DATE-1(5:4) ' through '
                       WS-DATE-2(1:2) '/'
                       WS-DATE-2(3:2) '/'
                       WS-DATE-2(5:4) ': '
                   WITH NO ADVANCING
               MOVE +1 TO WS-COUNT
               MOVE '~' TO WS-SEP
               IF WS-EDIT-YEARS NOT = SPACES
                   STRING TRIM(WS-EDIT-YEARS) 
                       DELIMITED BY SIZE
                       INTO WS-REPORT 
                       WITH POINTER WS-COUNT
                   IF Y2K-LAGEP-YEARS-PAST > 1
                       STRING ' Years' DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   ELSE
                       STRING ' Year' DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   END-IF
                   MOVE ', ~' TO WS-SEP
               END-IF
               IF WS-EDIT-MONTHS NOT = SPACES
                   STRING WS-SEP DELIMITED BY '~'
                       TRIM(WS-EDIT-MONTHS) 
                       DELIMITED BY SIZE
                       INTO WS-REPORT 
                       WITH POINTER WS-COUNT
                   IF Y2K-LAGEP-MONTHS-PAST > 1
                       STRING ' Months' DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   ELSE
                       STRING ' Month' DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   END-IF
                   MOVE ', ~' TO WS-SEP
               END-IF
               IF WS-EDIT-DAYS NOT = SPACES
                   STRING WS-SEP DELIMITED BY '~'
                       TRIM(WS-EDIT-DAYS) 
                       DELIMITED BY SIZE
                       INTO WS-REPORT 
                       WITH POINTER WS-COUNT
                   IF Y2K-LAGEP-DAYS-PAST > 1
                       STRING ' Days' 
                           DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   ELSE
                       STRING ' Day' 
                           DELIMITED BY SIZE
                           INTO WS-REPORT 
                           WITH POINTER WS-COUNT
                   END-IF
               END-IF.
               DISPLAY TRIM(WS-REPORT).

           GO TO 0400-EXIT-PROGRAM.

       200-PARSE-DATE.

           SET NO-ARGUMENT-ERROR TO TRUE.
           MOVE SPACES TO WS-ARGUMENT-DATE.
           MOVE +0 TO WS-ARGUMENT-SLASH-CNT.
           MOVE LENGTH(TRIM(WS-ARGUMENT-INPUT)) 
             TO WS-ARGUMENT-INPUT-LEN.

           IF NOT VALID-LENGTH
             SET ARGUMENT-LENGTH-ERROR TO TRUE
           END-IF.

           IF NO-ARGUMENT-ERROR
             INSPECT WS-ARGUMENT-INPUT
               TALLYING WS-ARGUMENT-SLASH-CNT FOR ALL '/'
             IF NOT VALID-SLASHCOUNT
               SET ARGUMENT-SLASH-ERROR TO TRUE
             END-IF
           END-IF.

           IF NO-ARGUMENT-ERROR
             EVALUATE WS-ARGUMENT-SLASH-CNT
               WHEN 1
                 UNSTRING WS-ARGUMENT-INPUT
                   DELIMITED BY '/' OR ' ' 
                   INTO WS-ARGUMENT-MONTH, 
                        WS-ARGUMENT-YEAR
                 END-UNSTRING
                 MOVE '01' TO WS-ARGUMENT-DAY
               WHEN 2
                 UNSTRING WS-ARGUMENT-INPUT
                   DELIMITED BY '/' OR ' ' 
                   INTO WS-ARGUMENT-MONTH, 
                        WS-ARGUMENT-DAY,
                        WS-ARGUMENT-YEAR
                 END-UNSTRING
             END-EVALUATE
             IF WS-ARGUMENT-YEAR-R (1:2) = SPACES 
               MOVE '19' TO WS-ARGUMENT-YEAR-R (1:2)
             END-IF
             INSPECT WS-ARGUMENT-DATE
               REPLACING ALL ' ' BY '0'
             MOVE WS-ARGUMENT-DATE TO Y2K-GTOAP-DATE-G
             CALL 'Y2KGTOA' USING Y2K-GTOA-PARAMETERS
             IF Y2K-GTOAP-RETURN-CODE NOT = 0
               ADD 1 TO Y2K-GTOAP-RETURN-CODE
                 GIVING WS-ERROR
             END-IF
           END-IF.

       0300-SHOW-SYNTAX.
           DISPLAY WS-S1.
           DISPLAY WS-S2.
           DISPLAY WS-S3.
           DISPLAY WS-S4.

       0400-EXIT-PROGRAM.
           DISPLAY '999999999999999999999999'.
           STOP RUN.

       END PROGRAM ELAPSED.
