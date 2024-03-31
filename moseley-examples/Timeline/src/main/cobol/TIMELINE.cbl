       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIMELINE.
       AUTHOR. JAY MOSELEY.
       DATE-WRITTEN. APRIL, 2013.
       DATE-COMPILED.

      * ************************************************************* 
      * The purpose of this program is to read a set of control
      * records from line sequential file 'tileline.dat' and produce
      * a report showing the timeline of a set of events and the
      * relative age of a group of individuals at each 'event' point
      * on the timeline.
      *
      * From each control record with a '0' in column 1 the name of 
      * an individual and their birth date is stored.
      *
      * From each control record with a '1' in column 1 the descrip-
      * tion of an event and the date on which it occurred is read.
      * For each event a line is produced describing the event
      * and then beneath that line a line is produced for each of the
      * ************************************************************* *
 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
 
          SELECT TIMELINE-DATA-FILE ASSIGN TO "timeline.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRINT-FILE ASSIGN TO "timeline.rpt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  TIMELINE-DATA-FILE
           DATA RECORD IS TIMELINE-DATA-RECORD.
       01  TIMELINE-DATA-RECORD.
           02  TDR-RECORD-TYPE         PIC X(01).
           02  FILLER                  PIC X(79).
       01  TIMELINE-INDIVIDUAL-RECORD.
           02  FILLER                  PIC X(02).
           02  TIR-BIRTH-DATE          PIC 9(08).  *> YYYYMMDD
           02  FILLER                  PIC X(01).
           02  TIR-NAME                PIC X(35).
           02  FILLER                  PIC X(01).
           02  TIR-DEATH-DATE          PIC 9(08).  *> YYYYMMDD
           02  TIR-DEATH-DATE-X        REDEFINES TIR-DEATH-DATE
                                       PIC X(08).
           02  FILLER                  PIC X(25).
       01  TIMELINE-EVENT-RECORD.
           02  FILLER                  PIC X(02).
           02  TER-EVENT-DATE          PIC 9(08).  *> YYYYMMDD
           02  FILLER                  PIC X(01).
           02  TER-EVENT-DESCRIPTION   PIC X(44).
           02  FILLER                  PIC X(25).

       FD  PRINT-FILE
           DATA RECORD IS PRINT-RECORD.
       01  PRINT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.

      * ************************************************************* *
      * THESE ARE PROGRAM CONTROL SWITCHES AND MISCELLANEOUS FIELDS.  *
      * ************************************************************* *
       01  PROGRAM-CONTROL-FIELDS.
           02  END-OF-DATA-SWITCH      PIC X(1) VALUE 'N'.
               88  END-OF-DATA         VALUE 'Y'.
           02  PAGE-COUNT              PIC 999 VALUE ZERO.
           02  LINE-COUNT              PIC 99  VALUE 99.
           02  PAGE-SIZE               PIC 99  VALUE 42.
           02  WORK-DATE               PIC 9(8).
           02  PTR                     PIC S9(4) COMP.
           02  ELAPSED                 PIC X(35).
           02  EDITED                  PIC Z(4)9.
           02  SEPARATOR               PIC X(3).

      * ************************************************************* *
      * FIELDS TO STORE INFORMATION FOR INDIVIDUALS FROM THE TYPE '0' *
      * RECORDS IN THE CONTROL FILE.                                  *
      * ************************************************************* *
       01  TIMELINE-INDIVIDUAL-FIELDS.
           02  TIF-MAX                 PIC S9(2) VALUE +15.
           02  TIF-HI                  PIC S9(2) VALUE +0.
           02  TIF-ENTRY               OCCURS 15 TIMES
                                       INDEXED BY TIF-INDEX.
               03  TIF-BIRTH-DATE      PIC 9(08).
               03  TIF-NAME            PIC X(35).
               03  TIF-DEATH-DATE      PIC 9(08).

        COPY Y2K.

      * ************************************************************* *
      * FOLLOWING ARE THE REPORT HEADING AND DETAIL DESCRIPTIONS.     *
      * ************************************************************* *

       01  HEADING-1.
           02  FILLER  PIC X(6) VALUE 'DATE: '.
           02  H1-DATE PIC X(10) VALUE '  /  /    '.
           02  FILLER  PIC X(38) VALUE SPACES.
           02  FILLER  PIC X(24) VALUE
               'PERSONAL TIMELINE EVENTS'.
           02  FILLER  PIC X(37) VALUE SPACES.
           02  FILLER  PIC X(17) VALUE 'PROGRAM: TIMELINE'.

       01  HEADING-2.
           02  FILLER  PIC X(6) VALUE 'TIME: '.
           02  H2-TIME PIC X(11).
           02  FILLER  PIC X(98) VALUE SPACES.
           02  FILLER  PIC X(9) VALUE 'PAGE:   '.
           02  H2-PAGE PIC ZZ9.
           02  FILLER  PIC X(5) VALUE SPACES.

       01  DETAIL-LINE.
           02  DL-EVENT-DATE           PIC X(12).
           02  FILLER                  PIC X(01).
           02  DL-EVENT-DESCRIPTION    PIC X(44).
           02  FILLER                  PIC X(75).
       01  FILLER                      REDEFINES DETAIL-LINE.
           02  FILLER                  PIC X(13).
           02  DL-INDIVIDUAL           PIC X(35).
           02  DL-AGE                  PIC X(84).

       PROCEDURE DIVISION.

       0000-MAIN SECTION.
       0050-SETUP.

           MOVE FUNCTION CURRENT-DATE(1:8) TO WORK-DATE.
           MOVE WORK-DATE(1:4) TO H1-DATE(7:4).
           MOVE WORK-DATE(5:2) TO H1-DATE(1:2).
           MOVE WORK-DATE(7:2) TO H1-DATE(4:2).
           MOVE FUNCTION LOCALE-TIME(FUNCTION CURRENT-DATE(9:6))
             TO H2-TIME.

       0100-CONTROL.

           OPEN INPUT TIMELINE-DATA-FILE,
                OUTPUT PRINT-FILE.
           PERFORM 0200-READ-DATA-FILE.
           PERFORM 0300-PROCESS-DATA
              THRU 0399-PROCESS-DATA-EXIT
             UNTIL END-OF-DATA.
           CLOSE TIMELINE-DATA-FILE, PRINT-FILE.
           STOP RUN.
      * - - - - - - - - - - - - - - - - PROGRAM EXIT POINT

       0200-READ-DATA-FILE.

           READ TIMELINE-DATA-FILE
             AT END MOVE 'Y' TO END-OF-DATA-SWITCH.
      * - - - - - - - - - - - - - - - - PERFORM EXIT POINT

       0300-PROCESS-DATA.

           EVALUATE TDR-RECORD-TYPE

             WHEN '0'
               IF TIF-HI LESS THAN TIF-MAX
                 ADD 1 TO TIF-HI
                 SET TIF-INDEX TO TIF-HI
                 MOVE TIR-BIRTH-DATE TO TIF-BIRTH-DATE (TIF-INDEX)
                 INSPECT TIR-NAME 
                   REPLACING TRAILING SPACES BY '.'
                 MOVE TIR-NAME TO TIF-NAME (TIF-INDEX)
                 IF TIR-DEATH-DATE-X NOT = SPACES
                   MOVE TIR-DEATH-DATE TO TIF-DEATH-DATE (TIF-INDEX)
                 ELSE
                   MOVE ZEROS TO TIF-DEATH-DATE (TIF-INDEX)
                 END-IF
               ELSE
                 DISPLAY 'NUMBER OF INDIVIDUAL RECORDS EXCEEDS TABLE'
               END-IF

             WHEN '1'
               PERFORM 0400-PROCESS-EVENT
                  THRU 0499-PROCESS-EVENT-EXIT

               WHEN OTHER *> IGNORE COMMENTS
                   CONTINUE

           END-EVALUATE.

           PERFORM 0200-READ-DATA-FILE.

       0399-PROCESS-DATA-EXIT.
           EXIT.
      * - - - - - - - - - - - - - - - - PERFORM EXIT POINT

       0400-PROCESS-EVENT.

           MOVE SPACES TO DETAIL-LINE.
           MOVE TER-EVENT-DATE(5:2) TO WORK-DATE(1:2).
           MOVE TER-EVENT-DATE(7:2) TO WORK-DATE(3:2).
           MOVE TER-EVENT-DATE(1:4) TO WORK-DATE(5:4).
           MOVE WORK-DATE TO Y2K-DFMTP-DATEIN.
           SET DFMT-MAJOR-TEXT2 TO TRUE.
           SET DFMT-MINOR-US TO TRUE.
           MOVE SPACES TO Y2K-DFMTP-OUTPUT.
           CALL 'Y2KDFMT' USING Y2K-DFMT-PARAMETERS.
           MOVE Y2K-DFMTP-OUTPUT TO DL-EVENT-DATE.
           MOVE TER-EVENT-DESCRIPTION TO DL-EVENT-DESCRIPTION.

           IF LINE-COUNT GREATER THAN PAGE-SIZE
               IF LINE-COUNT LESS THAN 99
                   MOVE SPACES TO PRINT-RECORD
                   WRITE PRINT-RECORD BEFORE ADVANCING PAGE
               END-IF
               ADD 1 TO PAGE-COUNT
               MOVE PAGE-COUNT TO H2-PAGE
               WRITE PRINT-RECORD FROM HEADING-1 BEFORE ADVANCING 1
               WRITE PRINT-RECORD FROM HEADING-2 BEFORE ADVANCING 2
               MOVE 3 TO LINE-COUNT
           END-IF.

           WRITE PRINT-RECORD FROM DETAIL-LINE BEFORE ADVANCING 1.
           ADD 1 TO LINE-COUNT.

           PERFORM 0500-PROCESS-INDIVIDUAL
              THRU 0599-PROCESS-INDIVIDUAL-EXIT
              VARYING TIF-INDEX FROM 1 BY 1
              UNTIL TIF-INDEX > TIF-HI.

           MOVE ALL '-' TO PRINT-RECORD.
           WRITE PRINT-RECORD BEFORE ADVANCING 1.
           ADD 1 TO LINE-COUNT.

       0499-PROCESS-EVENT-EXIT.
           EXIT.
      * - - - - - - - - - - - - - - - - PERFORM EXIT POINT

       0500-PROCESS-INDIVIDUAL.

           IF TER-EVENT-DATE NOT GREATER THAN 
              TIF-BIRTH-DATE (TIF-INDEX)
             GO TO 0599-PROCESS-INDIVIDUAL-EXIT
           END-IF.

           IF TIF-DEATH-DATE (TIF-INDEX) NOT ZEROS
             IF TER-EVENT-DATE GREATER THAN
                TIF-DEATH-DATE (TIF-INDEX)
               GO TO 0599-PROCESS-INDIVIDUAL-EXIT
             END-IF
           END-IF.

           INITIALIZE Y2K-LAGE-PARAMETERS.
           MOVE TIF-BIRTH-DATE (TIF-INDEX) TO WORK-DATE.
           MOVE WORK-DATE(5:2) TO Y2K-LAGEP-DATE1(1:2).
           MOVE WORK-DATE(7:2) TO Y2K-LAGEP-DATE1(3:2).
           MOVE WORK-DATE(1:4) TO Y2K-LAGEP-DATE1(5:4).
           MOVE TER-EVENT-DATE TO WORK-DATE.
           MOVE WORK-DATE(5:2) TO Y2K-LAGEP-DATE2(1:2).
           MOVE WORK-DATE(7:2) TO Y2K-LAGEP-DATE2(3:2).
           MOVE WORK-DATE(1:4) TO Y2K-LAGEP-DATE2(5:4).
           CALL 'Y2KLAGE' USING Y2K-LAGE-PARAMETERS.

           MOVE SPACES TO DETAIL-LINE.
           MOVE '~' TO SEPARATOR.
           MOVE TIF-NAME (TIF-INDEX) TO DL-INDIVIDUAL.
           MOVE '.age: ' TO ELAPSED.
           MOVE +7 TO PTR.
           IF Y2K-LAGEP-YEARS-PAST > 0
             MOVE Y2K-LAGEP-YEARS-PAST TO EDITED
             STRING FUNCTION TRIM(EDITED) DELIMITED BY SIZE
                  ' years' DELIMITED BY SIZE
               INTO ELAPSED
               WITH POINTER PTR
               MOVE ', ~' TO SEPARATOR
           END-IF.
           IF Y2K-LAGEP-MONTHS-PAST > 0
             MOVE Y2K-LAGEP-MONTHS-PAST TO EDITED
             STRING SEPARATOR DELIMITED BY '~'
                    FUNCTION TRIM(EDITED) DELIMITED BY SIZE
                    ' months' DELIMITED BY SIZE
               INTO ELAPSED
               WITH POINTER PTR
               MOVE ', ~' TO SEPARATOR
           END-IF.
           IF Y2K-LAGEP-DAYS-PAST > 0
             MOVE Y2K-LAGEP-DAYS-PAST TO EDITED
             STRING SEPARATOR DELIMITED BY '~'
                    FUNCTION TRIM(EDITED) DELIMITED BY SIZE
                    ' days' DELIMITED BY SIZE
               INTO ELAPSED
               WITH POINTER PTR
           END-IF.
           MOVE ELAPSED TO DL-AGE.

           IF LINE-COUNT GREATER THAN PAGE-SIZE
               ADD 1 TO PAGE-COUNT
               MOVE PAGE-COUNT TO H2-PAGE
               WRITE PRINT-RECORD FROM HEADING-1 BEFORE ADVANCING 1
               WRITE PRINT-RECORD FROM HEADING-2 BEFORE ADVANCING 2
               MOVE 3 TO LINE-COUNT
           END-IF.

           WRITE PRINT-RECORD FROM DETAIL-LINE BEFORE ADVANCING 1.
           ADD 1 TO LINE-COUNT.

       0599-PROCESS-INDIVIDUAL-EXIT.
           EXIT.
      * - - - - - - - - - - - - - - - - PERFORM EXIT POINT

       END PROGRAM TIMELINE.
