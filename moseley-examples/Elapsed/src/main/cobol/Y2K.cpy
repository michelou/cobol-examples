      ******************************************************************
      * Author: Jay Moseley, CCP
      * Date: May, 2016
      * Purpose: Parameters passed for Y2K routines.
      ******************************************************************
       01  Y2K-ATOG-PARAMETERS.
           02  Y2K-ATOGP-ANUM          PIC X(7).
           02  Y2K-ATOGP-RETURN-CODE   PIC 9(1).
           02  Y2K-ATOGP-DATE          PIC 9(8).
      
       01  Y2K-CONV-PARAMETERS.
           02  Y2K-CONVP-DATEIN        PIC X(6).
           02  Y2K-CONVP-FORMAT        PIC X(1).
           02  Y2K-CONVP-BREAK         PIC X(2).
           02  Y2K-CONVP-RETURN-CODE   PIC 9(1).
           02  Y2K-CONVP-DATE-G        PIC 9(8).
           02  Y2K-CONVP-DATE-J        PIC 9(7).
           02  Y2K-CONVP-ANUM          PIC S9(7).
      
       01  Y2K-DFMT-PARAMETERS.
           02  Y2K-DFMTP-DATEIN        PIC X(8).
           02  Y2K-DFMTP-MAJOR         PIC X(1).
               88  DFMT-MAJOR-COMMERCIAL        VALUE 1.
               88  DFMT-MAJOR-EUROPEAN          VALUE 2.
               88  DFMT-MAJOR-FIPS              VALUE 3.
               88  DFMT-MAJOR-TEXT1             VALUE 4.
               88  DFMT-MAJOR-TEXT2             VALUE 5.
               88  DFMT-MAJOR-TEXT3             VALUE 6.
               88  DFMT-MAJOR-TEXT4             VALUE 7.
               88  DFMT-MAJOR-TEXT5             VALUE 8.
               88  DFMT-MAJOR-TEXT6             VALUE 9.
           02  Y2K-DFMTP-MINOR         PIC X(1).
               88  DFMT-MINOR-NO-INSERT         VALUE 1.
               88  DFMT-MINOR-SPACE             VALUE 2.
               88  DFMT-MINOR-SLASH             VALUE 3.
               88  DFMT-MINOR-HYPHEN            VALUE 4.
               88  DFMT-MINOR-PERIOD            VALUE 5.
               88  DFMT-MINOR-US                VALUE 1.
               88  DFMT-MINOR-EURO              VALUE 2.
      ******************************************************************      
      *  THE MAJOR FORMAT CODE DETERMINES THE OVERALL OUTPUT FORMAT
      *  AND THE MINOR FORMAT CODE DETERMINES SUBTLE VARIATIONS OF 
      *  THE FORMAT:                                               
      *    MAJOR                                  MINOR            
      *      1 = COMMERCIAL (01051997)              1 = NO INSERT  
      *      1 = COMMERCIAL (01 05 1997)            2 = SPACE      
      *      2 = EUROPEAN   (05/01/1997)            3 = SLASH      
      *      2 = EUROPEAN   (05-01-1997)            4 = HYPHEN     
      *      3 = F.I.P.S.   (YYYY.MM.DD)            5 = PERIOD     
      *    ---------------------------------------------------------
      *      4 = TEXT 1 (JANUARY 5, 1997)           1 = U.S.        
      *      4 = TEXT 1 (5 JANUARY 1997)            2 = EUROPEAN    
      *      5 = TEXT 2 (JAN 5, 1997)               1 = U.S.        
      *      6 = TEXT 3 (SUNDAY, JANUARY 5, 1997)   1 = U.S.        
      *      7 = TEXT 4 (SUNDAY, 5 JAN 1997)        2 = EUROPEAN    
      *      8 = TEXT 5 (SUN, JANUARY 5, 1997)      1 = U.S.        
      *      9 = TEXT 6 (SUN, 5 JAN 1997)           2 = EUROPEAN    
      ******************************************************************      
           02  Y2K-DFMTP-RETURN-CODE   PIC 9(1).
           02  Y2K-DFMTP-OUTPUT-SIZE   PIC S9(2).
           02  Y2K-DFMTP-OUTPUT        PIC X(29).

       01  Y2K-DOWN-PARAMETERS.
           02  Y2K-DOWNP-DATE          PIC X(8).
           02  Y2K-DOWNP-RETURN-CODE   PIC 9(1).
           02  Y2K-DOWNP-DAY-NUMBER    PIC 9(1).
               88  Y2K-DOWN-MONDAY     VALUE 0.
               88  Y2K-DOWN-TUESDAY    VALUE 1.
               88  Y2K-DOWN-WEDNESDAY  VALUE 2.
               88  Y2K-DOWN-THURSDAY   VALUE 3.
               88  Y2K-DOWN-FRIDAY     VALUE 4.
               88  Y2K-DOWN-SATURDAY   VALUE 5.
               88  Y2K-DOWN-SUNDAY     VALUE 6.
      
       01  Y2K-ESTR-PARAMETERS.
           02  Y2K-ESTRP-YEAR          PIC X(4).
           02  Y2K-ESTRP-RETURN-CODE   PIC 9(1).
           02  Y2K-ESTRP-DATE          PIC 9(8).
      
       01  Y2K-GETD-PARAMETERS.
           02  Y2K-GETDP-DATE-G        PIC 9(8).
           02  Y2K-GETDP-DATE-J        PIC 9(7).
           02  Y2K-GETDP-ANUM          PIC S9(7).
      
       01  Y2K-GTOA-PARAMETERS.
           02  Y2K-GTOAP-DATE-G        PIC X(8).
           02  Y2K-GTOAP-RETURN-CODE   PIC 9(1).
           02  Y2K-GTOAP-ANUM          PIC S9(7).
      
       01  Y2K-GTOJ-PARAMETERS.
           02  Y2K-GTOJP-DATE-G        PIC X(8).
           02  Y2K-GTOJP-RETURN-CODE   PIC 9(1).
           02  Y2K-GTOJP-DATE-J        PIC 9(7).
      
       01  Y2K-JTOG-PARAMETERS.
           02  Y2K-JTOGP-DATE-J        PIC X(7).
           02  Y2K-JTOGP-RETURN-CODE   PIC 9(1).
           02  Y2K-JTOGP-DATE-G        PIC 9(8).
      
       01  Y2K-LAGE-PARAMETERS.
           02  Y2K-LAGEP-DATE1         PIC X(8).
           02  Y2K-LAGEP-DATE2         PIC X(8).
           02  Y2K-LAGEP-RETURN-CODE   PIC 9(1).
           02  Y2K-LAGEP-DAYS-PAST     PIC S9(2).
           02  Y2K-LAGEP-MONTHS-PAST   PIC S9(2).
           02  Y2K-LAGEP-YEARS-PAST    PIC S9(5).
      
       01  Y2K-PROJ-PARAMETERS.
           02  Y2K-PROJP-DATE          PIC X(8).
           02  Y2K-PROJP-INCREMENT     PIC X(6).
           02  Y2K-PROJP-RETURN-CODE   PIC 9(1).
           02  Y2K-PROJP-NEW-DATE      PIC 9(8).
      
       01  Y2K-SAGE-PARAMETERS.
           02  Y2K-SAGEP-DATE1         PIC X(8).
           02  Y2K-SAGEP-DATE2         PIC X(8).
           02  Y2K-SAGEP-RETURN-CODE   PIC 9(1).
           02  Y2K-SAGEP-DAYS-PAST     PIC S9(7).
      
       01  Y2K-TDOW-PARAMETERS.
           02  Y2K-TDOWP-DATEIN        PIC X(8).
           02  Y2K-TDOWP-DIRECTION     PIC X(1).
           02  Y2K-TDOWP-DAY-NUMBER    PIC X(1).
               88  Y2K-TDOW-MONDAY     VALUE 0.
               88  Y2K-TDOW-TUESDAY    VALUE 1.
               88  Y2K-TDOW-WEDNESDAY  VALUE 2.
               88  Y2K-TDOW-THURSDAY   VALUE 3.
               88  Y2K-TDOW-FRIDAY     VALUE 4.
               88  Y2K-TDOW-SATURDAY   VALUE 5.
               88  Y2K-TDOW-SUNDAY     VALUE 6.
           02  Y2K-TDOWP-RETURN-CODE   PIC 9(1).
           02  Y2K-TDOWP-NEW-DATE      PIC 9(8).
