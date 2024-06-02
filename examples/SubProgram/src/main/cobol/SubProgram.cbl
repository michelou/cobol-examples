   IDENTIFICATION DIVISION.
   PROGRAM-ID. TestSubProgram.
   ENVIRONMENT DIVISION.
   CONFIGURATION SECTION.
   Repository.
       FUNCTION functionABC2.
   DATA DIVISION.
   Working-Storage SECTION.
       01 ctr1         PIC 999 value 0.
       77 returnvalue  USAGE BINARY-LONG SIGNED.
       77 someValue    USAGE BINARY-LONG SIGNED.
   LINKAGE SECTION.
   PROCEDURE DIVISION.
   Begin.
       DISPLAY "Hello World!"
       MOVE 100 to ctr1.
       CALL 'functionABC' USING ctr1 returnvalue.  
       DISPLAY "Return value from functionABC sub-program: ", returnvalue.  
       MOVE 100 to someValue.
       COMPUTE returnvalue = functionABC2(someValue).
       DISPLAY "Return value from functionABC2 function: ", returnvalue.
       STOP RUN.
   END PROGRAM TestSubProgram.

   IDENTIFICATION DIVISION.
   PROGRAM-ID. functionABC.
   DATA DIVISION.
   working-storage SECTION.
       77 localvar     PIC 999.
   LINKAGE SECTION.
       01 param1       PIC 999.
       01 result       USAGE BINARY-LONG SIGNED.
   PROCEDURE DIVISION USING param1 result.
       MOVE 0 to result.
       COMPUTE result = param1 * 2
       DISPLAY "functionABC: param1=", param1, " result=", result
       EXIT.
   END PROGRAM functionABC.

   IDENTIFICATION DIVISION.
   FUNCTION-ID. functionABC2.
   DATA DIVISION.
   WORKING-storage SECTION.
   LINKAGE SECTION.
       77 param1       USAGE BINARY-LONG SIGNED.
       77 result       USAGE BINARY-LONG SIGNED.
   PROCEDURE DIVISION USING param1 RETURNING result.
       MOVE 0 to result
       COMPUTE result = param1 * 2
       DISPLAY "functionABC2: param1=", param1, " result=", result
       GOBACK.
   END FUNCTION functionABC2.
