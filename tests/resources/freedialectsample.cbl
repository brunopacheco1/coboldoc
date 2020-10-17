       >>SOURCE FORMAT FREE
*>**
*>  Short sample.
*>  @author Bruno Pacheco (https://brunopacheco1.github.io/)
*>  @license LGPL-3.0
*>**

*>*
*>  The first program.  
*>  Trying to see **what** happens to    huge text.
*>  @summary the first program summary.
*>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. first-program.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION firstprogramfunction.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-FIRSTMODULE PIC 9.
 01 WS-TRANSFER    PIC 9.
 
 PROCEDURE DIVISION.
 
 0001-FIRSTMODULE-MAIN.
    PERFORM 0002-FIRSTMODULE-NEWPARA.
    STOP RUN.
 
 0002-FIRSTMODULE-NEWPARA.
    ACCEPT WS-FIRSTMODULE.
    COMPUTE WS-FIRSTMODULE = firstprogramfunction(WS-FIRSTMODULE).
    DISPLAY "First Program: ", WS-FIRSTMODULE.
    CALL 'second-program' USING BY CONTENT WS-FIRSTMODULE
    END-CALL.
 END PROGRAM first-program.
 
*>*
*> The second program
*>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. second-program.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION secondprogramfunction.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-SECONDMODULE PIC 9.
   
 LINKAGE SECTION.
 01 WS-TRANSFER    PIC 9.
 
 PROCEDURE DIVISION USING WS-TRANSFER.

 0001-SECONDMODULE-MAIN.
    PERFORM 0002-SECONDMODULE-NEWPARAGRAPH.
    STOP RUN.

 0002-SECONDMODULE-NEWPARAGRAPH.
    MOVE WS-TRANSFER TO WS-SECONDMODULE.
    COMPUTE WS-SECONDMODULE = secondprogramfunction(WS-SECONDMODULE).
    DISPLAY "Second Program: ", WS-SECONDMODULE.
    CALL 'third-program' USING BY CONTENT WS-SECONDMODULE
    END-CALL.

 END PROGRAM second-program.
 
*>*
*> The third program
*>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. third-program.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION thirdprogramfunction.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-THIRDMODULE PIC 9.
   
 LINKAGE SECTION.
 01 WS-TRANSFER    PIC 9.
 
 PROCEDURE DIVISION USING WS-TRANSFER.

 0001-THIRDMODULE-MAIN.
    PERFORM 0002-THIRDMODULE-NEWPARAGRAPH.
    STOP RUN.

 0002-THIRDMODULE-NEWPARAGRAPH.
    MOVE WS-TRANSFER TO WS-THIRDMODULE.
    COMPUTE WS-THIRDMODULE = thirdprogramfunction(WS-THIRDMODULE).
    DISPLAY "Third Program: ", WS-THIRDMODULE.
 END PROGRAM third-program.

*>*
*> first program function
*> @summary the first function summary.
*> @param   {PIC 9}  first-arg   First arg
*> @return {PIC 9} First return
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. firstprogramfunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 first-arg PIC 9.
   01 firstresult PIC 9.
PROCEDURE DIVISION USING first-arg RETURNING firstresult.
    COMPUTE firstresult = firstarg + 1.
END FUNCTION firstprogramfunction.

*>*
*> second program function
*> @param {PIC 9} secondarg    Second arg
*> @return  {PIC 9}   Second return
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. secondprogramfunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 secondarg PIC 9.
   01 secondresult PIC 9.
PROCEDURE DIVISION USING secondarg RETURNING secondresult.
    COMPUTE secondresult = secondarg + 2.
END FUNCTION secondprogramfunction.

*>*
*> third program function
*> @param  {PIC 9}   thirdarg   Third arg
*> @return   {PIC 9}            Third return
*>*
IDENTIFICATION DIVISION.
FUNCTION-ID. thirdprogramfunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 thirdarg PIC 9.
   01 thirdresult PIC 9.
PROCEDURE DIVISION USING thirdarg RETURNING thirdresult.
    COMPUTE thirdresult = thirdarg + 3.
END FUNCTION thirdprogramfunction.
