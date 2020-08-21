       >>SOURCE FORMAT FREE
*>>**
*>>  <summary>Short sample.</summary>
*>>  <author>Bruno Pacheco (https://brunopacheco1.github.io/)</author>
*>>  <license>LGPL-3.0</license>
*>>**

*>>*
*>>  <summary>The first module.  
*>>  Trying to see **what** happens to    huge text.</summary>
*>>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. first-module.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION firstmodulefunction.

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
    COMPUTE WS-FIRSTMODULE = firstmodulefunction(WS-FIRSTMODULE).
    DISPLAY "First Module: ", WS-FIRSTMODULE.
    CALL 'second-module' USING BY CONTENT WS-FIRSTMODULE
    END-CALL.
 END PROGRAM first-module.
 
*>>*
*>> <summary>The second module</summary>
*>>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. second-module.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION secondmodulefunction.

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
    COMPUTE WS-SECONDMODULE = secondmodulefunction(WS-SECONDMODULE).
    DISPLAY "Second Module: ", WS-SECONDMODULE.
    CALL 'third-module' USING BY CONTENT WS-SECONDMODULE
    END-CALL.

 END PROGRAM second-module.
 
*>>*
*>> <summary>The third module</summary>
*>>*
 IDENTIFICATION DIVISION.
 PROGRAM-ID. third-module.

 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 REPOSITORY.
     FUNCTION thirdmodulefunction.

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
    COMPUTE WS-THIRDMODULE = thirdmodulefunction(WS-THIRDMODULE).
    DISPLAY "Third Module: ", WS-THIRDMODULE.
 END PROGRAM third-module.

*>>*
*>> <summary>first module function</summary>
*>> <param type="PIC 9" name="first-arg">First arg</param>
*>> <param type="PIC 9" name="second-arg">Second arg</param>
*>> <returns type="PIC 9">First return</returns>
*>>*
IDENTIFICATION DIVISION.
FUNCTION-ID. firstmodulefunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 first-arg PIC 9.
   01 firstresult PIC 9.
PROCEDURE DIVISION USING first-arg RETURNING firstresult.
    COMPUTE firstresult = firstarg + 1.
END FUNCTION firstmodulefunction.

*>>*
*>> <summary>second module function</summary>
*>> <param type="PIC 9" name="secondarg">Second arg</param>
*>> <returns type="PIC 9">Second return</returns>
*>>*
IDENTIFICATION DIVISION.
FUNCTION-ID. secondmodulefunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 secondarg PIC 9.
   01 secondresult PIC 9.
PROCEDURE DIVISION USING secondarg RETURNING secondresult.
    COMPUTE secondresult = secondarg + 2.
END FUNCTION secondmodulefunction.

*>>*
*>> <summary>third module function</summary>
*>> <param type="PIC 9" name="thirdarg">Third arg</param>
*>> <returns type="PIC 9">Third return</returns>
*>>*
IDENTIFICATION DIVISION.
FUNCTION-ID. thirdmodulefunction.

DATA DIVISION.
WORKING-STORAGE SECTION.
LINKAGE SECTION.
   01 thirdarg PIC 9.
   01 thirdresult PIC 9.
PROCEDURE DIVISION USING thirdarg RETURNING thirdresult.
    COMPUTE thirdresult = thirdarg + 3.
END FUNCTION thirdmodulefunction.
