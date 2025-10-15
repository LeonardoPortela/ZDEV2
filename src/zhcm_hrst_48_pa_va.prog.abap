************************************************************************
*  PROGRAM      : HBRCVTR0                                             *
*  DATE WRITTEN : 20.02.1998                                           *
*  SYSTEM       : HRMS Brazil                                          *
*  PURPOSE      : Reads master data and the work schedule for the      *
*                 requested period. Generates a list of employees      *
*                 and the amounts they should receive as transporta-   *
*                 tion aid. Updates infotype 15 with these informa-    *
*                 tions, via a batch-input session, if the program     *
*                 runs with the batch input indicator set.             *
************************************************************************
REPORT ZHCM_HRST_48_PA_VA MESSAGE-ID HRPAYBR13 LINE-SIZE 132.

INCLUDE YHBRCVT01.        " General data declaration
INCLUDE YHBRCVT02.        " Data declaration for list and tree outputs
INCLUDE YHBRCVT03.        " Main program
INCLUDE YHBRCVT04.        " General subroutines
INCLUDE YHBRCVT05.        " BUILD_PWS_FOR_GENPS and count days
INCLUDE YHBRCVT06.         " Batch input
INCLUDE YHBRCVT07.        " Table reading
INCLUDE YHBRCVT08.        " List output
"INCLUDE HBRCVT09.         " Tree output
INCLUDE HBRCVT10.         " Error handling
*
