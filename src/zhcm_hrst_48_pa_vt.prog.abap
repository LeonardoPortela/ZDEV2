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
REPORT ZHCM_HRST_48_PA_VT MESSAGE-ID HRPAYBR13 LINE-SIZE 132.

INCLUDE ZHBRCVT01.        " General data declaration
INCLUDE ZHBRCVT02.        " Data declaration for list and tree outputs
INCLUDE ZHBRCVT03.        " Main program
INCLUDE ZHBRCVT04.        " General subroutines
INCLUDE ZHBRCVT05.        " BUILD_PWS_FOR_GENPS and count days
INCLUDE ZHBRCVT06.         " Batch input
INCLUDE ZHBRCVT07.        " Table reading
INCLUDE ZHBRCVT08.        " List output
INCLUDE HBRCVT09.         " Tree output
INCLUDE HBRCVT10.         " Error handling
*
