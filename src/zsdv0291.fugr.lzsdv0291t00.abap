*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDV0291........................................*
TABLES: ZSDV0291, *ZSDV0291. "view work areas
CONTROLS: TCTRL_ZSDV0291
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZSDV0291. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZSDV0291.
* Table for entries selected to show on screen
DATA: BEGIN OF ZSDV0291_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZSDV0291.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0291_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZSDV0291_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZSDV0291.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZSDV0291_TOTAL.

*.........table declarations:.................................*
TABLES: MAKT                           .
TABLES: MARA                           .
TABLES: ZSDT0291                       .
