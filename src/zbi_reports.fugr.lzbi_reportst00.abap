*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZBIT0001........................................*
DATA:  BEGIN OF STATUS_ZBIT0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBIT0001                      .
*...processing: ZBIT0001VW......................................*
TABLES: ZBIT0001VW, *ZBIT0001VW. "view work areas
CONTROLS: TCTRL_ZBIT0001VW
TYPE TABLEVIEW USING SCREEN '0005'.
DATA: BEGIN OF STATUS_ZBIT0001VW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZBIT0001VW.
* Table for entries selected to show on screen
DATA: BEGIN OF ZBIT0001VW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZBIT0001VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0001VW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZBIT0001VW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZBIT0001VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0001VW_TOTAL.

*...processing: ZBIT0002........................................*
DATA:  BEGIN OF STATUS_ZBIT0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBIT0002                      .
CONTROLS: TCTRL_ZBIT0002
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZBIT0002VW......................................*
TABLES: ZBIT0002VW, *ZBIT0002VW. "view work areas
CONTROLS: TCTRL_ZBIT0002VW
TYPE TABLEVIEW USING SCREEN '0009'.
DATA: BEGIN OF STATUS_ZBIT0002VW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZBIT0002VW.
* Table for entries selected to show on screen
DATA: BEGIN OF ZBIT0002VW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZBIT0002VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0002VW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZBIT0002VW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZBIT0002VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0002VW_TOTAL.

*...processing: ZBIT0003........................................*
DATA:  BEGIN OF STATUS_ZBIT0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBIT0003                      .
CONTROLS: TCTRL_ZBIT0003
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZBIT0003VW......................................*
TABLES: ZBIT0003VW, *ZBIT0003VW. "view work areas
CONTROLS: TCTRL_ZBIT0003VW
TYPE TABLEVIEW USING SCREEN '0011'.
DATA: BEGIN OF STATUS_ZBIT0003VW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZBIT0003VW.
* Table for entries selected to show on screen
DATA: BEGIN OF ZBIT0003VW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZBIT0003VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0003VW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZBIT0003VW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZBIT0003VW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZBIT0003VW_TOTAL.

*.........table declarations:.................................*
TABLES: *ZBIT0001                      .
TABLES: *ZBIT0002                      .
TABLES: *ZBIT0003                      .
TABLES: ZBIT0001                       .
TABLES: ZBIT0002                       .
TABLES: ZBIT0003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
