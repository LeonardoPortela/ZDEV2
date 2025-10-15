*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZV_TZONE........................................*
TABLES: ZV_TZONE, *ZV_TZONE. "view work areas
CONTROLS: TCTRL_ZV_TZONE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZV_TZONE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZV_TZONE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZV_TZONE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZV_TZONE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TZONE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZV_TZONE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZV_TZONE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZV_TZONE_TOTAL.

*.........table declarations:.................................*
TABLES: TZONE                          .
TABLES: TZONT                          .
