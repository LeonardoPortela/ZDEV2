*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL008..........................................*
DATA:  BEGIN OF STATUS_ZGL008                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL008                        .
CONTROLS: TCTRL_ZGL008
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL008                        .
TABLES: ZGL008                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
