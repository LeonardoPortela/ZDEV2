*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIM010..........................................*
DATA:  BEGIN OF STATUS_ZIM010                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIM010                        .
CONTROLS: TCTRL_ZIM010
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIM010                        .
TABLES: ZIM010                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
