*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIM011..........................................*
DATA:  BEGIN OF STATUS_ZIM011                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIM011                        .
CONTROLS: TCTRL_ZIM011
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIM011                        .
TABLES: ZIM011                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
