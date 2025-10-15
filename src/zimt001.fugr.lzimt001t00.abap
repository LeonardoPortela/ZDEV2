*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMT001.........................................*
DATA:  BEGIN OF STATUS_ZIMT001                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMT001                       .
CONTROLS: TCTRL_ZIMT001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMT001                       .
TABLES: ZIMT001                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
