*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOPENSIS_003...................................*
DATA:  BEGIN OF STATUS_ZTOPENSIS_003                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOPENSIS_003                 .
CONTROLS: TCTRL_ZTOPENSIS_003
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOPENSIS_003                 .
TABLES: ZTOPENSIS_003                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
