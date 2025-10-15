*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOPENSIS_004...................................*
DATA:  BEGIN OF STATUS_ZTOPENSIS_004                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOPENSIS_004                 .
CONTROLS: TCTRL_ZTOPENSIS_004
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTOPENSIS_004                 .
TABLES: ZTOPENSIS_004                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
