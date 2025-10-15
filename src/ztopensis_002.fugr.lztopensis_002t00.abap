*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTOPENSIS_002...................................*
DATA:  BEGIN OF STATUS_ZTOPENSIS_002                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTOPENSIS_002                 .
CONTROLS: TCTRL_ZTOPENSIS_002
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZTOPENSIS_002                 .
TABLES: ZTOPENSIS_002                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
