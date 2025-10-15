*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT078.........................................*
DATA:  BEGIN OF STATUS_ZGLT078                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT078                       .
CONTROLS: TCTRL_ZGLT078
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZGLT078                       .
TABLES: ZGLT078                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
