*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0044........................................*
DATA:  BEGIN OF STATUS_ZSDT0044                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0044                      .
CONTROLS: TCTRL_ZSDT0044
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0044                      .
TABLES: ZSDT0044                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
