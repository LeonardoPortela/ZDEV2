*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0103........................................*
DATA:  BEGIN OF STATUS_ZSDT0103                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0103                      .
CONTROLS: TCTRL_ZSDT0103
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0103                      .
TABLES: ZSDT0103                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
