*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0260........................................*
DATA:  BEGIN OF STATUS_ZSDT0260                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0260                      .
CONTROLS: TCTRL_ZSDT0260
            TYPE TABLEVIEW USING SCREEN '0901'.
*.........table declarations:.................................*
TABLES: *ZSDT0260                      .
TABLES: ZSDT0260                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
