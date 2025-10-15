*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0094........................................*
DATA:  BEGIN OF STATUS_ZSDT0094                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0094                      .
CONTROLS: TCTRL_ZSDT0094
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0094                      .
TABLES: ZSDT0094                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
