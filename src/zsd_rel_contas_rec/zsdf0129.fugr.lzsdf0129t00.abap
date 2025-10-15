*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0350........................................*
DATA:  BEGIN OF STATUS_ZSDT0350                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0350                      .
CONTROLS: TCTRL_ZSDT0350
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0350                      .
TABLES: ZSDT0350                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
