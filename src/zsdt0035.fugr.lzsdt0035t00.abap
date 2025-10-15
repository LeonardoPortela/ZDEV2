*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0035........................................*
DATA:  BEGIN OF STATUS_ZSDT0035                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0035                      .
CONTROLS: TCTRL_ZSDT0035
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0035                      .
TABLES: ZSDT0035                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
