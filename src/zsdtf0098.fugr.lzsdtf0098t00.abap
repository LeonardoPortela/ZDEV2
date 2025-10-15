*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0098........................................*
DATA:  BEGIN OF STATUS_ZSDT0098                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0098                      .
CONTROLS: TCTRL_ZSDT0098
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0098                      .
TABLES: ZSDT0098                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
