*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0046........................................*
DATA:  BEGIN OF STATUS_ZSDT0046                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0046                      .
CONTROLS: TCTRL_ZSDT0046
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0046                      .
TABLES: ZSDT0046                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
