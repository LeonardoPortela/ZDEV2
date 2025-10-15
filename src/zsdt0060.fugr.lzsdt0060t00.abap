*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0060........................................*
DATA:  BEGIN OF STATUS_ZSDT0060                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0060                      .
CONTROLS: TCTRL_ZSDT0060
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0060                      .
TABLES: ZSDT0060                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
