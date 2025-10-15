*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0048........................................*
DATA:  BEGIN OF STATUS_ZSDT0048                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0048                      .
CONTROLS: TCTRL_ZSDT0048
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0048                      .
TABLES: ZSDT0048                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
