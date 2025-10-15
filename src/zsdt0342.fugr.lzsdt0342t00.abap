*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0342........................................*
DATA:  BEGIN OF STATUS_ZSDT0342                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0342                      .
CONTROLS: TCTRL_ZSDT0342
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0342                      .
TABLES: ZSDT0342                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
