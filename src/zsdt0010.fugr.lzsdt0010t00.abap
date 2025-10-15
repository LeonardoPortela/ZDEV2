*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0010........................................*
DATA:  BEGIN OF STATUS_ZSDT0010                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0010                      .
CONTROLS: TCTRL_ZSDT0010
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0010                      .
TABLES: ZSDT0010                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
