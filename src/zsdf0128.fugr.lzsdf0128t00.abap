*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0128........................................*
DATA:  BEGIN OF STATUS_ZSDT0128                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0128                      .
CONTROLS: TCTRL_ZSDT0128
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0128                      .
TABLES: ZSDT0128                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
