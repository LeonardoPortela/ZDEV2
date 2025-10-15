*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0124........................................*
DATA:  BEGIN OF STATUS_ZSDT0124                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0124                      .
CONTROLS: TCTRL_ZSDT0124
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0124                      .
TABLES: ZSDT0124                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
