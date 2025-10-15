*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0418........................................*
DATA:  BEGIN OF STATUS_ZSDT0418                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0418                      .
CONTROLS: TCTRL_ZSDT0418
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0418                      .
TABLES: ZSDT0418                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
