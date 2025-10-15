*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0078........................................*
DATA:  BEGIN OF STATUS_ZSDT0078                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0078                      .
CONTROLS: TCTRL_ZSDT0078
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0078                      .
TABLES: ZSDT0078                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
