*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0002........................................*
DATA:  BEGIN OF STATUS_ZSDT0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0002                      .
CONTROLS: TCTRL_ZSDT0002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0002                      .
TABLES: ZSDT0002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
