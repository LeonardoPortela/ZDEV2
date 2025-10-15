*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0039........................................*
DATA:  BEGIN OF STATUS_ZSDT0039                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0039                      .
CONTROLS: TCTRL_ZSDT0039
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0039                      .
TABLES: ZSDT0039                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
