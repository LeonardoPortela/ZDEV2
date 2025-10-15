*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0037........................................*
DATA:  BEGIN OF STATUS_ZSDT0037                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0037                      .
CONTROLS: TCTRL_ZSDT0037
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0037                      .
TABLES: ZSDT0037                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
