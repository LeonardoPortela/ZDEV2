*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0350_IN.....................................*
DATA:  BEGIN OF STATUS_ZSDT0350_IN                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0350_IN                   .
CONTROLS: TCTRL_ZSDT0350_IN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0350_IN                   .
TABLES: ZSDT0350_IN                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
