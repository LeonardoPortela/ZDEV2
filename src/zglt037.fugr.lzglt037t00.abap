*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT037.........................................*
DATA:  BEGIN OF STATUS_ZGLT037                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT037                       .
CONTROLS: TCTRL_ZGLT037
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT037                       .
TABLES: ZGLT037                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
