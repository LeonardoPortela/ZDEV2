*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT039.........................................*
DATA:  BEGIN OF STATUS_ZGLT039                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT039                       .
CONTROLS: TCTRL_ZGLT039
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZGLT039                       .
TABLES: ZGLT039                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
