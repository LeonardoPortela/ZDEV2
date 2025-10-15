*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT040.........................................*
DATA:  BEGIN OF STATUS_ZGLT040                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT040                       .
CONTROLS: TCTRL_ZGLT040
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZGLT040                       .
TABLES: ZGLT040                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
