*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT2000......................................*
DATA:  BEGIN OF STATUS_ZFIWRT2000                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT2000                    .
CONTROLS: TCTRL_ZFIWRT2000
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIWRT2000                    .
TABLES: ZFIWRT2000                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
