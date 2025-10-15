*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT0029......................................*
DATA:  BEGIN OF STATUS_ZFIWRT0029                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT0029                    .
CONTROLS: TCTRL_ZFIWRT0029
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIWRT0029                    .
TABLES: ZFIWRT0029                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
