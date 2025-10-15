*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT0007......................................*
DATA:  BEGIN OF STATUS_ZFIWRT0007                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT0007                    .
CONTROLS: TCTRL_ZFIWRT0007
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFIWRT0016......................................*
DATA:  BEGIN OF STATUS_ZFIWRT0016                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT0016                    .
CONTROLS: TCTRL_ZFIWRT0016
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZFIWRT0007                    .
TABLES: *ZFIWRT0016                    .
TABLES: ZFIWRT0007                     .
TABLES: ZFIWRT0016                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
