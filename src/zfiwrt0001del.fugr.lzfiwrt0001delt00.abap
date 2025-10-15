*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT0001DEL...................................*
DATA:  BEGIN OF STATUS_ZFIWRT0001DEL                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT0001DEL                 .
CONTROLS: TCTRL_ZFIWRT0001DEL
            TYPE TABLEVIEW USING SCREEN '0212'.
*.........table declarations:.................................*
TABLES: *ZFIWRT0001DEL                 .
TABLES: ZFIWRT0001DEL                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
