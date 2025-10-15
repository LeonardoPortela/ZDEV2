*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT2001......................................*
DATA:  BEGIN OF STATUS_ZFIWRT2001                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT2001                    .
CONTROLS: TCTRL_ZFIWRT2001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIWRT2001                    .
TABLES: ZFIWRT2001                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
