*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIWRT2002......................................*
DATA:  BEGIN OF STATUS_ZFIWRT2002                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIWRT2002                    .
CONTROLS: TCTRL_ZFIWRT2002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIWRT2002                    .
TABLES: ZFIWRT2002                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
