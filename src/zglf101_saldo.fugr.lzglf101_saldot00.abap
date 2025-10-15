*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT101_SALDO...................................*
DATA:  BEGIN OF STATUS_ZGLT101_SALDO                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT101_SALDO                 .
CONTROLS: TCTRL_ZGLT101_SALDO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT101_SALDO                 .
TABLES: ZGLT101_SALDO                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
