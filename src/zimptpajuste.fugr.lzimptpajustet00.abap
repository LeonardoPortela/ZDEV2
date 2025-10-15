*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMPTPAJUSTE....................................*
DATA:  BEGIN OF STATUS_ZIMPTPAJUSTE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMPTPAJUSTE                  .
CONTROLS: TCTRL_ZIMPTPAJUSTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMPTPAJUSTE                  .
TABLES: ZIMPTPAJUSTE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
