*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMPCODOBRIG....................................*
DATA:  BEGIN OF STATUS_ZIMPCODOBRIG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMPCODOBRIG                  .
CONTROLS: TCTRL_ZIMPCODOBRIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMPCODOBRIG                  .
TABLES: ZIMPCODOBRIG                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
