*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDCO_PRODUTOR...................................*
DATA:  BEGIN OF STATUS_ZDCO_PRODUTOR                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDCO_PRODUTOR                 .
CONTROLS: TCTRL_ZDCO_PRODUTOR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDCO_PRODUTOR                 .
TABLES: ZDCO_PRODUTOR                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
