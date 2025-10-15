*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTAB_PENSAO.....................................*
DATA:  BEGIN OF STATUS_ZTAB_PENSAO                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTAB_PENSAO                   .
CONTROLS: TCTRL_ZTAB_PENSAO
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZTAB_PENSAO                   .
TABLES: ZTAB_PENSAO                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
