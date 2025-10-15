*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIB_CONTABIL....................................*
DATA:  BEGIN OF STATUS_ZIB_CONTABIL                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIB_CONTABIL                  .
CONTROLS: TCTRL_ZIB_CONTABIL
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZIB_CONTABIL                  .
TABLES: ZIB_CONTABIL                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
