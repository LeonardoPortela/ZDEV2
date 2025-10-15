*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL014_DRE_DEPA.................................*
DATA:  BEGIN OF STATUS_ZGL014_DRE_DEPA               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL014_DRE_DEPA               .
CONTROLS: TCTRL_ZGL014_DRE_DEPA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL014_DRE_DEPA               .
TABLES: ZGL014_DRE_DEPA                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
