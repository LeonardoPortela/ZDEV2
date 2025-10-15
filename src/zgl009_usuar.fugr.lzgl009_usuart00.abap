*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL009_USUAR_DRE................................*
DATA:  BEGIN OF STATUS_ZGL009_USUAR_DRE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL009_USUAR_DRE              .
CONTROLS: TCTRL_ZGL009_USUAR_DRE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL009_USUAR_DRE              .
TABLES: ZGL009_USUAR_DRE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
