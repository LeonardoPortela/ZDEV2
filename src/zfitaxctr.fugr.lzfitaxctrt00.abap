*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFITAXCTR.......................................*
DATA:  BEGIN OF STATUS_ZFITAXCTR                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFITAXCTR                     .
CONTROLS: TCTRL_ZFITAXCTR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFITAXCTR                     .
TABLES: ZFITAXCTR                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
