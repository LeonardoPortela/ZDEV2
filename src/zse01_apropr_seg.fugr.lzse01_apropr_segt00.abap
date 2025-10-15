*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSE01_APROPR_SEG................................*
DATA:  BEGIN OF STATUS_ZSE01_APROPR_SEG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSE01_APROPR_SEG              .
CONTROLS: TCTRL_ZSE01_APROPR_SEG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSE01_APROPR_SEG              .
TABLES: ZSE01_APROPR_SEG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
