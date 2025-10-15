*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL002_DRE_EST..................................*
DATA:  BEGIN OF STATUS_ZGL002_DRE_EST                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL002_DRE_EST                .
CONTROLS: TCTRL_ZGL002_DRE_EST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL002_DRE_EST                .
TABLES: ZGL002_DRE_EST                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
