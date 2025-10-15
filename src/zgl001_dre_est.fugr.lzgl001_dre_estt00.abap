*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL001_COMP_F44.................................*
DATA:  BEGIN OF STATUS_ZGL001_COMP_F44               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL001_COMP_F44               .
CONTROLS: TCTRL_ZGL001_COMP_F44
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZGL001_DRE_EST..................................*
DATA:  BEGIN OF STATUS_ZGL001_DRE_EST                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL001_DRE_EST                .
CONTROLS: TCTRL_ZGL001_DRE_EST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL001_COMP_F44               .
TABLES: *ZGL001_DRE_EST                .
TABLES: ZGL001_COMP_F44                .
TABLES: ZGL001_DRE_EST                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
