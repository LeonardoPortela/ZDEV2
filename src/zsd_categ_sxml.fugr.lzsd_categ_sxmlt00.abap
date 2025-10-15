*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_CATEG_SXML..................................*
DATA:  BEGIN OF STATUS_ZSD_CATEG_SXML                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_CATEG_SXML                .
CONTROLS: TCTRL_ZSD_CATEG_SXML
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_CATEG_SXML                .
TABLES: ZSD_CATEG_SXML                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
