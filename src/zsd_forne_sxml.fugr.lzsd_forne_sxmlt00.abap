*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_FORNE_SXML..................................*
DATA:  BEGIN OF STATUS_ZSD_FORNE_SXML                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_FORNE_SXML                .
CONTROLS: TCTRL_ZSD_FORNE_SXML
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_FORNE_SXML                .
TABLES: ZSD_FORNE_SXML                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
