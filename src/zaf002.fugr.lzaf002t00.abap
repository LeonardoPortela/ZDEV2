*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAA002..........................................*
DATA:  BEGIN OF STATUS_ZAA002                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAA002                        .
CONTROLS: TCTRL_ZAA002
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZAA002                        .
TABLES: ZAA002                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
