*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0043.......................................*
DATA:  BEGIN OF STATUS_ZLEST0043                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0043                     .
CONTROLS: TCTRL_ZLEST0043
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST0043                     .
TABLES: ZLEST0043                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
