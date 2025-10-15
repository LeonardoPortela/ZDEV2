*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0085.......................................*
DATA:  BEGIN OF STATUS_ZLEST0085                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0085                     .
CONTROLS: TCTRL_ZLEST0085
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLEST0085                     .
TABLES: ZLEST0085                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
