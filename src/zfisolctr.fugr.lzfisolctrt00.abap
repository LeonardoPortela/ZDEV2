*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFISOLCTR.......................................*
DATA:  BEGIN OF STATUS_ZFISOLCTR                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFISOLCTR                     .
CONTROLS: TCTRL_ZFISOLCTR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZFISOLCTR                     .
TABLES: ZFISOLCTR                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
