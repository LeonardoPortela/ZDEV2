*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIYT_EXCLUSION.................................*
DATA:  BEGIN OF STATUS_ZFIYT_EXCLUSION               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIYT_EXCLUSION               .
CONTROLS: TCTRL_ZFIYT_EXCLUSION
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIYT_EXCLUSION               .
TABLES: ZFIYT_EXCLUSION                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
