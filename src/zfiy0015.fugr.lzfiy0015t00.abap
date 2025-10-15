*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIYT_PERC_IIBB.................................*
DATA:  BEGIN OF STATUS_ZFIYT_PERC_IIBB               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIYT_PERC_IIBB               .
CONTROLS: TCTRL_ZFIYT_PERC_IIBB
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIYT_PERC_IIBB               .
TABLES: ZFIYT_PERC_IIBB                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
