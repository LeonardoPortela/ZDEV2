*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSPED003........................................*
DATA:  BEGIN OF STATUS_ZSPED003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSPED003                      .
CONTROLS: TCTRL_ZSPED003
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSPED003                      .
TABLES: ZSPED003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
