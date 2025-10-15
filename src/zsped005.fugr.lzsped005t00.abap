*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSPED005........................................*
DATA:  BEGIN OF STATUS_ZSPED005                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSPED005                      .
CONTROLS: TCTRL_ZSPED005
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSPED005                      .
TABLES: ZSPED005                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
