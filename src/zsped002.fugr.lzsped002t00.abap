*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSPED002........................................*
DATA:  BEGIN OF STATUS_ZSPED002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSPED002                      .
CONTROLS: TCTRL_ZSPED002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSPED002                      .
TABLES: ZSPED002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
