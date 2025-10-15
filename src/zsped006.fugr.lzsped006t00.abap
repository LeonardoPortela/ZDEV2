*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSPED006........................................*
DATA:  BEGIN OF STATUS_ZSPED006                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSPED006                      .
CONTROLS: TCTRL_ZSPED006
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSPED006                      .
TABLES: ZSPED006                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
