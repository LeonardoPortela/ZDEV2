*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0148........................................*
DATA:  BEGIN OF STATUS_ZMMT0148                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0148                      .
CONTROLS: TCTRL_ZMMT0148
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0148                      .
TABLES: ZMMT0148                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
