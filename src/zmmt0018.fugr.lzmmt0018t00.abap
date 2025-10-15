*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0018........................................*
DATA:  BEGIN OF STATUS_ZMMT0018                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0018                      .
CONTROLS: TCTRL_ZMMT0018
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMMT0018                      .
TABLES: ZMMT0018                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
