*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0039........................................*
DATA:  BEGIN OF STATUS_ZMMT0039                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0039                      .
CONTROLS: TCTRL_ZMMT0039
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0039                      .
TABLES: ZMMT0039                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
