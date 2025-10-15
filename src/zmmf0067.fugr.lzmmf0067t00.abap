*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0067........................................*
DATA:  BEGIN OF STATUS_ZMMT0067                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0067                      .
CONTROLS: TCTRL_ZMMT0067
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZMMT0067                      .
TABLES: ZMMT0067                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
