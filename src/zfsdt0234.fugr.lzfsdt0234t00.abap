*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0234........................................*
DATA:  BEGIN OF STATUS_ZSDT0234                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0234                      .
CONTROLS: TCTRL_ZSDT0234
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0234                      .
TABLES: ZSDT0234                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
