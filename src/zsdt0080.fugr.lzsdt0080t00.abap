*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0080........................................*
DATA:  BEGIN OF STATUS_ZSDT0080                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0080                      .
CONTROLS: TCTRL_ZSDT0080
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZSDT0080                      .
TABLES: ZSDT0080                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
