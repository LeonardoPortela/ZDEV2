*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0008........................................*
DATA:  BEGIN OF STATUS_ZSDT0008                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0008                      .
CONTROLS: TCTRL_ZSDT0008
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSDT0008                      .
TABLES: ZSDT0008                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
