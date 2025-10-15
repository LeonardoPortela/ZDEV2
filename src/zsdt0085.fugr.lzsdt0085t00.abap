*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0085........................................*
DATA:  BEGIN OF STATUS_ZSDT0085                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0085                      .
CONTROLS: TCTRL_ZSDT0085
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0085                      .
TABLES: ZSDT0085                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
