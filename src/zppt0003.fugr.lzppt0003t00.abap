*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPPT0003........................................*
DATA:  BEGIN OF STATUS_ZPPT0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPPT0003                      .
CONTROLS: TCTRL_ZPPT0003
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPPT0003                      .
TABLES: ZPPT0003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
