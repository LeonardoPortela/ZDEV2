*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDDE............................................*
DATA:  BEGIN OF STATUS_ZDDE                          .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDDE                          .
CONTROLS: TCTRL_ZDDE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDDE                          .
TABLES: ZDDE                           .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
