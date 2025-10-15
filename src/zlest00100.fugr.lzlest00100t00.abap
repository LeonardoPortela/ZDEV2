*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST00100......................................*
DATA:  BEGIN OF STATUS_ZLEST00100                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST00100                    .
CONTROLS: TCTRL_ZLEST00100
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLEST00100                    .
TABLES: ZLEST00100                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
