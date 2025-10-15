*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPPT0002........................................*
DATA:  BEGIN OF STATUS_ZPPT0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPPT0002                      .
CONTROLS: TCTRL_ZPPT0002
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZPPT0002                      .
TABLES: ZPPT0002                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
