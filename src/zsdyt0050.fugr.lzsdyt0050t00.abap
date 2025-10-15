*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDYT0050.......................................*
DATA:  BEGIN OF STATUS_ZSDYT0050                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDYT0050                     .
CONTROLS: TCTRL_ZSDYT0050
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDYT0050                     .
TABLES: ZSDYT0050                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
