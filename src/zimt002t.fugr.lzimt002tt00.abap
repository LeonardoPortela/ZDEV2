*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMT002T........................................*
DATA:  BEGIN OF STATUS_ZIMT002T                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMT002T                      .
CONTROLS: TCTRL_ZIMT002T
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMT002T                      .
TABLES: ZIMT002T                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
