*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMT002.........................................*
DATA:  BEGIN OF STATUS_ZIMT002                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMT002                       .
CONTROLS: TCTRL_ZIMT002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMT002                       .
TABLES: *ZIMT002T                      .
TABLES: ZIMT002                        .
TABLES: ZIMT002T                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
