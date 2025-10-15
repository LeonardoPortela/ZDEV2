*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWAT0001........................................*
DATA:  BEGIN OF STATUS_ZWAT0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWAT0001                      .
CONTROLS: TCTRL_ZWAT0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZWAT0001                      .
TABLES: ZWAT0001                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
