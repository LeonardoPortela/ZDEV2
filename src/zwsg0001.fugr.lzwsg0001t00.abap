*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWST0001........................................*
DATA:  BEGIN OF STATUS_ZWST0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWST0001                      .
CONTROLS: TCTRL_ZWST0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZWST0002........................................*
DATA:  BEGIN OF STATUS_ZWST0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWST0002                      .
CONTROLS: TCTRL_ZWST0002
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZWST0003........................................*
DATA:  BEGIN OF STATUS_ZWST0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWST0003                      .
CONTROLS: TCTRL_ZWST0003
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZWST0001                      .
TABLES: *ZWST0002                      .
TABLES: *ZWST0003                      .
TABLES: ZWST0001                       .
TABLES: ZWST0002                       .
TABLES: ZWST0003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
