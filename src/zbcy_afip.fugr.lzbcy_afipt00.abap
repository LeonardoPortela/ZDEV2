*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZBCY_AFIP.......................................*
DATA:  BEGIN OF STATUS_ZBCY_AFIP                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZBCY_AFIP                     .
CONTROLS: TCTRL_ZBCY_AFIP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZBCY_AFIP                     .
TABLES: ZBCY_AFIP                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
