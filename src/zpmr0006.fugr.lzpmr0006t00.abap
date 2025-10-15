*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPMT001.........................................*
DATA:  BEGIN OF STATUS_ZPMT001                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT001                       .
CONTROLS: TCTRL_ZPMT001
            TYPE TABLEVIEW USING SCREEN '0100'.
*...processing: ZPMT002.........................................*
DATA:  BEGIN OF STATUS_ZPMT002                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPMT002                       .
CONTROLS: TCTRL_ZPMT002
            TYPE TABLEVIEW USING SCREEN '0200'.
*.........table declarations:.................................*
TABLES: *ZPMT001                       .
TABLES: *ZPMT002                       .
TABLES: ZPMT001                        .
TABLES: ZPMT002                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
