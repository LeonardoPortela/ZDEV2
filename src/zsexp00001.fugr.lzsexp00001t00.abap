*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSEXPT00001.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00001                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00001                   .
CONTROLS: TCTRL_ZSEXPT00001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZSEXPT00002.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00002                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00002                   .
CONTROLS: TCTRL_ZSEXPT00002
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZSEXPT00003.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00003                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00003                   .
CONTROLS: TCTRL_ZSEXPT00003
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZSEXPT00004.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00004                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00004                   .
CONTROLS: TCTRL_ZSEXPT00004
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZSEXPT00005.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00005                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00005                   .
CONTROLS: TCTRL_ZSEXPT00005
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZSEXPT00006.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00006                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00006                   .
CONTROLS: TCTRL_ZSEXPT00006
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZSEXPT00007.....................................*
DATA:  BEGIN OF STATUS_ZSEXPT00007                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSEXPT00007                   .
CONTROLS: TCTRL_ZSEXPT00007
            TYPE TABLEVIEW USING SCREEN '0009'.
*.........table declarations:.................................*
TABLES: *ZSEXPT00001                   .
TABLES: *ZSEXPT00002                   .
TABLES: *ZSEXPT00003                   .
TABLES: *ZSEXPT00004                   .
TABLES: *ZSEXPT00005                   .
TABLES: *ZSEXPT00006                   .
TABLES: *ZSEXPT00007                   .
TABLES: ZSEXPT00001                    .
TABLES: ZSEXPT00002                    .
TABLES: ZSEXPT00003                    .
TABLES: ZSEXPT00004                    .
TABLES: ZSEXPT00005                    .
TABLES: ZSEXPT00006                    .
TABLES: ZSEXPT00007                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
