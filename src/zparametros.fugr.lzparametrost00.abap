*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPARAMETROS.....................................*
DATA:  BEGIN OF STATUS_ZPARAMETROS                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPARAMETROS                   .
CONTROLS: TCTRL_ZPARAMETROS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPARAMETROS                   .
TABLES: ZPARAMETROS                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
