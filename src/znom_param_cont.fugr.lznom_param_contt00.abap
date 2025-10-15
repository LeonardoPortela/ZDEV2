*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNOM_PARAM_CONT.................................*
DATA:  BEGIN OF STATUS_ZNOM_PARAM_CONT               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNOM_PARAM_CONT               .
CONTROLS: TCTRL_ZNOM_PARAM_CONT
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZNOM_PARAM_CONT               .
TABLES: ZNOM_PARAM_CONT                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
