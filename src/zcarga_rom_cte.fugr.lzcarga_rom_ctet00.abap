*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCARGA_ROM_CTE..................................*
DATA:  BEGIN OF STATUS_ZCARGA_ROM_CTE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCARGA_ROM_CTE                .
CONTROLS: TCTRL_ZCARGA_ROM_CTE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCARGA_ROM_CTE                .
TABLES: ZCARGA_ROM_CTE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
