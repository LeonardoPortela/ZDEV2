*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZREG_EXPORTACAO.................................*
DATA:  BEGIN OF STATUS_ZREG_EXPORTACAO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZREG_EXPORTACAO               .
CONTROLS: TCTRL_ZREG_EXPORTACAO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZREG_EXPORTACAO               .
TABLES: ZREG_EXPORTACAO                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
