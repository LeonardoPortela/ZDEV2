*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRSI_ICMS_MONO..................................*
DATA:  BEGIN OF STATUS_ZRSI_ICMS_MONO                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRSI_ICMS_MONO                .
CONTROLS: TCTRL_ZRSI_ICMS_MONO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRSI_ICMS_MONO                .
TABLES: ZRSI_ICMS_MONO                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
