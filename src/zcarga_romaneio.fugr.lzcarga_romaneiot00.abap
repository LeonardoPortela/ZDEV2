*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCARGA_ROMANEIO.................................*
DATA:  BEGIN OF STATUS_ZCARGA_ROMANEIO               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCARGA_ROMANEIO               .
CONTROLS: TCTRL_ZCARGA_ROMANEIO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCARGA_ROMANEIO               .
TABLES: ZCARGA_ROMANEIO                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
