*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSD_MODELOS_XML.................................*
DATA:  BEGIN OF STATUS_ZSD_MODELOS_XML               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSD_MODELOS_XML               .
CONTROLS: TCTRL_ZSD_MODELOS_XML
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSD_MODELOS_XML               .
TABLES: ZSD_MODELOS_XML                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
