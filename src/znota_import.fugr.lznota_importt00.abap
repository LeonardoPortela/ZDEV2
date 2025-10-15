*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZNOTA_IMPORT....................................*
DATA:  BEGIN OF STATUS_ZNOTA_IMPORT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZNOTA_IMPORT                  .
CONTROLS: TCTRL_ZNOTA_IMPORT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZNOTA_IMPORT                  .
TABLES: ZNOTA_IMPORT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
