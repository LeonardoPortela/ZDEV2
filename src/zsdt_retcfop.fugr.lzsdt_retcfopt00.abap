*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT_RETCFOP....................................*
DATA:  BEGIN OF STATUS_ZSDT_RETCFOP                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_RETCFOP                  .
CONTROLS: TCTRL_ZSDT_RETCFOP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT_RETCFOP                  .
TABLES: ZSDT_RETCFOP                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
