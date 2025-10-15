*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT_APROVADORES................................*
DATA:  BEGIN OF STATUS_ZSDT_APROVADORES              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT_APROVADORES              .
CONTROLS: TCTRL_ZSDT_APROVADORES
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZSDT_APROVADORES              .
TABLES: ZSDT_APROVADORES               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
