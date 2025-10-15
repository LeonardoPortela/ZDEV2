*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_CAMPOS_GUIA................................*
DATA:  BEGIN OF STATUS_ZIMP_CAMPOS_GUIA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_CAMPOS_GUIA              .
CONTROLS: TCTRL_ZIMP_CAMPOS_GUIA
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZIMP_CAMPOS_GUIA              .
TABLES: ZIMP_CAMPOS_GUIA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
