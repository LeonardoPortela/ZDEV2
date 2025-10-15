*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_TIPOS_IMPOS................................*
DATA:  BEGIN OF STATUS_ZIMP_TIPOS_IMPOS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_TIPOS_IMPOS              .
CONTROLS: TCTRL_ZIMP_TIPOS_IMPOS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMP_TIPOS_IMPOS              .
TABLES: ZIMP_TIPOS_IMPOS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
