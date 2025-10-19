*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_CAD_DEPTO..................................*
DATA:  BEGIN OF STATUS_ZIMP_CAD_DEPTO                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_CAD_DEPTO                .
CONTROLS: TCTRL_ZIMP_CAD_DEPTO
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZIMP_CAD_DEPTO                .
TABLES: ZIMP_CAD_DEPTO                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
