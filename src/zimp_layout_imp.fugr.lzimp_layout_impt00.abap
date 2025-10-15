*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMP_LAYOUT_IMP.................................*
DATA:  BEGIN OF STATUS_ZIMP_LAYOUT_IMP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMP_LAYOUT_IMP               .
CONTROLS: TCTRL_ZIMP_LAYOUT_IMP
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZIMP_LAYOUT_IMP               .
TABLES: ZIMP_LAYOUT_IMP                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
