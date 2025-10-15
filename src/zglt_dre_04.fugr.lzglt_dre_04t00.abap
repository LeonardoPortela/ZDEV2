*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT_DRE_04.....................................*
DATA:  BEGIN OF STATUS_ZGLT_DRE_04                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT_DRE_04                   .
CONTROLS: TCTRL_ZGLT_DRE_04
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT_DRE_04                   .
TABLES: ZGLT_DRE_04                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
