*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL015_DRE_EST08................................*
DATA:  BEGIN OF STATUS_ZGL015_DRE_EST08              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL015_DRE_EST08              .
CONTROLS: TCTRL_ZGL015_DRE_EST08
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL015_DRE_EST08              .
TABLES: ZGL015_DRE_EST08               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
