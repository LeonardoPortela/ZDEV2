*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL015_DRE_CVEND................................*
DATA:  BEGIN OF STATUS_ZGL015_DRE_CVEND              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL015_DRE_CVEND              .
CONTROLS: TCTRL_ZGL015_DRE_CVEND
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL015_DRE_CVEND              .
TABLES: ZGL015_DRE_CVEND               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
