*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPFE_LOTE_HIS_DE................................*
DATA:  BEGIN OF STATUS_ZPFE_LOTE_HIS_DE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPFE_LOTE_HIS_DE              .
CONTROLS: TCTRL_ZPFE_LOTE_HIS_DE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPFE_LOTE_HIS_DE              .
TABLES: ZPFE_LOTE_HIS_DE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
