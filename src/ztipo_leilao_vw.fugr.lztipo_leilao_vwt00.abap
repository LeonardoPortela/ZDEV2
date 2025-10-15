*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDCO_TIPO_LEILAO................................*
DATA:  BEGIN OF STATUS_ZDCO_TIPO_LEILAO              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDCO_TIPO_LEILAO              .
CONTROLS: TCTRL_ZDCO_TIPO_LEILAO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDCO_TIPO_LEILAO              .
TABLES: ZDCO_TIPO_LEILAO               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
