*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSALDO_CTA_MOEDA................................*
DATA:  BEGIN OF STATUS_ZSALDO_CTA_MOEDA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSALDO_CTA_MOEDA              .
CONTROLS: TCTRL_ZSALDO_CTA_MOEDA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSALDO_CTA_MOEDA              .
TABLES: ZSALDO_CTA_MOEDA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
