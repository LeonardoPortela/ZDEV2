*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZINV_APROVADOR..................................*
DATA:  BEGIN OF STATUS_ZINV_APROVADOR                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZINV_APROVADOR                .
CONTROLS: TCTRL_ZINV_APROVADOR
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZINV_APROVADOR                .
TABLES: ZINV_APROVADOR                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
