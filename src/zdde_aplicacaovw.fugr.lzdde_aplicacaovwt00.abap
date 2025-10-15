*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDDE_APLICACAO..................................*
DATA:  BEGIN OF STATUS_ZDDE_APLICACAO                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDDE_APLICACAO                .
CONTROLS: TCTRL_ZDDE_APLICACAO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDDE_APLICACAO                .
TABLES: ZDDE_APLICACAO                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
