*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_APROV_RCC...................................*
DATA:  BEGIN OF STATUS_ZMM_APROV_RCC                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_APROV_RCC                 .
CONTROLS: TCTRL_ZMM_APROV_RCC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_APROV_RCC                 .
TABLES: ZMM_APROV_RCC                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
