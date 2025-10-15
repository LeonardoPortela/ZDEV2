*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAMB_HOMOLOG....................................*
DATA:  BEGIN OF STATUS_ZAMB_HOMOLOG                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAMB_HOMOLOG                  .
CONTROLS: TCTRL_ZAMB_HOMOLOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAMB_HOMOLOG                  .
TABLES: ZAMB_HOMOLOG                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
