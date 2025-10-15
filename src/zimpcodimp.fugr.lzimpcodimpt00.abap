*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIMPCODIMP......................................*
DATA:  BEGIN OF STATUS_ZIMPCODIMP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIMPCODIMP                    .
CONTROLS: TCTRL_ZIMPCODIMP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIMPCODIMP                    .
TABLES: ZIMPCODIMP                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
