*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0053D.......................................*
DATA:  BEGIN OF STATUS_ZSDT0053D                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0053D                     .
CONTROLS: TCTRL_ZSDT0053D
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0053D                     .
TABLES: ZSDT0053D                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
