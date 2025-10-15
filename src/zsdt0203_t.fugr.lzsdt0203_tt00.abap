*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0203_T1.....................................*
DATA:  BEGIN OF STATUS_ZSDT0203_T1                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0203_T1                   .
CONTROLS: TCTRL_ZSDT0203_T1
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0203_T1                   .
TABLES: ZSDT0203_T1                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
