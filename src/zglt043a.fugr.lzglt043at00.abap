*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT043A........................................*
DATA:  BEGIN OF STATUS_ZGLT043A                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT043A                      .
CONTROLS: TCTRL_ZGLT043A
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT043A                      .
TABLES: ZGLT043A                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
