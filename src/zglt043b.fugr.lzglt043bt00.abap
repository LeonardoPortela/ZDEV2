*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT043B........................................*
DATA:  BEGIN OF STATUS_ZGLT043B                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT043B                      .
CONTROLS: TCTRL_ZGLT043B
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGLT043B                      .
TABLES: ZGLT043B                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
