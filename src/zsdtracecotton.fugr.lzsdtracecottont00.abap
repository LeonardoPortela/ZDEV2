*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0176........................................*
DATA:  BEGIN OF STATUS_ZSDT0176                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0176                      .
CONTROLS: TCTRL_ZSDT0176
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZSDT0176                      .
TABLES: ZSDT0176                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
