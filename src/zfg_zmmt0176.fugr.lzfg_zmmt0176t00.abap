*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMMT0176........................................*
DATA:  BEGIN OF STATUS_ZMMT0176                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMMT0176                      .
CONTROLS: TCTRL_ZMMT0176
            TYPE TABLEVIEW USING SCREEN '0010'.
*.........table declarations:.................................*
TABLES: *ZMMT0176                      .
TABLES: ZMMT0176                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
