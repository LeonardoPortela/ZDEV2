*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT082.........................................*
DATA:  BEGIN OF STATUS_ZGLT082                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT082                       .
CONTROLS: TCTRL_ZGLT082
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT082                       .
TABLES: ZGLT082                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
