*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGLT064.........................................*
DATA:  BEGIN OF STATUS_ZGLT064                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGLT064                       .
CONTROLS: TCTRL_ZGLT064
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZGLT064                       .
TABLES: ZGLT064                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
