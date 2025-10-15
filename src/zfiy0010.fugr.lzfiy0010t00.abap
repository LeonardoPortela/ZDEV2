*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIYT_CTAREGION.................................*
DATA:  BEGIN OF STATUS_ZFIYT_CTAREGION               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIYT_CTAREGION               .
CONTROLS: TCTRL_ZFIYT_CTAREGION
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIYT_CTAREGION               .
TABLES: ZFIYT_CTAREGION                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
