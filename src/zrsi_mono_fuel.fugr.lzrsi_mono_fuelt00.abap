*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRSI_MONO_FUEL..................................*
DATA:  BEGIN OF STATUS_ZRSI_MONO_FUEL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRSI_MONO_FUEL                .
CONTROLS: TCTRL_ZRSI_MONO_FUEL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZRSI_MONO_FUEL                .
TABLES: ZRSI_MONO_FUEL                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
