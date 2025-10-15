*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIM02_SOL_AP_CTL................................*
DATA:  BEGIN OF STATUS_ZIM02_SOL_AP_CTL              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIM02_SOL_AP_CTL              .
CONTROLS: TCTRL_ZIM02_SOL_AP_CTL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIM02_SOL_AP_CTL              .
TABLES: ZIM02_SOL_AP_CTL               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
