*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0181.......................................*
DATA:  BEGIN OF STATUS_ZLEST0181                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0181                     .
CONTROLS: TCTRL_ZLEST0181
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZLEST0183.......................................*
DATA:  BEGIN OF STATUS_ZLEST0183                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0183                     .
CONTROLS: TCTRL_ZLEST0183
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZLEST0184.......................................*
DATA:  BEGIN OF STATUS_ZLEST0184                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0184                     .
CONTROLS: TCTRL_ZLEST0184
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZLEST0181                     .
TABLES: *ZLEST0183                     .
TABLES: *ZLEST0184                     .
TABLES: ZLEST0181                      .
TABLES: ZLEST0183                      .
TABLES: ZLEST0184                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
