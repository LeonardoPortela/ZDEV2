*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLEST0001.......................................*
DATA:  BEGIN OF STATUS_ZLEST0001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0001                     .
CONTROLS: TCTRL_ZLEST0001
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZLEST0002.......................................*
DATA:  BEGIN OF STATUS_ZLEST0002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0002                     .
CONTROLS: TCTRL_ZLEST0002
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZLEST0003.......................................*
DATA:  BEGIN OF STATUS_ZLEST0003                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0003                     .
CONTROLS: TCTRL_ZLEST0003
            TYPE TABLEVIEW USING SCREEN '0010'.
*...processing: ZLEST0004.......................................*
DATA:  BEGIN OF STATUS_ZLEST0004                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0004                     .
CONTROLS: TCTRL_ZLEST0004
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZLEST0005.......................................*
DATA:  BEGIN OF STATUS_ZLEST0005                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0005                     .
CONTROLS: TCTRL_ZLEST0005
            TYPE TABLEVIEW USING SCREEN '0012'.
*...processing: ZLEST0006.......................................*
DATA:  BEGIN OF STATUS_ZLEST0006                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0006                     .
CONTROLS: TCTRL_ZLEST0006
            TYPE TABLEVIEW USING SCREEN '0022'.
*...processing: ZLEST0008.......................................*
DATA:  BEGIN OF STATUS_ZLEST0008                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0008                     .
CONTROLS: TCTRL_ZLEST0008
            TYPE TABLEVIEW USING SCREEN '0020'.
*...processing: ZLEST0009.......................................*
DATA:  BEGIN OF STATUS_ZLEST0009                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0009                     .
CONTROLS: TCTRL_ZLEST0009
            TYPE TABLEVIEW USING SCREEN '0013'.
*...processing: ZLEST0010.......................................*
DATA:  BEGIN OF STATUS_ZLEST0010                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0010                     .
CONTROLS: TCTRL_ZLEST0010
            TYPE TABLEVIEW USING SCREEN '0014'.
*...processing: ZLEST0013.......................................*
DATA:  BEGIN OF STATUS_ZLEST0013                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0013                     .
CONTROLS: TCTRL_ZLEST0013
            TYPE TABLEVIEW USING SCREEN '0015'.
*...processing: ZLEST0019.......................................*
DATA:  BEGIN OF STATUS_ZLEST0019                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0019                     .
CONTROLS: TCTRL_ZLEST0019
            TYPE TABLEVIEW USING SCREEN '0021'.
*...processing: ZLEST0021.......................................*
DATA:  BEGIN OF STATUS_ZLEST0021                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0021                     .
CONTROLS: TCTRL_ZLEST0021
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZLEST0023.......................................*
DATA:  BEGIN OF STATUS_ZLEST0023                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0023                     .
CONTROLS: TCTRL_ZLEST0023
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZLEST0024.......................................*
DATA:  BEGIN OF STATUS_ZLEST0024                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0024                     .
CONTROLS: TCTRL_ZLEST0024
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZLEST0025.......................................*
DATA:  BEGIN OF STATUS_ZLEST0025                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0025                     .
CONTROLS: TCTRL_ZLEST0025
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZLEST0026.......................................*
DATA:  BEGIN OF STATUS_ZLEST0026                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0026                     .
CONTROLS: TCTRL_ZLEST0026
            TYPE TABLEVIEW USING SCREEN '0018'.
*...processing: ZLEST0027.......................................*
DATA:  BEGIN OF STATUS_ZLEST0027                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0027                     .
CONTROLS: TCTRL_ZLEST0027
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZLEST0028.......................................*
DATA:  BEGIN OF STATUS_ZLEST0028                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0028                     .
CONTROLS: TCTRL_ZLEST0028
            TYPE TABLEVIEW USING SCREEN '0016'.
*...processing: ZLEST0030.......................................*
DATA:  BEGIN OF STATUS_ZLEST0030                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0030                     .
CONTROLS: TCTRL_ZLEST0030
            TYPE TABLEVIEW USING SCREEN '0017'.
*...processing: ZLEST0031.......................................*
DATA:  BEGIN OF STATUS_ZLEST0031                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLEST0031                     .
CONTROLS: TCTRL_ZLEST0031
            TYPE TABLEVIEW USING SCREEN '0019'.
*.........table declarations:.................................*
TABLES: *ZLEST0001                     .
TABLES: *ZLEST0002                     .
TABLES: *ZLEST0003                     .
TABLES: *ZLEST0004                     .
TABLES: *ZLEST0005                     .
TABLES: *ZLEST0006                     .
TABLES: *ZLEST0008                     .
TABLES: *ZLEST0009                     .
TABLES: *ZLEST0010                     .
TABLES: *ZLEST0013                     .
TABLES: *ZLEST0019                     .
TABLES: *ZLEST0021                     .
TABLES: *ZLEST0023                     .
TABLES: *ZLEST0024                     .
TABLES: *ZLEST0025                     .
TABLES: *ZLEST0026                     .
TABLES: *ZLEST0027                     .
TABLES: *ZLEST0028                     .
TABLES: *ZLEST0030                     .
TABLES: *ZLEST0031                     .
TABLES: ZLEST0001                      .
TABLES: ZLEST0002                      .
TABLES: ZLEST0003                      .
TABLES: ZLEST0004                      .
TABLES: ZLEST0005                      .
TABLES: ZLEST0006                      .
TABLES: ZLEST0008                      .
TABLES: ZLEST0009                      .
TABLES: ZLEST0010                      .
TABLES: ZLEST0013                      .
TABLES: ZLEST0019                      .
TABLES: ZLEST0021                      .
TABLES: ZLEST0023                      .
TABLES: ZLEST0024                      .
TABLES: ZLEST0025                      .
TABLES: ZLEST0026                      .
TABLES: ZLEST0027                      .
TABLES: ZLEST0028                      .
TABLES: ZLEST0030                      .
TABLES: ZLEST0031                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
