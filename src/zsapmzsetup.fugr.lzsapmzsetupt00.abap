*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCIOT_WEBSERVICE................................*
DATA:  BEGIN OF STATUS_ZCIOT_WEBSERVICE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCIOT_WEBSERVICE              .
CONTROLS: TCTRL_ZCIOT_WEBSERVICE
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZSAPMZSETUP.....................................*
DATA:  BEGIN OF STATUS_ZSAPMZSETUP                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSAPMZSETUP                   .
CONTROLS: TCTRL_ZSAPMZSETUP
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZSAPMZSETUP_ROOT................................*
DATA:  BEGIN OF STATUS_ZSAPMZSETUP_ROOT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSAPMZSETUP_ROOT              .
CONTROLS: TCTRL_ZSAPMZSETUP_ROOT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCIOT_WEBSERVICE              .
TABLES: *ZSAPMZSETUP                   .
TABLES: *ZSAPMZSETUP_ROOT              .
TABLES: ZCIOT_WEBSERVICE               .
TABLES: ZSAPMZSETUP                    .
TABLES: ZSAPMZSETUP_ROOT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
