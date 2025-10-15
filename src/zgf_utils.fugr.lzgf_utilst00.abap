*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAUTH_WEBSERVICE................................*
DATA:  BEGIN OF STATUS_ZAUTH_WEBSERVICE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAUTH_WEBSERVICE              .
CONTROLS: TCTRL_ZAUTH_WEBSERVICE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAUTH_WEBSERVICE              .
TABLES: ZAUTH_WEBSERVICE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
