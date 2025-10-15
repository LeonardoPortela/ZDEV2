*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWAT0003........................................*
DATA:  BEGIN OF STATUS_ZWAT0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWAT0003                      .
*.........table declarations:.................................*
TABLES: *ZWAT0003                      .
TABLES: ZWAT0003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
