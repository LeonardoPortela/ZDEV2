*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZSDT0001CE......................................*
DATA:  BEGIN OF STATUS_ZSDT0001CE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001CE                    .
CONTROLS: TCTRL_ZSDT0001CE
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZSDT0001LE......................................*
DATA:  BEGIN OF STATUS_ZSDT0001LE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001LE                    .
CONTROLS: TCTRL_ZSDT0001LE
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZSDT0001LP......................................*
DATA:  BEGIN OF STATUS_ZSDT0001LP                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001LP                    .
CONTROLS: TCTRL_ZSDT0001LP
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZSDT0001OD......................................*
DATA:  BEGIN OF STATUS_ZSDT0001OD                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001OD                    .
CONTROLS: TCTRL_ZSDT0001OD
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZSDT0001PD......................................*
DATA:  BEGIN OF STATUS_ZSDT0001PD                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001PD                    .
CONTROLS: TCTRL_ZSDT0001PD
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZSDT0001TE......................................*
DATA:  BEGIN OF STATUS_ZSDT0001TE                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001TE                    .
CONTROLS: TCTRL_ZSDT0001TE
            TYPE TABLEVIEW USING SCREEN '0011'.
*...processing: ZSDT0001TECFOP..................................*
DATA:  BEGIN OF STATUS_ZSDT0001TECFOP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001TECFOP                .
CONTROLS: TCTRL_ZSDT0001TECFOP
            TYPE TABLEVIEW USING SCREEN '0015'.
*...processing: ZSDT0001TELN....................................*
DATA:  BEGIN OF STATUS_ZSDT0001TELN                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001TELN                  .
CONTROLS: TCTRL_ZSDT0001TELN
            TYPE TABLEVIEW USING SCREEN '0017'.
*...processing: ZSDT0001TETX....................................*
DATA:  BEGIN OF STATUS_ZSDT0001TETX                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZSDT0001TETX                  .
CONTROLS: TCTRL_ZSDT0001TETX
            TYPE TABLEVIEW USING SCREEN '0013'.
*.........table declarations:.................................*
TABLES: *ZSDT0001CE                    .
TABLES: *ZSDT0001LE                    .
TABLES: *ZSDT0001LP                    .
TABLES: *ZSDT0001OD                    .
TABLES: *ZSDT0001PD                    .
TABLES: *ZSDT0001TE                    .
TABLES: *ZSDT0001TECFOP                .
TABLES: *ZSDT0001TELN                  .
TABLES: *ZSDT0001TETX                  .
TABLES: ZSDT0001CE                     .
TABLES: ZSDT0001LE                     .
TABLES: ZSDT0001LP                     .
TABLES: ZSDT0001OD                     .
TABLES: ZSDT0001PD                     .
TABLES: ZSDT0001TE                     .
TABLES: ZSDT0001TECFOP                 .
TABLES: ZSDT0001TELN                   .
TABLES: ZSDT0001TETX                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
