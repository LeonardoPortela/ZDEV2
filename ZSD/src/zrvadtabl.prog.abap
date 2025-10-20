***INCLUDE RVADTABL .
TABLES: NAST,                          "Messages
        *NAST,                         "Messages
        TNAPR,                         "Programs & Forms
        ITCPO,                         "Communicationarea for Spool
        ARC_PARAMS,                    "Archive parameters
        TOA_DARA,                      "Archive parameters
        ADDR_KEY.                      "Adressnumber for ADDRESS

TYPE-POOLS SZADR.

*************************Start of Note changes 1508474*********************************

  DATA:  gv_inupd           TYPE i,
         gv_fp_outputparams  type  sfpoutputparams. " Output parameters..

*External Send
  DATA:  gv_comm_type     TYPE ad_comm,
         gv_comm_values   TYPE szadr_comm_values,
         gs_recipient     TYPE swotobjid,
         gs_sender        TYPE swotobjid,
         gs_intnast       TYPE snast,
         gv_xdevice(10),
         gv_xprogramm     TYPE tdprogram,
         gv_xdialog.

* sending output vai mail
DATA:  gv_pdf_content        TYPE solix_tab.


*************************End of Note changes 1508474*********************************
