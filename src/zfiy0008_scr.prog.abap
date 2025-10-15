*&---------------------------------------------------------------------*
*&  Include           ZFIY0008_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_mwskz       FOR  bseg-mwskz,   "Tax codes
                s_spcglk      FOR  bseg-umskz,   "Special GL ind. Vendor
                s_spcgld      FOR  bseg-umskz.   "Special GL ind. Custom
PARAMETERS:     s_event      LIKE bkorm-event      DEFAULT 'J1A01'.

SELECTION-SCREEN ULINE.
PARAMETERS:     s_plist       LIKE rfpdo-allgprin  DEFAULT 'X',
                s_precs       LIKE j_1afpdo-printrec.
SELECTION-SCREEN END OF BLOCK 1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-s02.
PARAMETERS:     s_file1       LIKE rfpdo1-allgunix.
SELECTION-SCREEN END OF BLOCK 2.

SELECTION-SCREEN BEGIN OF BLOCK var WITH FRAME TITLE vari_tit.
PARAMETERS: p_varia TYPE slis_vari.       " Layout
SELECTION-SCREEN END OF BLOCK var.

SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE text-001.
PARAMETERS: p_comp RADIOBUTTON GROUP r1.       " Compras
PARAMETERS: p_vent RADIOBUTTON GROUP r1.       " Ventas
SELECTION-SCREEN END OF BLOCK bl.

* Change 25.08.2011 - Valor mínimo AFIP - Sonda
SELECTION-SCREEN BEGIN OF BLOCK zl WITH FRAME TITLE text-z01.
PARAMETERS: p_vatam  TYPE BSEG-DMBTR DEFAULT '500.00'.       " Compras
SELECTION-SCREEN END OF BLOCK zl.
* End change 25.08.2011 .......................
