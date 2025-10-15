*&---------------------------------------------------------------------*
*&  Include           ZMMR173_SRC
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR gva_bukrs NO-EXTENSION NO INTERVALS ,
                s_data  FOR sy-datum NO-EXTENSION.
PARAMETERS: p_tipo   TYPE ekko-bsart,
            p_pedido TYPE ebeln,
            p_nome   TYPE char50 MODIF ID t2.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_servi RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND cm1,
            p_local RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_file TYPE rlgrap-filename MODIF ID t1.
SELECTION-SCREEN END OF BLOCK b3.
