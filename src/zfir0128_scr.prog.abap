*&---------------------------------------------------------------------*
*& Include          ZFIR0128_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_bukrs  FOR j_1bnfdoc-bukrs OBLIGATORY,
                  s_werks  FOR j_1bnflin-werks OBLIGATORY,
                  s_docdat FOR j_1bnfdoc-docdat,
                  s_dteve  FOR zib_nfe_dist_avb-dt_evento,
                  s_matnr  FOR j_1bnflin-matnr,
                  s_docnum FOR j_1bnfdoc-docnum,
                  s_chave  FOR zib_nfe_dist_avb-chave_nfe,
                  s_cfop   FOR j_1bnflin-cfop,
                  s_kunnr  FOR j_1bnfdoc-parid.

SELECTION-SCREEN END OF BLOCK b1.
