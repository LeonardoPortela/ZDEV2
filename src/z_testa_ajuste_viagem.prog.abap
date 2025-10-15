*&---------------------------------------------------------------------*
*& Report Z_TESTA_AJUSTE_VIAGEM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_testa_ajuste_viagem.

TABLES zcte_ajustes.

DATA: lv_status_ok TYPE char01,
      lv_mensagem  TYPE char300,
      lc_viagem    TYPE REF TO zcl_ciot_viagem.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME.
  PARAMETERS: p_nucon LIKE zcte_ajustes-nucontrato,
              p_chvid LIKE zcte_ajustes-chvid.
SELECTION-SCREEN END   OF BLOCK b5.

CREATE OBJECT lc_viagem.

lv_status_ok = lc_viagem->ajustar_viagem( EXPORTING i_nucontrato = p_nucon
                                                    i_chvid      = p_chvid
                                          IMPORTING e_mensagem   = lv_mensagem ).

BREAK-POINT.
