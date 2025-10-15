*&---------------------------------------------------------------------*
*&  Include           MZLES0030003
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.

  IF vg_tela_0003 IS INITIAL.
    vg_tela_0003 = c_0004.
  ENDIF.

  IF vg_tela_0003b IS INITIAL.
    vg_tela_0003b = c_0005.
  ENDIF.

ENDMODULE.                 " STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PESQUISA_ITENS_LOTE_CONF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pesquisa_itens_lote_conf_adm.
  DATA: it_zlest0025 TYPE TABLE OF zlest0025 WITH HEADER LINE.
  CLEAR: it_lotes_item_cof_adm_aux[], it_lotes_item_alv[].

  CALL FUNCTION 'Z_PFE_PSQ_ITENS'
    EXPORTING
      p_lote_alv  = wa_lotes_alv
    TABLES
      p_itens     = it_lotes_item_cof_adm_aux
*      p_itens_alv = it_lotes_item_alv
    EXCEPTIONS
      sem_itens   = 1
      OTHERS      = 2.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF it_lotes_item_cof_adm_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE it_zlest0025
      FROM zlest0025
       FOR ALL ENTRIES IN it_lotes_item_cof_adm_aux
       WHERE chvid EQ it_lotes_item_cof_adm_aux-chvid.

  ENDIF.

  REFRESH: it_lotes_item_cof_adm.
  LOOP AT it_lotes_item_cof_adm_aux.
    READ TABLE it_zlest0025
      WITH KEY chvid = it_lotes_item_cof_adm_aux-chvid.

    MOVE-CORRESPONDING: it_lotes_item_cof_adm_aux TO it_lotes_item_cof_adm.
    MOVE: it_zlest0025-deschvid   TO it_lotes_item_cof_adm-deschvid.
    it_lotes_item_cof_adm-vl_dif_adm = it_lotes_item_cof_adm-vl_conf_adm - it_lotes_item_cof_adm-vl_pago_lote.

    IF it_lotes_item_cof_adm-ck_conf_adm IS NOT INITIAL.
      it_lotes_item_cof_adm-status_conf =  icon_complete.
    ELSE.
      it_lotes_item_cof_adm-status_conf = icon_warning.
    ENDIF.

*    MODIFY IT_LOTES_ITEM_COF_ADM.
    APPEND it_lotes_item_cof_adm.
  ENDLOOP.
ENDFORM.                    " PESQUISA_ITENS_LOTE_CONF
