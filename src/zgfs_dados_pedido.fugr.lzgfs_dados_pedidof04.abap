*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDI01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form incluir_pedido
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM incluir_pedido .

  DATA: it_itens_alv_aux TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
        l_tabix          TYPE sy-tabix.

  CLEAR wa_itens_sel_lote.

  it_itens_alv_aux[] = it_itens_alv[].

  FREE: it_itens_alv.
  CLEAR: it_itens_alv.

  APPEND INITIAL LINE                TO it_itens_alv.
  APPEND LINES OF it_itens_alv_aux[] TO it_itens_alv[].

  READ TABLE it_itens_alv         INDEX 1.

  wa_style-fieldname = 'EBELN'.
  wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT wa_style                  INTO TABLE it_itens_alv-style[].
  wa_style-fieldname = 'EBELP'.
  wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT wa_style                  INTO TABLE it_itens_alv-style[].
  wa_style-fieldname = 'MENGE'.
  wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
  INSERT wa_style                  INTO TABLE it_itens_alv-style[].
  MODIFY it_itens_alv                   INDEX 1.

ENDFORM.

FORM deletar_pedido .

  CLEAR wa_itens_sel_lote.

  CLEAR: zib_nfe_dist_lot.

  LOOP AT it_selected_1601 INTO wa_selected_1601.
    READ TABLE it_itens_alv INDEX wa_selected_1601-index.
    IF sy-subrc = 0.
      LOOP AT it_lotes_alv_t WHERE chave_nfe = it_itens_alv-chave_nfe
                               AND prod_item = it_itens_alv-prod_item.
        DATA(l_tabix1) = sy-tabix.
        READ TABLE it_lotes_alv_u WITH KEY chave_nfe = it_lotes_alv_t-chave_nfe
                                           prod_item = it_lotes_alv_t-prod_item.
        IF sy-subrc = 0.
          DELETE it_lotes_alv_u INDEX sy-tabix.
        ENDIF.
        LOOP AT it_carac_alv INTO DATA(wa_carac_alv) WHERE cd_lote_item = it_lotes_alv_t-cd_lote_item.
          DATA(l_tabix2) = sy-tabix.
          READ TABLE it_carac_alv_u INTO DATA(wa_carac_alv_u) WITH KEY cd_lote_item = wa_carac_alv-cd_lote_item.
          IF sy-subrc = 0.
            DELETE it_carac_alv_u INDEX sy-tabix.
          ENDIF.
          DELETE it_carac_alv INDEX l_tabix2.
        ENDLOOP.
        DELETE it_lotes_alv_t INDEX l_tabix1.
      ENDLOOP.
      DELETE it_itens_alv INDEX wa_selected_1601-index.
    ENDIF.
  ENDLOOP.

ENDFORM.
