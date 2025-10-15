FUNCTION zsd_pedidos_importacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NRO_SOL) TYPE  ZDE_NRO_SOL OPTIONAL
*"     REFERENCE(I_SEQ) TYPE  NUMC3 OPTIONAL
*"     REFERENCE(I_FILIAL_RESP) TYPE  VKBUR OPTIONAL
*"     REFERENCE(I_SELECAO) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(E_EBELN) TYPE  EBELN
*"     VALUE(E_EBELP) TYPE  EBELP
*"----------------------------------------------------------------------

  FREE: t_zsdt0265,
        t_ekpo,
        t_makt,
        t_pedido,
        t_regs,
        g_ebeln_sai,
        g_ebelp_sai,
        e_ebeln,
        e_ebelp.

  g_nro_sol     = i_nro_sol.
  g_seq         = i_seq.
  g_filial_resp = i_filial_resp.
  g_selecao     = i_selecao.

  IF g_selecao  = abap_true.
    g_edit      = abap_false.
  ELSE.
    g_edit      = abap_true.
  ENDIF.

*-Verifica existe ZSDT0137
  SELECT *
    FROM zsdt0137
    INTO @DATA(w_zsdt0137)
      UP TO 1 ROWS
   WHERE nro_sol     = @g_nro_sol
     AND seq         = @g_seq
     AND filial_resp = @g_filial_resp
     AND status     <> @abap_on.
  ENDSELECT.

  CHECK sy-subrc = 0.

*-Pedidos importacao
  SELECT *
    FROM zsdt0265
    INTO TABLE t_zsdt0265
   WHERE nro_sol     = g_nro_sol
     AND seq         = g_seq
     AND filial_resp = g_filial_resp.

  IF t_zsdt0265[] IS NOT INITIAL.
    SELECT ebeln ebelp matnr
      INTO TABLE t_ekpo
      FROM ekpo
       FOR ALL ENTRIES IN t_zsdt0265
     WHERE ebeln = t_zsdt0265-ebeln
       AND ebelp = t_zsdt0265-ebelp.
  ENDIF.

  IF t_ekpo[] IS NOT INITIAL.
    SELECT matnr maktx
      INTO TABLE t_makt
      FROM makt
       FOR ALL ENTRIES IN t_ekpo
     WHERE matnr = t_ekpo-matnr
       AND spras = sy-langu.
  ENDIF.

  SORT t_ekpo BY ebeln ebelp.
  SORT t_makt BY matnr.

  LOOP AT t_zsdt0265 INTO w_zsdt0265.
    CLEAR: w_ekpo, w_makt.

    READ TABLE t_ekpo INTO w_ekpo WITH KEY ebeln = w_zsdt0265-ebeln
                                           ebelp = w_zsdt0265-ebelp
                                  BINARY SEARCH.

    READ TABLE t_makt INTO w_makt WITH KEY matnr = w_ekpo-matnr
                                  BINARY SEARCH.

    MOVE-CORRESPONDING w_zsdt0265  TO w_pedido.
    MOVE w_makt-maktx              TO w_pedido-maktx.
    APPEND w_pedido                TO t_pedido.
  ENDLOOP.

  IF g_selecao = abap_true AND t_pedido[] IS INITIAL.
    EXIT.
  ENDIF.

*-cadastro
  CALL SCREEN 100 STARTING AT 100 5
                    ENDING AT 168 14.

  IF g_selecao = abap_true.
    e_ebeln = g_ebeln_sai.
    e_ebelp = g_ebelp_sai.
  ENDIF.

ENDFUNCTION.
