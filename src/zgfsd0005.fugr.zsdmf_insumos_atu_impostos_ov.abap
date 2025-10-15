FUNCTION zsdmf_insumos_atu_impostos_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_VBELN) TYPE  VBELN_VA
*"     REFERENCE(IV_POSNR) TYPE  POSNR_VA
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     REFERENCE(IV_QTDE_VENDA) TYPE  KWMENG
*"     REFERENCE(IV_UN_VENDA) TYPE  MEINS
*"     REFERENCE(IV_UN_PRECO) TYPE  MEINS
*"     REFERENCE(IV_ICBS_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(IV_ICVA_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(IV_ICMI_ATUAL) TYPE  BAPIKBETR1
*"     REFERENCE(IV_COMMIT) TYPE  FLAG DEFAULT 'X'
*"  TABLES
*"      ET_BAPICOND STRUCTURE  BAPICOND OPTIONAL
*"      ET_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------

  DATA ls_header TYPE bapisdh1.
  DATA ls_headerx TYPE bapisdh1x.
  DATA ls_logic TYPE bapisdls.

  DATA lt_item TYPE TABLE OF bapisditm.
  DATA lt_itemx TYPE TABLE OF bapisditmx.

  DATA lt_cond_in TYPE TABLE OF bapicond.
  DATA lt_cond TYPE TABLE OF bapicond.
  DATA lt_condx TYPE TABLE OF bapicondx.

  DATA lv_erro.
  DATA lv_simu.
  DATA lv_pr00_novo TYPE bapikbetr1.
  DATA lv_rb00_novo TYPE bapikbetr1.

  CHECK iv_vbeln IS NOT INITIAL.
  CHECK iv_posnr IS NOT INITIAL.

  IF iv_commit = abap_false.
    lv_simu = abap_true.
  ELSE.
    lv_simu = abap_false.
  ENDIF.

  CALL FUNCTION 'ZSDMF_INSUMOS_SIMULA_PRECO2'
    EXPORTING
      iv_matnr      = iv_matnr
      iv_qtde_venda = iv_qtde_venda
      iv_un_venda   = iv_un_venda
      iv_un_preco   = iv_un_preco
      iv_icbs_novo  = iv_icbs_novo
      iv_icva_novo  = iv_icva_novo
      iv_icmi_atual = iv_icmi_atual
    IMPORTING
      ev_pr00_novo  = lv_pr00_novo
      ev_rb00_novo  = lv_rb00_novo
    TABLES
      et_bapicond   = lt_cond_in.

  SELECT SINGLE knumv FROM vbak
    INTO @DATA(lv_knumv)
      WHERE vbeln = @iv_vbeln.

  SELECT knumv,kposn,stunr,zaehk,kappl, kschl,waers FROM v_konv
      INTO TABLE @DATA(lt_konv)
          WHERE knumv = @lv_knumv.

*  CALL FUNCTION 'ZSDMF_INSUMOS_SIMULA_PRECO'
*    EXPORTING
*      iv_vbeln_ref  = iv_vbeln
*      iv_posnr      = iv_posnr
*      iv_qtde       = iv_qtde
*      iv_pr00_atual = iv_pr00_atual
*      iv_rb00_atual = iv_rb00_atual
*      iv_icmi_atual = iv_icmi_atual
*    IMPORTING
*      ev_pr00_novo  = lv_pr00_novo
*      ev_rb00_novo  = lv_rb00_novo
*    TABLES
*      et_bapicond   = lt_cond_in.

*  CHECK iv_pr00_atual <> lv_pr00_novo.

  lt_item = VALUE #( ( itm_number = iv_posnr price_date = sy-datum  ) ).
  lt_itemx = VALUE #( ( itm_number = iv_posnr price_date = abap_true updateflag = 'U' ) ).

  IF lv_pr00_novo IS NOT INITIAL.

    APPEND INITIAL LINE TO lt_cond ASSIGNING FIELD-SYMBOL(<fs_cond>).
    APPEND INITIAL LINE TO lt_condx ASSIGNING FIELD-SYMBOL(<fs_condx>).

    <fs_cond>-itm_number  = iv_posnr.
    <fs_cond>-cond_type   = 'PR00'.
    <fs_cond>-cond_value  = lv_pr00_novo.

    <fs_cond>-cond_st_no = lt_konv[ kposn = iv_posnr kschl = 'PR00' ]-stunr.
    <fs_cond>-cond_count = lt_konv[ kposn = iv_posnr kschl = 'PR00' ]-zaehk.
    <fs_cond>-currency =   lt_konv[ kposn = iv_posnr kschl = 'PR00' ]-waers.

    PERFORM f_preenche_x USING <fs_cond> 'U' CHANGING <fs_condx>.

  ENDIF.

  IF lv_rb00_novo IS NOT INITIAL.

    APPEND INITIAL LINE TO lt_cond ASSIGNING <fs_cond>.
    APPEND INITIAL LINE TO lt_condx ASSIGNING <fs_condx>.

    <fs_cond>-itm_number  = iv_posnr.
    <fs_cond>-cond_type   = 'RB00'.
    <fs_cond>-cond_value  = lv_rb00_novo.

    <fs_cond>-cond_st_no = lt_konv[ kposn = iv_posnr kschl = 'RB00' ]-stunr.
    <fs_cond>-cond_count = lt_konv[ kposn = iv_posnr kschl = 'RB00' ]-zaehk.
    <fs_cond>-currency =   lt_konv[ kposn = iv_posnr kschl = 'RB00' ]-waers.

    PERFORM f_preenche_x USING <fs_cond> 'U' CHANGING <fs_condx>.

  ENDIF.

  " -------------------------------------------------- ATUALIZA_PRICE

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = 'G'.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = iv_vbeln
      order_header_in  = ls_header
      order_header_inx = ls_headerx
      logic_switch     = ls_logic
      simulation       = lv_simu
    TABLES
      order_item_in    = lt_item
      order_item_inx   = lt_itemx
      "conditions_in    = lt_cond
      "conditions_inx   = lt_condx
      return           = et_return.

  PERFORM f_bapi_confirm_process CHANGING et_return[] lv_erro.

  CHECK lv_erro IS INITIAL.

  " -------------------------------------------------- ATUALIZA CONDITIONS

  ls_headerx-updateflag = 'U'.
  ls_logic-cond_handl   = 'X'.
  ls_logic-pricing = space.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = iv_vbeln
      order_header_in  = ls_header
      order_header_inx = ls_headerx
      logic_switch     = ls_logic
      simulation       = lv_simu
    TABLES
      "order_item_in    = lt_item
      "order_item_inx   = lt_itemx
      conditions_in    = lt_cond
      conditions_inx   = lt_condx
      return           = et_return.

  PERFORM f_bapi_confirm_process CHANGING et_return[] lv_erro.

ENDFUNCTION.
