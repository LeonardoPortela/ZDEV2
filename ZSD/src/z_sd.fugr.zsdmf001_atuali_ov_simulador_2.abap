FUNCTION zsdmf001_atuali_ov_simulador_2 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      TI_ITENS_OV STRUCTURE  ZSDT0041 OPTIONAL
*"      TE_SAIDA_EXEC STRUCTURE  ZSDS010 OPTIONAL
*"  EXCEPTIONS
*"      OV_NAO_ENCONTRADA
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_saida_alv,
*           INCO1         TYPE ZSDT0041-WERKS,
*           SPART         TYPE ZSDT0041-SPART,
*           AUART         TYPE ZSDT0041-AUART,
*           DESC_ABSOLUTO TYPE  ZSDT0041-DESC_ABSOLUTO,
*           VLRTOT        TYPE ZSDT0041-VLRTOT,
*           WERKS         TYPE ZSDT0041-WERKS,
           vbeln    TYPE vbak-vbeln,
           posnr    TYPE vbap-posnr,
           msg(255),
         END OF ty_saida_alv.

  DATA:
    wl_vbap           TYPE vbap,
    wl_orderheaderinx TYPE bapisdh1x,
    wl_logic_switch   TYPE bapisdls,
    tl_return         TYPE TABLE OF bapiret2 WITH HEADER LINE,
    wl_return         TYPE bapiret2,
    tl_saida_alv      TYPE TABLE OF ty_saida_alv WITH HEADER LINE,
    tl_conditions_in  TYPE TABLE OF bapicond WITH HEADER LINE,
    tl_conditions_inx TYPE TABLE OF bapicondx WITH HEADER LINE,
    tl_itens          TYPE TABLE OF zsdt0041 WITH HEADER LINE.

  IF ti_itens_ov[] IS INITIAL.
    RAISE ov_nao_encontrada.
  ELSE.
    tl_itens[] = ti_itens_ov[].
  ENDIF.

  wl_orderheaderinx-updateflag = 'U'.
  wl_logic_switch-cond_handl   = 'X'.

  LOOP AT tl_itens .
    CLEAR: tl_conditions_in[], tl_conditions_inx[], tl_return[].

    IF tl_itens-desc_absoluto IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM vbap
      INTO wl_vbap
     WHERE vbeln = tl_itens-vbeln
       AND matnr = tl_itens-matnr.

    tl_itens-desc_absoluto      = tl_itens-desc_absoluto.

    tl_conditions_in-itm_number = wl_vbap-posnr.
    tl_conditions_in-cond_count = '01'.
    tl_conditions_in-cond_type  = 'RB00'.
    tl_conditions_in-cond_value = tl_itens-desc_absoluto.
    tl_conditions_in-currency   = wl_vbap-waerk.
    tl_conditions_inx-itm_number = wl_vbap-posnr.
    tl_conditions_inx-cond_count = '01'.
    tl_conditions_inx-cond_type  = 'RB00'.
    tl_conditions_inx-updateflag = 'U'.
    tl_conditions_inx-cond_value = 'X'.
    tl_conditions_inx-currency   = 'X'.

    APPEND: tl_conditions_in,
            tl_conditions_inx.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = tl_itens-vbeln
        order_header_inx = wl_orderheaderinx
        logic_switch     = wl_logic_switch
      TABLES
        conditions_in    = tl_conditions_in
        conditions_inx   = tl_conditions_inx
        return           = tl_return.

    IF ( line_exists( tl_return[ type = 'E' ] ) ).
      tl_saida_alv-msg = tl_return[ type = 'E' ]-message.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    MOVE: wl_vbap-vbeln         TO tl_saida_alv-vbeln,
          wl_vbap-posnr         TO tl_saida_alv-posnr.

    APPEND tl_saida_alv.
  ENDLOOP.

ENDFUNCTION.
