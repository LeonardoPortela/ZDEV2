FUNCTION z_gerar_excel_balancete.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IT_SALDO_CONTAS) TYPE  ZSCO_SALDO_CONTAS_TT
*"     REFERENCE(IV_AUTOTP_MOEDA) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_FILE) TYPE  STRING
*"----------------------------------------------------------------------
  DATA: l_currency_brl TYPE tcurc-waers VALUE 'BRL'.

  DATA: l_amount_internal TYPE wertv12,
        wa_x001           LIKE x001,
        c_value           TYPE wertv12,
        l_amount_external TYPE c LENGTH 35.


  FREE: gt_tp_moeda.

  LOOP AT it_saldo_contas INTO DATA(ls_saldo_contas).

*    IF ls_saldo_contas-moeda_interna < 0.
*      MULTIPLY ls_saldo_contas-moeda_interna BY -1.
*    ENDIF.
*
*    IF ls_saldo_contas-moeda_forte < 0.
*      MULTIPLY ls_saldo_contas-moeda_forte BY -1.
*    ENDIF.
*
*    IF ls_saldo_contas-moeda_indice < 0.
*      MULTIPLY ls_saldo_contas-moeda_indice BY -1.
*    ENDIF.

    READ TABLE gt_saldo_contas ASSIGNING FIELD-SYMBOL(<saldo_contas>) WITH KEY rbukrs = ls_saldo_contas-rbukrs
                                                                               ryear  = ls_saldo_contas-ryear
                                                                               month  = ls_saldo_contas-month
                                                                               racct  = ls_saldo_contas-racct.
    IF sy-subrc IS INITIAL.
      <saldo_contas>-moeda_interna = <saldo_contas>-moeda_interna + ls_saldo_contas-moeda_interna.
      <saldo_contas>-moeda_forte   = <saldo_contas>-moeda_forte   + ls_saldo_contas-moeda_forte.
      <saldo_contas>-moeda_indice  = <saldo_contas>-moeda_indice  + ls_saldo_contas-moeda_indice.
    ELSE.
      APPEND ls_saldo_contas TO gt_saldo_contas.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_saldo_contas ASSIGNING FIELD-SYMBOL(<fs_saldo_conta>).
    IF <fs_saldo_conta>-moeda_interna < 0.
      <fs_saldo_conta>-dc_interna = 'C'.
    ELSE.
      <fs_saldo_conta>-dc_interna = 'D'.
    ENDIF.

    IF <fs_saldo_conta>-moeda_forte < 0.
      <fs_saldo_conta>-dc_forte = 'C'.
    ELSE.
      <fs_saldo_conta>-dc_forte = 'D'.
    ENDIF.

    IF <fs_saldo_conta>-moeda_indice < 0.
      <fs_saldo_conta>-dc_indice = 'C'.
    ELSE.
      <fs_saldo_conta>-dc_indice = 'D'.
    ENDIF.

    <fs_saldo_conta>-moeda_interna = abs( <fs_saldo_conta>-moeda_interna ).
    <fs_saldo_conta>-moeda_forte   = abs( <fs_saldo_conta>-moeda_forte   ).
    <fs_saldo_conta>-moeda_indice  = abs( <fs_saldo_conta>-moeda_indice  ).

  ENDLOOP.


  gt_saldo_contas_interna = CORRESPONDING #( gt_saldo_contas ).
  gt_saldo_contas_forte   = CORRESPONDING #( gt_saldo_contas ).
  gt_saldo_contas_indice  = CORRESPONDING #( gt_saldo_contas ).

  LOOP AT gt_saldo_contas_interna ASSIGNING FIELD-SYMBOL(<interna>).

*    REPLACE ALL OCCURRENCES OF '.' IN <interna>-moeda_interna WITH ','.
*    CONDENSE <interna>-moeda_interna NO-GAPS.

    gv_moeda_interna = <interna>-moeda_interna.

    REPLACE ALL OCCURRENCES OF '.' IN gv_moeda_interna WITH ','.
    CONDENSE gv_moeda_interna NO-GAPS.


    DATA(l_last_position) = strlen( <interna>-racct ) - 1.

    IF <interna>-racct+l_last_position(1) = '-'.
      <interna>-racct+l_last_position(1) = space.
    ENDIF.

    SHIFT <interna>-racct(10) LEFT DELETING LEADING '0'.
    CONDENSE <interna>-racct NO-GAPS.

    IF <interna>-dc_interna EQ 'C'.
      <interna>-moeda_interna = '-' && <interna>-moeda_interna.
    ENDIF.

    "pegar o tipo de moeda da moeda forte.
    IF <interna>-rbukrs IS NOT INITIAL.
      SELECT SINGLE waers  FROM t001 INTO <interna>-waers WHERE bukrs EQ <interna>-rbukrs AND spras EQ sy-langu.
    ENDIF.
    IF <interna>-waers EQ 'BRL' OR <interna>-waers EQ 'USD'.
      "Convert to amount
      CLEAR: c_value.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
        EXPORTING
          string             = gv_moeda_interna
        IMPORTING
          flstr              = c_value
        EXCEPTIONS
          exponent_too_big   = 1
          exponent_too_small = 2
          string_not_fltp    = 3
          too_many_decim     = 4
          OTHERS             = 5.

      MOVE-CORRESPONDING <interna> TO ws_tp_moeda.

      IF <interna>-dc_interna EQ 'C'.
        c_value = '-' && c_value.
      ENDIF.

      CASE <interna>-waers .
        WHEN 'BRL'.
          ws_tp_moeda-vlr_brl = c_value.
          ws_tp_moeda-dc_brl = <interna>-dc_interna.
        WHEN 'USD'.
          ws_tp_moeda-vlr_usd = c_value.
          ws_tp_moeda-dc_usd = <interna>-dc_interna.
      ENDCASE.
      IF ws_tp_moeda IS NOT INITIAL.
        APPEND ws_tp_moeda TO gt_tp_moeda.
      ENDIF.
    ENDIF.
    CLEAR ws_tp_moeda .
  ENDLOOP.

  LOOP AT gt_saldo_contas_forte ASSIGNING FIELD-SYMBOL(<forte>).

*    REPLACE ALL OCCURRENCES OF '.' IN <forte>-moeda_forte WITH ','.
*    CONDENSE <forte>-moeda_forte NO-GAPS.

    gv_moeda_forte = <forte>-moeda_forte.

    REPLACE ALL OCCURRENCES OF '.' IN gv_moeda_forte WITH ','.
    CONDENSE gv_moeda_forte NO-GAPS.


    l_last_position = strlen( <forte>-racct ) - 1.

    IF <forte>-racct+l_last_position(1) = '-'.
      <forte>-racct+l_last_position(1) = space.
    ENDIF.

    SHIFT <forte>-racct(10) LEFT DELETING LEADING '0'.
    CONDENSE <forte>-racct NO-GAPS.

    IF <forte>-dc_forte EQ 'C'.
      <forte>-moeda_forte = '-' && <forte>-moeda_forte.
    ENDIF.

    "pegar o tipo de moeda da moeda interna.
    IF <forte>-rbukrs IS NOT INITIAL.
      CLEAR: wa_x001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs                = <forte>-rbukrs
        IMPORTING
          e_x001                 = wa_x001
        EXCEPTIONS
          currency_2_not_defined = 1
          currency_3_not_defined = 2
          OTHERS                 = 3.

      IF sy-subrc = 0.
        <forte>-waers = wa_x001-hwae2.
      ENDIF.
    ENDIF.

    IF <forte>-waers EQ 'BRL' OR <forte>-waers EQ 'USD'.

      "Convert to amount
      CLEAR: c_value.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
        EXPORTING
          string             = gv_moeda_forte
        IMPORTING
          flstr              = c_value
        EXCEPTIONS
          exponent_too_big   = 1
          exponent_too_small = 2
          string_not_fltp    = 3
          too_many_decim     = 4
          OTHERS             = 5.

      IF <forte>-dc_forte EQ 'C'.
        c_value = '-' && c_value.
      ENDIF.

      "Verifica se ja tem a conta na estrutura.
      READ TABLE gt_tp_moeda ASSIGNING FIELD-SYMBOL(<ws_tp_moeda>) WITH KEY racct  = <forte>-racct
                                                                            rbukrs = <forte>-rbukrs.
      IF sy-subrc EQ 0. "Se ja existir, acrescentar o valor, senão incluir uma nova linha.
        CASE <forte>-waers .
          WHEN 'BRL'.
            <ws_tp_moeda>-vlr_brl = <ws_tp_moeda>-vlr_brl + c_value.

            IF <ws_tp_moeda>-dc_brl IS INITIAL.
              <ws_tp_moeda>-dc_brl = <forte>-dc_forte.
            ENDIF.

          WHEN 'USD'.
            <ws_tp_moeda>-vlr_usd = <ws_tp_moeda>-vlr_usd + c_value.

            IF <ws_tp_moeda>-dc_usd IS INITIAL.
              <ws_tp_moeda>-dc_usd = <forte>-dc_forte.
            ENDIF.
        ENDCASE.

      ELSE.
        MOVE-CORRESPONDING <forte> TO ws_tp_moeda.
        CASE <forte>-waers .
          WHEN 'BRL'.
            ws_tp_moeda-vlr_brl = c_value.
            ws_tp_moeda-dc_brl = <forte>-dc_forte.
          WHEN 'USD'.
            ws_tp_moeda-vlr_usd = c_value.
            ws_tp_moeda-dc_usd = <forte>-dc_forte.
        ENDCASE.
        IF ws_tp_moeda IS NOT INITIAL.
          APPEND ws_tp_moeda TO gt_tp_moeda.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR ws_tp_moeda .
  ENDLOOP.

  LOOP AT gt_saldo_contas_indice ASSIGNING FIELD-SYMBOL(<indice>).

*    REPLACE ALL OCCURRENCES OF '.' IN <indice>-moeda_indice WITH ','.
*    CONDENSE <indice>-moeda_indice NO-GAPS.

    gv_moeda_indice = <indice>-moeda_indice.

    REPLACE ALL OCCURRENCES OF '.' IN gv_moeda_indice WITH ','.
    CONDENSE gv_moeda_indice NO-GAPS.

    l_last_position = strlen( <indice>-racct ) - 1.

    IF <indice>-racct+l_last_position(1) = '-'.
      <indice>-racct+l_last_position(1) = space.
    ENDIF.

    SHIFT <indice>-racct(10) LEFT DELETING LEADING '0'.
    CONDENSE <indice>-racct NO-GAPS.

    IF <indice>-dc_indice EQ 'C'.
      <indice>-moeda_indice = '-' && <indice>-moeda_indice.
    ENDIF.

    "pegar o tipo de moeda indice.
    IF <indice>-rbukrs IS NOT INITIAL.
      CLEAR: wa_x001.
      CALL FUNCTION 'FI_CURRENCY_INFORMATION'
        EXPORTING
          i_bukrs                = <indice>-rbukrs
        IMPORTING
          e_x001                 = wa_x001
        EXCEPTIONS
          currency_2_not_defined = 1
          currency_3_not_defined = 2
          OTHERS                 = 3.

      IF sy-subrc = 0.
        <indice>-waers = wa_x001-hwae3.
      ENDIF.
    ENDIF.

    IF <indice>-waers EQ 'BRL' OR <indice>-waers EQ 'USD'.

      "Convert to amount
      CLEAR: c_value.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
        EXPORTING
          string             = gv_moeda_indice
        IMPORTING
          flstr              = c_value
        EXCEPTIONS
          exponent_too_big   = 1
          exponent_too_small = 2
          string_not_fltp    = 3
          too_many_decim     = 4
          OTHERS             = 5.


      IF <indice>-dc_indice EQ 'C'.
        c_value = '-' && c_value.
      ENDIF.

      "Verifica se ja tem a conta na estrutura.
      READ TABLE gt_tp_moeda ASSIGNING <ws_tp_moeda> WITH KEY racct  = <indice>-racct
                                                              rbukrs = <indice>-rbukrs.
      IF sy-subrc EQ 0. "Se ja existir, acrescentar o valor, senão incluir uma nova linha.
        CASE <indice>-waers .
          WHEN 'BRL'.
            <ws_tp_moeda>-vlr_brl = <ws_tp_moeda>-vlr_brl + c_value.

            IF <ws_tp_moeda>-dc_brl IS INITIAL.
              <ws_tp_moeda>-dc_brl = <indice>-dc_indice.
            ENDIF.
          WHEN 'USD'.
            <ws_tp_moeda>-vlr_usd = <ws_tp_moeda>-vlr_usd + c_value.

            IF <ws_tp_moeda>-dc_usd IS INITIAL.
              <ws_tp_moeda>-dc_usd = <indice>-dc_indice.
            ENDIF.
        ENDCASE.

      ELSE.

        MOVE-CORRESPONDING <indice> TO ws_tp_moeda.
        CASE <indice>-waers.
          WHEN 'BRL'.
            ws_tp_moeda-vlr_brl = c_value.
            ws_tp_moeda-dc_brl = <indice>-dc_indice.
          WHEN 'USD'.
            ws_tp_moeda-vlr_usd = c_value.
            ws_tp_moeda-dc_usd = <indice>-dc_indice.
        ENDCASE.
        IF ws_tp_moeda IS NOT INITIAL.
          APPEND ws_tp_moeda TO gt_tp_moeda.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR ws_tp_moeda.
  ENDLOOP.

  DELETE gt_saldo_contas_interna WHERE moeda_interna IS INITIAL. "= '0,00'.
  DELETE gt_saldo_contas_forte   WHERE moeda_forte  IS INITIAL. "= '0,00'.
  DELETE gt_saldo_contas_indice  WHERE moeda_indice IS INITIAL. " = '0,00'.
  DELETE gt_tp_moeda             WHERE vlr_brl IS INITIAL AND vlr_usd IS INITIAL.

  IF iv_autotp_moeda IS INITIAL. " RJF - CS2022001091 - Accountfy - API envio Balancetes
    CALL SCREEN 1000 STARTING AT 5 5.
* RJF - Ini - CS2022001091 - Accountfy - API envio Balancetes
  ELSE. "TP Moeda Balancete automático
    MOVE 'C:\TEMP\XLX_FILE.XLX' TO diretorio.
    gv_automoeda = abap_true.
    PERFORM f_gerar_excel TABLES gt_tp_moeda
                          USING 'ZSCO_TP_MOEDA'.
    gv_automoeda = abap_false.
    ev_file = gs_file_name.
  ENDIF.
* RJF - Fim - CS2022001091 - Accountfy - API envio Balancetes
ENDFUNCTION.
