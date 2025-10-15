CLASS zclfi_mep DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_e TYPE char1 VALUE 'E'.
    CONSTANTS gc_cta_param TYPE z_mep_parametro VALUE '03 - CTA - Ajuste acumulado de conversão'.
    CONSTANTS gc_result_param TYPE z_mep_parametro VALUE '15 - Resultado Período'.
    CONSTANTS gc_reserva_lucro TYPE z_mep_parametro VALUE '09-Reserva de lucros'.

    METHODS constructor
      IMPORTING
        is_mep     TYPE zsfi_mep_process
        iv_not_alv TYPE boolean OPTIONAL.

    METHODS processa
      EXPORTING
        et_alv TYPE zfitt_result_mep.

  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ty_contas,
             saknr TYPE saknr,
           END OF ty_contas .

  data GO_ALV type ref to CL_SALV_TABLE .
  data GS_HEADER type ZSFI_MEP_HEADER .
  data GS_MEP_PROC type ZSFI_MEP_PROCESS .
  data GS_ZGLT0111 type ZGLT0111 .
  data GS_ZGLT0107 type ZGLT0107 .
  data GT_MSG type BAPIRET2_TAB .
  data:
    gt_contas TYPE TABLE OF zglt0112 .
  data:
    gt_contas_tipo TYPE TABLE OF zglt0112 .
  data:
    gt_mep_alv TYPE TABLE OF zsfi_mep .
  data:
    gt_mep_tab_param TYPE TABLE OF zsfi_mep .
  data:
    gt_tipo_refl TYPE TABLE OF zi_vh_tipo_reflexa .
  data GT_CTA type ZFITT_RESULT_CTA .
  data GT_CTA_ANT type ZFITT_RESULT_CTA .
  data:
    gt_contas_get TYPE TABLE OF ty_contas .
  data:
    gt_saldo TYPE TABLE OF zde_fi_gl_saldo_faglflext .
  data:
    gt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext .
  data:
    gt_zglt0107 TYPE TABLE OF zglt0107 .
  data:
    gt_zglt0107_get TYPE TABLE OF zglt0107 .
  data GV_NOTALV type STRING .
  data GV_DUMMY type STRING .
  data GV_PARTICIPACAO type Z_PART_PERC .
  data GV_VLR_RES_BRL type HSLVT12 .
  data GV_VLR_RES_USD type HSLVT12 .

  methods ALV .
  methods ATUALIZA_DOCUMENTOS .
  methods ATUALIZA_STATUS .
  methods BOTAO_ALV .
  methods BUSCA_CONTAS_CONFIG .
  methods COLUNAS_ALV .
  methods CONC_INVEST .
  methods CONGELA_LINHA
    importing
      !IS_LINHA type ZSFI_MEP .
  methods ESTORNAR_DOCUMENTO .
  methods ESTORNA_DOC_LINE
    importing
      !IV_BELNR type BELNR_D
    exporting
      !EV_STBLG type STBLG .
  methods ESTORNA_SALDO .
  methods EXEC_REINICIALIZACAO .
  methods GERAR_DOCUMENTO .
  methods GERA_DOC_BRL
    importing
      !IS_TAB_PARAM type ZSFI_MEP .
  methods GERA_DOC_USD
    importing
      !IS_TAB_PARAM type ZSFI_MEP .
  methods GERA_DOIS_DOCUMENTOS
    importing
      !IS_TAB_PARAM type ZSFI_MEP .
  methods GERA_UNICO_DOCUMENTO
    importing
      !IS_TAB_PARAM type ZSFI_MEP .
  methods GET_CONTAS_RESULT_PER .
  methods GET_DADOS_CTA .
  methods GET_DADOS_CTA_ANT .
  methods GET_INFO_HEADER .
  methods GET_RESULTADO_PERIODO .
  methods GET_SALDO .
  methods GET_SALDO_ANO_ANT
    returning
      value(RS_RESULT) type ZGLT0111 .
  methods GET_SALDO_ANTERIOR
    importing
      !IS_CONTA type ZGLT0112
    exporting
      !EV_BRL type HSLVT12
      !EV_USD type HSLVT12 .
  methods GET_SALDO_ANTERIOR_CTA
    exporting
      !EV_BRL type HSLVT12 .
  methods GET_SALDO_ANTERIOR_RESER_LUCRO
    importing
      !IS_CONTA type ZGLT0112
    exporting
      !EV_BRL type HSLVT12
      !EV_USD type HSLVT12 .
  methods GET_SALDO_MES
    importing
      !IS_CONTA type ZGLT0112
    exporting
      !EV_BRL type HSLVT12
      !EV_USD type HSLVT12 .
  methods GET_SALDO_MES_RESERV_LUCRO
    importing
      !IS_CONTA type ZGLT0112
    exporting
      !EV_BRL type HSLVT12
      !EV_USD type HSLVT12 .
  methods GET_SALDO_ZGLT0107
    importing
      !IV_PARAMETRO type Z_MEP_PARAMETRO .
  methods GET_SALDO_ZGLT0111
    importing
      !IV_TIPO type Z_TP_SALDO_EQ .
  methods GET_TAB_PARAMENTRO .
  methods IR_CTA .
  methods LIMPA_DADOS_TABELAS .
  methods MES_ANT_RESERVA_LUCRO
    changing
      !CS_MEP_ALV type ZSFI_MEP .
        methods mes_anterior_prejuizo
    changing
      !CS_MEP_ALV type ZSFI_MEP .
  methods MSG .
  methods ON_ADDED_FUNCTION
    for event IF_SALV_EVENTS_FUNCTIONS~ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods ON_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE "Hotspot Handler
    importing
      !ROW
      !COLUMN .
  methods ORDENAR .
  methods PREENCHE_CTA .
  methods PROCESSA_LINHAS .
  methods REINICIALIZAR .
  methods SALDO_ATUAL_RESERVA_LUCRO
    changing
      !CS_MEP type ZSFI_MEP .
        methods saldo_mes_atual_prejuizo
    changing
      !CS_MEP type ZSFI_MEP .
  methods SET_HEADER .
  methods SET_SOMATORIO .
  methods STATUS_ALV .
  methods VALOR_POSITIVO
    importing
      !IV_VALOR type HSLVT12
    returning
      value(R_VALUE) type BOOLEAN .
  methods VERIFICA_SALDO_ANO .
  methods VISUALIZAR_LOG
    importing
      !IS_LINE type ZSFI_MEP .
ENDCLASS.



CLASS ZCLFI_MEP IMPLEMENTATION.


  METHOD constructor.

    CLEAR: gs_mep_proc.
    gs_mep_proc = is_mep.

    SELECT *
    FROM zi_vh_tipo_reflexa
    INTO TABLE @gt_tipo_refl.
    IF sy-subrc = 0.
      SORT gt_tipo_refl BY tipo.
    ENDIF.

    gv_notalv = iv_not_alv.


  ENDMETHOD.


  METHOD processa.

    get_info_header( ).

    processa_linhas( ).

    IF gv_notalv = abap_true.
      et_alv = gt_mep_alv.
    ELSE.
      alv( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_info_header.

    DATA: lv_data_base TYPE dats.


    CLEAR: gs_header.

    lv_data_base = gs_mep_proc-ano && gs_mep_proc-mes && '01'.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_data_base
      IMPORTING
        last_day_of_month = gs_header-data_base
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    SELECT SINGLE *
    INTO @DATA(ls_zglt0104)
    FROM zglt0104
    WHERE investidora = @gs_mep_proc-empresa_investidora
      AND investida = @gs_mep_proc-empresa_investida
      AND inicio_validade <= @lv_data_base
      AND fim_validade >= @gs_header-data_base.
    IF sy-subrc = 0.

      gs_header-participacao = ls_zglt0104-part_perc.
      gs_header-pais = ls_zglt0104-pais.
      gs_header-moeda_funcional = ls_zglt0104-moeda_funcional.
      gs_header-tipo = ls_zglt0104-tipo.
      gs_header-empresa_investida = gs_mep_proc-empresa_investida.
      gs_header-empresa_investidora = gs_mep_proc-empresa_investidora.

      CLEAR: gv_participacao.
      gv_participacao = gs_header-participacao / 100.



      CLEAR: gt_zglt0107_get.
      SELECT *
      FROM zglt0107
      INTO TABLE gt_zglt0107_get
      WHERE monat      = gs_header-data_base+4(2)
            AND gjahr       = gs_header-data_base(4)
            AND investidora = gs_header-empresa_investidora
            AND investida   = gs_header-empresa_investida.

      SORT: gt_zglt0107_get BY parametro item_balanco.


      get_dados_cta( ).
      get_dados_cta_ant( ).

    ELSE.

      "Erro ao buscar cadastro de empresas: ZGLT0104
      MESSAGE e003 INTO gv_dummy.
      msg( ).

    ENDIF.


  ENDMETHOD.


  METHOD msg.

    APPEND VALUE #(
    id = sy-msgid
    type = sy-msgty
    number = sy-msgno
    message_v1 = sy-msgv1
    message_v2 = sy-msgv2
    message_v3 = sy-msgv3
    message_v4 = sy-msgv4 ) TO gt_msg.

  ENDMETHOD.


  METHOD processa_linhas.

    DATA: lv_saldo_mes_brl TYPE hslvt12,
          lv_saldo_mes_usd TYPE hslvt12.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    busca_contas_config( ).

    LOOP AT gt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      APPEND INITIAL LINE TO gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>).

      <fs_mep>-conta = <fs_conta>-hkont.
      <fs_mep>-desc_cta_equ = <fs_conta>-hkont_nome.
      <fs_mep>-parametro = VALUE #( gt_tipo_refl[ tipo = <fs_conta>-tipo_reflexa_pl ]-descricao OPTIONAL  ).


      READ TABLE gt_zglt0107_get ASSIGNING FIELD-SYMBOL(<fs_zglt0107>)
                                WITH KEY parametro = <fs_mep>-parametro
                                         item_balanco = <fs_mep>-conta
                                         BINARY SEARCH.
      IF sy-subrc = 0.

        <fs_mep>-parametro = <fs_zglt0107>-parametro.
        <fs_mep>-conta = <fs_zglt0107>-item_balanco.
        <fs_mep>-desc_cta_equ = <fs_zglt0107>-desc_cta_equ.
        <fs_mep>-saldo_mes_ant_brl = <fs_zglt0107>-sld_brl_ant.
        <fs_mep>-saldo_mes_ant_usd = <fs_zglt0107>-sld_usd_ant.
        <fs_mep>-saldo_mes_atual_brl = <fs_zglt0107>-sld_brl_atu.
        <fs_mep>-saldo_mes_atual_usd = <fs_zglt0107>-sld_usd_atu.
        <fs_mep>-movimentacao_brl = <fs_zglt0107>-movimentacao_brl.
        <fs_mep>-movimentacao_usd = <fs_zglt0107>-movimentacao_usd.
        <fs_mep>-equivalencia_brl = <fs_zglt0107>-equiv_brl.
        <fs_mep>-equivalencia_usd = <fs_zglt0107>-equiv_usd.

        <fs_mep>-lote = <fs_zglt0107>-lote.
        <fs_mep>-objkey1 = <fs_zglt0107>-objkey.
        <fs_mep>-documento_1 = <fs_zglt0107>-belnr.
        <fs_mep>-estorno_1 = <fs_zglt0107>-doc_lcto_est.
        <fs_mep>-objkey2 = <fs_zglt0107>-objkey_2.
        <fs_mep>-documento_2 = <fs_zglt0107>-belnr_2.
        <fs_mep>-estorno_2 = <fs_zglt0107>-doc_lcto_est_2.
        <fs_mep>-lote2 = <fs_zglt0107>-lote_2.

        CONTINUE.
      ENDIF.

      IF <fs_mep>-parametro = '09-Reserva de lucros' OR
         <fs_mep>-parametro = '11-Prejuizos acumulados'.

        "SALDO MES ANTERIOR
        get_saldo_anterior_reser_lucro(
          EXPORTING
            is_conta = <fs_conta>
          IMPORTING
            ev_brl   = <fs_mep>-saldo_mes_ant_brl
            ev_usd   = <fs_mep>-saldo_mes_ant_usd
        ).

      ELSE.

        "SALDO MES ANTERIOR
        get_saldo_anterior(
          EXPORTING
            is_conta = <fs_conta>
          IMPORTING
            ev_brl   = <fs_mep>-saldo_mes_ant_brl
            ev_usd   = <fs_mep>-saldo_mes_ant_usd
        ).

      ENDIF.

      IF <fs_mep>-conta = '0000243001'.
        IF <fs_mep>-parametro = gc_reserva_lucro
          AND gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.
          mes_ant_reserva_lucro(
            CHANGING
              cs_mep_alv = <fs_mep> ).
        ELSEIF <fs_mep>-parametro = gc_reserva_lucro
        AND gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.
          mes_ant_reserva_lucro(
            CHANGING
              cs_mep_alv = <fs_mep> ).
        ENDIF.
      ENDIF.


      IF <fs_mep>-parametro = '11-Prejuizos acumulados' "151188 CS2023000082 Melhorias transação ZGL083 PSA
        AND gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.
        mes_anterior_prejuizo(
          CHANGING
            cs_mep_alv = <fs_mep> ).
      ELSEIF <fs_mep>-parametro = '11-Prejuizos acumulados' "151188 CS2023000082 Melhorias transação ZGL083 PSA
      AND gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.
        mes_anterior_prejuizo(
          CHANGING
            cs_mep_alv = <fs_mep> ).
      ENDIF.


      IF <fs_mep>-parametro = '09-Reserva de lucros'.

        "SALDO MES ATUAL
        get_saldo_mes_reserv_lucro(
          EXPORTING
            is_conta = <fs_conta>
          IMPORTING
            ev_brl   = <fs_mep>-saldo_mes_atual_brl
            ev_usd   = <fs_mep>-saldo_mes_atual_usd
        ).

      ELSE.

        "SALDO MES ATUAL
        get_saldo_mes(
          EXPORTING
            is_conta = <fs_conta>
          IMPORTING
            ev_brl   = <fs_mep>-saldo_mes_atual_brl
            ev_usd   = <fs_mep>-saldo_mes_atual_usd
        ).

      ENDIF.


      IF <fs_mep>-parametro = gc_reserva_lucro.
        saldo_atual_reserva_lucro( CHANGING cs_mep =  <fs_mep> ).
      ENDIF.

      IF <fs_mep>-parametro = '11-Prejuizos acumulados'. "151188 CS2023000082 Melhorias transação ZGL083 PSA
        saldo_mes_atual_prejuizo( CHANGING cs_mep =  <fs_mep> ).
      ENDIF.


      IF <fs_mep>-saldo_mes_atual_brl IS NOT INITIAL
       OR <fs_mep>-saldo_mes_atual_usd IS NOT INITIAL.

        <fs_mep>-movimentacao_brl = <fs_mep>-saldo_mes_atual_brl - <fs_mep>-saldo_mes_ant_brl.
        <fs_mep>-movimentacao_usd = <fs_mep>-saldo_mes_atual_usd - <fs_mep>-saldo_mes_ant_usd.


        "EQUIVALENCIA
        <fs_mep>-equivalencia_brl = <fs_mep>-movimentacao_brl * gv_participacao.
        <fs_mep>-equivalencia_usd = <fs_mep>-movimentacao_usd * gv_participacao.

      ENDIF.

    ENDLOOP.

    DELETE gt_mep_alv WHERE
*                            saldo_mes_ant_brl IS INITIAL
*                        AND saldo_mes_ant_usd IS INITIAL
                            saldo_mes_atual_brl IS INITIAL
                        AND saldo_mes_atual_usd IS INITIAL
                        AND movimentacao_brl IS INITIAL
                        AND movimentacao_usd IS INITIAL.


    preenche_cta( ).

    atualiza_status( ).

  ENDMETHOD.


  METHOD busca_contas_config.

    DATA: lr_contasnot TYPE RANGE OF z_tp_lan_eq.


    lr_contasnot = VALUE #( ( sign = 'I'
                              option = 'EQ'
                              low = '03')
                            ( sign = 'I'
                              option = 'EQ'
                              low = '15') ).

    CLEAR: gt_contas, gt_contas_tipo.

    SELECT *
    FROM zglt0112
    INTO TABLE @gt_contas
    WHERE tipo_reflexa_pl NOT IN @lr_contasnot.
    IF sy-subrc = 0.

      gt_contas_tipo = gt_contas.

      SORT: gt_contas_tipo BY tipo_reflexa_pl.
      DELETE ADJACENT DUPLICATES FROM gt_contas_tipo COMPARING tipo_reflexa_pl.

    ELSE.

      "Erro ao buscar contas configuradas: ZGLT0112
      MESSAGE e004 INTO gv_dummy.
      msg( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_anterior.

    DATA: lv_mes        TYPE monat,
          lv_ano        TYPE gjahr,
          lv_index      TYPE sy-tabix,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2.

    DATA: lt_saldo  TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo3 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_contas TYPE zct_emp_contas.

    lv_mes = gs_header-data_base+4(2).

    IF lv_mes = '01'.
      lv_mes = '16'.
      lv_ano = gs_mep_proc-ano - 1.
    ELSE.
      lv_mes = gs_mep_proc-mes - 1.
      lv_ano = gs_mep_proc-ano.
    ENDIF.


    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
    <fs_conta>-saknr = is_conta-hkont.
    <fs_conta>-bukrs = gs_mep_proc-empresa_investida.


    CLEAR: lt_saldo, lt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lv_ano
        contas        = lt_contas
        p_gerar_todas = abap_true
        rldnr         = '0L'
      TABLES
        it_saldos     = lt_saldo
        it_saldos_2   = lt_saldo2
        it_saldos_3   = lt_saldo3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    DELETE lt_saldo WHERE slvt IS INITIAL AND
                           sl01 IS INITIAL AND
                           sl02 IS INITIAL AND
                           sl03 IS INITIAL AND
                           sl04 IS INITIAL AND
                           sl05 IS INITIAL AND
                           sl06 IS INITIAL AND
                           sl07 IS INITIAL AND
                           sl08 IS INITIAL AND
                           sl09 IS INITIAL AND
                           sl10 IS INITIAL AND
                           sl11 IS INITIAL AND
                           sl12 IS INITIAL AND
                           sl13 IS INITIAL AND
                           sl14 IS INITIAL AND
                           sl15 IS INITIAL AND
                           sl16 IS INITIAL.

    DELETE lt_saldo2 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.

    DELETE lt_saldo3 WHERE slvt IS INITIAL AND
                   sl01 IS INITIAL AND
                   sl02 IS INITIAL AND
                   sl03 IS INITIAL AND
                   sl04 IS INITIAL AND
                   sl05 IS INITIAL AND
                   sl06 IS INITIAL AND
                   sl07 IS INITIAL AND
                   sl08 IS INITIAL AND
                   sl09 IS INITIAL AND
                   sl10 IS INITIAL AND
                   sl11 IS INITIAL AND
                   sl12 IS INITIAL AND
                   sl13 IS INITIAL AND
                   sl14 IS INITIAL AND
                   sl15 IS INITIAL AND
                   sl16 IS INITIAL.


    IF gs_header-pais <> 'BR'
    AND gs_header-moeda_funcional <> 'BRL'
      AND gs_header-empresa_investida <> '0004'.

      CLEAR: ev_brl , ev_usd.

      LOOP AT lt_saldo3 ASSIGNING FIELD-SYMBOL(<fs_saldo3>).

        ev_brl = ev_brl + ( <fs_saldo3>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo3> TO FIELD-SYMBOL(<fs_any3>).
          IF <fs_any3> IS ASSIGNED.
            lv_valor_sl = <fs_any3>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any3>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ELSE.

      CLEAR: ev_brl , ev_usd.

      LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

        ev_brl = ev_brl + ( <fs_saldo>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO FIELD-SYMBOL(<fs_any>).
          IF <fs_any> IS ASSIGNED.
            lv_valor_sl = <fs_any>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ENDIF.

    LOOP AT lt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      ev_usd = ev_usd + ( <fs_saldo2>-slvt * -1 ).

      lv_index = 1.
      DO .

        IF lv_index > lv_mes.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        ev_usd = ev_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_saldo_anterior_reser_lucro.

    DATA: lv_mes        TYPE monat,
          lv_ano        TYPE gjahr,
          lv_index      TYPE sy-tabix,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2.

    DATA: lt_saldo  TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo3 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_contas TYPE zct_emp_contas.

    lv_mes = gs_header-data_base+4(2).

    IF lv_mes = '01'.
      lv_mes = '16'.
      lv_ano = gs_mep_proc-ano - 1.
    ELSE.
      lv_mes = gs_mep_proc-mes - 1.
      lv_ano = gs_mep_proc-ano.
    ENDIF.


    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
    <fs_conta>-saknr = is_conta-hkont.
    <fs_conta>-bukrs = gs_mep_proc-empresa_investida.


    CLEAR: lt_saldo, lt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lv_ano
        contas        = lt_contas
        p_gerar_todas = abap_true
        rldnr         = '0L'
      TABLES
        it_saldos     = lt_saldo
        it_saldos_2   = lt_saldo2
        it_saldos_3   = lt_saldo3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    DELETE lt_saldo WHERE slvt IS INITIAL AND
                           sl01 IS INITIAL AND
                           sl02 IS INITIAL AND
                           sl03 IS INITIAL AND
                           sl04 IS INITIAL AND
                           sl05 IS INITIAL AND
                           sl06 IS INITIAL AND
                           sl07 IS INITIAL AND
                           sl08 IS INITIAL AND
                           sl09 IS INITIAL AND
                           sl10 IS INITIAL AND
                           sl11 IS INITIAL AND
                           sl12 IS INITIAL AND
                           sl13 IS INITIAL AND
                           sl14 IS INITIAL AND
                           sl15 IS INITIAL AND
                           sl16 IS INITIAL.

    DELETE lt_saldo2 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.

    DELETE lt_saldo3 WHERE slvt IS INITIAL AND
                   sl01 IS INITIAL AND
                   sl02 IS INITIAL AND
                   sl03 IS INITIAL AND
                   sl04 IS INITIAL AND
                   sl05 IS INITIAL AND
                   sl06 IS INITIAL AND
                   sl07 IS INITIAL AND
                   sl08 IS INITIAL AND
                   sl09 IS INITIAL AND
                   sl10 IS INITIAL AND
                   sl11 IS INITIAL AND
                   sl12 IS INITIAL AND
                   sl13 IS INITIAL AND
                   sl14 IS INITIAL AND
                   sl15 IS INITIAL AND
                   sl16 IS INITIAL.



    CLEAR: ev_brl , ev_usd.

    IF gs_header-pais <> 'BR'
     AND gs_header-moeda_funcional <> 'BRL'.

    ELSE.

      LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

        ev_brl = ev_brl + ( <fs_saldo>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO FIELD-SYMBOL(<fs_any>).
          IF <fs_any> IS ASSIGNED.
            lv_valor_sl = <fs_any>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ENDIF.


    LOOP AT lt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      ev_usd = ev_usd + ( <fs_saldo2>-slvt * -1 ).

      lv_index = 1.
      DO .

        IF lv_index > lv_mes.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        ev_usd = ev_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_saldo_anterior_cta.

    DATA: lv_per_ini TYPE datum,
          lv_per_fim TYPE datum,
          lv_gjahr   TYPE bseg-gjahr.

    lv_gjahr = gs_header-data_base(4) - 1.


    lv_per_ini = lv_gjahr && '1231'.
    lv_per_fim = lv_gjahr && '1231'.

    SELECT SINGLE *
    FROM zglt0111
    INTO @DATA(ls_zglt0111)
    WHERE per_inicial = @lv_per_ini
      AND per_final = @lv_per_fim
      AND tipo_saldo = '03'
      AND investidora = @gs_header-empresa_investidora
      AND investida = @gs_header-empresa_investida.

    IF sy-subrc = 0.

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_mes.

    DATA: lv_mes        TYPE monat,
          lv_ano        TYPE gjahr,
          lv_index      TYPE sy-tabix,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2.

    DATA: lt_saldo  TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo3 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_contas TYPE zct_emp_contas.


    lv_mes = gs_mep_proc-mes.
    lv_ano = gs_mep_proc-ano.


    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
    <fs_conta>-saknr = is_conta-hkont.
    <fs_conta>-bukrs = gs_mep_proc-empresa_investida.


    CLEAR: lt_saldo, lt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lv_ano
        contas        = lt_contas
        p_gerar_todas = abap_true
        rldnr         = '0L'
      TABLES
        it_saldos     = lt_saldo
        it_saldos_2   = lt_saldo2
        it_saldos_3   = lt_saldo3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    DELETE lt_saldo WHERE slvt IS INITIAL AND
                           sl01 IS INITIAL AND
                           sl02 IS INITIAL AND
                           sl03 IS INITIAL AND
                           sl04 IS INITIAL AND
                           sl05 IS INITIAL AND
                           sl06 IS INITIAL AND
                           sl07 IS INITIAL AND
                           sl08 IS INITIAL AND
                           sl09 IS INITIAL AND
                           sl10 IS INITIAL AND
                           sl11 IS INITIAL AND
                           sl12 IS INITIAL AND
                           sl13 IS INITIAL AND
                           sl14 IS INITIAL AND
                           sl15 IS INITIAL AND
                           sl16 IS INITIAL.

    DELETE lt_saldo2 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.

    DELETE lt_saldo3 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.

    IF gs_header-pais <> 'BR'
    AND gs_header-moeda_funcional <> 'BRL'
    and gs_header-empresa_investida <> '0004' .

      CLEAR: ev_brl , ev_usd.

      LOOP AT lt_saldo3 ASSIGNING FIELD-SYMBOL(<fs_saldo3>).

        ev_brl = ev_brl + ( <fs_saldo3>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo3> TO FIELD-SYMBOL(<fs_any3>).
          IF <fs_any3> IS ASSIGNED.
            lv_valor_sl = <fs_any3>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any3>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ELSE.

      CLEAR: ev_brl , ev_usd.

      LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

        ev_brl = ev_brl + ( <fs_saldo>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO FIELD-SYMBOL(<fs_any>).
          IF <fs_any> IS ASSIGNED.
            lv_valor_sl = <fs_any>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ENDIF.

    LOOP AT lt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      ev_usd = ev_usd + ( <fs_saldo2>-slvt * -1 ).

      lv_index = 1.
      DO .

        IF lv_index > lv_mes.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        ev_usd = ev_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_saldo_mes_reserv_lucro.

    DATA: lv_mes        TYPE monat,
          lv_ano        TYPE gjahr,
          lv_index      TYPE sy-tabix,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2.

    DATA: lt_saldo  TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo3 TYPE TABLE OF zde_fi_gl_saldo_faglflext,
          lt_contas TYPE zct_emp_contas.


    lv_mes = gs_mep_proc-mes.
    lv_ano = gs_mep_proc-ano.


    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
    <fs_conta>-saknr = is_conta-hkont.
    <fs_conta>-bukrs = gs_mep_proc-empresa_investida.


    CLEAR: lt_saldo, lt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lv_ano
        contas        = lt_contas
        p_gerar_todas = abap_true
        rldnr         = '0L'
      TABLES
        it_saldos     = lt_saldo
        it_saldos_2   = lt_saldo2
        it_saldos_3   = lt_saldo3
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    DELETE lt_saldo WHERE slvt IS INITIAL AND
                           sl01 IS INITIAL AND
                           sl02 IS INITIAL AND
                           sl03 IS INITIAL AND
                           sl04 IS INITIAL AND
                           sl05 IS INITIAL AND
                           sl06 IS INITIAL AND
                           sl07 IS INITIAL AND
                           sl08 IS INITIAL AND
                           sl09 IS INITIAL AND
                           sl10 IS INITIAL AND
                           sl11 IS INITIAL AND
                           sl12 IS INITIAL AND
                           sl13 IS INITIAL AND
                           sl14 IS INITIAL AND
                           sl15 IS INITIAL AND
                           sl16 IS INITIAL.

    DELETE lt_saldo2 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.

    DELETE lt_saldo3 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.



    CLEAR: ev_brl , ev_usd.

    IF gs_header-pais <> 'BR'
    AND gs_header-moeda_funcional <> 'BRL'.

    ELSE.

      LOOP AT lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

        ev_brl = ev_brl + ( <fs_saldo>-slvt * -1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_mes.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO FIELD-SYMBOL(<fs_any>).
          IF <fs_any> IS ASSIGNED.
            lv_valor_sl = <fs_any>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.
          ev_brl = ev_brl + lv_valor_sl.
          UNASSIGN <fs_any>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ENDIF.

    LOOP AT lt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      ev_usd = ev_usd + ( <fs_saldo2>-slvt * -1 ).

      lv_index = 1.
      DO .

        IF lv_index > lv_mes.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        ev_usd = ev_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.


  ENDMETHOD.


  METHOD alv.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = gt_mep_alv.

        set_header( ).
        status_alv( ).
        colunas_alv( ).
        botao_alv( ).
        set_somatorio( ).
        ordenar( ).

      CATCH cx_root INTO DATA(lo_error).

    ENDTRY.

    go_alv->display( ).


  ENDMETHOD.


  METHOD set_header.

    DATA:
      lo_header  TYPE REF TO cl_salv_form_layout_grid,
      lo_h_label TYPE REF TO cl_salv_form_label,
      lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.

    DATA: lv_descr TYPE char50.



*   header object
    CREATE OBJECT lo_header.

*   Writing Bold phrase
    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
    lo_h_label->set_text( 'Movimentação do Patrimônio Líquido da investida e cálculo da Equivalência Patrimonial' ).

*   Writing Header texts
    lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
    lo_h_flow->create_text( text = 'Data Base:' ).
    lo_h_flow = lo_header->create_flow( row = 2  column = 2 ).
    lo_h_flow->create_text( text = gs_header-data_base ).

    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
    lo_h_flow->create_text( text = 'Empresa Investidora:' ).
    SELECT SINGLE companycodename
        FROM i_companycodevh
        INTO @DATA(lv_desc_emp)
        WHERE companycode = @gs_header-empresa_investidora.

    CONCATENATE gs_header-empresa_investidora '-' lv_desc_emp INTO lv_descr SEPARATED BY space.
    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
    lo_h_flow->create_text( text = lv_descr ).

    lo_h_flow = lo_header->create_flow( row = 4  column = 1 ).
    lo_h_flow->create_text( text = 'Empresa Investida:' ).
    SELECT SINGLE companycodename
        FROM i_companycodevh
        INTO @DATA(lv_desc_emp2)
        WHERE companycode = @gs_header-empresa_investida.

    CONCATENATE gs_header-empresa_investida '-' lv_desc_emp2 INTO lv_descr SEPARATED BY space.
    lo_h_flow = lo_header->create_flow( row = 4  column = 2 ).
    lo_h_flow->create_text( text = lv_descr ).

    lo_h_flow = lo_header->create_flow( row = 5  column = 1 ).
    lo_h_flow->create_text( text = 'Participação:' ).
    lo_h_flow = lo_header->create_flow( row = 5  column = 2 ).
    lo_h_flow->create_text( text = gs_header-participacao ).

    lo_h_flow = lo_header->create_flow( row = 6  column = 1 ).
    lo_h_flow->create_text( text = 'Tipo:' ).
    lo_h_flow = lo_header->create_flow( row = 6  column = 2 ).
    lo_h_flow->create_text( text = gs_header-tipo ).


    lo_h_flow = lo_header->create_flow( row = 7  column = 1 ).
    lo_h_flow->create_text( text = 'Moeda Funcional:' ).
    lo_h_flow = lo_header->create_flow( row = 7  column = 2 ).
    lo_h_flow->create_text( text = gs_header-moeda_funcional ).

    lo_h_flow = lo_header->create_flow( row = 8  column = 1 ).
    lo_h_flow->create_text( text = 'País:' ).
    lo_h_flow = lo_header->create_flow( row = 8  column = 2 ).
    lo_h_flow->create_text( text = gs_header-pais ).

*   Set the top of list
    go_alv->set_top_of_list( lo_header ).

*   Print on top of list
    go_alv->set_top_of_list_print( lo_header ).

  ENDMETHOD.


  METHOD status_alv.

    go_alv->set_screen_status(
      pfstatus      = 'STANDARD'
      report        = 'ZGLR080'
      set_functions = go_alv->c_functions_all ).

    go_alv->get_functions( )->set_all( ).

  ENDMETHOD.


  METHOD colunas_alv.

    DATA: lr_columns   TYPE REF TO cl_salv_columns_table,
          lr_column    TYPE REF TO cl_salv_column,
          lr_column_tp TYPE REF TO cl_salv_column_table.

    lr_columns = go_alv->get_columns( ).
**    lr_columns->set_optimize( ).

    TRY.
        lr_column = lr_columns->get_column( 'PARAMETRO' ).
        lr_column->set_long_text( 'Parâmetro' ).
        lr_column->set_medium_text( 'Parâmetro' ).
        lr_column->set_short_text( 'Parâmetro' ).
        lr_column->set_output_length( 40 ).


      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'CONTA' ).
        lr_column->set_long_text( 'Conta Razão' ).
        lr_column->set_medium_text( 'Conta Razão' ).
        lr_column->set_short_text( 'Conta' ).
        lr_column->set_output_length( 12 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DESC_CTA_EQU' ).
        lr_column->set_long_text( 'Descrição' ).
        lr_column->set_medium_text( 'Descrição' ).
        lr_column->set_short_text( 'Descrição' ).
        lr_column->set_output_length( 30 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SALDO_MES_ANT_BRL' ).
        lr_column->set_long_text( 'Saldo Mês anterior BRL' ).
        lr_column->set_medium_text( 'Saldo Mês Ant. BRL' ).
        lr_column->set_short_text( 'SaldoAnt' ).
        lr_column->set_output_length( 18 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SALDO_MES_ANT_USD' ).
        lr_column->set_long_text( 'Saldo Mês anterior USD' ).
        lr_column->set_medium_text( 'Saldo Mês Ant. USD' ).
        lr_column->set_short_text( 'SaldoAnt' ).
        lr_column->set_output_length( 18 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'MOVIMENTACAO_BRL' ).
        lr_column->set_long_text( 'Movimentação BRL' ).
        lr_column->set_medium_text( 'Movimentação BRL' ).
        lr_column->set_short_text( 'Mov.BRL' ).
        lr_column->set_output_length( 15 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'MOVIMENTACAO_USD' ).
        lr_column->set_long_text( 'Movimentação USD' ).
        lr_column->set_medium_text( 'Movimentação USD' ).
        lr_column->set_short_text( 'Mov.BRL' ).
        lr_column->set_output_length( 15 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SALDO_MES_ATUAL_BRL' ).
        lr_column->set_long_text( 'Saldo Mês atualr BRL' ).
        lr_column->set_medium_text( 'Saldo Mês atual BRL' ).
        lr_column->set_short_text( 'SaldoAnt' ).
        lr_column->set_output_length( 18 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SALDO_MES_ATUAL_USD' ).
        lr_column->set_long_text( 'Saldo Mês atual USD' ).
        lr_column->set_medium_text( 'Saldo Mês Atual USD' ).
        lr_column->set_short_text( 'SaldoAnt' ).
        lr_column->set_output_length( 18 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'EQUIVALENCIA_BRL' ).
        lr_column->set_long_text( 'Equivalência BRL' ).
        lr_column->set_medium_text( 'Equivalência BRL' ).
        lr_column->set_short_text( 'Eq. BRL' ).
        lr_column->set_output_length( 16 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'EQUIVALENCIA_USD' ).
        lr_column->set_long_text( 'Equivalência USD' ).
        lr_column->set_medium_text( 'Equivalência USD' ).
        lr_column->set_short_text( 'Eq. USD' ).
        lr_column->set_output_length( 16 ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DOCUMENTO_1' ).
        lr_column->set_long_text( 'Documento' ).
        lr_column->set_medium_text( 'Documento' ).
        lr_column->set_short_text( 'Documento' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'DOCUMENTO_1' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'ESTORNO_1' ).
        lr_column->set_long_text( 'Estorno' ).
        lr_column->set_medium_text( 'Estorno' ).
        lr_column->set_short_text( 'Estorno' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'ESTORNO_1' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).


      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DOCUMENTO_2' ).
        lr_column->set_long_text( 'Documento' ).
        lr_column->set_medium_text( 'Documento' ).
        lr_column->set_short_text( 'Documento' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'DOCUMENTO_2' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'ESTORNO_2' ).
        lr_column->set_long_text( 'Estorno' ).
        lr_column->set_medium_text( 'Estorno' ).
        lr_column->set_short_text( 'Estorno' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'ESTORNO_2' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'LOTE' ).
        lr_column->set_long_text( 'Lote' ).
        lr_column->set_medium_text( 'Lote' ).
        lr_column->set_short_text( 'Lote' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'LOTE' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'LOTE2' ).
        lr_column->set_long_text( 'Lote USD' ).
        lr_column->set_medium_text( 'Lote USD' ).
        lr_column->set_short_text( 'Lote' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'LOTE2' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'STATUS' ).
        lr_column->set_long_text( 'Status' ).
        lr_column->set_medium_text( 'Status' ).
        lr_column->set_short_text( 'Status' ).
        lr_column->set_optimized( 'X' ).

        lr_column_tp ?= lr_columns->get_column( 'STATUS' ).
        lr_column_tp->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'MES' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'ANO' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'USNAM' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DT_ATUAL' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'HR_ATUAL' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'INVESTIDORA' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'INVESTIDA' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'OBJKEY1' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'OBJKEY2' ).
        lr_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


  ENDMETHOD.


  METHOD botao_alv.

* add custom handler method to the table (implementation details follows in the next step)
    DATA(lo_events) = go_alv->get_event( ).
    SET HANDLER on_added_function FOR lo_events.
    SET HANDLER on_link_click   FOR lo_events.

  ENDMETHOD.


  METHOD preenche_cta.


    READ TABLE gt_zglt0107_get ASSIGNING FIELD-SYMBOL(<fs_zglt0107>)
                          WITH KEY parametro = gc_cta_param "'03 - CTA - Ajuste acumulado de conversão'
                                   BINARY SEARCH.
    IF sy-subrc = 0.

      APPEND INITIAL LINE TO gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>).

      <fs_mep>-parametro = <fs_zglt0107>-parametro.
      <fs_mep>-conta = <fs_zglt0107>-item_balanco.
      <fs_mep>-desc_cta_equ = <fs_zglt0107>-desc_cta_equ.
      <fs_mep>-saldo_mes_ant_brl = <fs_zglt0107>-sld_brl_ant.
      <fs_mep>-saldo_mes_ant_usd = <fs_zglt0107>-sld_usd_ant.
      <fs_mep>-saldo_mes_atual_brl = <fs_zglt0107>-sld_brl_atu.
      <fs_mep>-saldo_mes_atual_usd = <fs_zglt0107>-sld_usd_atu.
      <fs_mep>-movimentacao_brl = <fs_zglt0107>-movimentacao_brl.
      <fs_mep>-movimentacao_usd = <fs_zglt0107>-movimentacao_usd.
      <fs_mep>-equivalencia_brl = <fs_zglt0107>-equiv_brl.
      <fs_mep>-equivalencia_usd = <fs_zglt0107>-equiv_usd.

      <fs_mep>-lote = <fs_zglt0107>-lote.
      <fs_mep>-objkey1 = <fs_zglt0107>-objkey.
      <fs_mep>-documento_1 = <fs_zglt0107>-belnr.
      <fs_mep>-estorno_1 = <fs_zglt0107>-doc_lcto_est.
      <fs_mep>-objkey2 = <fs_zglt0107>-objkey_2.
      <fs_mep>-documento_2 = <fs_zglt0107>-belnr_2.
      <fs_mep>-estorno_2 = <fs_zglt0107>-doc_lcto_est_2.
      <fs_mep>-lote2 = <fs_zglt0107>-lote_2.

    ELSE.


      get_saldo_zglt0111( '03' ).
      get_saldo_zglt0107( gc_cta_param ).

      " VALOR CTA
      READ TABLE gt_cta ASSIGNING FIELD-SYMBOL(<fs_cta>) WITH KEY tipo = 'CTA'.
      IF sy-subrc = 0.

        "Saldo mes
        APPEND INITIAL LINE TO gt_mep_alv ASSIGNING <fs_mep>.
        <fs_mep>-parametro = gc_cta_param. " '03 - CTA - Ajuste acumulado de conversão'.
        <fs_mep>-saldo_mes_atual_brl = <fs_cta>-valor_brl.
        <fs_mep>-saldo_mes_atual_usd = <fs_cta>-valor_usd.


        "Saldo mes anterior
        IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.
          IF gs_zglt0111 IS NOT INITIAL.
            <fs_mep>-saldo_mes_ant_usd = gs_zglt0111-vl_pat_liq.
          ELSE.
            <fs_mep>-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
            <fs_mep>-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          ENDIF.

        ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

          IF gs_zglt0111 IS NOT INITIAL.
            <fs_mep>-saldo_mes_ant_brl = gs_zglt0111-vl_pat_liq.
          ELSE.
            <fs_mep>-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
            <fs_mep>-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          ENDIF.
        ENDIF.
        "Movimentacao
        <fs_mep>-movimentacao_brl = <fs_mep>-saldo_mes_atual_brl - <fs_mep>-saldo_mes_ant_brl .
        <fs_mep>-movimentacao_usd = <fs_mep>-saldo_mes_atual_usd - <fs_mep>-saldo_mes_ant_usd .


        "Movimentacao
        <fs_mep>-equivalencia_brl = <fs_mep>-movimentacao_brl * gv_participacao.
        <fs_mep>-equivalencia_usd = <fs_mep>-movimentacao_usd * gv_participacao.

      ENDIF.

    ENDIF.


    READ TABLE gt_zglt0107_get ASSIGNING <fs_zglt0107>
                      WITH KEY parametro = '15 - Resultado Período'
                               BINARY SEARCH.
    IF sy-subrc = 0.

      APPEND INITIAL LINE TO gt_mep_alv ASSIGNING <fs_mep>.

      <fs_mep>-parametro = <fs_zglt0107>-parametro.
      <fs_mep>-conta = <fs_zglt0107>-item_balanco.
      <fs_mep>-desc_cta_equ = <fs_zglt0107>-desc_cta_equ.
      <fs_mep>-saldo_mes_ant_brl = <fs_zglt0107>-sld_brl_ant.
      <fs_mep>-saldo_mes_ant_usd = <fs_zglt0107>-sld_usd_ant.
      <fs_mep>-saldo_mes_atual_brl = <fs_zglt0107>-sld_brl_atu.
      <fs_mep>-saldo_mes_atual_usd = <fs_zglt0107>-sld_usd_atu.
      <fs_mep>-movimentacao_brl = <fs_zglt0107>-movimentacao_brl.
      <fs_mep>-movimentacao_usd = <fs_zglt0107>-movimentacao_usd.
      <fs_mep>-equivalencia_brl = <fs_zglt0107>-equiv_brl.
      <fs_mep>-equivalencia_usd = <fs_zglt0107>-equiv_usd.

      <fs_mep>-lote = <fs_zglt0107>-lote.
      <fs_mep>-objkey1 = <fs_zglt0107>-objkey.
      <fs_mep>-documento_1 = <fs_zglt0107>-belnr.
      <fs_mep>-estorno_1 = <fs_zglt0107>-doc_lcto_est.
      <fs_mep>-objkey2 = <fs_zglt0107>-objkey_2.
      <fs_mep>-documento_2 = <fs_zglt0107>-belnr_2.
      <fs_mep>-estorno_2 = <fs_zglt0107>-doc_lcto_est_2.
      <fs_mep>-lote2 = <fs_zglt0107>-lote_2.

    ELSE.

      get_resultado_periodo(  ).

      " VALOR CTA
      READ TABLE gt_cta ASSIGNING <fs_cta> WITH KEY tipo = '15 - Resultado Período'.
      IF sy-subrc = 0.

        APPEND INITIAL LINE TO gt_mep_alv ASSIGNING <fs_mep>.

        <fs_mep>-parametro = <fs_cta>-tipo.
        <fs_mep>-saldo_mes_atual_brl = <fs_cta>-valor_brl.
        <fs_mep>-saldo_mes_atual_usd = <fs_cta>-valor_usd.


        "Saldo mes anterior
        IF gs_header-pais = 'BR' AND gs_header-moeda_funcional <> 'BRL'.
          <fs_mep>-saldo_mes_ant_usd = gv_vlr_res_usd.
          <fs_mep>-saldo_mes_ant_brl = gv_vlr_res_brl.
        ELSE.


          get_saldo_zglt0111( '15' ).
          get_saldo_zglt0107( gc_result_param ).

          "Saldo mes anterior
          IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.
            IF gs_zglt0111 IS NOT INITIAL.
              <fs_mep>-saldo_mes_ant_brl = gv_vlr_res_brl.
              <fs_mep>-saldo_mes_ant_usd = gs_zglt0111-vl_pat_liq.
            ELSE.
              <fs_mep>-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
              <fs_mep>-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
            ENDIF.

          ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

            IF gs_zglt0111 IS NOT INITIAL.
              <fs_mep>-saldo_mes_ant_usd = gv_vlr_res_usd.
              <fs_mep>-saldo_mes_ant_brl = gs_zglt0111-vl_pat_liq.
            ELSE.
              <fs_mep>-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
              <fs_mep>-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
            ENDIF.
          ENDIF.

        ENDIF.

        "Movimentacao
        <fs_mep>-movimentacao_brl = <fs_mep>-saldo_mes_atual_brl - <fs_mep>-saldo_mes_ant_brl .
        <fs_mep>-movimentacao_usd = <fs_mep>-saldo_mes_atual_usd - <fs_mep>-saldo_mes_ant_usd .


        "Movimentacao
        <fs_mep>-equivalencia_brl = <fs_mep>-movimentacao_brl * gv_participacao.
        <fs_mep>-equivalencia_usd = <fs_mep>-movimentacao_usd * gv_participacao.


      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_zglt0111.

    DATA: lv_per_ini TYPE datum,
          lv_per_fim TYPE datum,
          lv_gjahr   TYPE bseg-gjahr,
          lv_monat   TYPE monat.


    lv_monat = gs_header-data_base+4(2).
    lv_gjahr = gs_header-data_base(4).

    IF lv_monat = '01'.
      lv_gjahr = lv_gjahr - 1.
      lv_monat = '12'.
    ELSE.
      lv_monat = lv_monat - 1.
    ENDIF.


    lv_per_ini = lv_gjahr && lv_monat && '01'.


    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_per_ini
      IMPORTING
        last_day_of_month = lv_per_fim
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR: gs_zglt0111.
    SELECT SINGLE *
    FROM zglt0111
    INTO gs_zglt0111
    WHERE per_inicial >= lv_per_ini
      AND per_final <= lv_per_fim
      AND tipo_saldo = iv_tipo
      AND investidora = gs_header-empresa_investidora
      AND investida = gs_header-empresa_investida.

  ENDMETHOD.


  METHOD get_dados_cta.

    DATA lv_dt_ini TYPE dats.

    CLEAR: gt_cta.

    lv_dt_ini = gs_header-data_base(6) && '01'.
    .
    DATA(lo_cta) = NEW zclfi_cta(
      is_cta     = VALUE #( pais = gs_header-pais
                            moeda_funcional = gs_header-moeda_funcional
                            empresa_investida = gs_header-empresa_investida
                            empresa_investidora = gs_header-empresa_investidora
                            inicio_validade = lv_dt_ini
                            fim_validade = gs_header-data_base )
    iv_not_alv = abap_true ).

    lo_cta->processa( ).
    lo_cta->get_result(
      IMPORTING
        et_result_cta = gt_cta
    ).

  ENDMETHOD.


  METHOD get_dados_cta_ant.

    DATA: lv_mes TYPE monat,
          lv_ano TYPE gjahr.

    DATA lv_dt_ini TYPE dats.

    lv_mes = gs_mep_proc-mes.
    lv_ano = gs_mep_proc-ano.

    IF lv_mes = '01'.
      lv_ano = lv_ano - 1.
      lv_mes = '01'.
    ELSE.
      lv_mes = lv_mes - 1.
    ENDIF.

    CLEAR: gt_cta_ant.

    lv_dt_ini = lv_ano && lv_mes && '01'.
    .
    DATA(lo_cta) = NEW zclfi_cta(
      is_cta     = VALUE #( pais = gs_header-pais
                            moeda_funcional = gs_header-moeda_funcional
                            empresa_investida = gs_header-empresa_investida
                            empresa_investidora = gs_header-empresa_investidora
                            inicio_validade = lv_dt_ini
                            fim_validade = gs_header-data_base )
    iv_not_alv = abap_true ).

    lo_cta->processa( ).
    lo_cta->get_result(
      IMPORTING
        et_result_cta = gt_cta_ant
    ).

  ENDMETHOD.


  METHOD get_resultado_periodo.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    get_contas_result_per( ).
    get_saldo( ).

    lv_meses = gs_header-data_base+4(2).
    lv_meses = lv_meses - 1.

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * -1 ).


      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.
        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO FIELD-SYMBOL(<fs_any>).
        IF <fs_any> IS ASSIGNED.
          lv_valor_sl = <fs_any>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        lv_valor_brl = lv_valor_brl + lv_valor_sl.
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo_usd>).

      lv_valor_usd = lv_valor_usd + ( <fs_saldo_usd>-slvt * -1 ).


      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.
        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo_usd> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.
        lv_valor_usd = lv_valor_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    CLEAR: gv_vlr_res_brl, gv_vlr_res_usd.

    gv_vlr_res_brl = lv_valor_brl.
    gv_vlr_res_usd = lv_valor_usd.

  ENDMETHOD.


  METHOD get_contas_result_per.

    CLEAR: gt_contas_get.
    SELECT *
    FROM zi_eq_pat_contas_result
    INTO TABLE @gt_contas_get.

    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'Resultado Período' INTO gv_dummy .
      msg( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo.

    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano TYPE gjahr.

    LOOP AT gt_contas_get ASSIGNING FIELD-SYMBOL(<fs_contas>).

      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
      <fs_conta>-saknr = <fs_contas>-saknr.
      <fs_conta>-bukrs = gs_header-empresa_investida.

    ENDLOOP.

    lv_ano = gs_header-data_base(4).
    IF gs_header-data_base+4(2) = '01'.
      lv_ano = lv_ano - 1.
    ENDIF.

    CLEAR: gt_saldo, gt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear         = lv_ano
        contas        = lt_contas
        p_gerar_todas = abap_true
        rldnr         = '0L'
      TABLES
        it_saldos     = gt_saldo
        it_saldos_2   = gt_saldo2
      EXCEPTIONS
        moeda_nao_adm = 1
        erro_ledger   = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    DELETE gt_saldo WHERE slvt IS INITIAL AND
                          sl01 IS INITIAL AND
                          sl02 IS INITIAL AND
                          sl03 IS INITIAL AND
                          sl04 IS INITIAL AND
                          sl05 IS INITIAL AND
                          sl06 IS INITIAL AND
                          sl07 IS INITIAL AND
                          sl08 IS INITIAL AND
                          sl09 IS INITIAL AND
                          sl10 IS INITIAL AND
                          sl11 IS INITIAL AND
                          sl12 IS INITIAL AND
                          sl13 IS INITIAL AND
                          sl14 IS INITIAL AND
                          sl15 IS INITIAL AND
                          sl16 IS INITIAL.

    DELETE gt_saldo2 WHERE slvt IS INITIAL AND
                    sl01 IS INITIAL AND
                    sl02 IS INITIAL AND
                    sl03 IS INITIAL AND
                    sl04 IS INITIAL AND
                    sl05 IS INITIAL AND
                    sl06 IS INITIAL AND
                    sl07 IS INITIAL AND
                    sl08 IS INITIAL AND
                    sl09 IS INITIAL AND
                    sl10 IS INITIAL AND
                    sl11 IS INITIAL AND
                    sl12 IS INITIAL AND
                    sl13 IS INITIAL AND
                    sl14 IS INITIAL AND
                    sl15 IS INITIAL AND
                    sl16 IS INITIAL.



  ENDMETHOD.


  METHOD set_somatorio.

    DATA: lo_aggrs TYPE REF TO cl_salv_aggregations.

    lo_aggrs = go_alv->get_aggregations( ).


    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'SALDO_MES_ANT_BRL'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.


    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'SALDO_MES_ANT_USD'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'MOVIMENTACAO_BRL'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'MOVIMENTACAO_USD'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'SALDO_MES_ATUAL_BRL'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'SALDO_MES_ATUAL_USD'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'EQUIVALENCIA_BRL'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = 'EQUIVALENCIA_USD'
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.


  ENDMETHOD.


  METHOD on_added_function.

    CASE e_salv_function.

      WHEN 'ZCTA'.

        ir_cta( ).

      WHEN 'ZATU'.

        atualiza_documentos( ).

      WHEN 'ZGERAR'.

        gerar_documento( ).

      WHEN 'ZESTO'.

        estornar_documento( ).

      WHEN 'ZINI'.

        reinicializar(  ).

      WHEN 'ZCONC'.

        conc_invest( ).

      WHEN OTHERS.

    ENDCASE.

    go_alv->refresh(  ).

  ENDMETHOD.


  METHOD ir_cta.

    DATA: lv_inicio TYPE dats.

    lv_inicio = gs_header-data_base(6) && '01'.

    SUBMIT zglr079 AND RETURN
    WITH p_pais = gs_header-pais
    WITH p_mfunc = gs_header-moeda_funcional
    WITH p_ini = lv_inicio
    WITH p_fim = gs_header-data_base
    WITH p_ivtd = gs_header-empresa_investidora
    WITH p_ivda = gs_header-empresa_investida.

  ENDMETHOD.


  METHOD gerar_documento.

    DATA: lv_saldo_ano TYPE boolean.

    get_tab_paramentro(  ).

    IF gt_mep_tab_param IS INITIAL.

      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe documento gerado!'.
      EXIT.

    ELSE.

      LOOP AT gt_mep_tab_param ASSIGNING FIELD-SYMBOL(<fs_tab_param>).

        IF <fs_tab_param>-objkey1 IS NOT INITIAL OR
           <fs_tab_param>-objkey2 IS NOT INITIAL.

          CONTINUE.

        ELSEIF <fs_tab_param>-equivalencia_brl = 0 AND
          <fs_tab_param>-equivalencia_usd = 0.

          congela_linha( <fs_tab_param> ).

        ELSE.

          IF ( valor_positivo( <fs_tab_param>-equivalencia_brl ) =
               valor_positivo( <fs_tab_param>-equivalencia_usd  )
               AND ( <fs_tab_param>-equivalencia_brl <> 0
                 AND <fs_tab_param>-equivalencia_usd <> 0 ) ).

            gera_unico_documento( <fs_tab_param> ).
            lv_saldo_ano = abap_true.

          ELSE.

            gera_dois_documentos( <fs_tab_param> ).
            lv_saldo_ano = abap_true.

          ENDIF.

        ENDIF.

      ENDLOOP.

      IF lv_saldo_ano = abap_true.
        verifica_saldo_ano( ).
      ENDIF.

    ENDIF.




  ENDMETHOD.


  METHOD estornar_documento.

    DATA: lv_doc_estornado TYPE boolean.


    lv_doc_estornado = abap_false.

    LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>).

      IF <fs_mep>-documento_1 IS NOT INITIAL.

        SELECT SINGLE doc_lcto_est
          FROM zglt0107
          INTO @DATA(lv_estorno)
          WHERE monat      = @gs_header-data_base+4(2)
           AND gjahr       = @gs_header-data_base(4)
           AND investidora = @gs_header-empresa_investidora
           AND investida   = @gs_header-empresa_investida
           AND parametro = @<fs_mep>-parametro.

        IF sy-subrc = 0.
          IF lv_estorno IS NOT INITIAL.
            <fs_mep>-estorno_1 = lv_estorno.
          ELSE.
            CLEAR: <fs_mep>-estorno_1.
          ENDIF.
        ENDIF.

        IF <fs_mep>-estorno_1 IS INITIAL AND
          <fs_mep>-documento_1 IS NOT INITIAL .
          estorna_doc_line(
            EXPORTING
              iv_belnr = <fs_mep>-documento_1
            IMPORTING
              ev_stblg = <fs_mep>-estorno_1  ).

          lv_doc_estornado = abap_true.

          UPDATE zglt0107
          SET doc_lcto_est = <fs_mep>-estorno_1
          WHERE monat      = gs_header-data_base+4(2)
           AND gjahr       = gs_header-data_base(4)
           AND investidora = gs_header-empresa_investidora
           AND investida   = gs_header-empresa_investida
           AND parametro = <fs_mep>-parametro.

          COMMIT WORK AND WAIT.

        ENDIF.

      ELSEIF <fs_mep>-documento_2 IS NOT INITIAL.

        SELECT SINGLE doc_lcto_est_2
            FROM zglt0107
            INTO @DATA(lv_estorno2)
            WHERE monat      = @gs_header-data_base+4(2)
             AND gjahr       = @gs_header-data_base(4)
             AND investidora = @gs_header-empresa_investidora
             AND investida   = @gs_header-empresa_investida
             AND parametro = @<fs_mep>-parametro.

        IF sy-subrc = 0.
          IF lv_estorno IS NOT INITIAL.
            <fs_mep>-estorno_2 = lv_estorno2.
          ELSE.
            CLEAR: lv_doc_estornado.
          ENDIF.
        ENDIF.

        IF <fs_mep>-estorno_2 IS INITIAL
        AND <fs_mep>-documento_2 IS NOT INITIAL.
          estorna_doc_line(
              EXPORTING
                iv_belnr = <fs_mep>-documento_2
              IMPORTING
                ev_stblg = <fs_mep>-estorno_2  ).

          lv_doc_estornado = abap_true.

          UPDATE zglt0107
          SET doc_lcto_est_2 = <fs_mep>-estorno_2
          WHERE monat      = gs_header-data_base+4(2)
           AND gjahr       = gs_header-data_base(4)
           AND investidora = gs_header-empresa_investidora
           AND investida   = gs_header-empresa_investida
           AND parametro = <fs_mep>-parametro.

          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF lv_doc_estornado = abap_false.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não existe documento para estorno!'.
      EXIT.
    ELSE.

      estorna_saldo( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_tab_paramentro.

    DATA: ls_mep TYPE zsfi_mep.

    CLEAR: gt_mep_tab_param.

    LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>).

      CLEAR: ls_mep.
      MOVE-CORRESPONDING <fs_mep> TO ls_mep.

      CLEAR: ls_mep-conta, ls_mep-desc_cta_equ.

      COLLECT ls_mep INTO gt_mep_tab_param.

    ENDLOOP.

    DELETE gt_mep_tab_param WHERE documento_1 IS NOT INITIAL
                                      OR documento_2 IS NOT INITIAL.

  ENDMETHOD.


  METHOD gera_unico_documento.

    DATA: lva_num_lote  TYPE zlote_num,
          lva_dp_resp   TYPE char2,
          lva_blart     TYPE zglt031-blart,
          lva_bktxt     TYPE zglt031-bktxt,
          lva_prov_est  TYPE zglt031-prov_est,
          lva_liberado  TYPE char01,
          lva_equiv_brl TYPE zsld_brl_ant,
          lva_equiv_usd TYPE zsld_usd_ant,
          lva_objkey    TYPE zib_contabil_chv-obj_key.

    DATA: lit_zglt036  TYPE TABLE OF zglt036,
          lit_zglt032  TYPE TABLE OF zglt032,
          lit_zglt0108 TYPE TABLE OF zglt0108.

    DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
          wa_zglt036_flg TYPE zde_zglt036_flg.

    DATA: lwa_zglt035  TYPE zglt035,
          lwa_zglt0105 TYPE zglt0105,
          lwa_zglt036  TYPE zglt036,
          chave_saldo  TYPE c,
          lwa_zglt032  TYPE zglt032,
          lwa_zglt0108 TYPE zglt0108.

    DATA: lv_doc_lcto TYPE num10.
    DATA: ls_zglt0107 TYPE zglt0107.


    IF is_tab_param-equivalencia_brl < 0.
      chave_saldo = '2'.  "Negativo
    ELSE.
      chave_saldo = '1'.  "Positivo
    ENDIF.

    SELECT SINGLE *
      FROM zglt0105 INTO lwa_zglt0105
    WHERE investidora EQ gs_header-empresa_investidora
      AND investida EQ gs_header-empresa_investida
      AND tp_lancamento EQ is_tab_param-parametro(2)
      AND saldo EQ chave_saldo.


    IF lwa_zglt0105 IS NOT INITIAL.

      SELECT SINGLE blart,
                    bktxt,
                    prov_est
              FROM zglt031 INTO ( @lva_blart, @lva_bktxt, @lva_prov_est )
              WHERE tp_lcto = @lwa_zglt0105-modelo_zgl.

      lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = gs_header-empresa_investidora
          i_descr_lote  = 'Equivalencia Reflexa'
          i_user_resp   = sy-uname
          i_dep_resp    = lva_dp_resp
          i_status_lote = 'L'
        IMPORTING
          e_num_lote    = lva_num_lote.

* Criar cabeçalho na ZGLT035
      MOVE:    lva_num_lote                  TO lwa_zglt035-lote,
               gs_header-empresa_investidora TO lwa_zglt035-bukrs,
               lwa_zglt0105-modelo_zgl  TO lwa_zglt035-tp_lcto,
               lva_dp_resp              TO lwa_zglt035-dpto_resp,
               'BRL'                    TO lwa_zglt035-moeda_doc,
               lva_blart                TO lwa_zglt035-blart,
               lva_bktxt                TO lwa_zglt035-bktxt,
               gs_header-data_base      TO lwa_zglt035-bldat,
               gs_header-data_base      TO lwa_zglt035-budat,
               sy-datum                 TO lwa_zglt035-dt_lcto,
               lva_prov_est             TO lwa_zglt035-prov_est,
               gs_header-data_base+4(2) TO lwa_zglt035-monat,
               gs_header-data_base(4)   TO lwa_zglt035-gjahr,
               sy-uname                 TO lwa_zglt035-usnam,
               sy-datum                 TO lwa_zglt035-dt_entrada,
               sy-uzeit                 TO lwa_zglt035-hr_entrada,
               ''                       TO lwa_zglt035-st_lc_moeda.

      CONCATENATE gs_header-empresa_investidora
                  '-'
                  gs_header-empresa_investida
                  INTO lwa_zglt035-xblnr.

* Criar item na ZGLT036
      SELECT *
        FROM zglt032 INTO TABLE lit_zglt032
      WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

      IF lit_zglt032 IS NOT INITIAL.

        CLEAR: gt_zglt0107.

        CLEAR: lva_equiv_brl, lva_equiv_usd.
        LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>)
                     WHERE parametro = is_tab_param-parametro.

          MOVE:
                gs_header-data_base+4(2) TO ls_zglt0107-monat,
                gs_header-data_base(4)   TO ls_zglt0107-gjahr,
                gs_header-empresa_investidora TO ls_zglt0107-investidora,
                gs_header-empresa_investida TO ls_zglt0107-investida,
                lwa_zglt035-lote          TO ls_zglt0107-lote,
                lwa_zglt035-doc_lcto      TO ls_zglt0107-doc_lcto,
                ''                        TO ls_zglt0107-doc_lcto_est,
                sy-uname                  TO ls_zglt0107-usnam,
                sy-datum                  TO ls_zglt0107-dt_atual,
                sy-uzeit                  TO ls_zglt0107-hr_atual.

          ls_zglt0107-parametro  = <fs_mep>-parametro.
          ls_zglt0107-item_balanco  = <fs_mep>-conta.
          ls_zglt0107-desc_cta_equ  = <fs_mep>-desc_cta_equ.
          ls_zglt0107-sld_brl_atu = <fs_mep>-saldo_mes_atual_brl.
          ls_zglt0107-sld_usd_atu = <fs_mep>-saldo_mes_atual_usd.
          ls_zglt0107-sld_brl_ant = <fs_mep>-saldo_mes_ant_brl.
          ls_zglt0107-sld_usd_ant = <fs_mep>-saldo_mes_ant_usd.
          ls_zglt0107-equiv_brl = <fs_mep>-equivalencia_brl.
          ls_zglt0107-equiv_usd = <fs_mep>-equivalencia_usd.
          ls_zglt0107-movimentacao_brl = <fs_mep>-movimentacao_brl.
          ls_zglt0107-movimentacao_usd = <fs_mep>-movimentacao_usd.

          lva_equiv_brl = lva_equiv_brl + <fs_mep>-equivalencia_brl.
          lva_equiv_usd = lva_equiv_usd + <fs_mep>-equivalencia_usd.

          APPEND ls_zglt0107 TO gt_zglt0107.

        ENDLOOP.


        LOOP AT lit_zglt032 INTO lwa_zglt032.

          lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

          MOVE: sy-tabix                    TO lwa_zglt036-seqitem,
                lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                lwa_zglt032-hkont           TO lwa_zglt036-hkont,
                lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,
                lwa_zglt032-anbwa           TO lwa_zglt036-anbwa,
                lwa_zglt032-kostl           TO lwa_zglt036-kostl,
                lwa_zglt032-prctr           TO lwa_zglt036-prctr,
                lwa_zglt032-aufnr           TO lwa_zglt036-aufnr,
                lwa_zglt032-matnr           TO lwa_zglt036-matnr,
                lwa_zglt032-matnr_fi        TO lwa_zglt036-matnr_fi,
                lwa_zglt032-zuonr           TO lwa_zglt036-zuonr,
                lwa_zglt032-umskz           TO lwa_zglt036-umskz,
                lwa_zglt032-vbund           TO lwa_zglt036-vbund.

          CASE gs_header-empresa_investidora.
            WHEN '0100'.
              MOVE 'T001' TO lwa_zglt036-gsber.
            WHEN OTHERS.
              CONCATENATE gs_header-empresa_investidora+2(2) '01' INTO lwa_zglt036-gsber.
          ENDCASE.

          MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int,
                abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.


          APPEND lwa_zglt036 TO lit_zglt036.
          CLEAR: lwa_zglt036, lwa_zglt032.

        ENDLOOP.


      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
        EXIT.
      ENDIF.

      CASE gs_header-moeda_funcional.
        WHEN 'BRL'.

          CLEAR lwa_zglt036.
          LOOP AT lit_zglt036 INTO lwa_zglt036.
            wa_zglt036_flg-doc_lcto        = lwa_zglt036-doc_lcto.
            wa_zglt036_flg-seqitem         = lwa_zglt036-seqitem.
            wa_zglt036_flg-seqsub          = lwa_zglt036-seqsub.
            wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
            APPEND  wa_zglt036_flg TO it_zglt036_flg.
          ENDLOOP.
      ENDCASE.

* Contabilizar
      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        EXPORTING
          i_arredonda = abap_true
        CHANGING
          i_zglt036   = lit_zglt036
          i_zglt035   = lwa_zglt035 ).


      CLEAR: lva_liberado.
      CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
        EXPORTING
          p_num_lote = lwa_zglt035-lote
        IMPORTING
          p_liberado = lva_liberado.

      CHECK lva_liberado EQ abap_true.
      COMMIT WORK.
      MOVE lwa_zglt035-doc_lcto TO lv_doc_lcto.


      CLEAR: lva_objkey.
      CONCATENATE 'ZGL17' lv_doc_lcto gs_header-data_base(4) INTO lva_objkey.

      LOOP AT gt_zglt0107 ASSIGNING FIELD-SYMBOL(<fs_zglt0107>).
        <fs_zglt0107>-lote = lva_num_lote.
        <fs_zglt0107>-objkey = lva_objkey.
        <fs_zglt0107>-doc_lcto = lv_doc_lcto.
        <fs_zglt0107>-doc_lcto_est = ''.
      ENDLOOP.

      LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep_alv>)
                 WHERE parametro = is_tab_param-parametro.
        <fs_mep_alv>-objkey1   = lva_objkey.
        <fs_mep_alv>-lote = lva_num_lote.
      ENDLOOP.

      DELETE FROM zglt0107 WHERE monat       = gs_header-data_base+4(2)
                            AND  gjahr       = gs_header-data_base(4)
                            AND  investidora = gs_header-empresa_investidora
                            AND  investida   = gs_header-empresa_investida
                            AND  parametro = is_tab_param-parametro.
      COMMIT WORK AND WAIT.

      MODIFY zglt0107 FROM TABLE gt_zglt0107.
      COMMIT WORK AND WAIT.

      MESSAGE s836(sd) WITH 'O documento foi gerado com sucesso!'.

      CALL METHOD go_alv->refresh( ).

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                              'com parâmetros informados!'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD gera_dois_documentos.

    gera_doc_brl( is_tab_param ).

    gera_doc_usd( is_tab_param ).


  ENDMETHOD.


  METHOD valor_positivo.

    IF iv_valor > 0.
      r_value = abap_true.
    ELSE.
      r_value = abap_false.
    ENDIF.


  ENDMETHOD.


  METHOD gera_doc_brl.

    DATA: lva_num_lote  TYPE zlote_num,
          lva_dp_resp   TYPE char2,
          lva_blart     TYPE zglt031-blart,
          lva_bktxt     TYPE zglt031-bktxt,
          lva_prov_est  TYPE zglt031-prov_est,
          lva_liberado  TYPE char01,
          lva_equiv_brl TYPE zsld_brl_ant,
          lva_equiv_usd TYPE zsld_usd_ant,
          lva_objkey    TYPE zib_contabil_chv-obj_key.

    DATA: lit_zglt036  TYPE TABLE OF zglt036,
          lit_zglt032  TYPE TABLE OF zglt032,
          lit_zglt0108 TYPE TABLE OF zglt0108.

    DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
          wa_zglt036_flg TYPE zde_zglt036_flg.

    DATA: lwa_zglt035  TYPE zglt035,
          lwa_zglt0105 TYPE zglt0105,
          lwa_zglt036  TYPE zglt036,
          chave_saldo  TYPE c,
          lwa_zglt032  TYPE zglt032,
          lwa_zglt0108 TYPE zglt0108.

    DATA: lv_doc_lcto TYPE num10.
    DATA: ls_zglt0107 TYPE zglt0107.

    CHECK is_tab_param-equivalencia_brl <> 0.

    IF is_tab_param-equivalencia_brl < 0.
      chave_saldo = '2'.  "Negativo
    ELSE.
      chave_saldo = '1'.  "Positivo
    ENDIF.

    SELECT SINGLE *
      FROM zglt0105 INTO lwa_zglt0105
    WHERE investidora EQ gs_header-empresa_investidora
      AND investida EQ gs_header-empresa_investida
      AND tp_lancamento EQ is_tab_param-parametro(2)
      AND saldo EQ chave_saldo.


    IF lwa_zglt0105 IS NOT INITIAL.

      SELECT SINGLE blart,
                    bktxt,
                    prov_est
              FROM zglt031 INTO ( @lva_blart, @lva_bktxt, @lva_prov_est )
              WHERE tp_lcto = @lwa_zglt0105-modelo_zgl.

      lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = gs_header-empresa_investidora
          i_descr_lote  = 'Equivalencia Reflexa'
          i_user_resp   = sy-uname
          i_dep_resp    = lva_dp_resp
          i_status_lote = 'L'
        IMPORTING
          e_num_lote    = lva_num_lote.

* Criar cabeçalho na ZGLT035
      MOVE:    lva_num_lote                  TO lwa_zglt035-lote,
               gs_header-empresa_investidora TO lwa_zglt035-bukrs,
               lwa_zglt0105-modelo_zgl  TO lwa_zglt035-tp_lcto,
               lva_dp_resp              TO lwa_zglt035-dpto_resp,
               'BRL'                    TO lwa_zglt035-moeda_doc,
               lva_blart                TO lwa_zglt035-blart,
               lva_bktxt                TO lwa_zglt035-bktxt,
               gs_header-data_base      TO lwa_zglt035-bldat,
               gs_header-data_base      TO lwa_zglt035-budat,
               sy-datum                 TO lwa_zglt035-dt_lcto,
               lva_prov_est             TO lwa_zglt035-prov_est,
               gs_header-data_base+4(2) TO lwa_zglt035-monat,
               gs_header-data_base(4)   TO lwa_zglt035-gjahr,
               sy-uname                 TO lwa_zglt035-usnam,
               sy-datum                 TO lwa_zglt035-dt_entrada,
               sy-uzeit                 TO lwa_zglt035-hr_entrada,
               'X'                       TO lwa_zglt035-st_lc_moeda.


      CONCATENATE gs_header-empresa_investidora
                  '-'
                  gs_header-empresa_investida
                  INTO lwa_zglt035-xblnr.

* Criar item na ZGLT036
      SELECT *
        FROM zglt032 INTO TABLE lit_zglt032
      WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

      IF lit_zglt032 IS NOT INITIAL.

        CLEAR: gt_zglt0107.

        CLEAR: lva_equiv_brl, lva_equiv_usd.
        LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>)
                     WHERE parametro = is_tab_param-parametro.

          MOVE:
                gs_header-data_base+4(2) TO ls_zglt0107-monat,
                gs_header-data_base(4)   TO ls_zglt0107-gjahr,
                gs_header-empresa_investidora TO ls_zglt0107-investidora,
                gs_header-empresa_investida TO ls_zglt0107-investida,
                lwa_zglt035-lote          TO ls_zglt0107-lote,
                lwa_zglt035-doc_lcto      TO ls_zglt0107-doc_lcto,
                ''                        TO ls_zglt0107-doc_lcto_est,
                sy-uname                  TO ls_zglt0107-usnam,
                sy-datum                  TO ls_zglt0107-dt_atual,
                sy-uzeit                  TO ls_zglt0107-hr_atual.


          ls_zglt0107-parametro  = <fs_mep>-parametro.
          ls_zglt0107-item_balanco  = <fs_mep>-conta.
          ls_zglt0107-desc_cta_equ  = <fs_mep>-desc_cta_equ.
          ls_zglt0107-sld_brl_atu = <fs_mep>-saldo_mes_atual_brl.
          ls_zglt0107-sld_usd_atu = <fs_mep>-saldo_mes_atual_usd.
          ls_zglt0107-sld_brl_ant = <fs_mep>-saldo_mes_ant_brl.
          ls_zglt0107-sld_usd_ant = <fs_mep>-saldo_mes_ant_usd.
          ls_zglt0107-equiv_brl = <fs_mep>-equivalencia_brl.
          ls_zglt0107-equiv_usd = <fs_mep>-equivalencia_usd.
          ls_zglt0107-movimentacao_brl = <fs_mep>-movimentacao_brl.
          ls_zglt0107-movimentacao_usd = <fs_mep>-movimentacao_usd.


          lva_equiv_brl = lva_equiv_brl + <fs_mep>-equivalencia_brl.
          lva_equiv_usd = lva_equiv_usd + <fs_mep>-equivalencia_usd.

          APPEND ls_zglt0107 TO gt_zglt0107.

        ENDLOOP.


        LOOP AT lit_zglt032 INTO lwa_zglt032.

          lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

          MOVE: sy-tabix                    TO lwa_zglt036-seqitem,
                lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                lwa_zglt032-hkont           TO lwa_zglt036-hkont,
                lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,
                lwa_zglt032-anbwa           TO lwa_zglt036-anbwa,
                lwa_zglt032-kostl           TO lwa_zglt036-kostl,
                lwa_zglt032-prctr           TO lwa_zglt036-prctr,
                lwa_zglt032-aufnr           TO lwa_zglt036-aufnr,
                lwa_zglt032-matnr           TO lwa_zglt036-matnr,
                lwa_zglt032-matnr_fi        TO lwa_zglt036-matnr_fi,
                lwa_zglt032-zuonr           TO lwa_zglt036-zuonr,
                lwa_zglt032-umskz           TO lwa_zglt036-umskz,
                lwa_zglt032-vbund           TO lwa_zglt036-vbund.

          CASE gs_header-empresa_investidora.
            WHEN '0100'.
              MOVE 'T001' TO lwa_zglt036-gsber.
            WHEN OTHERS.
              CONCATENATE gs_header-empresa_investidora+2(2) '01' INTO lwa_zglt036-gsber.
          ENDCASE.

          MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.
*                abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.


          APPEND lwa_zglt036 TO lit_zglt036.
          CLEAR: lwa_zglt036, lwa_zglt032.

        ENDLOOP.


      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
        EXIT.
      ENDIF.

* Contabilizar
      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        EXPORTING
          i_arredonda = abap_true
        CHANGING
          i_zglt036   = lit_zglt036
          i_zglt035   = lwa_zglt035 ).

      CLEAR: lva_liberado.
      CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
        EXPORTING
          p_num_lote = lwa_zglt035-lote
        IMPORTING
          p_liberado = lva_liberado.

      CHECK lva_liberado EQ abap_true.
      COMMIT WORK.
      MOVE lwa_zglt035-doc_lcto TO lv_doc_lcto.


      CLEAR: lva_objkey.
      CONCATENATE 'ZGL17' lv_doc_lcto gs_header-data_base(4) INTO lva_objkey.

      LOOP AT gt_zglt0107 ASSIGNING FIELD-SYMBOL(<fs_zglt0107>).
        <fs_zglt0107>-lote = lva_num_lote.
        <fs_zglt0107>-objkey = lva_objkey.
        <fs_zglt0107>-doc_lcto = lv_doc_lcto.
        <fs_zglt0107>-doc_lcto_est = ''.
      ENDLOOP.

      LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep_alv>)
                 WHERE parametro = is_tab_param-parametro.
        <fs_mep_alv>-objkey1   = lva_objkey.
        <fs_mep_alv>-lote = lva_num_lote.
      ENDLOOP.

      DELETE FROM zglt0107 WHERE monat       = gs_header-data_base+4(2)
                            AND  gjahr       = gs_header-data_base(4)
                            AND  investidora = gs_header-empresa_investidora
                            AND  investida   = gs_header-empresa_investida
                            AND  parametro = is_tab_param-parametro.
      COMMIT WORK AND WAIT.

      MODIFY zglt0107 FROM TABLE gt_zglt0107.
      COMMIT WORK AND WAIT.

      MESSAGE s836(sd) WITH 'O documento foi gerado com sucesso!'.

      CALL METHOD go_alv->refresh( ).

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                              'com parâmetros informados!'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD gera_doc_usd.

    DATA: lva_num_lote  TYPE zlote_num,
          lva_dp_resp   TYPE char2,
          lva_blart     TYPE zglt031-blart,
          lva_bktxt     TYPE zglt031-bktxt,
          lva_prov_est  TYPE zglt031-prov_est,
          lva_liberado  TYPE char01,
          lva_equiv_brl TYPE zsld_brl_ant,
          lva_equiv_usd TYPE zsld_usd_ant,
          lva_objkey    TYPE zib_contabil_chv-obj_key.

    DATA: lit_zglt036  TYPE TABLE OF zglt036,
          lit_zglt032  TYPE TABLE OF zglt032,
          lit_zglt0108 TYPE TABLE OF zglt0108.

    DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
          wa_zglt036_flg TYPE zde_zglt036_flg.

    DATA: lwa_zglt035  TYPE zglt035,
          lwa_zglt0105 TYPE zglt0105,
          lwa_zglt036  TYPE zglt036,
          chave_saldo  TYPE c,
          lwa_zglt032  TYPE zglt032,
          lwa_zglt0108 TYPE zglt0108.

    DATA: lv_doc_lcto TYPE num10.
    DATA: ls_zglt0107 TYPE zglt0107.

    CHECK is_tab_param-equivalencia_usd <> 0.

    IF is_tab_param-equivalencia_usd < 0.
      chave_saldo = '2'.  "Negativo
    ELSE.
      chave_saldo = '1'.  "Positivo
    ENDIF.

    SELECT SINGLE *
      FROM zglt0105 INTO lwa_zglt0105
    WHERE investidora EQ gs_header-empresa_investidora
      AND investida EQ gs_header-empresa_investida
      AND tp_lancamento EQ is_tab_param-parametro(2)
      AND saldo EQ chave_saldo.


    IF lwa_zglt0105 IS NOT INITIAL.

      SELECT SINGLE blart,
                    bktxt,
                    prov_est
              FROM zglt031 INTO ( @lva_blart, @lva_bktxt, @lva_prov_est )
              WHERE tp_lcto = @lwa_zglt0105-modelo_zgl.

      lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = gs_header-empresa_investidora
          i_descr_lote  = 'Equivalencia Reflexa'
          i_user_resp   = sy-uname
          i_dep_resp    = lva_dp_resp
          i_status_lote = 'L'
        IMPORTING
          e_num_lote    = lva_num_lote.

* Criar cabeçalho na ZGLT035
      MOVE:    lva_num_lote                  TO lwa_zglt035-lote,
               gs_header-empresa_investidora TO lwa_zglt035-bukrs,
               lwa_zglt0105-modelo_zgl  TO lwa_zglt035-tp_lcto,
               lva_dp_resp              TO lwa_zglt035-dpto_resp,
               'USD'                    TO lwa_zglt035-moeda_doc,
               lva_blart                TO lwa_zglt035-blart,
               lva_bktxt                TO lwa_zglt035-bktxt,
               gs_header-data_base      TO lwa_zglt035-bldat,
               gs_header-data_base      TO lwa_zglt035-budat,
               sy-datum                 TO lwa_zglt035-dt_lcto,
               lva_prov_est             TO lwa_zglt035-prov_est,
               gs_header-data_base+4(2) TO lwa_zglt035-monat,
               gs_header-data_base(4)   TO lwa_zglt035-gjahr,
               sy-uname                 TO lwa_zglt035-usnam,
               sy-datum                 TO lwa_zglt035-dt_entrada,
               sy-uzeit                 TO lwa_zglt035-hr_entrada,
               'X'                       TO lwa_zglt035-st_lc_moeda.


      CONCATENATE gs_header-empresa_investidora
                  '-'
                  gs_header-empresa_investida
                  INTO lwa_zglt035-xblnr.

* Criar item na ZGLT036
      SELECT *
        FROM zglt032 INTO TABLE lit_zglt032
      WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

      IF lit_zglt032 IS NOT INITIAL.

        CLEAR: gt_zglt0107.

        CLEAR: lva_equiv_brl, lva_equiv_usd.
        LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep>)
                     WHERE parametro = is_tab_param-parametro.

          MOVE:
                gs_header-data_base+4(2) TO ls_zglt0107-monat,
                gs_header-data_base(4)   TO ls_zglt0107-gjahr,
                gs_header-empresa_investidora TO ls_zglt0107-investidora,
                gs_header-empresa_investida TO ls_zglt0107-investida,
                lwa_zglt035-lote          TO ls_zglt0107-lote_2,
                lwa_zglt035-doc_lcto      TO ls_zglt0107-doc_lcto,
                ''                        TO ls_zglt0107-doc_lcto_est,
                sy-uname                  TO ls_zglt0107-usnam,
                sy-datum                  TO ls_zglt0107-dt_atual,
                sy-uzeit                  TO ls_zglt0107-hr_atual.


          ls_zglt0107-parametro  = <fs_mep>-parametro.
          ls_zglt0107-item_balanco  = <fs_mep>-conta.
          ls_zglt0107-desc_cta_equ  = <fs_mep>-desc_cta_equ.
          ls_zglt0107-sld_brl_atu = <fs_mep>-saldo_mes_atual_brl.
          ls_zglt0107-sld_usd_atu = <fs_mep>-saldo_mes_atual_usd.
          ls_zglt0107-sld_brl_ant = <fs_mep>-saldo_mes_ant_brl.
          ls_zglt0107-sld_usd_ant = <fs_mep>-saldo_mes_ant_usd.
          ls_zglt0107-equiv_brl = <fs_mep>-equivalencia_brl.
          ls_zglt0107-equiv_usd = <fs_mep>-equivalencia_usd.
          ls_zglt0107-movimentacao_brl = <fs_mep>-movimentacao_brl.
          ls_zglt0107-movimentacao_usd = <fs_mep>-movimentacao_usd.


          lva_equiv_brl = lva_equiv_brl + <fs_mep>-equivalencia_brl.
          lva_equiv_usd = lva_equiv_usd + <fs_mep>-equivalencia_usd.

          APPEND ls_zglt0107 TO gt_zglt0107.

        ENDLOOP.


        LOOP AT lit_zglt032 INTO lwa_zglt032.

          lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

          MOVE: sy-tabix                    TO lwa_zglt036-seqitem,
                lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                lwa_zglt032-hkont           TO lwa_zglt036-hkont,
                lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,
                lwa_zglt032-anbwa           TO lwa_zglt036-anbwa,
                lwa_zglt032-kostl           TO lwa_zglt036-kostl,
                lwa_zglt032-prctr           TO lwa_zglt036-prctr,
                lwa_zglt032-aufnr           TO lwa_zglt036-aufnr,
                lwa_zglt032-matnr           TO lwa_zglt036-matnr,
                lwa_zglt032-matnr_fi        TO lwa_zglt036-matnr_fi,
                lwa_zglt032-zuonr           TO lwa_zglt036-zuonr,
                lwa_zglt032-umskz           TO lwa_zglt036-umskz,
                lwa_zglt032-vbund           TO lwa_zglt036-vbund.

          CASE gs_header-empresa_investidora.
            WHEN '0100'.
              MOVE 'T001' TO lwa_zglt036-gsber.
            WHEN OTHERS.
              CONCATENATE gs_header-empresa_investidora+2(2) '01' INTO lwa_zglt036-gsber.
          ENDCASE.

          MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_doc,
                abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.


          APPEND lwa_zglt036 TO lit_zglt036.
          CLEAR: lwa_zglt036, lwa_zglt032.

        ENDLOOP.


      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
        EXIT.
      ENDIF.

      CLEAR lwa_zglt036.
      LOOP AT lit_zglt036 INTO lwa_zglt036.

        wa_zglt036_flg-doc_lcto        = lwa_zglt036-doc_lcto.
        wa_zglt036_flg-seqitem         = lwa_zglt036-seqitem.
        wa_zglt036_flg-seqsub          = lwa_zglt036-seqsub.
        wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
        "IF chave_saldo EQ 2. Comentado 11.1.2024 - CBRAND
          wa_zglt036_flg-fl_cv_moeda_int = abap_true.
        "ENDIF.

        "IF chave_saldo EQ 2. Comentado 11.1.2024 - CBRAND
          wa_zglt036_flg-fl_cv_moeda_for = abap_true.
        "ENDIF.
        wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
        APPEND  wa_zglt036_flg TO it_zglt036_flg.
      ENDLOOP.

* Contabilizar
      CALL METHOD zcl_gerar_lote=>contabilizar_lote(
        EXPORTING
          i_zglt036_flg = it_zglt036_flg
          i_arredonda   = abap_true
        CHANGING
          i_zglt036     = lit_zglt036
          i_zglt035     = lwa_zglt035 ).

      CLEAR: lva_liberado.
      CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
        EXPORTING
          p_num_lote = lwa_zglt035-lote
        IMPORTING
          p_liberado = lva_liberado.

      CHECK lva_liberado EQ abap_true.
      COMMIT WORK.
      MOVE lwa_zglt035-doc_lcto TO lv_doc_lcto.


      CLEAR: lva_objkey.
      CONCATENATE 'ZGL17' lv_doc_lcto gs_header-data_base(4) INTO lva_objkey.

      LOOP AT gt_zglt0107 ASSIGNING FIELD-SYMBOL(<fs_zglt0107>).
        <fs_zglt0107>-lote_2 = lva_num_lote.
        <fs_zglt0107>-objkey_2 = lva_objkey.
        <fs_zglt0107>-doc_lcto_2 = lv_doc_lcto.
        <fs_zglt0107>-doc_lcto_est_2 = ''.
      ENDLOOP.

      LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_mep_alv>)
                 WHERE parametro = is_tab_param-parametro.
        <fs_mep_alv>-objkey2   = lva_objkey.
        <fs_mep_alv>-lote2 = lva_num_lote.
      ENDLOOP.


      SELECT COUNT(*)
        FROM zglt0107
        WHERE monat      = gs_header-data_base+4(2)
               AND gjahr       = gs_header-data_base(4)
               AND investidora = gs_header-empresa_investidora
               AND investida   = gs_header-empresa_investida
               AND parametro = is_tab_param-parametro.
      IF sy-subrc = 0.

        UPDATE zglt0107
                SET lote_2 = lva_num_lote
                   doc_lcto_2 = lv_doc_lcto
                   objkey_2 = lva_objkey
                WHERE monat      = gs_header-data_base+4(2)
                 AND gjahr       = gs_header-data_base(4)
                 AND investidora = gs_header-empresa_investidora
                 AND investida   = gs_header-empresa_investida
                 AND parametro = is_tab_param-parametro.
        COMMIT WORK AND WAIT.

      ELSE.

        MODIFY zglt0107 FROM TABLE gt_zglt0107.
        COMMIT WORK AND WAIT.

      ENDIF.

      MESSAGE s836(sd) WITH 'O documento foi gerado com sucesso!'.

      CALL METHOD go_alv->refresh( ).

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                              'com parâmetros informados!'.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD on_link_click.


*
    READ TABLE gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_line>)
        INDEX row.
    IF <fs_line> IS ASSIGNED.

      IF column EQ 'DOCUMENTO_1'.
        IF <fs_line>-documento_1 IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD <fs_line>-documento_1.
          SET PARAMETER ID 'BUK' FIELD gs_header-empresa_investidora .
          SET PARAMETER ID 'GJR' FIELD gs_header-data_base(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

      IF column EQ 'DOCUMENTO_2'.
        IF <fs_line>-documento_2 IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD <fs_line>-documento_2.
          SET PARAMETER ID 'BUK' FIELD gs_header-empresa_investidora .
          SET PARAMETER ID 'GJR' FIELD gs_header-data_base(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

      IF column EQ 'ESTORNO_1'.
        IF <fs_line>-estorno_1 IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD <fs_line>-estorno_1.
          SET PARAMETER ID 'BUK' FIELD gs_header-empresa_investidora .
          SET PARAMETER ID 'GJR' FIELD gs_header-data_base(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

      IF column EQ 'ESTORNO_2'.
        IF <fs_line>-estorno_2 IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD <fs_line>-estorno_2.
          SET PARAMETER ID 'BUK' FIELD gs_header-empresa_investidora .
          SET PARAMETER ID 'GJR' FIELD gs_header-data_base(4).
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

      IF column EQ 'STATUS'.
        visualizar_log( <fs_line> ).
      ENDIF.


      IF column EQ 'LOTE' OR column EQ 'LOTE2'.
        IF <fs_line>-lote IS NOT INITIAL.
          SUBMIT zgl018 WITH p_bukrs = gs_header-empresa_investidora
                        WITH p_lote = <fs_line>-lote
                        AND RETURN.
        ELSEIF <fs_line>-lote2 IS NOT INITIAL.
          SUBMIT zgl018 WITH p_bukrs = gs_header-empresa_investidora
                      WITH p_lote = <fs_line>-lote2
                      AND RETURN.
        ENDIF.
      ENDIF.

    ENDIF. "row in ALV obtained


  ENDMETHOD.


  METHOD atualiza_status.

    LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

      IF <fs_alv>-objkey1 IS NOT INITIAL.

        IF <fs_alv>-documento_1 IS INITIAL.

          SELECT SINGLE *
              FROM zib_contabil_chv
              INTO @DATA(ls_contabil)
              WHERE obj_key = @<fs_alv>-objkey1.

          IF ls_contabil-belnr IS NOT INITIAL.

            <fs_alv>-documento_1 = ls_contabil-belnr.
            <fs_alv>-status = '@01@'.

            UPDATE zglt0107
            SET belnr = <fs_alv>-documento_1
            WHERE monat      = gs_header-data_base+4(2)
             AND gjahr       = gs_header-data_base(4)
             AND investidora = gs_header-empresa_investidora
             AND investida   = gs_header-empresa_investida
             AND parametro = <fs_alv>-parametro.

            COMMIT WORK AND WAIT.

          ELSE.

            SELECT COUNT(*)
              FROM zib_contabil_err
              WHERE obj_key = @<fs_alv>-objkey1.

            IF sy-subrc = 0.
              <fs_alv>-status = '@02@'.
            ENDIF.

          ENDIF.

        ELSE.
          <fs_alv>-status = '@01@'.
        ENDIF.

      ELSE.
        <fs_alv>-status = '@00@'.
      ENDIF.

      IF <fs_alv>-objkey2 IS NOT INITIAL.

        IF <fs_alv>-documento_2 IS INITIAL.

          SELECT SINGLE *
           FROM zib_contabil_chv
           INTO @DATA(ls_contabil2)
           WHERE obj_key = @<fs_alv>-objkey2.

          IF ls_contabil2-belnr IS NOT INITIAL.

            <fs_alv>-documento_2 = ls_contabil2-belnr.
            <fs_alv>-status = '@01@'.

            UPDATE zglt0107
              SET belnr_2 = <fs_alv>-documento_2
              WHERE monat      = gs_header-data_base+4(2)
               AND gjahr       = gs_header-data_base(4)
               AND investidora = gs_header-empresa_investidora
               AND investida   = gs_header-empresa_investida
               AND parametro = <fs_alv>-parametro.

            COMMIT WORK AND WAIT.

          ELSE.

            SELECT COUNT(*)
              FROM zib_contabil_err
              WHERE obj_key = @<fs_alv>-objkey2.

            IF sy-subrc = 0.
              <fs_alv>-status = '@02@'.
            ENDIF.

          ENDIF.


        ELSE.
          <fs_alv>-status = '@01@'.
        ENDIF.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD atualiza_documentos.

    atualiza_status( ).

  ENDMETHOD.


  METHOD estorna_doc_line.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata,
          lt_msg     TYPE TABLE OF bdcmsgcoll,
          lt_bapiret TYPE STANDARD TABLE OF bapiret2.
    DATA: ls_opt     TYPE ctu_params.
    DATA: lva_budat(10) TYPE c,
          lva_stblg     TYPE bkpf-stblg.


    CONCATENATE gs_header-data_base+6(2)
                    gs_header-data_base+4(2)
                    gs_header-data_base(4)
                    INTO lva_budat.


    CLEAR: lt_bdcdata.
    lt_bdcdata = VALUE #(
    ( program = 'SAPMF05A' dynpro = '0105'  dynbegin = 'X' fnam = ' '  fval = ' '  )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'BDC_CURSOR'  fval = 'UF05A-STGRD' )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'BDC_OKCODE'  fval = '=BU' )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'RF05A-BELNS'  fval = iv_belnr )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'BKPF-BUKRS'  fval = gs_header-empresa_investidora )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'RF05A-GJAHS'  fval = gs_header-data_base(4) )
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'UF05A-STGRD'  fval = '01')
    ( program = ' ' dynpro = ' '  dynbegin = ' ' fnam = 'BSIS-BUDAT'  fval = lva_budat )
     ).

    ls_opt-dismode = 'N'.

    CALL TRANSACTION 'FB08' USING lt_bdcdata OPTIONS FROM ls_opt
            MESSAGES INTO lt_msg.

    READ TABLE lt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>)
                                      WITH KEY msgtyp = 'E'.

    IF sy-subrc IS INITIAL.

      REFRESH: lt_bapiret.
      LOOP AT lt_msg ASSIGNING <fs_msg>.

        APPEND INITIAL LINE TO lt_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).

        <fs_bapiret>-type       = <fs_msg>-msgtyp.
        <fs_bapiret>-id         = <fs_msg>-msgid.
        <fs_bapiret>-number     = <fs_msg>-msgnr.
        <fs_bapiret>-message_v1 = <fs_msg>-msgv1.
        <fs_bapiret>-message_v2 = <fs_msg>-msgv2.
        <fs_bapiret>-message_v3 = <fs_msg>-msgv3.
        <fs_bapiret>-message_v4 = <fs_msg>-msgv4.

      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM lt_bapiret COMPARING ALL FIELDS.

      CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
        EXPORTING
          it_message = lt_bapiret.

      LEAVE LIST-PROCESSING.

    ELSE.

      SELECT SINGLE stblg
        FROM bkpf INTO lva_stblg
       WHERE bukrs = gs_header-empresa_investidora
         AND belnr = iv_belnr
         AND gjahr = gs_header-data_base(4).

      CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

      ev_stblg = lva_stblg.

      MESSAGE s836(sd) WITH 'O documento foi estornado com sucesso!'.

    ENDIF.

  ENDMETHOD.


  METHOD visualizar_log.

    TYPES: BEGIN OF ty_zib_err,
             obj_key        TYPE zib_contabil_err-obj_key,
             dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
             hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
             message        TYPE zib_contabil_err-message,
           END OF ty_zib_err.

    DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

    IF ( is_line-objkey1 IS NOT INITIAL ).


      SELECT obj_key, dt_atualizacao, hr_atualizacao, message
         FROM zib_contabil_err INTO TABLE @lit_zib_err
          WHERE obj_key = @is_line-objkey1.

      IF ( sy-subrc = 0 ).
        cl_demo_output=>new(
          )->begin_section( `ZIB_CONTABIL_ERR:`
          )->write_text( |Erros encontrados na crição do documento: \n|
          ")->WRITE_DATA( SY-DATUM
          )->write_data( lit_zib_err[]
          )->end_section(
          )->display( ).
      ENDIF.

    ELSEIF is_line-objkey2 IS NOT INITIAL.

      SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
      WHERE obj_key = @is_line-objkey2.

      IF ( sy-subrc = 0 ).
        cl_demo_output=>new(
          )->begin_section( `ZIB_CONTABIL_ERR:`
          )->write_text( |Erros encontrados na crição do documento: \n|
          ")->WRITE_DATA( SY-DATUM
          )->write_data( lit_zib_err[]
          )->end_section(
          )->display( ).
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD reinicializar.

    DATA: lv_nok TYPE boolean.


    LOOP AT gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

      IF <fs_alv>-objkey1 IS NOT INITIAL AND
         <fs_alv>-estorno_1 IS INITIAL.
        lv_nok = abap_true.
        EXIT.
      ENDIF.

      IF <fs_alv>-objkey2 IS NOT INITIAL AND
         <fs_alv>-estorno_2 IS INITIAL.
        lv_nok = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.


    IF lv_nok = abap_true.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Verifique documentos não estornados!'.
      EXIT.
    ELSE.

      exec_reinicializacao( ).


    ENDIF.


  ENDMETHOD.


  METHOD exec_reinicializacao.

    limpa_dados_tabelas( ).

    CLEAR: gt_mep_alv.

    get_info_header( ).
    processa_linhas( ).

    go_alv->refresh( ).

  ENDMETHOD.


  METHOD limpa_dados_tabelas.

    DELETE FROM zglt0107 WHERE monat       = gs_header-data_base+4(2)
                         AND gjahr       = gs_header-data_base(4)
                         AND investidora = gs_header-empresa_investidora
                         AND investida   = gs_header-empresa_investida.
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD get_saldo_zglt0107.

    DATA: lv_per_ini TYPE datum,
          lv_per_fim TYPE datum,
          lv_gjahr   TYPE bseg-gjahr,
          lv_monat   TYPE monat.


    lv_monat = gs_header-data_base+4(2).
    lv_gjahr = gs_header-data_base(4).

    IF lv_monat = '01'.
      lv_gjahr = lv_gjahr - 1.
      lv_monat = '12'.
    ELSE.
      lv_monat = lv_monat - 1.
    ENDIF.


    CLEAR: gs_zglt0107.
    SELECT SINGLE *
    FROM zglt0107
    INTO @gs_zglt0107
    WHERE monat = @lv_monat
      AND gjahr = @lv_gjahr
      AND investidora = @gs_header-empresa_investidora
      AND investida = @gs_header-empresa_investida
      AND parametro = @iv_parametro.


  ENDMETHOD.


  METHOD verifica_saldo_ano.

    DATA: lv_valor_brl TYPE z_val_pat_liq,
          lv_valor_usd TYPE z_val_pat_liq.
    DATA ls_zglt0111 TYPE zglt0111.

    "Verifica se é final do ano
    CHECK gs_header-data_base+4(2) = 12.

    READ TABLE gt_mep_alv ASSIGNING FIELD-SYMBOL(<fs_result>)
                  WITH KEY parametro = gc_result_param.
    IF sy-subrc = 0.

      ls_zglt0111 = get_saldo_ano_ant( ).

      ls_zglt0111-data = sy-datum.
      ls_zglt0111-usnam = sy-uname.
      ls_zglt0111-hora = sy-uzeit.
      ls_zglt0111-per_inicial = gs_header-data_base.
      ls_zglt0111-per_final = gs_header-data_base.

      IF ls_zglt0111 IS NOT INITIAL.

        IF gs_header-pais = 'BR' AND
           gs_header-moeda_funcional = 'BRL'.

          lv_valor_usd = <fs_result>-equivalencia_usd * -1.
          lv_valor_usd = lv_valor_usd + ls_zglt0111-vl_pat_liq.

        ELSEIF gs_header-pais <> 'BR' AND
               gs_header-moeda_funcional = 'USD'.

          lv_valor_brl = <fs_result>-equivalencia_brl * -1.
          lv_valor_brl = lv_valor_brl + ls_zglt0111-vl_pat_liq.

        ELSEIF gs_header-pais = 'BR' AND
               gs_header-moeda_funcional = 'USD'.

          lv_valor_usd = <fs_result>-equivalencia_usd * -1.
          lv_valor_usd = lv_valor_usd + ls_zglt0111-vl_pat_liq.

        ENDIF.

        IF lv_valor_brl > 0 OR
           lv_valor_usd > 0.

          IF lv_valor_usd IS NOT INITIAL.
            ls_zglt0111-vl_pat_liq = lv_valor_usd.
          ELSE.
            ls_zglt0111-vl_pat_liq = lv_valor_brl.
          ENDIF.

          ls_zglt0111-tipo_saldo = '11'.

          MODIFY zglt0111 FROM ls_zglt0111.
          COMMIT WORK AND WAIT.

        ELSE.

          IF lv_valor_usd IS NOT INITIAL.
            ls_zglt0111-vl_pat_liq = lv_valor_usd.
          ELSE.
            ls_zglt0111-vl_pat_liq = lv_valor_brl.
          ENDIF.

          ls_zglt0111-tipo_saldo = '09'.

          MODIFY zglt0111 FROM ls_zglt0111.
          COMMIT WORK AND WAIT.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_ano_ant.

    DATA: lv_data_base TYPE dats.
    DATA: lv_ano TYPE gjahr .
    DATA: lr_tp_saldo TYPE RANGE OF z_tp_saldo_eq.

    lr_tp_saldo = VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                           ( sign = 'I' option = 'EQ' low = '11' ) ).

    lv_ano = gs_header-data_base(4).
    lv_ano = lv_ano - 1.
    lv_data_base = lv_ano && gs_header-data_base+4(4).

    SELECT SINGLE *
    FROM zglt0111
    INTO @rs_result
    WHERE investida = @gs_header-empresa_investida
        AND investidora = @gs_header-empresa_investidora
        AND per_inicial = @lv_data_base
        AND tipo_saldo IN @lr_tp_saldo.

  ENDMETHOD.


  METHOD estorna_saldo.

    DATA: lr_tp_saldo TYPE RANGE OF z_tp_saldo_eq.

    lr_tp_saldo = VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                           ( sign = 'I' option = 'EQ' low = '11' ) ).

    CHECK gs_header-data_base+4(2) = 12.

    DELETE FROM zglt0111
    WHERE investida = @gs_header-empresa_investida
        AND investidora = @gs_header-empresa_investidora
        AND per_inicial = @gs_header-data_base
        AND tipo_saldo IN @lr_tp_saldo.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD conc_invest.

    SUBMIT zglr081
            WITH p_monat = gs_mep_proc-mes
            WITH p_exer = gs_mep_proc-ano
            WITH p_ivtd = gs_mep_proc-empresa_investidora
            WITH p_ivda = gs_mep_proc-empresa_investida
            AND RETURN.

  ENDMETHOD.


  METHOD mes_ant_reserva_lucro.

    get_saldo_zglt0107( gc_reserva_lucro ).
    get_saldo_zglt0111( '09' ).

    READ TABLE gt_cta_ant WITH KEY
                               tipo = '09 - Reserva de Lucro'
                               saknr = cs_mep_alv-conta
        ASSIGNING FIELD-SYMBOL(<fs_cta_9>) .
    IF sy-subrc = 0.

      "Saldo mes anterior
      IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.

        CLEAR: cs_mep_alv-saldo_mes_ant_usd.

        IF gs_zglt0111 IS NOT INITIAL.
          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0111-vl_pat_liq * -1.
        ELSE.
*          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
*          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          cs_mep_alv-saldo_mes_ant_brl = <fs_cta_9>-valor_brl.
          cs_mep_alv-saldo_mes_ant_usd = <fs_cta_9>-valor_usd.
        ENDIF.

      ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

        CLEAR: cs_mep_alv-saldo_mes_ant_brl.

        IF gs_zglt0111 IS NOT INITIAL.
          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0111-vl_pat_liq * -1.
        ELSE.
**          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
**          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          cs_mep_alv-saldo_mes_ant_brl = <fs_cta_9>-valor_brl.
          cs_mep_alv-saldo_mes_ant_usd = <fs_cta_9>-valor_usd.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


    METHOD mes_anterior_prejuizo.
    "151188 CS2023000082 Melhorias transação ZGL083 PSA

    get_saldo_zglt0107( gc_reserva_lucro ).
    get_saldo_zglt0111( '11' ).

    READ TABLE gt_cta_ant WITH KEY
                               tipo = '11 - Prejuízos Acumulados'
                               saknr = cs_mep_alv-conta
        ASSIGNING FIELD-SYMBOL(<fs_cta_11>) .
    IF sy-subrc = 0.

      "Saldo mes anterior
      IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.

        CLEAR: cs_mep_alv-saldo_mes_ant_usd.

        IF gs_zglt0111 IS NOT INITIAL.
          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0111-vl_pat_liq * -1.
        ELSE.
*          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
*          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          cs_mep_alv-saldo_mes_ant_brl = <fs_cta_11>-valor_brl.
          cs_mep_alv-saldo_mes_ant_usd = <fs_cta_11>-valor_usd.
        ENDIF.

      ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

        CLEAR: cs_mep_alv-saldo_mes_ant_brl.

        IF gs_zglt0111 IS NOT INITIAL.
          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0111-vl_pat_liq * -1.
        ELSE.
**          cs_mep_alv-saldo_mes_ant_brl = gs_zglt0107-sld_brl_atu.
**          cs_mep_alv-saldo_mes_ant_usd = gs_zglt0107-sld_usd_atu.
          cs_mep_alv-saldo_mes_ant_brl = <fs_cta_11>-valor_brl.
          cs_mep_alv-saldo_mes_ant_usd = <fs_cta_11>-valor_usd.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD congela_linha.

    DATA: ls_zglt0107 TYPE zglt0107,
          lt_zglt0107 TYPE TABLE OF zglt0107.

    ls_zglt0107-monat = gs_header-data_base+4(2).
    ls_zglt0107-gjahr = gs_header-data_base(4).
    ls_zglt0107-investidora = gs_header-empresa_investidora.
    ls_zglt0107-investida = gs_header-empresa_investida.
    ls_zglt0107-usnam = sy-uname.
    ls_zglt0107-dt_atual = sy-datum.
    ls_zglt0107-hr_atual = sy-uzeit.
    ls_zglt0107-parametro  = is_linha-parametro.
    ls_zglt0107-item_balanco  = is_linha-conta.
    ls_zglt0107-desc_cta_equ  = is_linha-desc_cta_equ.
    ls_zglt0107-sld_brl_atu = is_linha-saldo_mes_atual_brl.
    ls_zglt0107-sld_usd_atu = is_linha-saldo_mes_atual_usd.
    ls_zglt0107-sld_brl_ant = is_linha-saldo_mes_ant_brl.
    ls_zglt0107-sld_usd_ant = is_linha-saldo_mes_ant_usd.
    ls_zglt0107-equiv_brl = is_linha-equivalencia_brl.
    ls_zglt0107-equiv_usd = is_linha-equivalencia_usd.
    ls_zglt0107-movimentacao_brl = is_linha-movimentacao_brl.
    ls_zglt0107-movimentacao_usd = is_linha-movimentacao_usd.

    MODIFY zglt0107 FROM ls_zglt0107.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD saldo_atual_reserva_lucro.

    " VALOR CTA
    READ TABLE gt_cta ASSIGNING FIELD-SYMBOL(<fs_cta>) WITH KEY
            saknr = cs_mep-conta
            tipo = '09 - Reserva de Lucro'.
    IF sy-subrc = 0.

      "Saldo mes atual
      IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.

        CLEAR: cs_mep-saldo_mes_atual_usd.

        cs_mep-saldo_mes_atual_usd = <fs_cta>-valor_usd.

      ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

        CLEAR: cs_mep-saldo_mes_atual_brl.

        cs_mep-saldo_mes_atual_brl = <fs_cta>-valor_brl.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD saldo_mes_atual_prejuizo.

    " VALOR CTA
    READ TABLE gt_cta ASSIGNING FIELD-SYMBOL(<fs_cta>) WITH KEY
            "saknr = cs_mep-conta
            tipo = '11 - Prejuízos Acumulados'.
    IF sy-subrc = 0.

      "Saldo mes atual
      IF gs_header-pais = 'BR' AND gs_header-moeda_funcional = 'BRL'.

        CLEAR: cs_mep-saldo_mes_atual_usd.

        cs_mep-saldo_mes_atual_usd = <fs_cta>-valor_usd.

      ELSEIF gs_header-pais <> 'BR' AND gs_header-moeda_funcional <> 'BRL'.

        CLEAR: cs_mep-saldo_mes_atual_brl.

        cs_mep-saldo_mes_atual_brl = <fs_cta>-valor_brl.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD ordenar.

    DATA: lo_sorts TYPE REF TO cl_salv_sorts.

    TRY.
*-- SORT
        lo_sorts = go_alv->get_sorts( ).
        lo_sorts->add_sort( 'PARAMETRO' ).

      CATCH cx_salv_not_found
              cx_salv_existing
              cx_salv_data_error.

    ENDTRY..


  ENDMETHOD.
ENDCLASS.
