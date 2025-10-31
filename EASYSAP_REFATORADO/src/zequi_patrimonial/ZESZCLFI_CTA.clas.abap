CLASS ZESZCLFI_CTA DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_e TYPE char1 VALUE 'E'.

    METHODS constructor
      IMPORTING
        !is_cta    TYPE ZESZSFI_CTA
        iv_not_alv TYPE boolean OPTIONAL .

    METHODS processa.

    METHODS get_result
      EXPORTING
        et_result_cta TYPE zfitt_result_cta.


  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_contas,
             saknr TYPE saknr,
           END OF ty_contas.

    TYPES: BEGIN OF ty_vlr_conta,
             saknr      TYPE saknr,
             desc_conta TYPE char40,
             valor_brl  TYPE hslvt12,
             valor_usd  TYPE hslvt12,
           END OF ty_vlr_conta.

    TYPES: BEGIN OF ty_alv,
             tipo       TYPE char40,
             saknr      TYPE saknr,
             desc_conta TYPE char40,
             valor_brl  TYPE hslvt12,
             valor_usd  TYPE hslvt12,
           END OF ty_alv.

    TYPES: BEGIN OF ty_alv_conv,
             tipo          TYPE char40,
             saknr         TYPE saknr,
             desc_conta    TYPE char40,
             valor_brl     TYPE hslvt12,
             valor_usd     TYPE hslvt12,
             valor_br_conv TYPE hslvt12,
           END OF ty_alv_conv.


    DATA gs_cta TYPE ZESZSFI_CTA.
    DATA gs_zglt0111 TYPE zglt0111.

    DATA gv_tx_med_medv TYPE zdefi_tx_med_medv.
    DATA gv_taxa_venda TYPE ukurs_curr.
    DATA gv_dummy TYPE string.
    DATA gv_notalv TYPE string.

    DATA gv_vlr_reserva_conv TYPE hslvt12.
    DATA gv_vlr_patr_liq_brl TYPE hslvt12.
    DATA gv_vlr_patr_liq_usd TYPE hslvt12.

    DATA gv_passivo_liq_brl TYPE hslvt12.
    DATA gv_passivo_liq_usd TYPE hslvt12.
    DATA gv_ativo_liq_brl TYPE hslvt12.
    DATA gv_ativo_liq_usd TYPE hslvt12.

    DATA gt_alv TYPE TABLE OF ty_alv.
    DATA gt_alv_br_usd TYPE TABLE OF ty_alv_conv.
    DATA gt_alv_seq TYPE TABLE OF ty_alv.
    DATA gt_msg TYPE bapiret2_tab.
    DATA gt_contas TYPE TABLE OF ty_contas.
    DATA gt_vlr_conta TYPE TABLE OF ty_vlr_conta.
    DATA gt_saldo TYPE TABLE OF zde_fi_gl_saldo_faglflext.
    DATA gt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext.

    DATA go_alv TYPE REF TO cl_salv_table.

    METHODS get_dados.
    METHODS get_taxa_media.
    METHODS get_taxa_venda.
    METHODS msg.
    METHODS processa_br_brl.
    METHODS get_contas_00001.
    METHODS get_saldo_br_brl
      IMPORTING iv_usd_internacional TYPE boolean OPTIONAL.
    METHODS append_ativo_brl.
    METHODS append_passivo_brl.
    METHODS get_contas_00002.
    METHODS patrimonio_liquido.
    METHODS get_contas_patri_liq.
    METHODS alv.
    METHODS colunas_alv.
    METHODS botao_alv.
    METHODS status_alv.
    METHODS reserva_lucro.
    METHODS get_contas_reser_luc.
    METHODS get_saldo_zglt0111
      IMPORTING iv_tipo TYPE ZESZ_TP_SALDO_EQ.
    METHODS prej_acumulado.
    METHODS get_contas_prej_acum.
    METHODS resultado_per.
    METHODS get_contas_result_per.
    METHODS get_contas_result_per_ant.
    METHODS get_saldo_br_brl_ant.
    METHODS preenche_seq_linhas.
    METHODS cta_total_usd.
    METHODS preenche_desc_contas.
    METHODS set_header.
    METHODS processa_usd.
    METHODS processa_br_usd.
    METHODS preenche_alv_br_usd.
    METHODS alv_br_usd.
    METHODS colunas_alv_br_usd.
    METHODS calculo_valor_passivo.
    METHODS append_ativo_usd.
    METHODS append_passivo_usd.
    METHODS reserva_lucro_usd.
    METHODS resultado_per_usd.
    METHODS calculo_valor_passivo_usd.
    METHODS calculo_valor_passivo_br_usd.
    METHODS deleta_linhas_brls_zero.
    METHODS calcula_patr_liq_conv.

ENDCLASS.



CLASS ZESZCLFI_CTA IMPLEMENTATION.


  METHOD constructor.

    CLEAR: gs_cta.
    gs_cta = is_cta.

    gv_notalv = iv_not_alv.

  ENDMETHOD.


  METHOD get_taxa_media.

    DATA: lv_mes TYPE zdefi_mes,
          lv_ano TYPE zdefi_ano.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    lv_mes = gs_cta-fim_validade+4(2).
    lv_ano = gs_cta-fim_validade(4).

    CLEAR:gv_tx_med_medv.

    SELECT SINGLE tx_med_medv
        FROM zfit0160
        INTO gv_tx_med_medv
        WHERE mes = lv_mes
        AND ano = lv_ano.

    IF sy-subrc <> 0.

      "Erro ao tentar buscar taxa média da média venda &1 &2
      MESSAGE e001 WITH lv_mes lv_ano INTO gv_dummy.
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


  METHOD processa_br_brl.

    append_ativo_brl( ).
    append_passivo_brl( ).
    patrimonio_liquido( ).
    reserva_lucro(  ).
    prej_acumulado(  ).
    resultado_per( ).
    calculo_valor_passivo( ).
    cta_total_usd( ).
    preenche_seq_linhas( ).
    preenche_desc_contas( ).

    alv( ).


  ENDMETHOD.


  METHOD get_contas_00001.

    CLEAR: gt_contas.
    SELECT *
    FROM zi_eq_pat_contas_00001
    INTO TABLE @gt_contas.

    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'Ativo' INTO gv_dummy .
      msg( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_br_brl.
    DATA: lt_saldo3 TYPE TABLE OF zde_fi_gl_saldo_faglflext.
    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano TYPE gjahr.

    LOOP AT gt_contas ASSIGNING FIELD-SYMBOL(<fs_contas>).

      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
      <fs_conta>-saknr = <fs_contas>-saknr.
      <fs_conta>-bukrs = gs_cta-empresa_investida.

    ENDLOOP.

    lv_ano = gs_cta-inicio_validade(4).

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
        it_saldos_3   = lt_saldo3
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

    IF iv_usd_internacional = abap_true.
      IF gs_cta-moeda_funcional <> 'BRL' AND
          gs_cta-pais <> 'BR'
         AND gs_cta-empresa_investida <> '0004'.
        gt_saldo = lt_saldo3.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_taxa_venda.

    DATA: lv_data      TYPE datum,
          lv_data_wrt  TYPE char10,
          lv_data_conv TYPE datum,
          lv_mes       TYPE t5a4a-dlymo,
          lv_ano       TYPE t5a4a-dlyyr,
          lv_dia       TYPE t5a4a-dlydy.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    lv_mes = '1'.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = gs_cta-inicio_validade
        days      = lv_dia
        months    = lv_mes
        years     = lv_ano
      IMPORTING
        calc_date = lv_data.

    lv_data+6(2) = '01'.

    WRITE lv_data TO lv_data_wrt.

    " Função standard para converter data em formato gravado na TCURR
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_data_wrt
      IMPORTING
        output = lv_data_conv.

    CLEAR: gv_taxa_venda.
    SELECT SINGLE ukurs
        FROM tcurr
        INTO gv_taxa_venda
        WHERE kurst = 'B'
           AND fcurr = 'BRL'
           AND tcurr = 'USD'
           AND gdatu = lv_data_conv.

    IF sy-subrc = 0.
      gv_taxa_venda = abs( gv_taxa_venda ).
    ENDIF.

  ENDMETHOD.


  METHOD append_ativo_brl.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_00001( ).
    get_saldo_br_brl( ).

    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_brl = lv_valor_brl + <fs_saldo>-slvt.

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

        lv_valor_brl = lv_valor_brl + lv_valor_sl.
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    IF gs_cta-pais = 'BR' AND
       gs_cta-moeda_funcional = 'USD'.

      CLEAR: lv_valor_usd.
      LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

        lv_valor_usd = lv_valor_usd + <fs_saldo2>-slvt.

        lv_index = 1.
        DO .

          IF lv_index > lv_meses.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
          IF <fs_any2> IS ASSIGNED.
            lv_valor_sl = <fs_any2>.
          ENDIF.

          lv_valor_usd = lv_valor_usd + lv_valor_sl.
          UNASSIGN <fs_any2>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ELSE.

      lv_valor_usd = lv_valor_brl / gv_taxa_venda.

    ENDIF.

    APPEND VALUE #(
             tipo = 'Ativo'
             valor_brl = lv_valor_brl
             valor_usd = lv_valor_usd ) TO gt_alv.

    CLEAR: gv_ativo_liq_brl,
           gv_ativo_liq_usd.

    gv_ativo_liq_brl = lv_valor_brl.
    gv_ativo_liq_usd = lv_valor_usd.

  ENDMETHOD.


  METHOD append_ativo_usd.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_00001( ).
    get_saldo_br_brl( ).

    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      lv_valor_usd = lv_valor_usd + <fs_saldo2>-slvt.

      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_usd = lv_valor_usd + lv_valor_sl.
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    lv_valor_brl = lv_valor_usd * gv_taxa_venda.

    APPEND VALUE #(
             tipo = 'Ativo'
             valor_brl = lv_valor_brl
             valor_usd = lv_valor_usd ) TO gt_alv.

    CLEAR: gv_ativo_liq_brl,
           gv_ativo_liq_usd.

    gv_ativo_liq_brl = lv_valor_brl.
    gv_ativo_liq_usd = lv_valor_usd.

  ENDMETHOD.


  METHOD processa.

    get_dados( ).

    IF gs_cta-moeda_funcional = 'BRL' AND
       gs_cta-pais = 'BR'.

      processa_br_brl( ).

    ELSEIF gs_cta-moeda_funcional = 'USD' AND
       gs_cta-pais = 'BR'.

      processa_br_usd( ).

    ELSEIF gs_cta-moeda_funcional = 'USD' AND
       gs_cta-pais <> 'BR'.

      processa_usd( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_dados.

    get_taxa_media( ).
    get_taxa_venda( ).

  ENDMETHOD.


  METHOD append_passivo_brl.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_00002( ).
    get_saldo_br_brl( ).


    lv_meses = gs_cta-inicio_validade+4(2).

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

        lv_valor_brl = lv_valor_brl + ( lv_valor_sl * -1 ).
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.


    CLEAR: gv_passivo_liq_brl, gv_passivo_liq_usd.

    gv_passivo_liq_brl = lv_valor_brl .
    gv_passivo_liq_usd = lv_valor_brl / gv_taxa_venda.

*    APPEND VALUE #(
*             tipo = 'Passivo'
*             valor_brl = lv_valor_brl
*             valor_usd = lv_valor_usd ) TO gt_alv.


  ENDMETHOD.


  METHOD append_passivo_usd.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_00002( ).
    get_saldo_br_brl( ).


    lv_meses = gs_cta-inicio_validade+4(2).


    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      lv_valor_usd = lv_valor_usd + ( <fs_saldo2>-slvt * -1 ).

      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_usd = lv_valor_usd + ( lv_valor_sl * -1 ).
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    CLEAR: gv_passivo_liq_brl, gv_passivo_liq_usd.

    gv_passivo_liq_brl = lv_valor_usd * gv_taxa_venda .
    gv_passivo_liq_usd = lv_valor_usd.


  ENDMETHOD.


  METHOD get_contas_00002.

    CLEAR: gt_contas.
    SELECT *
    FROM zi_eq_pat_contas_00002
    INTO TABLE @gt_contas.

    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'Passivo' INTO gv_dummy .
      msg( ).

    ENDIF.


  ENDMETHOD.


  METHOD patrimonio_liquido.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    DATA: ls_vlr_conta TYPE ty_vlr_conta,
          ls_alv       TYPE ty_alv.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_patri_liq( ).
    get_saldo_br_brl( iv_usd_internacional = abap_true ).

    "CONTA AQUI

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VALOR BRL
    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      CLEAR: ls_vlr_conta.

      "Somar brl conta - SLVT
      ls_vlr_conta-saknr = <fs_saldo>-racct.
      ls_vlr_conta-valor_brl = <fs_saldo>-slvt * -1.
      COLLECT ls_vlr_conta INTO gt_vlr_conta.

      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * -1 ) .

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

        lv_valor_sl = ( lv_valor_sl * -1 ) .

        CLEAR: ls_vlr_conta.
        "Somar conta - SL
        ls_vlr_conta-saknr = <fs_saldo>-racct.
        ls_vlr_conta-valor_brl = lv_valor_sl .
        COLLECT ls_vlr_conta INTO gt_vlr_conta.

        lv_valor_brl = lv_valor_brl + lv_valor_sl .
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VALOR USD
    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      CLEAR: ls_vlr_conta.

      "Somar usd conta - SLVT
      ls_vlr_conta-saknr = <fs_saldo2>-racct.
      ls_vlr_conta-valor_usd = <fs_saldo2>-slvt * -1 .
      COLLECT ls_vlr_conta INTO gt_vlr_conta.

      lv_valor_usd = lv_valor_usd + ( <fs_saldo2>-slvt * -1 ) .

      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.

        CLEAR: ls_vlr_conta.
        "Somar conta - SL
        ls_vlr_conta-saknr = <fs_saldo2>-racct.
        ls_vlr_conta-valor_usd = lv_valor_sl .
        COLLECT ls_vlr_conta INTO gt_vlr_conta.

        lv_valor_usd = lv_valor_usd + lv_valor_sl .
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

    DELETE gt_vlr_conta WHERE valor_brl IS INITIAL.
    DELETE gt_vlr_conta WHERE valor_usd IS INITIAL.

    CLEAR: lv_valor_brl, lv_valor_usd.
    LOOP AT gt_vlr_conta ASSIGNING FIELD-SYMBOL(<fs_vlr>).
      lv_valor_brl = lv_valor_brl + <fs_vlr>-valor_brl.
      lv_valor_usd = lv_valor_usd + <fs_vlr>-valor_usd.
    ENDLOOP.

    gv_vlr_patr_liq_brl = lv_valor_brl.
    gv_vlr_patr_liq_usd = lv_valor_usd.


    DELETE gt_vlr_conta WHERE valor_brl IS INITIAL AND
                              valor_usd IS INITIAL.


  ENDMETHOD.


  METHOD get_contas_patri_liq.

    DATA: lr_tp TYPE RANGE OF ZESZ_TP_LAN_EQ.


    lr_tp = VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                     ( sign = 'I' option = 'EQ' low = '11' )
                     ( sign = 'I' option = 'EQ' low = '15' ) ).

    SELECT *
        FROM zglt0112
        INTO TABLE @DATA(lt_contas)
         WHERE tipo_reflexa_pl NOT IN @lr_tp.
    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'zglt0112' INTO gv_dummy .
      msg( ).

    ELSE.

      CLEAR: gt_contas.
      gt_contas = VALUE #( FOR ls_conta IN lt_contas
                           ( saknr = ls_conta-hkont ) ).

    ENDIF.

  ENDMETHOD.


  METHOD alv.

    CHECK gv_notalv IS INITIAL.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = gt_alv.

        set_header( ).
        status_alv( ).
        colunas_alv( ).
        botao_alv( ).

      CATCH cx_root INTO DATA(lo_error).

    ENDTRY.

    go_alv->display( ).

  ENDMETHOD.


  METHOD botao_alv.

  ENDMETHOD.


  METHOD colunas_alv.

    DATA: lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column  TYPE REF TO cl_salv_column.

    lr_columns = go_alv->get_columns( ).
    lr_columns->set_optimize( ).

    TRY.
        lr_column = lr_columns->get_column( 'TIPO' ).
        lr_column->set_long_text( 'Tipo' ).
        lr_column->set_medium_text( 'Tipo' ).
        lr_column->set_short_text( 'Tipo' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SAKNR' ).
        lr_column->set_long_text( 'Conta' ).
        lr_column->set_medium_text( 'Conta' ).
        lr_column->set_short_text( 'Conta' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DESC_CONTA' ).
        lr_column->set_long_text( 'Descrição' ).
        lr_column->set_medium_text( 'Descrição' ).
        lr_column->set_short_text( 'Descrição' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


    TRY.
        lr_column = lr_columns->get_column( 'VALOR_BRL' ).
        lr_column->set_long_text( 'BRL' ).
        lr_column->set_medium_text( 'BRL' ).
        lr_column->set_short_text( 'BRL' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'VALOR_USD' ).
        lr_column->set_long_text( 'USD' ).
        lr_column->set_medium_text( 'USD' ).
        lr_column->set_short_text( 'USD' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


  ENDMETHOD.


  METHOD colunas_alv_br_usd.

    DATA: lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column  TYPE REF TO cl_salv_column.

    lr_columns = go_alv->get_columns( ).
    lr_columns->set_optimize( ).

    TRY.
        lr_column = lr_columns->get_column( 'TIPO' ).
        lr_column->set_long_text( 'Tipo' ).
        lr_column->set_medium_text( 'Tipo' ).
        lr_column->set_short_text( 'Tipo' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'SAKNR' ).
        lr_column->set_long_text( 'Conta' ).
        lr_column->set_medium_text( 'Conta' ).
        lr_column->set_short_text( 'Conta' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'DESC_CONTA' ).
        lr_column->set_long_text( 'Descrição' ).
        lr_column->set_medium_text( 'Descrição' ).
        lr_column->set_short_text( 'Descrição' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


    TRY.
        lr_column = lr_columns->get_column( 'VALOR_BRL' ).
        lr_column->set_long_text( 'BRL' ).
        lr_column->set_medium_text( 'BRL' ).
        lr_column->set_short_text( 'BRL' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'VALOR_USD' ).
        lr_column->set_long_text( 'USD' ).
        lr_column->set_medium_text( 'USD' ).
        lr_column->set_short_text( 'USD' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.

    TRY.
        lr_column = lr_columns->get_column( 'VALOR_BR_CONV' ).
        lr_column->set_long_text( 'BRL Convertido' ).
        lr_column->set_medium_text( 'BRL Convertido' ).
        lr_column->set_short_text( 'BRL Conv.' ).

      CATCH cx_salv_not_found.
        "handle exception
    ENDTRY.


  ENDMETHOD.


  METHOD status_alv.

    go_alv->set_screen_status(
      pfstatus      = 'STANDARD'
      report        = 'SAPLSALV'
      set_functions = go_alv->c_functions_all ).

    go_alv->get_functions( )->set_all( ).

  ENDMETHOD.


  METHOD reserva_lucro.

    DATA: lt_vlr_conta TYPE TABLE OF ty_vlr_conta.
    DATA: lv_valor_brl   TYPE hslvt12,
          lv_valor_usd   TYPE hslvt12,
          lv_valor_saldo TYPE hslvt12,
          lv_valor_sl    TYPE hslvt12,
          ls_vlr_conta   TYPE ty_vlr_conta,
          lv_campo       TYPE string,
          lv_conta       TYPE hkont,
          lv_campo_cont  TYPE n LENGTH 2,
          lv_data_ini    TYPE datum,
          lv_meses       TYPE vtbbewe-atage,
          lv_index       TYPE vtbbewe-atage.

    DATA: ls_vlr_alv TYPE ty_alv.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_reser_luc( ).
    get_saldo_br_brl( ).
    get_saldo_zglt0111( '09' ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VALOR BRL
    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * -1 ) .

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

        lv_valor_brl = lv_valor_brl + lv_valor_sl .
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

      CLEAR: ls_vlr_conta.
      "Somar conta - SL
      ls_vlr_conta-saknr = <fs_saldo>-racct.
      ls_vlr_conta-valor_brl = lv_valor_brl .
      COLLECT ls_vlr_conta INTO lt_vlr_conta.

      CLEAR: lv_valor_brl.

    ENDLOOP.


    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

      lv_valor_usd = lv_valor_usd + ( <fs_saldo2>-slvt * -1 ) .

      lv_index = 1.
      DO .

        IF lv_index > lv_meses.
          EXIT.
        ENDIF.

        lv_campo_cont = lv_index.

        lv_campo = 'SL' && lv_campo_cont.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
        IF <fs_any2> IS ASSIGNED.
          lv_valor_sl = <fs_any2>.
        ENDIF.

        lv_valor_sl = lv_valor_sl * -1.

        lv_valor_usd = lv_valor_usd + lv_valor_sl .
        UNASSIGN <fs_any2>.

        lv_index = lv_index + 1.

      ENDDO.

      CLEAR: ls_vlr_conta.
      "Somar conta - SL
      ls_vlr_conta-saknr = <fs_saldo2>-racct.
      ls_vlr_conta-valor_usd = lv_valor_usd .
      COLLECT ls_vlr_conta INTO lt_vlr_conta.
      CLEAR: lv_valor_usd.

    ENDLOOP.


    DELETE lt_vlr_conta WHERE valor_brl = 0
                          AND valor_usd = 0.

    LOOP AT lt_vlr_conta ASSIGNING FIELD-SYMBOL(<fs_vlr_conta>).

      CLEAR: ls_vlr_alv.
      "Somar brl conta - SLVT
      ls_vlr_alv-tipo = '09 - Reserva de Lucro'.
      ls_vlr_alv-saknr = <fs_vlr_conta>-saknr.
      ls_vlr_alv-valor_brl = <fs_vlr_conta>-valor_brl.
      ls_vlr_alv-valor_usd = <fs_vlr_conta>-valor_usd.

      IF <fs_vlr_conta>-saknr = '0000243001'.

        IF gs_cta-moeda_funcional = 'USD' AND
        gs_cta-pais = 'BR'.
          gv_vlr_reserva_conv = ls_vlr_alv-valor_brl = gs_zglt0111-vl_pat_liq .
        ELSE.
          gv_vlr_reserva_conv = ls_vlr_alv-valor_usd = gs_zglt0111-vl_pat_liq * -1 .
        ENDIF.

      ENDIF.

      COLLECT ls_vlr_alv INTO gt_alv_seq.

      gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + ls_vlr_alv-valor_brl.
      gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + ls_vlr_alv-valor_usd.

    ENDLOOP.

  ENDMETHOD.


  METHOD reserva_lucro_usd.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    DATA: ls_vlr_conta TYPE ty_alv.
    DATA: lt_vlr_conta TYPE TABLE OF ty_alv.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_reser_luc( ).
    get_saldo_br_brl( ).
    get_saldo_zglt0111( '09' ).

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VALOR BRL
    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_usd = lv_valor_usd + ( <fs_saldo>-slvt * -1 ) .

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

        lv_valor_usd = lv_valor_usd + lv_valor_sl .
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

      CLEAR: ls_vlr_conta.
      "Somar conta - SL
      ls_vlr_conta-tipo = '09 - Reserva de Lucro'.
      ls_vlr_conta-saknr = <fs_saldo>-racct.
      ls_vlr_conta-valor_usd = lv_valor_usd .
      COLLECT ls_vlr_conta INTO lt_vlr_conta.
      CLEAR: lv_valor_usd.

    ENDLOOP.


    LOOP AT lt_vlr_conta ASSIGNING FIELD-SYMBOL(<fs_alv>).

      IF <fs_alv>-saknr = '0000243001'.
        <fs_alv>-valor_brl = gs_zglt0111-vl_pat_liq * -1 .
      ENDIF.

      gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + <fs_alv>-valor_brl.
      gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + <fs_alv>-valor_usd.

    ENDLOOP.

    APPEND LINES OF lt_vlr_conta TO gt_alv_seq.

**    CLEAR: ls_vlr_conta.
**    "Somar brl conta - SLVT
**    ls_vlr_conta-tipo = '09 - Reserva de Lucro'.
**    ls_vlr_conta-saknr = <fs_saldo>-racct.
**    ls_vlr_conta-valor_brl = lv_valor_brl.
**    ls_vlr_conta-valor_usd = lv_valor_usd.
**    COLLECT ls_vlr_conta INTO gt_alv_seq.

**    gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + lv_valor_brl.
**    gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + lv_valor_usd.


  ENDMETHOD.


  METHOD get_contas_reser_luc.

    DATA: lr_tp TYPE RANGE OF ZESZ_TP_LAN_EQ.

    lr_tp = VALUE #( ( sign = 'I' option = 'EQ' low = '09' ) ).

    SELECT *
        FROM zglt0112
        INTO TABLE @DATA(lt_contas)
         WHERE tipo_reflexa_pl IN @lr_tp.
    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'zglt0112' INTO gv_dummy .
      msg( ).

    ELSE.

      CLEAR: gt_contas.
      gt_contas = VALUE #( FOR ls_conta IN lt_contas
                           ( saknr = ls_conta-hkont ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_zglt0111.

    DATA: lv_per_ini TYPE datum,
          lv_per_fim TYPE datum,
          lv_gjahr   TYPE bseg-gjahr.

    lv_gjahr = gs_cta-inicio_validade(4) - 1.

    lv_per_ini = lv_gjahr && '1231'.
    lv_per_fim = lv_gjahr && '1231'.

    CLEAR: gs_zglt0111.
    SELECT SINGLE *
    FROM zglt0111
    INTO gs_zglt0111
    WHERE per_inicial = lv_per_ini
      AND per_final = lv_per_fim
      AND tipo_saldo = iv_tipo
      AND investidora = gs_cta-empresa_investidora
      AND investida = gs_cta-empresa_investida.

  ENDMETHOD.


  METHOD prej_acumulado.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_conta      TYPE hkont,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    DATA: ls_vlr_conta TYPE ty_alv.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_prej_acum( ).
    get_saldo_br_brl( ).
    get_saldo_zglt0111( '11' ).


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " VALOR BRL
    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * -1 ) .

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
        lv_valor_brl = lv_valor_brl + lv_valor_sl. "// wbarbosa 11/12/2024 BUG-160730
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

      lv_conta = <fs_saldo>-racct.

    ENDLOOP.


    lv_valor_usd = gs_zglt0111-vl_pat_liq * -1.

    CLEAR: ls_vlr_conta.
    "Somar brl conta - SLVT
    ls_vlr_conta-tipo = '11 - Prejuízos Acumulados'.
    ls_vlr_conta-saknr = lv_conta.
    ls_vlr_conta-valor_brl = lv_valor_brl.
    ls_vlr_conta-valor_usd = lv_valor_usd.
    COLLECT ls_vlr_conta INTO gt_alv_seq.


    gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + lv_valor_brl.
    gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + lv_valor_usd.


  ENDMETHOD.


  METHOD get_contas_prej_acum.

    DATA: lr_tp TYPE RANGE OF ZESZ_TP_LAN_EQ.

    lr_tp = VALUE #( ( sign = 'I' option = 'EQ' low = '11' ) ).

    SELECT *
        FROM zglt0112
        INTO TABLE @DATA(lt_contas)
         WHERE tipo_reflexa_pl IN @lr_tp.
    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'zglt0112' INTO gv_dummy .
      msg( ).

    ELSE.

      CLEAR: gt_contas.
      gt_contas = VALUE #( FOR ls_conta IN lt_contas
                           ( saknr = ls_conta-hkont ) ).

    ENDIF.

  ENDMETHOD.


  METHOD resultado_per.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_result_per( ).
    get_saldo_br_brl( ).


    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_brl.
    LOOP AT gt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * 1 ).

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

    IF gs_cta-pais = 'BR' AND
       gs_cta-moeda_funcional = 'USD'.

      CLEAR: lv_valor_usd.
      LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo2>).

        lv_valor_usd = lv_valor_usd + ( <fs_saldo2>-slvt * 1 ).

        lv_index = 1.
        DO .

          IF lv_index > lv_meses.
            EXIT.
          ENDIF.

          lv_campo_cont = lv_index.

          lv_campo = 'SL' && lv_campo_cont.
          ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo2> TO FIELD-SYMBOL(<fs_any2>).
          IF <fs_any2> IS ASSIGNED.
            lv_valor_sl = <fs_any2>.
          ENDIF.

          lv_valor_sl = lv_valor_sl * -1.

          lv_valor_usd = lv_valor_usd + lv_valor_sl.
          UNASSIGN <fs_any2>.

          lv_index = lv_index + 1.

        ENDDO.

      ENDLOOP.

    ELSE.

      lv_valor_usd = lv_valor_brl / gv_tx_med_medv.

    ENDIF.

***    "Busca contas - Calcular resultado periodo anterior
***    get_contas_result_per_ant( ).
***    get_saldo_br_brl_ant( ).
***
***    LOOP AT gt_saldo ASSIGNING <fs_saldo>.
***
***      lv_valor_brl = lv_valor_brl + ( <fs_saldo>-slvt * -1 ).
***
***      lv_index = 1.
***      DO .
***
***        IF lv_index > lv_meses.
***          EXIT.
***        ENDIF.
***
***        lv_campo_cont = lv_index.
***
***        lv_campo = 'SL' && lv_campo_cont.
***        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO <fs_any>.
***        IF <fs_any> IS ASSIGNED.
***          lv_valor_sl = <fs_any>.
***        ENDIF.
***
***        lv_valor_sl = lv_valor_sl * -1.
***
***        lv_valor_brl = lv_valor_brl + lv_valor_sl.
***        UNASSIGN <fs_any>.
***
***        lv_index = lv_index + 1.
***
***      ENDDO.
***
***    ENDLOOP.


    APPEND VALUE #(
             tipo = '15 - Resultado Período'
             valor_brl = lv_valor_brl
             valor_usd = lv_valor_usd ) TO gt_alv_seq.

    gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + lv_valor_brl.
    gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + lv_valor_usd.


  ENDMETHOD.


  METHOD resultado_per_usd.

    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    get_contas_result_per( ).
    get_saldo_br_brl( ).


    lv_meses = gs_cta-inicio_validade+4(2).

    CLEAR: lv_valor_usd.
    LOOP AT gt_saldo2 ASSIGNING FIELD-SYMBOL(<fs_saldo>).

      lv_valor_usd = lv_valor_usd + ( <fs_saldo>-slvt * 1 ).

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

        lv_valor_usd = lv_valor_usd + lv_valor_sl.
        UNASSIGN <fs_any>.

        lv_index = lv_index + 1.

      ENDDO.

    ENDLOOP.

***    "Busca contas - Calcular resultado periodo anterior
***    get_contas_result_per_ant( ).
***    get_saldo_br_brl_ant( ).
***
***    LOOP AT gt_saldo2 ASSIGNING <fs_saldo>.
***
***      lv_valor_usd = lv_valor_usd + ( <fs_saldo>-slvt * -1 ).
***
***      lv_index = 1.
***      DO .
***
***        IF lv_index > lv_meses.
***          EXIT.
***        ENDIF.
***
***        lv_campo_cont = lv_index.
***
***        lv_campo = 'SL' && lv_campo_cont.
***        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_saldo> TO <fs_any>.
***        IF <fs_any> IS ASSIGNED.
***          lv_valor_sl = <fs_any>.
***        ENDIF.
***
***        lv_valor_sl = lv_valor_sl * -1.
***
***        lv_valor_usd = lv_valor_usd + lv_valor_sl.
***        UNASSIGN <fs_any>.
***
***        lv_index = lv_index + 1.
***
***      ENDDO.
***
***    ENDLOOP.


    lv_valor_brl = lv_valor_usd * gv_tx_med_medv.

    APPEND VALUE #(
             tipo = '15 - Resultado Período'
             valor_brl = lv_valor_brl
             valor_usd = lv_valor_usd ) TO gt_alv_seq.

    gv_vlr_patr_liq_brl = gv_vlr_patr_liq_brl + lv_valor_brl.
    gv_vlr_patr_liq_usd = gv_vlr_patr_liq_usd + lv_valor_usd.


  ENDMETHOD.


  METHOD get_contas_result_per.

    CLEAR: gt_contas.
    SELECT *
    FROM zi_eq_pat_contas_result
    INTO TABLE @gt_contas.

    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'Resultado Período' INTO gv_dummy .
      msg( ).

    ENDIF.

  ENDMETHOD.


  METHOD get_contas_result_per_ant.

    DATA: lr_tp TYPE RANGE OF ZESZ_TP_LAN_EQ.

    lr_tp = VALUE #( ( sign = 'I' option = 'EQ' low = '15' ) ).

    SELECT *
        FROM zglt0112
        INTO TABLE @DATA(lt_contas)
         WHERE tipo_reflexa_pl IN @lr_tp.
    IF sy-subrc <> 0.

      "Erro ao buscar contas para CTA.
      MESSAGE e002 WITH 'zglt0112' INTO gv_dummy .
      msg( ).

    ELSE.

      CLEAR: gt_contas.
      gt_contas = VALUE #( FOR ls_conta IN lt_contas
                           ( saknr = ls_conta-hkont ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_saldo_br_brl_ant.

    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano TYPE gjahr.

    LOOP AT gt_contas ASSIGNING FIELD-SYMBOL(<fs_contas>).

      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
      <fs_conta>-saknr = <fs_contas>-saknr.
      <fs_conta>-bukrs = gs_cta-empresa_investida.

    ENDLOOP.

    lv_ano = gs_cta-inicio_validade(4).
    lv_ano = lv_ano - 1.

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


  ENDMETHOD.


  METHOD preenche_seq_linhas.

    DATA: ls_alv TYPE ty_alv.

*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " Passivo
*    gv_passivo_liq_brl = gv_passivo_liq_brl - abs( gv_vlr_patr_liq_brl ).
*    gv_passivo_liq_usd = gv_passivo_liq_brl / gv_taxa_venda .
*
*    APPEND VALUE #(
*         tipo = 'Passivo'
*         valor_brl = gv_passivo_liq_brl
*         valor_usd = gv_passivo_liq_usd ) TO gt_alv.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Patrimonio liquido
    ls_alv-tipo = 'Patrimônio Liquido'.
    ls_alv-saknr = 'Conta'.
    ls_alv-desc_conta = 'Descrição'.
    ls_alv-valor_brl = gv_vlr_patr_liq_brl.
    ls_alv-valor_usd = gv_vlr_patr_liq_usd.

    APPEND ls_alv TO gt_alv.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Contas - Patrimonio liquido
    LOOP AT gt_vlr_conta ASSIGNING FIELD-SYMBOL(<fs_vlr>).
      CLEAR: ls_alv.
      ls_alv-saknr = <fs_vlr>-saknr.
      ls_alv-valor_brl = <fs_vlr>-valor_brl.
      ls_alv-valor_usd = <fs_vlr>-valor_usd.
      APPEND ls_alv TO gt_alv.
    ENDLOOP.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "  09 - Reserva de lucros /  11 - Prejuízos acumulados /  15 - Resultado do período
    LOOP AT gt_alv_seq ASSIGNING FIELD-SYMBOL(<fs_alv_seq>).
      MOVE-CORRESPONDING <fs_alv_seq> TO ls_alv.
      APPEND ls_alv TO gt_alv.
    ENDLOOP.

**    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_vlr_abs>) WHERE tipo <> 'CTA'.
**      <fs_vlr_abs>-valor_brl = abs( <fs_vlr_abs>-valor_brl ).
**      <fs_vlr_abs>-valor_usd = abs( <fs_vlr_abs>-valor_usd ).
**    ENDLOOP.

  ENDMETHOD.


  METHOD cta_total_usd.

    DATA: lv_vlr_usd TYPE hslvt12,
          lv_vlr_brl TYPE hslvt12.

    IF gs_cta-pais = 'BR' AND
       gs_cta-moeda_funcional = 'BRL'.

      CLEAR:lv_vlr_usd.
      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        IF lv_vlr_usd IS INITIAL.
          lv_vlr_usd =  <fs_alv>-valor_usd .
        ELSE.
          lv_vlr_usd = lv_vlr_usd -  <fs_alv>-valor_usd .
        ENDIF.

      ENDLOOP.

      LOOP AT gt_vlr_conta ASSIGNING FIELD-SYMBOL(<fs_vlr_conta>).
        lv_vlr_usd = lv_vlr_usd -  <fs_vlr_conta>-valor_usd     .
      ENDLOOP.

      LOOP AT gt_alv_seq ASSIGNING FIELD-SYMBOL(<fs_alv_seq>).
        lv_vlr_usd = lv_vlr_usd -  <fs_alv_seq>-valor_usd  .
      ENDLOOP.

      APPEND VALUE #(
           tipo = 'CTA'
           valor_usd = lv_vlr_usd ) TO gt_alv_seq.


      gv_vlr_patr_liq_usd =  gv_vlr_patr_liq_usd + lv_vlr_usd.

    ELSEIF gs_cta-pais <> 'BR' AND
       gs_cta-moeda_funcional = 'USD'.

      CLEAR:lv_vlr_brl.
      LOOP AT gt_alv ASSIGNING <fs_alv>.
        IF lv_vlr_brl IS INITIAL.
          lv_vlr_brl = <fs_alv>-valor_brl.
        ELSE.
          lv_vlr_brl = lv_vlr_brl - <fs_alv>-valor_brl.
        ENDIF.
      ENDLOOP.

      LOOP AT gt_vlr_conta ASSIGNING <fs_vlr_conta>.
        lv_vlr_brl = lv_vlr_brl - <fs_vlr_conta>-valor_brl.
      ENDLOOP.

      LOOP AT gt_alv_seq ASSIGNING <fs_alv_seq>.
        lv_vlr_brl = lv_vlr_brl - <fs_alv_seq>-valor_brl.
      ENDLOOP.

      APPEND VALUE #(
           tipo = 'CTA'
           valor_brl = lv_vlr_brl ) TO gt_alv_seq.

      gv_vlr_patr_liq_brl =  gv_vlr_patr_liq_brl  + lv_vlr_brl.

    ELSEIF gs_cta-pais = 'BR' AND
      gs_cta-moeda_funcional = 'USD'.

      CLEAR:lv_vlr_usd.
      LOOP AT gt_alv_br_usd ASSIGNING FIELD-SYMBOL(<fs_alv_br_usd>).
        IF <fs_alv_br_usd>-tipo = 'Patrimônio Liquido'.
          CONTINUE.
        ENDIF.
        IF lv_vlr_usd IS INITIAL.
          lv_vlr_usd =  <fs_alv_br_usd>-valor_br_conv .
        ELSE.
          lv_vlr_usd = lv_vlr_usd - <fs_alv_br_usd>-valor_br_conv .
        ENDIF.
      ENDLOOP.

      APPEND VALUE #(
           tipo = 'CTA'
           valor_br_conv = lv_vlr_usd ) TO gt_alv_br_usd.

      gv_vlr_patr_liq_usd =  gv_vlr_patr_liq_usd + lv_vlr_usd.

    ENDIF.

  ENDMETHOD.


  METHOD preenche_desc_contas.


    SELECT saknr, txt20
    FROM skat
    INTO TABLE @DATA(lt_desc)
    FOR ALL ENTRIES IN @gt_alv
    WHERE spras = @sy-langu
      AND ktopl = '0050'
      AND saknr = @gt_alv-saknr.

    IF sy-subrc = 0.

      LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        <fs_alv>-desc_conta = VALUE #( lt_desc[ saknr = <fs_alv>-saknr ]-txt20 OPTIONAL  ).

      ENDLOOP.

    ENDIF.
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
    lo_h_label->set_text( 'Conversão do balanço' ).

*   Writing Header texts
    lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
    lo_h_flow->create_text( text = 'Data Base:' ).
    lo_h_flow = lo_header->create_flow( row = 2  column = 2 ).
    lo_h_flow->create_text( text = gs_cta-fim_validade ).

    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
    lo_h_flow->create_text( text = 'Empresa Investidora:' ).
    SELECT SINGLE companycodename
        FROM i_companycodevh
        INTO @DATA(lv_desc_emp)
        WHERE companycode = @gs_cta-empresa_investidora.

    CONCATENATE gs_cta-empresa_investidora '-' lv_desc_emp INTO lv_descr SEPARATED BY space.
    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
    lo_h_flow->create_text( text = lv_descr ).

    lo_h_flow = lo_header->create_flow( row = 4  column = 1 ).
    lo_h_flow->create_text( text = 'Empresa Investida:' ).
    SELECT SINGLE companycodename
        FROM i_companycodevh
        INTO @DATA(lv_desc_emp2)
        WHERE companycode = @gs_cta-empresa_investida.

    CONCATENATE gs_cta-empresa_investida '-' lv_desc_emp2 INTO lv_descr SEPARATED BY space.
    lo_h_flow = lo_header->create_flow( row = 4  column = 2 ).
    lo_h_flow->create_text( text = lv_descr ).

    lo_h_flow = lo_header->create_flow( row = 5  column = 1 ).
    lo_h_flow->create_text( text = 'Taxa de venda do dólar:' ).
    lo_h_flow = lo_header->create_flow( row = 5  column = 2 ).
    lo_h_flow->create_text( text = gv_taxa_venda ).

    lo_h_flow = lo_header->create_flow( row = 6  column = 1 ).
    lo_h_flow->create_text( text = 'Taxa média/média:' ).
    lo_h_flow = lo_header->create_flow( row = 6  column = 2 ).
    lo_h_flow->create_text( text = gv_tx_med_medv ).


    lo_h_flow = lo_header->create_flow( row = 7  column = 1 ).
    lo_h_flow->create_text( text = 'Moeda Funcional:' ).
    lo_h_flow = lo_header->create_flow( row = 7  column = 2 ).
    lo_h_flow->create_text( text = gs_cta-moeda_funcional ).

    lo_h_flow = lo_header->create_flow( row = 8  column = 1 ).
    lo_h_flow->create_text( text = 'País:' ).
    lo_h_flow = lo_header->create_flow( row = 8  column = 2 ).
    lo_h_flow->create_text( text = gs_cta-pais ).

*   Set the top of list
    go_alv->set_top_of_list( lo_header ).

*   Print on top of list
    go_alv->set_top_of_list_print( lo_header ).

  ENDMETHOD.


  METHOD processa_usd.

    append_ativo_usd( ).
    append_passivo_usd( ).
    patrimonio_liquido( ).
    reserva_lucro_usd(  ).
    prej_acumulado(  ).
    resultado_per_usd( ).
    calculo_valor_passivo_usd( ).
    cta_total_usd( ).
    preenche_seq_linhas( ).
    preenche_desc_contas( ).

    alv( ).



  ENDMETHOD.


  METHOD processa_br_usd.

    append_ativo_brl( ).
    append_passivo_brl( ).
    patrimonio_liquido( ).
    reserva_lucro(  ).
    prej_acumulado(  ).
    resultado_per( ).
    calculo_valor_passivo_br_usd( ).
    preenche_seq_linhas( ).
    preenche_desc_contas( ).
    preenche_alv_br_usd( ).
    cta_total_usd( ).
    calcula_patr_liq_conv(  ).


    alv_br_usd( ).



  ENDMETHOD.


  METHOD preenche_alv_br_usd.

    LOOP AT gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
      APPEND INITIAL LINE TO gt_alv_br_usd ASSIGNING FIELD-SYMBOL(<fs_alv_conv>).
      MOVE-CORRESPONDING <fs_alv> TO <fs_alv_conv>.


      IF <fs_alv>-tipo = 'Ativo'.
        <fs_alv_conv>-valor_br_conv = <fs_alv>-valor_usd * gv_taxa_venda.
      ELSE.
        <fs_alv_conv>-valor_br_conv = <fs_alv>-valor_usd * gv_tx_med_medv.
      ENDIF.

      IF <fs_alv>-tipo IS INITIAL
       AND  <fs_alv>-saknr IS NOT INITIAL.
        <fs_alv_conv>-valor_br_conv = <fs_alv_conv>-valor_brl.
      ENDIF.

      IF <fs_alv>-tipo = '09 - Reserva de Lucro'.
        IF <fs_alv>-saknr = '0000243001'.
          <fs_alv_conv>-valor_br_conv = gv_vlr_reserva_conv.
        ELSE.
          <fs_alv_conv>-valor_br_conv = <fs_alv_conv>-valor_brl.
        ENDIF..
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD alv_br_usd.

    CHECK gv_notalv IS INITIAL.

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = go_alv
          CHANGING
            t_table      = gt_alv_br_usd.

        set_header( ).
        status_alv( ).
        colunas_alv_br_usd( ).
        botao_alv( ).

      CATCH cx_root INTO DATA(lo_error).

    ENDTRY.

    go_alv->display( ).

  ENDMETHOD.


  METHOD calculo_valor_passivo.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Passivo
    gv_passivo_liq_brl = gv_ativo_liq_brl - gv_vlr_patr_liq_brl  .
    gv_passivo_liq_usd = gv_passivo_liq_brl / gv_taxa_venda .

    APPEND VALUE #(
         tipo = 'Passivo'
         valor_brl = gv_passivo_liq_brl
         valor_usd = gv_passivo_liq_usd ) TO gt_alv.


  ENDMETHOD.


  METHOD calculo_valor_passivo_br_usd.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Passivo
    gv_passivo_liq_brl = gv_ativo_liq_brl - gv_vlr_patr_liq_brl  .
    gv_passivo_liq_usd = gv_ativo_liq_usd - gv_vlr_patr_liq_usd .

    APPEND VALUE #(
         tipo = 'Passivo'
         valor_brl = gv_passivo_liq_brl
         valor_usd = gv_passivo_liq_usd ) TO gt_alv.


  ENDMETHOD.


  METHOD calculo_valor_passivo_usd.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Passivo
    gv_passivo_liq_usd = gv_ativo_liq_usd - gv_vlr_patr_liq_usd  .
    gv_passivo_liq_brl = gv_passivo_liq_usd * gv_taxa_venda .

    APPEND VALUE #(
         tipo = 'Passivo'
         valor_brl = gv_passivo_liq_brl
         valor_usd = gv_passivo_liq_usd ) TO gt_alv.


  ENDMETHOD.


  METHOD get_result.

    et_result_cta = gt_alv.

  ENDMETHOD.


  METHOD deleta_linhas_brls_zero.

    DELETE gt_alv WHERE valor_brl IS INITIAL.

  ENDMETHOD.


  METHOD calcula_patr_liq_conv.

    DATA: lv_tabix TYPE sy-tabix.

    READ TABLE gt_alv_br_usd ASSIGNING FIELD-SYMBOL(<fs_alv>) WITH KEY tipo = 'Patrimônio Liquido'.
    IF sy-subrc = 0.
      lv_tabix = sy-tabix.

      CLEAR: <fs_alv>-valor_br_conv.
      LOOP AT gt_alv_br_usd ASSIGNING FIELD-SYMBOL(<fs_alv_calc>) FROM lv_tabix.
        <fs_alv>-valor_br_conv = <fs_alv>-valor_br_conv + <fs_alv_calc>-valor_br_conv.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
