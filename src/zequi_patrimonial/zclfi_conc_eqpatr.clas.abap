CLASS zclfi_conc_eqpatr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA at_contas TYPE zglt0114_t .

    METHODS constructor
      IMPORTING
        !is_mep TYPE zsfi_mep_process .
    METHODS processa
      EXPORTING
        !et_alv1 TYPE zfitt_conc_eqpatr
        !et_alv2 TYPE zfitt_conc_eqpatr .
    METHODS set_contas
      IMPORTING
        !i_conta TYPE zglt0114_t .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_e TYPE char1 VALUE 'E' ##NO_TEXT.
    CONSTANTS gc_s TYPE char1 VALUE 'S' ##NO_TEXT.
    DATA gs_header TYPE zsfi_mep_header .
    DATA gv_dummy TYPE string .
    DATA gv_participacao TYPE z_part_perc .
    DATA gv_socidade TYPE rcomp_d .
    DATA gv_tx_med_medv TYPE zdefi_tx_med_medv .
    DATA gv_taxa_venda TYPE ukurs_curr .
    DATA gv_valor_brl_ck3 TYPE hslvt12 .
    DATA gv_valor_usd_ck3 TYPE hslvt12 .
    DATA go_alv1 TYPE REF TO cl_salv_table .
    DATA go_alv2 TYPE REF TO cl_salv_table .
    DATA:
      gt_mep TYPE TABLE OF zsfi_mep .
    DATA:
      gt_alv1 TYPE TABLE OF zsfi_conc .
    DATA:
      gt_alv2 TYPE TABLE OF zsfi_conc .
    DATA:
      gt_saldo TYPE TABLE OF zde_fi_gl_saldo_faglflext .
    DATA:
      gt_saldo2 TYPE TABLE OF zde_fi_gl_saldo_faglflext .
    DATA gt_msg TYPE bapiret2_tab .
    DATA container_main TYPE REF TO cl_gui_custom_container .
    DATA painel_control TYPE REF TO cl_gui_splitter_container .
    DATA painel1 TYPE REF TO cl_gui_container .
    DATA painel2 TYPE REF TO cl_gui_container .
    DATA gs_mep_proc TYPE zsfi_mep_process .

    METHODS processa_alv1 .
    METHODS busca_dados1 .
    METHODS processa_alv2 .
    METHODS busca_dados2 .
    METHODS get_info_header .
    METHODS msg .
    METHODS processa_parametro .
    METHODS processa_br_brl .
    METHODS processa_br_usd .
    METHODS processa_usd .
    METHODS get_taxas .
    METHODS get_taxa_media .
    METHODS get_taxa_venda .
    METHODS investimento."PORTING iv_conta TYPE saknr .
    METHODS equiv_pratimonial."PORTING iv_conta TYPE saknr .
    METHODS ajuste_acumulado."PORTING iv_conta TYPE saknr .
    METHODS get_saldo."PORTING iv_conta TYPE saknr .
    METHODS equiv_prat_br_brl .
    METHODS get_saldo_eqpat_br_brl .
    METHODS ajuste_acumulado_br_brl .
    METHODS get_saldo_ajacum_br_brl .
    METHODS set_alv_1 .
    METHODS equiv_prat_usd .
    METHODS get_saldo_eqpat_usd .
    METHODS agio_desagio."PORTING iv_conta TYPE saknr .
    METHODS equiv_prat_sem_custo_atr."PORTING iv_conta TYPE saknr .
    METHODS ajuste_ava_patr_custo .
    METHODS get_saldo_ava_patr_custo."PORTING VUE(i_contas) TYPE zglt0114_t OPTIONAL .
ENDCLASS.



CLASS zclfi_conc_eqpatr IMPLEMENTATION.


  METHOD constructor.

    CLEAR: gs_mep_proc.
    gs_mep_proc = is_mep.


    CLEAR: gt_mep.
    DATA(lo_mep) = NEW zclfi_mep(
      is_mep     = is_mep
      iv_not_alv = abap_true
    ).

    lo_mep->processa(
      IMPORTING
        et_alv = gt_mep
    ).


  ENDMETHOD.


  METHOD processa.

    get_info_header( ).
    get_taxas( ).
    set_alv_1( ).

    processa_parametro( ).

    et_alv1 = gt_alv1.
    et_alv2 = gt_alv2.
  ENDMETHOD.


  METHOD processa_alv1.

    busca_dados1( ).

  ENDMETHOD.


  METHOD busca_dados1.

  ENDMETHOD.


  METHOD processa_alv2.

    busca_dados2( ).

  ENDMETHOD.


  METHOD busca_dados2.

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


      CLEAR: gv_socidade.
      SELECT SINGLE rcomp
      INTO gv_socidade
      FROM t001
      WHERE bukrs = gs_header-empresa_investida.


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
          message_v4 = sy-msgv4 )
  TO gt_msg.

  ENDMETHOD.


  METHOD processa_parametro.

    TYPES: BEGIN OF ty_conta,
             conta TYPE zglt0114-conta,
           END OF ty_conta.

    DATA: it_contas TYPE STANDARD TABLE OF ty_conta.


    DATA: _saknr(10)    TYPE c,
          it_zglt0114_t TYPE zglt0114_t.

    SELECT * FROM zglt0114 INTO TABLE @DATA(it_zglt0114) WHERE pais = @gs_header-pais AND moeda_funcional = @gs_header-moeda_funcional.

    SORT it_zglt0114 BY sequencia ASCENDING.

    LOOP AT it_zglt0114 INTO DATA(wa_zglt0114).

      IF wa_zglt0114-conta IS NOT INITIAL AND wa_zglt0114-agrupamento IS NOT INITIAL.
        FREE: it_contas.
        SPLIT wa_zglt0114-conta AT ',' INTO TABLE it_contas.

        IF it_contas IS NOT INITIAL.
          FREE: it_zglt0114_t.

          it_zglt0114_t = VALUE #( FOR i IN it_contas ( conta = i-conta ) ).

          me->set_contas( it_zglt0114_t ). "set_contas é usado no get_contas

        ELSE.

        ENDIF.

        CASE wa_zglt0114-agrupamento.
          WHEN 'Investimento'.
            investimento( ).
          WHEN 'Equiv. Patrimonial'.
            equiv_pratimonial( ).
          WHEN 'Equiv. patrimonial – Resultado'.
            IF wa_zglt0114-moeda_funcional = 'USD' AND wa_zglt0114-pais <> 'BR'.
              equiv_prat_usd( ).
            ELSE.
              equiv_prat_br_brl( ).
            ENDIF.
          WHEN 'Ajuste acumulado de conversão em controladas'.
            ajuste_acumulado_br_brl( ).
          WHEN 'Equiv. patrim. s/ custo atribuído'.
            equiv_prat_sem_custo_atr( ).
          WHEN 'Ajuste Avaliação Patrimonial - Custo Atribuído'.
            ajuste_ava_patr_custo( ).
          WHEN 'Ágio/deságio'.
            agio_desagio( ).
        ENDCASE.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD processa_br_brl.

    investimento(  ).                                       "'131020'
    equiv_pratimonial( ).                                   " '131118'
    equiv_prat_br_brl( ).
    ajuste_acumulado_br_brl( ).

  ENDMETHOD.


  METHOD processa_br_usd.

    investimento( ).                                        "'131012'
    equiv_pratimonial( ).                                   "'131112'
    equiv_prat_sem_custo_atr( ).                            "'131112'
    equiv_prat_br_brl( ).
    ajuste_acumulado_br_brl( ).
    ajuste_ava_patr_custo( ).

  ENDMETHOD.


  METHOD processa_usd.

    investimento( ).                                        "'131031'
    equiv_pratimonial( ).                                   "'131137'
    agio_desagio( ).                                        "'131207'
    equiv_prat_usd( ).
    ajuste_acumulado_br_brl( ).

  ENDMETHOD.


  METHOD get_taxas.

    get_taxa_media( ).
    get_taxa_venda( ).

  ENDMETHOD.


  METHOD get_taxa_media.

    DATA: lv_mes TYPE zdefi_mes,
          lv_ano TYPE zdefi_ano.

    CHECK NOT line_exists( gt_msg[ type = gc_e ] ).

    lv_mes = gs_header-data_base+4(2).
    lv_ano = gs_header-data_base(4).

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
        date      = gs_header-data_base
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


  METHOD investimento.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    "Busca saldo da conta
    get_saldo( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Investimento'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    "Check1
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '05-Capital Social'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl - lv_valor_brl_chk )
             conc_usd = ( lv_valor_usd - lv_valor_usd_chk ) ) TO gt_alv2.



  ENDMETHOD.


  METHOD equiv_pratimonial.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    "Busca saldo da conta
    get_saldo( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Equiv. Patrimonial'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    IF gs_header-pais <> 'BR' AND
       gs_header-moeda_funcional <> 'BRL'.

      CLEAR: gv_valor_brl_ck3, gv_valor_usd_ck3.
      gv_valor_brl_ck3 = lv_valor_brl.
      gv_valor_usd_ck3 = lv_valor_usd.

    ELSE.

      " VAI PERMANECER FIXO O CHECK
      " VERIFICAR O TEXTO DO PARAMETRO
      " CONTINUAR

      "Check2
      READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                  WITH KEY parametro = '06-Reserva de capital'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '08-Reserva legal'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '10-Reserva de incentivos fiscais'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '12-Provisao de agio em incorporação'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '13-Agio em transação de capital'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '14-Variação de participação em controlada'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
        WITH KEY parametro = '02-Hedge accounting'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '04-Outros resultados abrangentes'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.


      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '09-Reserva de lucros'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '11-Prejuizos acumulados'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
                WITH KEY parametro = '03 - CTA - Ajuste acumulado de conversão'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.

      READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
            WITH KEY parametro = '15 - Resultado Período'.
      IF sy-subrc = 0.
        lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
        lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
      ENDIF.


      APPEND VALUE #(
               parametro = 'Check'
               conc_brl = ( lv_valor_brl_chk - lv_valor_brl  )
               conc_usd = ( lv_valor_usd_chk - lv_valor_usd  ) ) TO gt_alv2.


    ENDIF.

  ENDMETHOD.


  METHOD ajuste_acumulado.


    DATA: lv_valor_brl  TYPE hslvt12,
          lv_valor_usd  TYPE hslvt12,
          lv_valor_sl   TYPE hslvt12,
          lv_campo      TYPE string,
          lv_campo_cont TYPE n LENGTH 2,
          lv_data_ini   TYPE datum,
          lv_meses      TYPE vtbbewe-atage,
          lv_index      TYPE vtbbewe-atage.

    "Busca saldo da conta
    get_saldo( ). "iv_conta

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Ajuste acumulado de conversão em controladas'
             conc_usd = lv_valor_usd ) TO gt_alv2.


  ENDMETHOD.


  METHOD get_saldo.

    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano TYPE gjahr.

    CHECK me->at_contas IS NOT INITIAL.

    LOOP AT me->at_contas ASSIGNING FIELD-SYMBOL(<_set>).


      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <_set>-conta "iv_conta
        IMPORTING
          output = <fs_conta>-saknr.

      <fs_conta>-bukrs = gs_header-empresa_investidora.

    ENDLOOP.

    lv_ano = gs_header-data_base(4).

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


  METHOD equiv_prat_br_brl.
    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    get_saldo_eqpat_br_brl( ).
    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Equiv. patrimonial – Resultado'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    "Check3
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '15 - Resultado Período'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl + lv_valor_brl_chk )
             conc_usd = ( lv_valor_usd + lv_valor_usd_chk ) ) TO gt_alv2.


  ENDMETHOD.


  METHOD get_saldo_eqpat_br_brl.

    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano   TYPE gjahr,
          lv_conta TYPE saknr.


    CHECK me->at_contas IS NOT INITIAL.

    LOOP AT me->at_contas ASSIGNING FIELD-SYMBOL(<_set>).


      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <_set>-conta "iv_conta
        IMPORTING
          output = <fs_conta>-saknr.

      <fs_conta>-bukrs = gs_header-empresa_investidora.

    ENDLOOP.



    lv_ano = gs_header-data_base(4).

    CLEAR: gt_saldo, gt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear                = lv_ano
        contas               = lt_contas
        p_gerar_todas        = abap_true
        p_gerar_soc_parceira = abap_true
        rldnr                = '0L'
      TABLES
        it_saldos            = gt_saldo
        it_saldos_2          = gt_saldo2
      EXCEPTIONS
        moeda_nao_adm        = 1
        erro_ledger          = 2
        OTHERS               = 3.
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

    DELETE gt_saldo WHERE rassc <> gv_socidade.
    DELETE gt_saldo2 WHERE rassc <> gv_socidade.

  ENDMETHOD.


  METHOD ajuste_acumulado_br_brl.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    get_saldo_ajacum_br_brl( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Ajuste acumulado de conversão em controladas'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    "Check4
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '03 - CTA - Ajuste acumulado de conversão'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl + lv_valor_brl_chk )
             conc_usd = ( lv_valor_usd + lv_valor_usd_chk ) ) TO gt_alv2.

  ENDMETHOD.


  METHOD get_saldo_ajacum_br_brl.

    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano   TYPE gjahr,
          lv_conta TYPE saknr.


    CHECK me->at_contas IS NOT INITIAL.

    LOOP AT me->at_contas ASSIGNING FIELD-SYMBOL(<_set>).


      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <_set>-conta "iv_conta
        IMPORTING
          output = <fs_conta>-saknr.

      <fs_conta>-bukrs = gs_header-empresa_investidora.

    ENDLOOP.


    lv_ano = gs_header-data_base(4).

    CLEAR: gt_saldo, gt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear                = lv_ano
        contas               = lt_contas
        p_gerar_todas        = abap_true
        p_gerar_soc_parceira = abap_true
        rldnr                = '0L'
      TABLES
        it_saldos            = gt_saldo
        it_saldos_2          = gt_saldo2
      EXCEPTIONS
        moeda_nao_adm        = 1
        erro_ledger          = 2
        OTHERS               = 3.
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

    DELETE gt_saldo WHERE rassc <> gv_socidade.
    DELETE gt_saldo2 WHERE rassc <> gv_socidade.

  ENDMETHOD.


  METHOD set_alv_1.

    LOOP AT gt_mep ASSIGNING FIELD-SYMBOL(<fs_mep>).

      APPEND INITIAL LINE TO gt_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>).
      <fs_alv1>-parametro = <fs_mep>-parametro.
      <fs_alv1>-conc_brl = <fs_mep>-saldo_mes_atual_brl * gv_participacao.
      <fs_alv1>-conc_usd = <fs_mep>-saldo_mes_atual_usd * gv_participacao.

    ENDLOOP.

  ENDMETHOD.


  METHOD equiv_prat_usd.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    get_saldo_eqpat_br_brl( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Equiv. patrimonial – Resultado'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.


    "Check3
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '15 - Resultado Período'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl + lv_valor_brl_chk )
             conc_usd = ( lv_valor_usd + lv_valor_usd_chk ) ) TO gt_alv2.



  ENDMETHOD.


  METHOD get_saldo_eqpat_usd.


    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano   TYPE gjahr,
          lv_conta TYPE saknr.


    APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).
    lv_conta = '332006'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_conta
      IMPORTING
        output = <fs_conta>-saknr.

    <fs_conta>-bukrs = gs_header-empresa_investidora.

    APPEND INITIAL LINE TO lt_contas ASSIGNING <fs_conta>.
    lv_conta = '424006'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_conta
      IMPORTING
        output = <fs_conta>-saknr.

    <fs_conta>-bukrs = gs_header-empresa_investidora.



    lv_ano = gs_header-data_base(4).

    CLEAR: gt_saldo, gt_saldo2.
    CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
      EXPORTING
        ryear                = lv_ano
        contas               = lt_contas
        p_gerar_todas        = abap_true
        p_gerar_soc_parceira = abap_true
        rldnr                = '0L'
      TABLES
        it_saldos            = gt_saldo
        it_saldos_2          = gt_saldo2
      EXCEPTIONS
        moeda_nao_adm        = 1
        erro_ledger          = 2
        OTHERS               = 3.
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

    DELETE gt_saldo WHERE rassc <> gv_socidade.
    DELETE gt_saldo2 WHERE rassc <> gv_socidade.


  ENDMETHOD.


  METHOD agio_desagio.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    "Busca saldo da conta
    get_saldo( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Ágio/deságio'
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    gv_valor_brl_ck3 = gv_valor_brl_ck3 + lv_valor_brl.
    gv_valor_usd_ck3 = gv_valor_usd_ck3 + lv_valor_usd.

    "Check2

    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '06-Reserva de capital'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '08-Reserva legal'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '10-Reserva de incentivos fiscais'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '12-Provisao de agio em incorporação'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '13-Agio em transação de capital'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '14-Variação de participação em controlada'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
      WITH KEY parametro = '01-Ajuste Avaliação Patrimonial'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
      WITH KEY parametro = '02-Hedge accounting'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '04-Outros resultados abrangentes'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.


    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '09-Reserva de lucros'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '11-Prejuizos acumulados'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
              WITH KEY parametro = '03 - CTA - Ajuste acumulado de conversão'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.

    READ TABLE gt_alv1 ASSIGNING <Fs_alv1>
          WITH KEY parametro = '15 - Resultado Período'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = lv_valor_brl_chk + <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = lv_valor_usd_chk + <Fs_alv1>-conc_usd.
    ENDIF.


    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl_chk - gv_valor_brl_ck3  )
             conc_usd = ( lv_valor_usd_chk - gv_valor_usd_ck3  ) ) TO gt_alv2.


  ENDMETHOD.


  METHOD equiv_prat_sem_custo_atr.


    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    "Busca saldo da conta
    "get_saldo( ).
    get_saldo_ava_patr_custo( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Equiv. patrim. s/ custo atribuído '
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.


    "Check3
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '01-Ajuste Avaliação Patrimonial'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.


    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl_chk - lv_valor_brl  )
             conc_usd = ( lv_valor_usd_chk - lv_valor_usd  ) ) TO gt_alv2.

  ENDMETHOD.


  METHOD ajuste_ava_patr_custo.

    DATA: lv_valor_brl     TYPE hslvt12,
          lv_valor_usd     TYPE hslvt12,
          lv_valor_brl_chk TYPE hslvt12,
          lv_valor_usd_chk TYPE hslvt12,
          lv_valor_sl      TYPE hslvt12,
          lv_campo         TYPE string,
          lv_campo_cont    TYPE n LENGTH 2,
          lv_data_ini      TYPE datum,
          lv_meses         TYPE vtbbewe-atage,
          lv_index         TYPE vtbbewe-atage.

    get_saldo_ava_patr_custo( ).

    lv_meses = gs_header-data_base+4(2).

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


    APPEND VALUE #(
             parametro = 'Ajuste Avaliação Patrimonial - Custo Atribuído '
             conc_brl = lv_valor_brl
             conc_usd = lv_valor_usd ) TO gt_alv2.

    "Check3
    READ TABLE gt_alv1 ASSIGNING FIELD-SYMBOL(<Fs_alv1>)
                WITH KEY parametro = '01-Ajuste Avaliação Patrimonial'.
    IF sy-subrc = 0.
      lv_valor_brl_chk = <Fs_alv1>-conc_brl.
      lv_valor_usd_chk = <Fs_alv1>-conc_usd.
    ENDIF.

    APPEND VALUE #(
             parametro = 'Check'
             conc_brl = ( lv_valor_brl + lv_valor_brl_chk )
             conc_usd = ( lv_valor_usd + lv_valor_usd_chk ) ) TO gt_alv2.

  ENDMETHOD.


  METHOD get_saldo_ava_patr_custo.


    DATA: lt_contas TYPE zct_emp_contas.
    DATA: lv_ano   TYPE gjahr,
          lv_conta TYPE saknr.


    CHECK me->at_contas IS NOT INITIAL.

    LOOP AT me->at_contas ASSIGNING FIELD-SYMBOL(<_set>).


      APPEND INITIAL LINE TO lt_contas ASSIGNING FIELD-SYMBOL(<fs_conta>).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <_set>-conta "iv_conta
        IMPORTING
          output = <fs_conta>-saknr.

      <fs_conta>-bukrs = gs_header-empresa_investidora.

    ENDLOOP.

    lv_ano = gs_header-data_base(4).

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


  METHOD set_contas.

    CHECK i_conta IS NOT INITIAL.

    me->at_contas = i_conta.

  ENDMETHOD.
ENDCLASS.
