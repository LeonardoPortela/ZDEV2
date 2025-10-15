CLASS lcl_account_balance_ativo IMPLEMENTATION. "Responsabilidade -> Buscar o saldo de contas contábeis de ativo passivo.

  METHOD sum_values_per_month.

    DATA: l_index TYPE numc2.

    ASSIGN COMPONENT 'SLVT' OF STRUCTURE iv_saldo_contas TO FIELD-SYMBOL(<saldo_anterior>).
    IF <saldo_anterior> IS ASSIGNED.
      ADD <saldo_anterior> TO rt_saldo.
    ENDIF.

    DO me->gv_meses TIMES.
      l_index = sy-index.

      CONCATENATE 'SL' l_index INTO DATA(l_fiedname).
      ASSIGN COMPONENT l_fiedname OF STRUCTURE iv_saldo_contas TO FIELD-SYMBOL(<saldo_mes_atual>).
      IF <saldo_mes_atual> IS ASSIGNED.
        ADD <saldo_mes_atual> TO rt_saldo.
      ENDIF.
    ENDDO.

    IF rt_saldo >= 0.
      ev_indicador_dc = 'D'.
    ELSE.
      ev_indicador_dc = 'C'.
    ENDIF.

  ENDMETHOD.

  METHOD if_load_account_balance~build_account_balance.

    DATA: lt_saldo_moeda_interna TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_forte   TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_indice  TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    DATA: lt_saldo_moeda_interna_line TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_forte_line   TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_indice_line  TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    DATA: ls_saldo_contas TYPE zsco_saldo_contas.

    DATA: lt_moedas_totais TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    LOOP AT me->gt_contas_busca INTO DATA(ls_contas_busca).

      FREE: lt_saldo_moeda_interna_line, lt_saldo_moeda_forte_line, lt_saldo_moeda_indice_line.

      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear                = me->gv_ano
          contas               = ls_contas_busca-contas
          p_gerar_todas        = 'X'
          p_gerar_soc_parceira = ls_contas_busca-gerar_soc_parceira
          rldnr                = '0L'
        TABLES
          it_saldos            = lt_saldo_moeda_interna_line
          it_saldos_2          = lt_saldo_moeda_forte_line
          it_saldos_3          = lt_saldo_moeda_indice_line
        EXCEPTIONS
          moeda_nao_adm        = 1
          erro_ledger          = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT lt_saldo_moeda_interna_line INTO DATA(ls_saldo_moeda_interna_line).
        IF ls_saldo_moeda_interna_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_interna WITH KEY rbukrs = ls_saldo_moeda_interna_line-rbukrs
                                                     racct  = ls_saldo_moeda_interna_line-racct
                                                     rassc  = ls_saldo_moeda_interna_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_interna_line TO lt_saldo_moeda_interna.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_interna WITH KEY rbukrs = ls_saldo_moeda_interna_line-rbukrs
                                                     racct  = ls_saldo_moeda_interna_line-racct
                                                     rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_interna_line TO lt_saldo_moeda_interna.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_saldo_moeda_forte_line INTO DATA(ls_saldo_moeda_forte_line).
        IF ls_saldo_moeda_forte_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_forte WITH KEY rbukrs = ls_saldo_moeda_forte_line-rbukrs
                                                   racct  = ls_saldo_moeda_forte_line-racct
                                                   rassc  = ls_saldo_moeda_forte_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_forte_line TO lt_saldo_moeda_forte.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_forte WITH KEY rbukrs = ls_saldo_moeda_forte_line-rbukrs
                                                   racct  = ls_saldo_moeda_forte_line-racct
                                                   rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_forte_line TO lt_saldo_moeda_forte.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_saldo_moeda_indice_line INTO DATA(ls_saldo_moeda_indice_line).
        IF ls_saldo_moeda_indice_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_indice WITH KEY rbukrs = ls_saldo_moeda_indice_line-rbukrs
                                                    racct  = ls_saldo_moeda_indice_line-racct
                                                    rassc  = ls_saldo_moeda_indice_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_indice_line TO lt_saldo_moeda_indice.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_indice WITH KEY rbukrs = ls_saldo_moeda_indice_line-rbukrs
                                                    racct  = ls_saldo_moeda_indice_line-racct
                                                    rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_indice_line TO lt_saldo_moeda_indice.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF: lt_saldo_moeda_interna TO lt_moedas_totais,
                     lt_saldo_moeda_forte   TO lt_moedas_totais,
                     lt_saldo_moeda_indice  TO lt_moedas_totais.

    IF lt_moedas_totais[] IS NOT INITIAL.
      SORT lt_moedas_totais BY racct.
      DELETE ADJACENT DUPLICATES FROM lt_moedas_totais COMPARING racct.

      SELECT saknr, txt50, spras
        FROM skat
        INTO TABLE @DATA(lt_texto_contas)
        FOR ALL ENTRIES IN @lt_moedas_totais
        WHERE spras IN ( 'E', 'P' )
        AND   ktopl = '0050'
        AND   saknr = @lt_moedas_totais-racct.
      IF sy-subrc IS INITIAL.
        SORT lt_texto_contas BY saknr spras.
      ENDIF.
    ENDIF.

    SORT: lt_saldo_moeda_interna BY rbukrs racct rassc,
          lt_saldo_moeda_forte   BY rbukrs racct rassc,
          lt_saldo_moeda_indice  BY rbukrs racct rassc.

    LOOP AT lt_saldo_moeda_interna INTO DATA(ls_moeda_interna).
      CLEAR: ls_saldo_contas.

      MOVE-CORRESPONDING ls_moeda_interna TO ls_saldo_contas.

      ls_saldo_contas-month = me->gv_mes.

      READ TABLE lt_texto_contas INTO DATA(ls_text_contas) WITH KEY saknr = ls_moeda_interna-racct
                                                                    spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_moeda_interna-racct
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-moeda_interna = me->sum_values_per_month( EXPORTING
                                                                 iv_saldo_contas = ls_moeda_interna
                                                                IMPORTING
                                                                 ev_indicador_dc = ls_saldo_contas-dc_interna ).

      READ TABLE lt_saldo_moeda_forte INTO DATA(ls_saldo_moeda_forte) WITH KEY rbukrs = ls_moeda_interna-rbukrs
                                                                               racct  = ls_moeda_interna-racct
                                                                               rassc  = ls_moeda_interna-rassc BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-moeda_forte = me->sum_values_per_month( EXPORTING
                                                                 iv_saldo_contas = ls_saldo_moeda_forte
                                                                IMPORTING
                                                                 ev_indicador_dc = ls_saldo_contas-dc_forte ).
      ENDIF.

      READ TABLE lt_saldo_moeda_indice INTO DATA(ls_saldo_moeda_indice) WITH KEY rbukrs = ls_moeda_interna-rbukrs
                                                                                 racct  = ls_moeda_interna-racct
                                                                                 rassc  = ls_moeda_interna-rassc BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-moeda_indice = me->sum_values_per_month( EXPORTING
                                                                  iv_saldo_contas = ls_saldo_moeda_indice
                                                                 IMPORTING
                                                                  ev_indicador_dc = ls_saldo_contas-dc_indice ).
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      APPEND ls_saldo_contas TO rt_saldo_contas.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_contas_busca.

    "Tabelas internas
    DATA: lt_setleaft TYPE STANDARD TABLE OF setleaf,
          lt_contas   TYPE STANDARD TABLE OF zlc_emp_contas.
    "Estruturas
    DATA: ls_saldo_contas_busca TYPE zsco_saldo_contas_busca.

    SELECT *
      FROM setleaf
      INTO TABLE lt_setleaft
      WHERE setname = 'CONTAS_EC-CS'.
    IF sy-subrc IS INITIAL.
      LOOP AT me->gt_empresas INTO DATA(l_empresa).

        LOOP AT lt_setleaft INTO DATA(ls_setleaf).
          APPEND VALUE #( saknr = ls_setleaf-valfrom bukrs = l_empresa-low ) TO lt_contas.
        ENDLOOP.

        ls_saldo_contas_busca-gerar_soc_parceira = 'X'.
        ls_saldo_contas_busca-contas[]           = lt_contas[].
        APPEND ls_saldo_contas_busca TO me->gt_contas_busca.
        CLEAR: lt_contas, ls_saldo_contas_busca, lt_setleaft, ls_setleaf.

        APPEND VALUE #( bukrs = l_empresa-low  ) TO lt_contas.

        ls_saldo_contas_busca-gerar_soc_parceira = ' '.
        ls_saldo_contas_busca-contas[]           = lt_contas[].
        APPEND ls_saldo_contas_busca TO me->gt_contas_busca.
        CLEAR: lt_contas, ls_saldo_contas_busca.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.

    me->gv_ano        = iv_ano.
    me->gv_mes        = iv_mes.
    me->gt_empresas[] = iv_empresas[].

    IF iv_mes = 12.
      me->gv_meses = iv_mes + 4.
    ELSE.
      me->gv_meses = iv_mes.
    ENDIF.

    me->set_contas_busca( ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_account_balance_resultado IMPLEMENTATION. "Responsabilidade -> Buscar o saldo de contas contábeis de resultado

  METHOD set_contas_busca.

    "Tabelas internas
    DATA: lt_contas   TYPE STANDARD TABLE OF zlc_emp_contas,
          lt_setleaft TYPE STANDARD TABLE OF setleaf.
    "Estruturas
    DATA: ls_saldo_contas_busca TYPE zsco_saldo_contas_busca.

    SELECT *
      FROM setleaf
      INTO TABLE lt_setleaft
      WHERE setname = 'CONTAS_EC-CS'.

    SELECT *
      FROM zglt049
      INTO TABLE @DATA(lt_zglt049)
      WHERE versn = 'BP01'.
    IF sy-subrc IS INITIAL.

      SORT lt_zglt049 BY cod_clas_bal cod_clas_not.

      SELECT *
        FROM zglt041
        INTO TABLE @DATA(lt_zglt041)
        FOR ALL ENTRIES IN @lt_zglt049
        WHERE bukrs        IN @me->gt_empresas
        AND   cod_clas_bal EQ @lt_zglt049-cod_clas_bal
        AND  cod_clas_not2 EQ @lt_zglt049-cod_clas_not.
      IF sy-subrc IS INITIAL.
        SORT lt_zglt041 BY bukrs saknr.
      ENDIF.

    ENDIF.

    LOOP AT me->gt_empresas INTO DATA(l_empresa).

      LOOP AT lt_zglt041 INTO DATA(ls_zglt041) WHERE bukrs = l_empresa-low.
        READ TABLE lt_setleaft INTO DATA(ls_setleaf) WITH KEY valfrom = ls_zglt041-saknr.
        IF sy-subrc IS INITIAL.
          APPEND VALUE #( saknr = ls_zglt041-saknr bukrs = l_empresa-low ) TO lt_contas.
        ENDIF.
      ENDLOOP.

      ls_saldo_contas_busca-gerar_soc_parceira = 'X'.
      ls_saldo_contas_busca-contas[]           = lt_contas[].
      APPEND ls_saldo_contas_busca TO me->gt_contas_busca.
      CLEAR: lt_contas, ls_saldo_contas_busca.

      LOOP AT lt_zglt041 INTO ls_zglt041 WHERE bukrs = l_empresa-low.
        READ TABLE lt_setleaft INTO ls_setleaf WITH KEY valfrom = ls_zglt041-saknr.
        IF sy-subrc IS NOT INITIAL.
          APPEND VALUE #( saknr = ls_zglt041-saknr bukrs = l_empresa-low ) TO lt_contas.
        ENDIF.
      ENDLOOP.

      ls_saldo_contas_busca-gerar_soc_parceira = ' '.
      ls_saldo_contas_busca-contas[]           = lt_contas[].
      APPEND ls_saldo_contas_busca TO me->gt_contas_busca.
      CLEAR: lt_contas, ls_saldo_contas_busca.

    ENDLOOP.

  ENDMETHOD.

  METHOD load_accounts_zglt041.

    SELECT *
      FROM zglt041
      INTO TABLE rt_zglt041
      WHERE bukrs         IN me->gt_empresas
      AND   cod_clas_bal  IN iv_classificacao_balanco
      AND   cod_clas_not2 IN iv_classificacao_nota.
    IF sy-subrc IS INITIAL.
      SORT rt_zglt041 BY bukrs saknr.
    ENDIF.

  ENDMETHOD.

  METHOD load_balance_note_base.

    SELECT *
      FROM zglt047
      INTO TABLE et_niveis
      WHERE versn = iv_versn.
    IF sy-subrc IS INITIAL.
      SORT et_niveis BY versn nivel.

      SELECT *
        FROM zglt049
        INTO TABLE @DATA(lt_balance_note)
        FOR ALL ENTRIES IN @et_niveis
        WHERE versn = @et_niveis-versn
        AND   nivel = @et_niveis-nivel.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_balance_note INTO DATA(ls_balance_note).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_balance_note-cod_clas_bal ) TO et_balance.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_balance_note-cod_clas_not ) TO et_note.
        ENDLOOP.
      ENDIF.
    ENDIF.

    FREE: lt_balance_note, ls_balance_note.

  ENDMETHOD.

  METHOD load_balance_note_cost_center.

    SELECT *
      FROM zglt049c
      INTO TABLE @DATA(lt_cost_center)
      WHERE cod_clas_bal IN @it_balance
      AND   cod_clas_not IN @it_note.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_cost_center INTO DATA(ls_cost_center).
        IF ls_cost_center-kosar EQ 'F'.
          CONTINUE.
        ENDIF.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_cost_center-cod_clas_bal  ) TO et_balance_cost_center.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_cost_center-cod_clas_not  ) TO et_note_cost_center.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_cost_center-kosar         ) TO et_type_cost_center.
        APPEND ls_cost_center TO et_zglt049c.
      ENDLOOP.
    ENDIF.

    FREE: lt_cost_center, ls_cost_center.

  ENDMETHOD.

  METHOD load_balance_note_profit_c.

    SELECT *
      FROM zglt049l
      INTO TABLE @DATA(lt_profit_center)
      WHERE cod_clas_bal IN @it_balance
      AND   cod_clas_not IN @it_note.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_profit_center INTO DATA(ls_profit_center).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_profit_center-cod_clas_bal  ) TO et_balance_profit_center.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_profit_center-cod_clas_not  ) TO et_note_profit_center.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_profit_center-prctr         ) TO et_profit_center.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_profit_center-kokrs         ) TO et_account_aerea.
        APPEND ls_profit_center TO et_zglt049l.
      ENDLOOP.
    ENDIF.

    FREE: lt_profit_center, ls_profit_center.

  ENDMETHOD.

  METHOD load_balance_note_mat_group.

    SELECT *
      FROM zglt049m
      INTO TABLE @DATA(lt_material_group)
      WHERE cod_clas_bal IN @it_balance
      AND   cod_clas_not IN @it_note.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_material_group INTO DATA(ls_material_group).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_material_group-cod_clas_bal ) TO et_balance_material_group.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_material_group-cod_clas_not ) TO et_note_material_group.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_material_group-matkl        ) TO et_material_group .
        APPEND ls_material_group TO et_zglt049m.
      ENDLOOP.
    ENDIF.

    FREE: lt_material_group, ls_material_group.

  ENDMETHOD.

  METHOD load_saldo_mensal_dre.

    DATA: lva_data_val_cc TYPE csks-datab.

    DATA: lt_account_area TYPE ace_generic_range_t.

    IF it_accounts IS INITIAL.
      RETURN.
    ENDIF.

    SELECT 'I'   AS sign,
           'EQ'  AS option,
           kokrs AS low
      FROM tka02
      INTO CORRESPONDING FIELDS OF TABLE @lt_account_area
      WHERE bukrs IN @me->gt_empresas.

    SELECT *
      FROM zgl_contas
      INTO TABLE @DATA(lt_contas_excluidas).
    IF sy-subrc IS INITIAL.
      SORT lt_contas_excluidas BY rbukrs racct.
    ENDIF.

    SELECT *
      FROM zgl029_dre_dados
      INTO CORRESPONDING FIELDS OF TABLE et_dre_values
      FOR ALL ENTRIES IN it_accounts
      WHERE bukrs IN me->gt_empresas
      AND   versn EQ 'DRE2'
      AND   gjahr EQ me->gv_ano
      AND   monat IN me->gv_meses
      AND   saknr EQ it_accounts-saknr
      "AND   kosar IN it_kosar        "Fazer filtro na sequencia do select
      "AND   kokrs IN lt_account_area "Fazer filtro na sequencia do select
      AND   prctr IN it_prctr
      AND   matkl IN it_matkl.

    CHECK et_dre_values[] IS NOT INITIAL.


*------------------------------------------------------------------------------------------------*
*   Atribuição Tipo de Centro de Custo e Area de Contabilidade nos registros de Saldo
*------------------------------------------------------------------------------------------------*
    DATA(lit_bukrs_dre) = et_dre_values[].
    SORT lit_bukrs_dre BY bukrs.
    DELETE ADJACENT DUPLICATES FROM lit_bukrs_dre COMPARING bukrs.

    IF lit_bukrs_dre[] IS NOT INITIAL.
      SELECT *
        FROM tka02 INTO TABLE @DATA(lit_tka02)
         FOR ALL ENTRIES IN @lit_bukrs_dre
       WHERE bukrs = @lit_bukrs_dre-bukrs.

      SORT lit_tka02 BY bukrs.

      IF lit_tka02[] IS NOT INITIAL.
        LOOP AT et_dre_values ASSIGNING FIELD-SYMBOL(<fs_dre_value>).
          READ TABLE lit_tka02 INTO DATA(lwa_tka02) WITH KEY bukrs = <fs_dre_value>-bukrs BINARY SEARCH.
          CHECK sy-subrc EQ 0.
          <fs_dre_value>-kokrs = lwa_tka02-kokrs.
        ENDLOOP.
      ENDIF.
    ENDIF.

    SELECT *
      FROM csks INTO TABLE @DATA(lit_csks)
       FOR ALL ENTRIES IN @et_dre_values
     WHERE kokrs EQ @et_dre_values-kokrs
       AND kostl EQ @et_dre_values-kostl.

    LOOP AT et_dre_values ASSIGNING <fs_dre_value>.

      lva_data_val_cc = <fs_dre_value>-gjahr && <fs_dre_value>-monat && '01'.

      LOOP AT lit_csks INTO DATA(lwa_csks) WHERE kokrs EQ <fs_dre_value>-kokrs
                                             AND kostl EQ <fs_dre_value>-kostl
                                             AND datab LE lva_data_val_cc  "Data de validade inicial
                                             AND datbi GE lva_data_val_cc. "Data de validade final
        <fs_dre_value>-kosar = lwa_csks-kosar.
        EXIT.
      ENDLOOP.
    ENDLOOP.


    DELETE et_dre_values WHERE NOT kokrs IN lt_account_area.
    DELETE et_dre_values WHERE NOT kosar IN it_kosar.


    "Não precisa usando a zgl039_dre_dados - Ini
*    IF sy-subrc IS INITIAL.
*
*      DATA(lt_ordens) = et_dre_values[].
*
*      DELETE lt_ordens WHERE aufnr IS INITIAL.
*
*      SORT lt_ordens BY aufnr.
*      DELETE ADJACENT DUPLICATES FROM lt_ordens COMPARING aufnr.
*
*      IF lt_ordens IS NOT INITIAL.
*        SELECT aufnr, auart
*          FROM coas
*          INTO TABLE @DATA(lt_ordens_check)
*          FOR ALL ENTRIES IN @lt_ordens
*          WHERE aufnr = @lt_ordens-aufnr
*          AND   auart IN ( 'ZSTA' , 'ZSIN' ).
*        IF sy-subrc IS INITIAL.
*          SORT lt_ordens_check BY aufnr.
*        ENDIF.
*      ENDIF.
*
*      LOOP AT et_dre_values INTO DATA(ls_dre_values).
*
*        DATA(l_tabix) = sy-tabix.
*
*        READ TABLE lt_contas_excluidas WITH KEY rbukrs = ls_dre_values-bukrs
*                                                racct  = ls_dre_values-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          DELETE et_dre_values INDEX l_tabix.
*          CONTINUE.
*        ENDIF.
*
*        CHECK ls_dre_values-aufnr IS NOT INITIAL.
*
*        READ TABLE lt_ordens_check WITH KEY aufnr = ls_dre_values-aufnr TRANSPORTING NO FIELDS BINARY SEARCH.
*
*        CHECK sy-subrc IS NOT INITIAL.
*
*        DELETE et_dre_values INDEX l_tabix.
*
*      ENDLOOP.
*    ENDIF.
    "Não precisa usando a zgl039_dre_dados - Fim


  ENDMETHOD.

  METHOD sum_values_centro_custo.

    DATA: ls_soma_saldo_cc TYPE zglt_dre_cc.

    LOOP AT it_dre_values INTO DATA(ls_dre_values).
      MOVE-CORRESPONDING ls_dre_values TO ls_soma_saldo_cc.

      ls_soma_saldo_cc-vlhsl  = ls_dre_values-vlr_rea.
      ls_soma_saldo_cc-vlksl  = ls_dre_values-vlr_dolar.
      ls_soma_saldo_cc-vlosl  = ls_dre_values-vlr_grupo.

      COLLECT ls_soma_saldo_cc INTO et_sum_values.
    ENDLOOP.

  ENDMETHOD.

  METHOD sum_values_centro_lucro.

    DATA: ls_soma_saldo_cl TYPE zglt_dre_cl.

    LOOP AT it_dre_values INTO DATA(ls_dre_values).
      MOVE-CORRESPONDING ls_dre_values TO ls_soma_saldo_cl.

      ls_soma_saldo_cl-vlhsl  = ls_dre_values-vlr_rea.
      ls_soma_saldo_cl-vlksl  = ls_dre_values-vlr_dolar.
      ls_soma_saldo_cl-vlosl  = ls_dre_values-vlr_grupo.

      COLLECT ls_soma_saldo_cl INTO et_sum_values.
    ENDLOOP.

  ENDMETHOD.

  METHOD sum_values_grupo_mercadoria.

    DATA: ls_soma_saldo_gm TYPE zglt_dre_gm.

    LOOP AT it_dre_values INTO DATA(ls_dre_values).
      MOVE-CORRESPONDING ls_dre_values TO ls_soma_saldo_gm.

      ls_soma_saldo_gm-vlhsl  = ls_dre_values-vlr_rea.
      ls_soma_saldo_gm-vlksl  = ls_dre_values-vlr_dolar.
      ls_soma_saldo_gm-vlosl  = ls_dre_values-vlr_grupo.

      COLLECT ls_soma_saldo_gm INTO et_sum_values.
    ENDLOOP.

  ENDMETHOD.

  METHOD sum_values_razao.

    DATA: it_accounts_reason_s TYPE SORTED TABLE OF zglt041 WITH UNIQUE KEY mandt bukrs saknr cod_clas_bal cod_clas_not2.

    DATA: it_dre_values_s      TYPE SORTED TABLE OF zde_zgl029_dre_dados WITH UNIQUE KEY mandt bukrs versn monat gjahr saknr kostl prctr matkl vbund.


    DATA: ls_soma_saldo_r TYPE zglt_dre_r.

    DATA: l_has_been_summed TYPE xflag.

    DATA: lt_zgl015_dre_est06 TYPE STANDARD TABLE OF zgl015_dre_est06.

    IF it_dre_values IS NOT INITIAL.

      DATA(it_dre_values_aux) = it_dre_values[].
      SORT  it_dre_values_aux BY saknr.
      DELETE ADJACENT DUPLICATES FROM it_dre_values_aux COMPARING saknr.

      SELECT *
        FROM zgl015_dre_est06
        INTO TABLE lt_zgl015_dre_est06
        FOR ALL ENTRIES IN it_dre_values_aux
        WHERE versn = 'DRE2'
        AND   saknr = it_dre_values_aux-saknr.
      IF lt_zgl015_dre_est06[] IS NOT INITIAL.
        SORT lt_zgl015_dre_est06 BY saknr.
      ENDIF.
    ENDIF.

    MOVE it_accounts_reason TO it_accounts_reason_s.
    MOVE it_dre_values      TO it_dre_values_s.

    LOOP AT it_dre_values INTO DATA(ls_dre_values).
      CLEAR: l_has_been_summed.

      LOOP AT it_accounts_reason_s INTO DATA(ls_accounts_reason) WHERE bukrs = ls_dre_values-bukrs
                                                                   AND saknr = ls_dre_values-saknr.


        READ TABLE it_zglt049c WITH KEY cod_clas_bal = ls_accounts_reason-cod_clas_bal
                                        cod_clas_not = ls_accounts_reason-cod_clas_not2 TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc IS INITIAL. "Nota é de centro de custo?
          READ TABLE it_cc_processado WITH KEY bukrs = ls_dre_values-bukrs
                                               saknr = ls_dre_values-saknr
                                               kosar = ls_dre_values-kosar TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc IS INITIAL. "Já foi procesada como centro de custo? Se sim, desconsidera
            l_has_been_summed = 'X'.
          ENDIF.

          IF ls_dre_values-kosar IS INITIAL.
            l_has_been_summed = 'X'.

            DATA(_achou_reg_cc) = abap_false.
            LOOP AT it_dre_values_s INTO DATA(ls_dre_values_cc) WHERE bukrs = ls_dre_values-bukrs
                                                                AND saknr = ls_dre_values-saknr
                                                                AND kosar IS NOT INITIAL.

              _achou_reg_cc = abap_true.
              EXIT.
*              IF ls_dre_values_cc-kosar IS INITIAL.
*                CLEAR: l_has_been_summed.
*              ELSE.
*                l_has_been_summed = 'X'.
*                EXIT.
*              ENDIF.

            ENDLOOP.

            IF _achou_reg_cc EQ abap_false.
              CLEAR: l_has_been_summed.
            ENDIF.

          ENDIF.

*          IF ls_dre_values-kosar EQ 'F'.
*            l_has_been_summed = 'X'.
*          ENDIF.

        ENDIF.

        READ TABLE it_zglt049l WITH KEY cod_clas_bal = ls_accounts_reason-cod_clas_bal
                                        cod_clas_not = ls_accounts_reason-cod_clas_not2 TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_cl_processado WITH KEY bukrs = ls_dre_values-bukrs
                                               saknr = ls_dre_values-saknr
                                               prctr = ls_dre_values-prctr TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            l_has_been_summed = 'X'.
            EXIT.
          ENDIF.

          IF ls_dre_values-prctr IS INITIAL.
            l_has_been_summed = 'X'.

            DATA(_achou_reg_cl) = abap_false.
            LOOP AT it_dre_values_s INTO ls_dre_values_cc WHERE bukrs = ls_dre_values-bukrs
                                                            AND saknr = ls_dre_values-saknr
                                                            AND prctr IS NOT INITIAL.
              _achou_reg_cl = abap_true.
              EXIT.
*              IF ls_dre_values-prctr IS INITIAL.
*                CLEAR: l_has_been_summed.
*              ELSE.
*                l_has_been_summed = 'X'.
*                EXIT.
*              ENDIF.
            ENDLOOP.

            IF _achou_reg_cl EQ abap_false.
              CLEAR: l_has_been_summed.
            ENDIF.

          ENDIF.

        ENDIF.

        READ TABLE it_zglt049m WITH KEY cod_clas_bal = ls_accounts_reason-cod_clas_bal
                                        cod_clas_not = ls_accounts_reason-cod_clas_not2 TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE it_gm_processado WITH KEY bukrs = ls_dre_values-bukrs
                                               saknr = ls_dre_values-saknr
                                               matkl = ls_dre_values-matkl TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            l_has_been_summed = 'X'.
            EXIT.
          ENDIF.

          READ TABLE lt_zgl015_dre_est06 WITH KEY saknr = ls_dre_values-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            l_has_been_summed = 'X'.
            EXIT.
          ENDIF.

          IF ls_dre_values-matkl IS INITIAL.
            l_has_been_summed = 'X'.

            DATA(_achou_reg_gm) = abap_false.
            LOOP AT it_dre_values_s INTO ls_dre_values_cc WHERE bukrs = ls_dre_values-bukrs
                                                            AND saknr = ls_dre_values-saknr
                                                            AND matkl IS NOT INITIAL.

              _achou_reg_gm = abap_true.
              EXIT.
*              IF ls_dre_values-matkl IS INITIAL.
*                CLEAR: l_has_been_summed.
*              ELSE.
*                l_has_been_summed = 'X'.
*                EXIT.
*              ENDIF.
            ENDLOOP.

            IF _achou_reg_gm EQ abap_false.
              CLEAR: l_has_been_summed.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

      IF l_has_been_summed EQ 'X'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING ls_dre_values TO ls_soma_saldo_r.

      ls_soma_saldo_r-vlhsl  = ls_dre_values-vlr_rea.
      ls_soma_saldo_r-vlksl  = ls_dre_values-vlr_dolar.
      ls_soma_saldo_r-vlosl  = ls_dre_values-vlr_grupo.

      COLLECT ls_soma_saldo_r INTO et_sum_values.
    ENDLOOP.

  ENDMETHOD.

  METHOD if_load_account_balance~build_account_balance.
    "Estrutura base balanço
    DATA: lt_niveis       TYPE ztt_zglt047,
          lt_balance_base TYPE ace_generic_range_t,
          lt_note_base    TYPE ace_generic_range_t.
    "Centros de custo balanço
    DATA: lt_balance_cost_center  TYPE ace_generic_range_t,
          lt_note_cost_center     TYPE ace_generic_range_t,
          lt_type_costs_center    TYPE ace_generic_range_t,
          lt_accounts_cost_center TYPE ztt_zglt041,
          lt_zglt049c             TYPE zglt049c_tt.
    "Centros de lucro balanço
    DATA: lt_balance_profit_center  TYPE ace_generic_range_t,
          lt_note_profit_center     TYPE ace_generic_range_t,
          lt_profit_center          TYPE ace_generic_range_t,
          lt_account_aerea          TYPE ace_generic_range_t,
          lt_accounts_profit_center TYPE ztt_zglt041,
          lt_zglt049l               TYPE zglt049l_tt.
    "Grupo de mercadoria balanço
    DATA: lt_balance_material_group  TYPE ace_generic_range_t,
          lt_note_material_group     TYPE ace_generic_range_t,
          lt_material_group          TYPE ace_generic_range_t,
          lt_accounts_material_group TYPE ztt_zglt041,
          lt_zglt049m                TYPE zglt049m_tt.
    "Contas contábeis conta do razão
    DATA: lt_balance_account TYPE ace_generic_range_t,
          lt_note_account    TYPE ace_generic_range_t,
          lt_accounts_reason TYPE ztt_zglt041.
    "Saldo DRE
    DATA: lt_saldo_dre_centro_custo   TYPE ztt_zgl029_dre_dados,
          lt_saldo_dre_centro_lucro   TYPE ztt_zgl029_dre_dados,
          lt_saldo_dre_grupo_material TYPE ztt_zgl029_dre_dados,
          lt_saldo_dre_razao          TYPE ztt_zgl029_dre_dados.
    "Saldo DRE sumarizado
    DATA: lt_soma_saldo_cc TYPE zglt_dre_cc_tt,
          lt_soma_saldo_cl TYPE zglt_dre_cl_tt,
          lt_soma_saldo_gm TYPE zglt_dre_gm_tt,
          lt_soma_saldo_r  TYPE zglt_dre_r_tt.
    "Texto contas
    DATA: lt_contas TYPE ztt_zglt041.
    "Estruturas
    DATA: ls_saldo_contas TYPE zsco_saldo_contas.

    DATA: lt_centro_custos_processados TYPE zglt_conta_centro_custo_tt,
          lt_centro_lucros_processados TYPE zglt_conta_centro_lucro_tt,
          lt_grupo_merca_processados   TYPE zglt_conta_grupo_mercadoria_tt.

    "Retornar todos os códigos de classificação de balanço e classificação de nota para uma estrutura de balanço
    me->load_balance_note_base( EXPORTING
                                iv_versn   = 'DRE1'
                              IMPORTING
                                et_niveis  = lt_niveis
                                et_balance = lt_balance_base
                                et_note    = lt_note_base ).

    "Com todos os códigos de classificação de balanço e classificação de nota para uma estrutura de balanço, busca
    "os códigos de balanço e nota que estejam atrelados a um centro de custo
    me->load_balance_note_cost_center( EXPORTING
                                      it_balance = lt_balance_base
                                      it_note    = lt_note_base
                                     IMPORTING
                                       et_balance_cost_center = lt_balance_cost_center
                                       et_note_cost_center    = lt_note_cost_center
                                       et_type_cost_center    = lt_type_costs_center
                                       et_zglt049c            = lt_zglt049c ).

    "Busca as contas contábeis da estrutura de balanço que estejam atreladas a centro de custos
    lt_accounts_cost_center = me->load_accounts_zglt041( EXPORTING
                                                        iv_classificacao_balanco = lt_balance_cost_center
                                                        iv_classificacao_nota    = lt_note_cost_center ).

    "Com todos os códigos de classificação de balanço e classificação de nota para uma estrutura de balanço, busca
    "os códigos de balanço e nota que estejam atrelados a um centro de lucro
    me->load_balance_note_profit_c( EXPORTING
                                    it_balance = lt_balance_base
                                    it_note    = lt_note_base
                                  IMPORTING
                                    et_balance_profit_center = lt_balance_profit_center
                                    et_note_profit_center    = lt_note_profit_center
                                    et_profit_center         = lt_profit_center
                                    et_account_aerea         = lt_account_aerea
                                    et_zglt049l              = lt_zglt049l ).

    "Busca as contas contábeis da estrutura de balanço que estejam atreladas a centro de lucros
    lt_accounts_profit_center = me->load_accounts_zglt041( EXPORTING
                                                          iv_classificacao_balanco = lt_balance_profit_center
                                                          iv_classificacao_nota    = lt_note_profit_center ).

    "Com todos os códigos de classificação de balanço e classificação de nota para uma estrutura de balanço, busca
    "os códigos de balanço e nota que estejam atrelados a um grupo de mercadoria
    me->load_balance_note_mat_group( EXPORTING
                                    it_balance = lt_balance_base
                                    it_note    = lt_note_base
                                   IMPORTING
                                    et_balance_material_group = lt_balance_material_group
                                    et_note_material_group    = lt_note_material_group
                                    et_material_group         = lt_material_group
                                    et_zglt049m               = lt_zglt049m ) .

    "Busca as contas contábeis da estrutura de balanço que estejam atreladas a grupo de mercadoria
    lt_accounts_material_group = me->load_accounts_zglt041( EXPORTING
                                                          iv_classificacao_balanco = lt_balance_material_group
                                                          iv_classificacao_nota    = lt_note_material_group ).

    APPEND LINES OF: lt_balance_cost_center    TO lt_balance_account,
                   lt_balance_profit_center  TO lt_balance_account,
                   lt_balance_material_group TO lt_balance_account.

    APPEND LINES OF: lt_note_cost_center       TO lt_note_account,
                   lt_note_profit_center     TO lt_note_account,
                   lt_note_material_group    TO lt_note_account.

    LOOP AT lt_balance_account ASSIGNING FIELD-SYMBOL(<balance_account>).
      <balance_account>-option = 'NE'.
    ENDLOOP.

    LOOP AT lt_note_account ASSIGNING FIELD-SYMBOL(<note_account>).
      <note_account>-option = 'NE'.
    ENDLOOP.

    "Buscar todas as notas existentes que não foram classificadas  como Centro de custo, centro de lucro ou material.
    lt_accounts_reason = me->load_accounts_zglt041( EXPORTING
                                                   iv_classificacao_balanco = lt_balance_account
                                                   iv_classificacao_nota    = lt_note_account ).

    "Centro de custo
    me->load_saldo_mensal_dre( EXPORTING
                              it_accounts   = lt_accounts_cost_center
                              it_kosar      = lt_type_costs_center
                            IMPORTING
                              et_dre_values = lt_saldo_dre_centro_custo ).

    "Centro de lucro
    me->load_saldo_mensal_dre( EXPORTING
                              it_accounts   = lt_accounts_profit_center
                              it_prctr      = lt_profit_center
                            IMPORTING
                              et_dre_values = lt_saldo_dre_centro_lucro ).

    "Grupo de mercadoria
    me->load_saldo_mensal_dre( EXPORTING
                              it_accounts   = lt_accounts_material_group
                              it_matkl      = lt_material_group
                            IMPORTING
                              et_dre_values = lt_saldo_dre_grupo_material ).

    "Razão
    me->load_saldo_mensal_dre( EXPORTING
                              it_accounts   = lt_accounts_reason
                            IMPORTING
                              et_dre_values = lt_saldo_dre_razao ).

    "Sumarizar valores centro de custo
    me->sum_values_centro_custo( EXPORTING
                                it_dre_values      = lt_saldo_dre_centro_custo
                               IMPORTING
                                et_sum_values = lt_soma_saldo_cc ).

    "Sumarizar valores centro de lucro
    me->sum_values_centro_lucro( EXPORTING
                                it_dre_values = lt_saldo_dre_centro_lucro
                               IMPORTING
                                et_sum_values = lt_soma_saldo_cl ).

    "Sumarizar valores grupo de mercadoria
    me->sum_values_grupo_mercadoria( EXPORTING
                                    it_dre_values = lt_saldo_dre_grupo_material
                                   IMPORTING
                                    et_sum_values = lt_soma_saldo_gm ).

    APPEND LINES OF: lt_accounts_cost_center    TO lt_contas,
                   lt_accounts_profit_center  TO lt_contas,
                   lt_accounts_material_group TO lt_contas,
                   lt_accounts_reason         TO lt_contas.

    SORT lt_contas BY saknr.
    DELETE ADJACENT DUPLICATES FROM lt_contas COMPARING saknr.

    IF lt_contas IS NOT INITIAL.
      SELECT saknr, txt50, spras
        FROM skat
        INTO TABLE @DATA(lt_texto_contas)
        FOR ALL ENTRIES IN @lt_contas
        WHERE spras IN ( 'E', 'P' )
        AND   ktopl = '0050'
        AND   saknr = @lt_contas-saknr.
      IF sy-subrc IS INITIAL.
        SORT lt_texto_contas BY saknr spras.
      ENDIF.

      SELECT *
        FROM zglt041
        INTO TABLE @DATA(lt_zglt041)
        FOR ALL ENTRIES IN @lt_contas
        WHERE bukrs IN @me->gt_empresas
        AND   saknr = @lt_contas-saknr.
      IF sy-subrc IS INITIAL.

        SELECT *
          FROM zglt049
          INTO TABLE @DATA(lt_zglt049)
          FOR ALL ENTRIES IN @lt_zglt041
          WHERE versn        = 'DRE1'
          AND   cod_clas_bal = @lt_zglt041-cod_clas_bal
          AND   cod_clas_not = @lt_zglt041-cod_clas_not2.
        IF sy-subrc IS INITIAL.

          SORT: lt_zglt041       BY bukrs saknr,
                lt_zglt049       BY cod_clas_bal cod_clas_not,
                lt_niveis        BY versn nivel,
                lt_zglt049c      BY cod_clas_bal cod_clas_not kosar,
                lt_soma_saldo_cc BY bukrs saknr.

        ENDIF.

      ENDIF.

    ENDIF.

    SELECT *
    FROM setleaf
    INTO TABLE @DATA(lt_setleaft)
    WHERE setname = 'CONTAS_EC-CS'.

    LOOP AT lt_soma_saldo_cc INTO DATA(ls_soma_saldo_cc).

      CLEAR: ls_saldo_contas.

      ls_saldo_contas-rbukrs = ls_soma_saldo_cc-bukrs.
      ls_saldo_contas-ryear  = me->gv_ano.
      ls_saldo_contas-month  = me->gv_mes.
      ls_saldo_contas-kosar  = ls_soma_saldo_cc-kosar.

      READ TABLE lt_setleaft WITH KEY valfrom = ls_soma_saldo_cc-saknr TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: ls_soma_saldo_cc-vbund.
      ENDIF.

      READ TABLE lt_zglt041 WITH KEY bukrs = ls_soma_saldo_cc-bukrs
                                     saknr = ls_soma_saldo_cc-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_zglt041 INTO DATA(ls_zglt041) FROM sy-tabix.
          IF ls_zglt041-saknr NE ls_soma_saldo_cc-saknr.
            EXIT.
          ENDIF.

          READ TABLE lt_zglt049c INTO DATA(ls_zglt049c) WITH KEY cod_clas_bal = ls_zglt041-cod_clas_bal
                                                                 cod_clas_not = ls_zglt041-cod_clas_not2
                                                                 kosar        = ls_soma_saldo_cc-kosar BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_zglt049 INTO DATA(ls_zglt049) WITH KEY cod_clas_bal = ls_zglt049c-cod_clas_bal
                                                                 cod_clas_not = ls_zglt049c-cod_clas_not BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_saldo_contas-cod_clas_not = ls_zglt049-cod_clas_not.
              READ TABLE lt_niveis INTO DATA(ls_niveis) WITH KEY versn = 'DRE1'
                                                                 nivel = ls_zglt049-nivel BINARY SEARCH.
              IF sy-subrc IS INITIAL.
*                IF ls_niveis-nivelpai IS NOT INITIAL.
*                  CONCATENATE ls_soma_saldo_cc-saknr '-' ls_niveis-nivelpai '-' ls_soma_saldo_cc-vbund INTO ls_saldo_contas-racct SEPARATED BY space.
*                  ls_saldo_contas-nivel = ls_niveis-nivelpai.
*                  EXIT.
*                ELSE.
                CONCATENATE ls_soma_saldo_cc-saknr '-' ls_zglt049-nivel '-' ls_soma_saldo_cc-vbund  INTO ls_saldo_contas-racct SEPARATED BY space.
                ls_saldo_contas-nivel = ls_zglt049-nivel.
                EXIT.
                "ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF ls_saldo_contas-racct IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_texto_contas INTO DATA(ls_text_contas) WITH KEY saknr = ls_soma_saldo_cc-saknr
                                                                    spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_cc-saknr
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-rassc         = ls_soma_saldo_cc-vbund.

      ls_saldo_contas-moeda_interna = ls_soma_saldo_cc-vlhsl.

      IF ls_saldo_contas-moeda_interna >= 0.
        ls_saldo_contas-dc_interna = 'D'.
      ELSE.
        ls_saldo_contas-dc_interna = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_forte = ls_soma_saldo_cc-vlksl.

      IF ls_saldo_contas-moeda_forte >= 0.
        ls_saldo_contas-dc_forte = 'D'.
      ELSE.
        ls_saldo_contas-dc_forte = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_indice = ls_soma_saldo_cc-vlosl.

      IF ls_saldo_contas-moeda_indice >= 0.
        ls_saldo_contas-dc_indice = 'D'.
      ELSE.
        ls_saldo_contas-dc_indice = 'C'.
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      APPEND ls_saldo_contas TO rt_saldo_contas.
      APPEND VALUE #( bukrs = ls_saldo_contas-rbukrs saknr = ls_saldo_contas-racct kosar = ls_saldo_contas-kosar  ) TO lt_centro_custos_processados.
    ENDLOOP.

    SORT lt_zglt049l BY cod_clas_bal cod_clas_not prctr.

    LOOP AT lt_soma_saldo_cl INTO DATA(ls_soma_saldo_cl).

      CLEAR: ls_saldo_contas.

      ls_saldo_contas-rbukrs = ls_soma_saldo_cl-bukrs.
      ls_saldo_contas-ryear  = me->gv_ano.
      ls_saldo_contas-month  = me->gv_mes.
      ls_saldo_contas-prctr  = ls_soma_saldo_cl-prctr.

      READ TABLE lt_setleaft WITH KEY valfrom = ls_soma_saldo_cl-saknr TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: ls_soma_saldo_cl-vbund.
      ENDIF.

      READ TABLE lt_zglt041 WITH KEY bukrs = ls_soma_saldo_cl-bukrs
                                     saknr = ls_soma_saldo_cl-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_zglt041 INTO ls_zglt041 FROM sy-tabix.
          IF ls_zglt041-saknr NE ls_soma_saldo_cl-saknr.
            EXIT.
          ENDIF.

          READ TABLE lt_zglt049l INTO DATA(ls_zglt049l) WITH KEY cod_clas_bal = ls_zglt041-cod_clas_bal
                                                                 cod_clas_not = ls_zglt041-cod_clas_not2
                                                                 prctr        = ls_soma_saldo_cl-prctr BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_zglt049 INTO ls_zglt049 WITH KEY cod_clas_bal = ls_zglt049l-cod_clas_bal
                                                           cod_clas_not = ls_zglt049l-cod_clas_not BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_saldo_contas-cod_clas_not = ls_zglt049-cod_clas_not.
              READ TABLE lt_niveis INTO ls_niveis WITH KEY versn = 'DRE1'
                                                           nivel = ls_zglt049-nivel BINARY SEARCH.
              IF sy-subrc IS INITIAL.
*                IF ls_niveis-nivelpai IS NOT INITIAL.
*                  CONCATENATE ls_soma_saldo_cl-saknr '-' ls_niveis-nivelpai '-' ls_soma_saldo_cl-vbund INTO ls_saldo_contas-racct SEPARATED BY space.
*                  ls_saldo_contas-nivel = ls_niveis-nivelpai.
*                  EXIT.
*                ELSE.
                CONCATENATE ls_soma_saldo_cl-saknr '-' ls_zglt049-nivel '-' ls_soma_saldo_cl-vbund  INTO ls_saldo_contas-racct SEPARATED BY space.
                ls_saldo_contas-nivel = ls_zglt049-nivel.
                EXIT.
                "ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF ls_saldo_contas-racct IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_cl-saknr
                                                              spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_cl-saknr
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-rassc         = ls_soma_saldo_cl-vbund.

      ls_saldo_contas-moeda_interna = ls_soma_saldo_cl-vlhsl.

      IF ls_saldo_contas-moeda_interna >= 0.
        ls_saldo_contas-dc_interna = 'D'.
      ELSE.
        ls_saldo_contas-dc_interna = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_forte = ls_soma_saldo_cl-vlksl.

      IF ls_saldo_contas-moeda_forte >= 0.
        ls_saldo_contas-dc_forte = 'D'.
      ELSE.
        ls_saldo_contas-dc_forte = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_indice = ls_soma_saldo_cl-vlosl.

      IF ls_saldo_contas-moeda_indice >= 0.
        ls_saldo_contas-dc_indice = 'D'.
      ELSE.
        ls_saldo_contas-dc_indice = 'C'.
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      APPEND ls_saldo_contas TO rt_saldo_contas.
      APPEND VALUE #( bukrs = ls_saldo_contas-rbukrs saknr = ls_saldo_contas-racct prctr = ls_saldo_contas-prctr  ) TO lt_centro_lucros_processados.
    ENDLOOP.

    SORT lt_zglt049m BY cod_clas_bal cod_clas_not matkl.

    LOOP AT lt_soma_saldo_gm INTO DATA(ls_soma_saldo_gm).

      CLEAR: ls_saldo_contas.

      ls_saldo_contas-rbukrs = ls_soma_saldo_gm-bukrs.
      ls_saldo_contas-ryear  = me->gv_ano.
      ls_saldo_contas-month  = me->gv_mes.
      ls_saldo_contas-matkl  = ls_soma_saldo_gm-matkl.

      READ TABLE lt_setleaft WITH KEY valfrom = ls_soma_saldo_gm-saknr TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: ls_soma_saldo_gm-vbund.
      ENDIF.

      READ TABLE lt_zglt041 WITH KEY bukrs = ls_soma_saldo_gm-bukrs
                                     saknr = ls_soma_saldo_gm-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_zglt041 INTO ls_zglt041 FROM sy-tabix.
          IF ls_zglt041-saknr NE ls_soma_saldo_gm-saknr.
            EXIT.
          ENDIF.

          READ TABLE lt_zglt049m INTO DATA(ls_zglt049m) WITH KEY cod_clas_bal = ls_zglt041-cod_clas_bal
                                                                 cod_clas_not = ls_zglt041-cod_clas_not2
                                                                 matkl        = ls_soma_saldo_gm-matkl BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_zglt049 INTO ls_zglt049 WITH KEY cod_clas_bal = ls_zglt049m-cod_clas_bal
                                                           cod_clas_not = ls_zglt049m-cod_clas_not BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_saldo_contas-cod_clas_not = ls_zglt049-cod_clas_not.
              READ TABLE lt_niveis INTO ls_niveis WITH KEY versn = 'DRE1'
                                                           nivel = ls_zglt049-nivel BINARY SEARCH.
              IF sy-subrc IS INITIAL.
*                IF ls_niveis-nivelpai IS NOT INITIAL.
*                  CONCATENATE ls_soma_saldo_gm-saknr '-' ls_niveis-nivelpai '-' ls_soma_saldo_gm-vbund INTO ls_saldo_contas-racct SEPARATED BY space.
*                  ls_saldo_contas-nivel = ls_niveis-nivelpai.
*                  EXIT.
*                ELSE.
                CONCATENATE ls_soma_saldo_gm-saknr '-' ls_zglt049-nivel '-' ls_soma_saldo_gm-vbund  INTO ls_saldo_contas-racct SEPARATED BY space.
                ls_saldo_contas-nivel = ls_zglt049-nivel.
                EXIT.
                "ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF ls_saldo_contas-racct IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_gm-saknr
                                                              spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_gm-saknr
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-rassc         = ls_soma_saldo_gm-vbund.

      ls_saldo_contas-moeda_interna = ls_soma_saldo_gm-vlhsl.

      IF ls_saldo_contas-moeda_interna >= 0.
        ls_saldo_contas-dc_interna = 'D'.
      ELSE.
        ls_saldo_contas-dc_interna = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_forte = ls_soma_saldo_gm-vlksl.

      IF ls_saldo_contas-moeda_forte >= 0.
        ls_saldo_contas-dc_forte = 'D'.
      ELSE.
        ls_saldo_contas-dc_forte = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_indice = ls_soma_saldo_gm-vlosl.

      IF ls_saldo_contas-moeda_indice >= 0.
        ls_saldo_contas-dc_indice = 'D'.
      ELSE.
        ls_saldo_contas-dc_indice = 'C'.
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      APPEND ls_saldo_contas TO rt_saldo_contas.
      APPEND VALUE #( bukrs = ls_saldo_contas-rbukrs saknr = ls_saldo_contas-racct matkl = ls_saldo_contas-matkl ) TO lt_grupo_merca_processados.
    ENDLOOP.

    "Sumarizar valores razão
    SORT: lt_centro_custos_processados BY bukrs saknr kosar,
          lt_centro_lucros_processados BY bukrs saknr prctr,
          lt_grupo_merca_processados   BY bukrs saknr matkl.

    me->sum_values_razao( EXPORTING
                           it_dre_values      = lt_saldo_dre_razao
                           it_accounts_reason = lt_accounts_reason
                           it_zglt049c        = lt_zglt049c
                           it_zglt049l        = lt_zglt049l
                           it_zglt049m        = lt_zglt049m
                           it_cc_processado   = lt_centro_custos_processados
                           it_cl_processado   = lt_centro_lucros_processados
                           it_gm_processado   = lt_grupo_merca_processados
                          IMPORTING
                           et_sum_values      = lt_soma_saldo_r ).

    SORT lt_zglt041 BY bukrs saknr cod_clas_bal cod_clas_not2.

    LOOP AT lt_soma_saldo_r INTO DATA(ls_soma_saldo_r).

      CLEAR: ls_saldo_contas.

      ls_saldo_contas-rbukrs = ls_soma_saldo_r-bukrs.
      ls_saldo_contas-ryear  = me->gv_ano.
      ls_saldo_contas-month  = me->gv_mes.

      READ TABLE lt_setleaft WITH KEY valfrom = ls_soma_saldo_r-saknr TRANSPORTING NO FIELDS.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: ls_soma_saldo_r-vbund.
      ENDIF.

      READ TABLE lt_zglt041 WITH KEY bukrs = ls_soma_saldo_r-bukrs
                                     saknr = ls_soma_saldo_r-saknr TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_zglt041 INTO ls_zglt041 FROM sy-tabix.
          IF ls_zglt041-saknr NE ls_soma_saldo_r-saknr.
            EXIT.
          ENDIF.

          READ TABLE lt_zglt049 INTO ls_zglt049 WITH KEY cod_clas_bal = ls_zglt041-cod_clas_bal
                                                         cod_clas_not = ls_zglt041-cod_clas_not2 BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_saldo_contas-cod_clas_not = ls_zglt049-cod_clas_not.
            READ TABLE lt_niveis INTO ls_niveis WITH KEY versn = 'DRE1'
                                                         nivel = ls_zglt049-nivel BINARY SEARCH.
            IF sy-subrc IS INITIAL.
*              IF ls_niveis-nivelpai IS NOT INITIAL.
*                CONCATENATE ls_soma_saldo_r-saknr '-' ls_niveis-nivelpai '-' ls_soma_saldo_r-vbund INTO ls_saldo_contas-racct SEPARATED BY space.
*                ls_saldo_contas-nivel = ls_niveis-nivelpai.
*                EXIT.
*              ELSE.
              CONCATENATE ls_soma_saldo_r-saknr '-' ls_zglt049-nivel '-' ls_soma_saldo_r-vbund  INTO ls_saldo_contas-racct SEPARATED BY space.
              ls_saldo_contas-nivel = ls_zglt049-nivel.
              EXIT.
              "ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.

      IF ls_saldo_contas-racct IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_r-saknr
                                                              spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_soma_saldo_r-saknr
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-rassc         = ls_soma_saldo_r-vbund.

      ls_saldo_contas-moeda_interna = ls_soma_saldo_r-vlhsl.

      IF ls_saldo_contas-moeda_interna >= 0.
        ls_saldo_contas-dc_interna = 'D'.
      ELSE.
        ls_saldo_contas-dc_interna = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_forte = ls_soma_saldo_r-vlksl.

      IF ls_saldo_contas-moeda_forte >= 0.
        ls_saldo_contas-dc_forte = 'D'.
      ELSE.
        ls_saldo_contas-dc_forte = 'C'.
      ENDIF.

      ls_saldo_contas-moeda_indice = ls_soma_saldo_r-vlosl.

      IF ls_saldo_contas-moeda_indice >= 0.
        ls_saldo_contas-dc_indice = 'D'.
      ELSE.
        ls_saldo_contas-dc_indice = 'C'.
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      APPEND ls_saldo_contas TO rt_saldo_contas.
    ENDLOOP.

* Lógica de processamento abaixo é uma inclusão de contas de ativo e passivo dentro do processamento das contas de resultado

    DATA: lt_saldo_moeda_interna TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_forte   TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_indice  TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    DATA: lt_saldo_moeda_interna_line TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_forte_line   TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext,
          lt_saldo_moeda_indice_line  TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    DATA: lt_moedas_totais TYPE STANDARD TABLE OF zde_fi_gl_saldo_faglflext.

    LOOP AT me->gt_contas_busca INTO DATA(ls_contas_busca).

      FREE: lt_saldo_moeda_interna_line, lt_saldo_moeda_forte_line, lt_saldo_moeda_indice_line.

      CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
        EXPORTING
          ryear                = me->gv_ano
          contas               = ls_contas_busca-contas
          p_gerar_todas        = 'X'
          p_gerar_soc_parceira = ls_contas_busca-gerar_soc_parceira
          rldnr                = '0L'
        TABLES
          it_saldos            = lt_saldo_moeda_interna_line
          it_saldos_2          = lt_saldo_moeda_forte_line
          it_saldos_3          = lt_saldo_moeda_indice_line
        EXCEPTIONS
          moeda_nao_adm        = 1
          erro_ledger          = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT lt_saldo_moeda_interna_line INTO DATA(ls_saldo_moeda_interna_line).
        IF ls_saldo_moeda_interna_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_interna WITH KEY rbukrs = ls_saldo_moeda_interna_line-rbukrs
                                                     racct  = ls_saldo_moeda_interna_line-racct
                                                     rassc  = ls_saldo_moeda_interna_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_interna_line TO lt_saldo_moeda_interna.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_interna WITH KEY rbukrs = ls_saldo_moeda_interna_line-rbukrs
                                                     racct  = ls_saldo_moeda_interna_line-racct
                                                     rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_interna_line TO lt_saldo_moeda_interna.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_saldo_moeda_forte_line INTO DATA(ls_saldo_moeda_forte_line).
        IF ls_saldo_moeda_forte_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_forte WITH KEY rbukrs = ls_saldo_moeda_forte_line-rbukrs
                                                   racct  = ls_saldo_moeda_forte_line-racct
                                                   rassc  = ls_saldo_moeda_forte_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_forte_line TO lt_saldo_moeda_forte.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_forte WITH KEY rbukrs = ls_saldo_moeda_forte_line-rbukrs
                                                   racct  = ls_saldo_moeda_forte_line-racct
                                                   rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_forte_line TO lt_saldo_moeda_forte.
          ENDIF.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_saldo_moeda_indice_line INTO DATA(ls_saldo_moeda_indice_line).
        IF ls_saldo_moeda_indice_line-rassc IS NOT INITIAL.
          READ TABLE lt_saldo_moeda_indice WITH KEY rbukrs = ls_saldo_moeda_indice_line-rbukrs
                                                    racct  = ls_saldo_moeda_indice_line-racct
                                                    rassc  = ls_saldo_moeda_indice_line-rassc TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_indice_line TO lt_saldo_moeda_indice.
          ENDIF.
        ELSE.
          READ TABLE lt_saldo_moeda_indice WITH KEY rbukrs = ls_saldo_moeda_indice_line-rbukrs
                                                    racct  = ls_saldo_moeda_indice_line-racct
                                                    rassc  = space TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            APPEND ls_saldo_moeda_indice_line TO lt_saldo_moeda_indice.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    APPEND LINES OF: lt_saldo_moeda_interna TO lt_moedas_totais,
                     lt_saldo_moeda_forte   TO lt_moedas_totais,
                     lt_saldo_moeda_indice  TO lt_moedas_totais.

    IF lt_moedas_totais[] IS NOT INITIAL.
      SORT lt_moedas_totais BY racct.
      DELETE ADJACENT DUPLICATES FROM lt_moedas_totais COMPARING racct.

      SELECT saknr txt50 spras
        FROM skat
        INTO TABLE lt_texto_contas
        FOR ALL ENTRIES IN lt_moedas_totais
        WHERE spras IN ( 'E', 'P' )
        AND   ktopl = '0050'
        AND   saknr = lt_moedas_totais-racct.
      IF sy-subrc IS INITIAL.
        SORT lt_texto_contas BY saknr spras.
      ENDIF.

      SELECT *
        FROM zglt041
        INTO TABLE @DATA(lt_zglt041_passivos)
        FOR ALL ENTRIES IN @lt_moedas_totais
        WHERE bukrs IN @me->gt_empresas
        AND   saknr = @lt_moedas_totais-racct
    ORDER BY PRIMARY KEY .

      IF sy-subrc IS INITIAL.
        SORT lt_zglt041 BY bukrs saknr.
      ENDIF.

    ENDIF.

    SORT: lt_saldo_moeda_interna BY rbukrs racct rassc,
          lt_saldo_moeda_forte   BY rbukrs racct rassc,
          lt_saldo_moeda_indice  BY rbukrs racct rassc.

    LOOP AT lt_saldo_moeda_interna INTO DATA(ls_moeda_interna).
      CLEAR: ls_saldo_contas.

      MOVE-CORRESPONDING ls_moeda_interna TO ls_saldo_contas.

      CONCATENATE ls_moeda_interna-racct '-' ls_moeda_interna-rassc INTO ls_saldo_contas-racct.
*      IF ls_moeda_interna-rassc IS NOT INITIAL.
*        ls_saldo_contas-nivel = ls_moeda_interna-rassc.
*      ENDIF.


      ls_saldo_contas-month = me->gv_mes.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_moeda_interna-racct
                                                              spras = 'P' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_pt = ls_text_contas-txt50.
      ENDIF.

      READ TABLE lt_texto_contas INTO ls_text_contas WITH KEY saknr = ls_moeda_interna-racct
                                                              spras = 'E' BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-txt50_en = ls_text_contas-txt50.
      ENDIF.

      ls_saldo_contas-moeda_interna = me->sum_values_per_month( EXPORTING
                                                                 iv_saldo_contas = ls_moeda_interna
                                                                IMPORTING
                                                                 ev_indicador_dc = ls_saldo_contas-dc_interna ).

      READ TABLE lt_saldo_moeda_forte INTO DATA(ls_saldo_moeda_forte) WITH KEY rbukrs = ls_moeda_interna-rbukrs
                                                                               racct  = ls_moeda_interna-racct
                                                                               rassc  = ls_moeda_interna-rassc BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-moeda_forte = me->sum_values_per_month( EXPORTING
                                                                 iv_saldo_contas = ls_saldo_moeda_forte
                                                                IMPORTING
                                                                 ev_indicador_dc = ls_saldo_contas-dc_forte ).
      ENDIF.

      READ TABLE lt_saldo_moeda_indice INTO DATA(ls_saldo_moeda_indice) WITH KEY rbukrs = ls_moeda_interna-rbukrs
                                                                                 racct  = ls_moeda_interna-racct
                                                                                 rassc  = ls_moeda_interna-rassc BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-moeda_indice = me->sum_values_per_month( EXPORTING
                                                                  iv_saldo_contas = ls_saldo_moeda_indice
                                                                 IMPORTING
                                                                  ev_indicador_dc = ls_saldo_contas-dc_indice ).
      ENDIF.

      READ TABLE lt_zglt041_passivos INTO DATA(ls_zglt041_passivos) WITH KEY bukrs = ls_moeda_interna-rbukrs
                                                                             saknr = ls_moeda_interna-racct BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_saldo_contas-cod_clas_not = ls_zglt041_passivos-cod_clas_not2.
      ENDIF.

      READ TABLE me->gt_bsis WITH KEY bukrs = ls_moeda_interna-rbukrs
                                      hkont = ls_moeda_interna-racct TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT me->gt_bsis INTO DATA(ls_bsis) FROM sy-tabix.
          DATA(l_tabix) = sy-tabix.

          IF ls_bsis-hkont NE ls_moeda_interna-racct.
            EXIT.
          ENDIF.

          ls_saldo_contas-moeda_interna = ls_saldo_contas-moeda_interna - ls_bsis-dmbtr.
          ls_saldo_contas-moeda_forte   = ls_saldo_contas-moeda_forte   - ls_bsis-dmbe2.
          ls_saldo_contas-moeda_indice  = ls_saldo_contas-moeda_indice  - ls_bsis-dmbe3.

          DELETE me->gt_bsis INDEX l_tabix.
        ENDLOOP.
      ENDIF.

      IF ls_saldo_contas-moeda_interna IS INITIAL AND ls_saldo_contas-moeda_forte IS INITIAL AND ls_saldo_contas-moeda_indice IS INITIAL. "Não apresentar contas com saldo zerado.
        CONTINUE.
      ENDIF.

      CASE ls_saldo_contas-rbukrs.
        WHEN '0101'.
          ls_saldo_contas-moeda_interna = ls_saldo_contas-moeda_interna * 100.
      ENDCASE.

      APPEND ls_saldo_contas TO rt_saldo_contas.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    me->gv_ano        = iv_ano.
    me->gv_mes        = iv_mes.
    me->gt_empresas[] = iv_empresas[].

    IF iv_mes = 12.
      APPEND VALUE #( sign = 'I' option = 'BT' low = '01' high = '15' ) TO  me->gv_meses.
      me->gv_meses_i = iv_mes + 3.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'BT' low = '01' high = iv_mes ) TO  me->gv_meses.
      me->gv_meses_i = iv_mes.
    ENDIF.

    me->set_contas_busca( ).

    IF iv_mes = 12.
      SELECT *
        FROM zglt099
        INTO TABLE me->gt_zglt099
        WHERE bukrs IN me->gt_empresas
        AND   gjahr EQ iv_ano.
      IF sy-subrc IS INITIAL.
        SORT me->gt_zglt099 BY bukrs hkont gjahr belnr.

        SELECT bukrs hkont belnr gjahr buzei
               dmbtr dmbe2 dmbe3 shkzg
          FROM bsis
          INTO TABLE me->gt_bsis
          FOR ALL ENTRIES IN me->gt_zglt099
          WHERE bukrs = me->gt_zglt099-bukrs
          AND   hkont = me->gt_zglt099-hkont
          AND   gjahr = me->gt_zglt099-gjahr
          AND   belnr = me->gt_zglt099-belnr.
        IF sy-subrc IS INITIAL.
          SORT me->gt_bsis BY bukrs hkont.

          LOOP AT me->gt_bsis ASSIGNING FIELD-SYMBOL(<bsis>).
            IF <bsis>-shkzg EQ 'H'.
              <bsis>-dmbtr = <bsis>-dmbtr * -1.
              <bsis>-dmbe2 = <bsis>-dmbe2 * -1.
              <bsis>-dmbe3 = <bsis>-dmbe3 * -1.
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD sum_values_per_month.

    DATA: l_index TYPE numc2.

    ASSIGN COMPONENT 'SLVT' OF STRUCTURE iv_saldo_contas TO FIELD-SYMBOL(<saldo_anterior>).
    IF <saldo_anterior> IS ASSIGNED.
      ADD <saldo_anterior> TO rt_saldo.
    ENDIF.

    DO me->gv_meses_i TIMES.
      l_index = sy-index.

      CONCATENATE 'SL' l_index INTO DATA(l_fiedname).
      ASSIGN COMPONENT l_fiedname OF STRUCTURE iv_saldo_contas TO FIELD-SYMBOL(<saldo_mes_atual>).
      IF <saldo_mes_atual> IS ASSIGNED.
        ADD <saldo_mes_atual> TO rt_saldo.
      ENDIF.
    ENDDO.

    IF rt_saldo >= 0.
      ev_indicador_dc = 'D'.
    ELSE.
      ev_indicador_dc = 'C'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_saldo_contas_eventos IMPLEMENTATION. "Responsabilidade - Lidar com eventos do ALV de balancente contábil.

  METHOD on_function_code.

    CASE sy-ucomm.
      WHEN 'GERAR'.
        CALL FUNCTION 'Z_GERAR_EXCEL_BALANCETE'
          EXPORTING
            it_saldo_contas = gt_saldo_contas.
      WHEN 'INTEGRAR'.
        "Efetuar chamada de classe de integração.

** RJF - Ini - 2023.04.25 - CS2022001091 - Accountfy - API envio Balancetes - 96125
** --------------------------------------------------------
** Declaração
        "Nota.
        "Chamado cancelado, não sera mais necessario a implementação devido o cancelamento do contrato com o cliente Accountfy
** --------------------------------------------------------
*        TYPES: BEGIN OF ty_tab,
*                 line TYPE c LENGTH 1000,
*               END OF ty_tab.
*
*        TYPES: BEGIN OF ty_str,
*                 cnpj   TYPE stcd1,
*                 ano(6) TYPE c,
*                 str    TYPE string,
*                 file   TYPE string,
*                 filex  TYPE xstring,
*                 fsize  TYPE i,
*                 path   TYPE string,
*               END OF ty_str.
*
*        DATA :
*          lv_tp       TYPE c LENGTH 1000,
*          lt_itab     TYPE TABLE OF ty_tab,
*          ls_str      TYPE ty_str,
*          lv_filesize TYPE i,
*          flen        TYPE i,
*          xhead       TYPE xstring,
**          xtab        TYPE TABLE OF x255,
*          lv_xstr     TYPE xstring,
*          xtab        LIKE TABLE OF lv_tp,
*          lv_resposta TYPE c,
*          lo_zip      TYPE REF TO cl_abap_zip,
*          lv_str      TYPE string,
*          lv_line     TYPE i,
*          lo_int      TYPE REF TO zcl_int_ob_file_accountfy,
**          lt_data     TYPE STANDARD TABLE OF char255,
*          lo_str      TYPE REF TO zcl_string,
*          lt_saldox   TYPE zsco_saldo_contas_tt.
*        CONSTANTS: lc_path  TYPE string VALUE 'C:\TEMP\',
*                   lc_nfile TYPE string VALUE 'ARQ_BALANC.CSV',
*                   lc_zfile TYPE string VALUE 'ARQ_BALANC.ZIP'.
*
*        FREE: lt_itab[].
*
*        IF gt_saldo_contas[] IS NOT INITIAL. " Screen
*
*          lt_saldox[] = gt_saldo_contas[].
*          SORT lt_saldox[] BY rbukrs.
*          DELETE ADJACENT DUPLICATES FROM lt_saldox[] COMPARING rbukrs.
*          lv_line = lines( lt_saldox[] ).
*          IF lv_line EQ 1.
*
*            SELECT SINGLE stcd1 FROM j_1bbranch
*              INTO @DATA(lv_cnpj)
*              WHERE bukrs EQ @s_bukrs-low
*                AND branch EQ '0001'.
*            IF sy-subrc IS NOT INITIAL OR lv_cnpj IS INITIAL.
*              MESSAGE 'CNPJ não encontrado para integração!'(E03) TYPE 'E'.
*              EXIT.
*            ENDIF.
*
*            ls_str-ano   = |{ p_month }| && |{ p_year }|.
*            ls_str-cnpj  = lv_cnpj.
*
*          ELSEIF lv_line GT 1.
*            ls_str-cnpj  = 0.
*            ls_str-ano   = 0.
*          ELSE.
**            MESSAGE 'Integrar apenas uma empresa por vez!'(I02) TYPE 'I'.
*            MESSAGE 'Não existe informações para a geração do arquivo!'(I01) TYPE 'I'.
*            EXIT.
*          ENDIF.
*
*          IF ls_str-cnpj IS NOT INITIAL.
*            CONDENSE ls_str-cnpj NO-GAPS.
*          ENDIF.
** --------------------------------------------------------
** Processamento
** --------------------------------------------------------
*          TRY.
*              FREE gs_file_name.
*
*              IF ls_str-cnpj GT 0.
*                DATA: lv_txt TYPE string.
*                lv_txt = text-002.
*              ELSE.
*                lv_txt = text-005.
*              ENDIF.
*
*              FREE lv_resposta.
*              CALL FUNCTION 'POPUP_TO_CONFIRM'
*                EXPORTING
*                  titlebar              = 'Confirmar'(001)
*                  text_question         = lv_txt
*                  text_button_1         = 'Sim'(003)
*                  text_button_2         = 'Não'(004)
*                  default_button        = '2'
*                  display_cancel_button = ''
*                IMPORTING
*                  answer                = lv_resposta
*                EXCEPTIONS
*                  text_not_found        = 1
*                  OTHERS                = 2.
*              IF lv_resposta = '2'.
*                EXIT.
*              ENDIF.
*
**              CALL FUNCTION 'Z_GERAR_EXCEL_BALANCETE'
**                EXPORTING
**                  it_saldo_contas = gt_saldo_contas
**                  iv_autotp_moeda = abap_true
**                IMPORTING
**                  ev_file         = gs_file_name.
*
*              CALL FUNCTION 'Z_GERAR_CSV_BALANCETE'
*                EXPORTING
*                  it_saldo_contas = gt_saldo_contas
*                  iv_autotp_moeda = abap_true
*                IMPORTING
*                  ev_file         = gs_file_name.
*
*              IF sy-subrc IS INITIAL AND gs_file_name IS NOT INITIAL. "Created
*
*                cl_gui_frontend_services=>gui_upload( EXPORTING filename = gs_file_name
*                filetype = 'BIN'
*                IMPORTING filelength = flen
*                CHANGING data_tab = xtab ).
*
** --------------------------------------------------------
** Convert Binary to XString
** --------------------------------------------------------
*                CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*                  EXPORTING
*                    input_length = flen
*                  IMPORTING
*                    buffer       = xhead
*                  TABLES
*                    binary_tab   = xtab
*                  EXCEPTIONS
*                    failed       = 1
*                    OTHERS       = 2.
*
*                IF sy-subrc <> 0.
*                  MESSAGE 'Erro na geração do arquivo Balancete!'(E02) TYPE 'E'.
*                ENDIF.
*
*                lo_zip = NEW #( ). "Create instance of Zip Class
*
*                IF lo_zip IS BOUND.
*                  lo_zip->add(  name    = lc_nfile
*                                content = xhead ).
*
*                  xhead = lo_zip->save( ).
*                ENDIF.
*                lo_str = NEW #( ).
*
*                IF lo_str IS BOUND.
*                  lv_str = lo_str->xstring_to_string( xhead ).
*                ENDIF.
*
*                IF xhead IS NOT INITIAL.
*                  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*                    EXPORTING
*                      buffer        = xhead
*                    IMPORTING
*                      output_length = lv_filesize
*                    TABLES
*                      binary_tab    = lt_itab.
*
*                  gs_file_name = |{ lc_path }| && |{ ls_str-cnpj }| && |{ lc_zfile }|.
*
*                  cl_gui_frontend_services=>gui_download(
*                        EXPORTING
*                          bin_filesize              = lv_filesize
*                          filename                  = gs_file_name
*                          filetype                  = 'BIN'
*                        IMPORTING
*                          filelength                = lv_filesize
*                        CHANGING
*                          data_tab                  = lt_itab
*                        EXCEPTIONS  OTHERS       = 1 ).
*
*                  lv_xstr = zcl_abap_convert=>string_to_xstring_utf8_ok( iv_string  = lv_str ).
*
**lv_xstr = zcl_fmcall_handler=>convert_to_utf8( EXPORTING i_texto = lv_str ).
*
*                  IF lt_itab[] IS NOT INITIAL.
*                    LOOP AT lt_itab ASSIGNING FIELD-SYMBOL(<fs_itab>).
*                      IF sy-tabix EQ 1.
*                        lv_str = <fs_itab>-line.
*                      ELSE.
*                        lv_str = |{ lv_str }| && |{ <fs_itab>-line }|.
*                        CONDENSE lv_str NO-GAPS.
*                      ENDIF.
*                    ENDLOOP.
*
*                    ls_str-str   = lv_str.
**                    ls_str-filex = lv_xstr. "xhead.
*                    ls_str-filex = xhead.
*                    ls_str-file  = |{ ls_str-cnpj }| && |{ lc_zfile }|." lc_zfile.
*                    ls_str-fsize = lv_filesize.
*                    ls_str-path  = lc_path.
*
*                  ELSE.
*                    MESSAGE 'Erro na geração do arquivo Balancete!'(E02) TYPE 'E'.
*                    EXIT.
*                  ENDIF.
*                ELSE.
*                  MESSAGE 'Erro na geração do arquivo Balancete!'(E02) TYPE 'E'.
*                  EXIT.
*                ENDIF.
*
** --------------------------------------------------------
** Processamento Call class integrãção
** --------------------------------------------------------
*                TRY.
*                    DATA:
*                      ev_id_integracao TYPE  zde_id_integracao,
*                      ev_integracao    TYPE  zintegracao,
*                      ev_text          TYPE  string.
*
*                    lo_int = NEW #( ).
*
*                    IF lo_int IS BOUND.
*                      DATA(lv_int_ret) = lo_int->zif_integracao_outbound~execute_request( EXPORTING i_info_request  = ls_str
*                                                                                          IMPORTING e_id_integracao = ev_id_integracao
*                                                                                                    e_integracao    = ev_integracao ).
*                    ENDIF.
*
*                  CATCH zcx_integracao INTO DATA(ex_int).
*                    ev_text = ex_int->get_longtext( ).
*                    MESSAGE ev_text TYPE 'E'.
*                  CATCH zcx_error INTO DATA(ex_erro).
*                    ev_text = ex_erro->get_longtext( ).
*                    MESSAGE ev_text TYPE 'E'.
*                ENDTRY.
*
*                IF sy-subrc IS INITIAL AND lv_xstr IS NOT INITIAL AND ev_integracao-nm_code EQ '0200'.
*                  IF ls_str-cnpj GT 0.
*                    MESSAGE 'Integração arquivo Balancete realizada com sucesso!'(S02) TYPE 'S'.
*                  ELSE.
*                    MESSAGE 'Integração arquivo Multi Balancete realizada com sucesso!'(S03) TYPE 'S'.
*                  ENDIF.
*                ENDIF.
*              ELSE.
*                MESSAGE 'Erro na geração do arquivo Balancete!'(E02) TYPE 'E'.
*              ENDIF.
*
*            CATCH cx_sy_range_out_of_bounds.
*              MESSAGE 'Erro na geração/integração do arquivo Balancete!'(E03) TYPE 'E'.
*            CATCH cx_root.
*              MESSAGE 'Erro na geração/integração do arquivo Balancete!'(E03) TYPE 'E'.
*          ENDTRY.
*
*        ELSE.
*          MESSAGE 'Não existe informações para a geração do arquivo!'(I01) TYPE 'I'.
*        ENDIF.
** RJF - Fim - 2023.04.25 - CS2022001091 - Accountfy - API envio Balancetes - 96125

        "Nota.
        "Chamado cancelado, não sera mais necessario a implementação devido o cancelamento do contrato com o cliente Accountfy
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_builder IMPLEMENTATION. "Responsabilidade - Demonstrar ALV.

  METHOD constructor.
    RETURN.
  ENDMETHOD.

  METHOD build_alv.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->go_alv
          CHANGING
            t_table      = rt_table[] ).
      CATCH cx_salv_msg.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD config_alv_header.

    DATA: lo_label TYPE REF TO cl_salv_form_label,
          lo_flow  TYPE REF TO cl_salv_form_layout_flow.

    IF me->go_header IS NOT BOUND.
      CREATE OBJECT go_header.
    ENDIF.

    IF iv_label EQ 'X'.
      lo_label = me->go_header->create_label( row = iv_row  column = iv_column ).
      lo_label->set_text( iv_text ).
    ENDIF.

    IF iv_flow EQ 'X'.
      lo_flow = me->go_header->create_flow( row = iv_row  column = iv_column ).
      lo_flow->create_text( text = iv_text ).
    ENDIF.

    me->go_alv->set_top_of_list( me->go_header ).
    me->go_alv->set_top_of_list_print( me->go_header ).

  ENDMETHOD.

  METHOD display.

    CHECK me->go_alv IS BOUND.

    me->go_alv->display( ).

  ENDMETHOD.

  METHOD config_alv_functions.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

    DATA: lo_layout  TYPE REF TO cl_salv_layout,
          lf_variant TYPE slis_vari,
          ls_key     TYPE salv_s_layout_key.

    lo_functions = me->go_alv->get_functions( ).
    lo_functions->set_default( abap_true ).

    lo_layout = me->go_alv->get_layout( ).

    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).

    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

    lf_variant = 'DEFAULT'.
    lo_layout->set_initial_layout( lf_variant ).

    SET PF-STATUS iv_pf_status.

    me->go_alv->set_screen_status(
       pfstatus      = iv_pf_status
       report        = iv_report
       set_functions = iv_functions ).

  ENDMETHOD.

  METHOD config_columns.

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column_table,
          lo_display TYPE REF TO cl_salv_display_settings.

    lo_columns = me->go_alv->get_columns( ).
    lo_columns->set_optimize( iv_set_optimize ).

    lo_display = me->go_alv->get_display_settings( ).
    lo_display->set_striped_pattern( iv_set_striped_pattern ).
    lo_display->set_list_header( iv_set_list_header ).

    IF p_f01 IS NOT INITIAL.
      TRY.
          lo_column ?= lo_columns->get_column( columnname = 'KOSAR' ).
        CATCH cx_salv_not_found .
      ENDTRY.
      lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      TRY.
          lo_column ?= lo_columns->get_column( columnname = 'PRCTR' ).
        CATCH cx_salv_not_found .
      ENDTRY.
      lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      TRY.
          lo_column ?= lo_columns->get_column( columnname = 'MATKL' ).
        CATCH cx_salv_not_found .
      ENDTRY.
      lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).

      TRY.
          lo_column ?= lo_columns->get_column( columnname = 'COD_CLAS_NOT' ).
        CATCH cx_salv_not_found .
      ENDTRY.
      lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
    ENDIF.

  ENDMETHOD.

  METHOD config_events.

    DATA: lo_object TYPE REF TO lcl_alv_saldo_contas_eventos.

    DATA: lo_events TYPE REF TO cl_salv_events_table.

    CREATE OBJECT lo_object.

    lo_events = me->go_alv->get_event( ).

    SET HANDLER lo_object->on_function_code FOR lo_events.

  ENDMETHOD.

  METHOD config_sort_columns.

    DATA: lo_sort TYPE REF TO cl_salv_sorts.

    lo_sort = me->go_alv->get_sorts( ).

    lo_sort->add_sort( columnname = iv_column
                       sequence   = iv_sequence ).

  ENDMETHOD.

ENDCLASS.
