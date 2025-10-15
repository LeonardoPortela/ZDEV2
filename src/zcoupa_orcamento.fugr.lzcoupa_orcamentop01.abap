CLASS lcl_check_budget_k IMPLEMENTATION. "Verificar orçamento: Centro de Custo

  METHOD constructor.
    me->gs_parametros_orcamento = is_parametros_orcamento.
  ENDMETHOD.

  METHOD if_check_budget~check_budget_value.

    DATA: l_resultado   TYPE p DECIMALS 2,
          l_desvio_cost TYPE p DECIMALS 2.

    DATA(l_planned_cost) = me->get_planned_costs( ).

    DATA(l_real_cost)    = me->get_real_costs( ).

    l_desvio_cost  = l_planned_cost - l_real_cost.

    IF me->gs_parametros_orcamento-currency = 'BRL'.
      l_resultado = l_desvio_cost - me->gs_parametros_orcamento-total.
    ELSE.
      l_resultado = l_desvio_cost - ( me->gs_parametros_orcamento-total / lcl_co_utilities=>get_currency_value_brl( iv_currency = me->gs_parametros_orcamento-currency ) ).
    ENDIF.

    me->if_check_budget~set_return_for_budget_value( EXPORTING
                                                      iv_result   = CONV #( l_resultado )
                                                      iv_no_valid = 'X'
                                                     IMPORTING
                                                      ev_status   = ev_status
                                                      ev_saldo    = ev_saldo
                                                      ev_mensagem = ev_mensagem ).

  ENDMETHOD.

  METHOD get_planned_costs.

    DATA: lt_idx_structure TYPE STANDARD TABLE OF bapiacpstru,
          lt_object        TYPE STANDARD TABLE OF bapipcpobj,
          lt_tot_value     TYPE STANDARD TABLE OF bapipcptot,
          lt_per_value     TYPE STANDARD TABLE OF bapipcpval,
          lt_contrl        TYPE STANDARD TABLE OF bapipcpctrl,
          lt_return        TYPE STANDARD TABLE OF bapiret2.

    DATA: ls_header_info   TYPE bapiplnhdr.

    DATA: l_kostl TYPE csks-kostl,
          l_saknr TYPE ska1-saknr.

    ls_header_info-co_area       =  lcl_co_utilities=>get_controlling_area( EXPORTING
                                                                             iv_bukrs = me->gs_parametros_orcamento-bukrs
                                                                             iv_gsber = space ).
    ls_header_info-fisc_year     = sy-datum(4).
    ls_header_info-period_from   = '001'.
    ls_header_info-period_to     = '012'.
    ls_header_info-version       = '0'.
    ls_header_info-plan_currtype = 'O'. "Moeda do objeto

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-kostl
                                             IMPORTING
                                               ev_ouput = l_kostl ).

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-saknr
                                             IMPORTING
                                               ev_ouput = l_saknr ).

    APPEND VALUE #( object_index = '000001' value_index = '000001' ) TO lt_idx_structure.

    APPEND VALUE #( object_index = '000001' costcenter  = l_kostl ) TO lt_object.

    APPEND VALUE #( value_index = '000001'  cost_elem   = l_saknr ) TO lt_tot_value.

    CALL FUNCTION 'BAPI_PRIM_COST_READ' "#EC CI_USAGE_OK[2628699]
      EXPORTING
        header_info   = ls_header_info
      TABLES
        idx_structure = lt_idx_structure
        object        = lt_object
        per_value     = lt_per_value
        tot_value     = lt_tot_value
        contrl        = lt_contrl
        return        = lt_return.

    READ TABLE lt_tot_value INTO DATA(ls_to_value_new) INDEX 1.
    IF sy-subrc IS INITIAL.
      rv_planned_cost = ls_to_value_new-fix_value.
    ENDIF.

    FREE: lt_idx_structure, lt_object, lt_tot_value, lt_return, ls_header_info.

  ENDMETHOD.

  METHOD get_real_costs.

    DATA: ls_pcpobj  TYPE bapipcpobj,
          ls_ionra   TYPE ionra,
          ls_pcpline TYPE pcpvalline_i.

    DATA: l_kstar TYPE coep-kstar.

    DATA(l_kokrs) = lcl_co_utilities=>get_controlling_area( EXPORTING
                                                              iv_bukrs = me->gs_parametros_orcamento-bukrs
                                                              iv_gsber = space ).

    ls_pcpobj-object_index = '000001'.

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-kostl
                                            IMPORTING
                                              ev_ouput = ls_pcpobj-costcenter ).


    CALL FUNCTION 'MAP2I_BAPIPCPOBJ_TO_PCPVALLINE'
      EXPORTING
        bapipcpobj   = ls_pcpobj
      CHANGING
        pcpvalline_i = ls_pcpline.

    MOVE-CORRESPONDING ls_pcpline TO ls_ionra.
    MOVE               l_kokrs    TO ls_ionra-kokrs.

    CALL FUNCTION 'K_OBJNR_GET_FROM_IONRA'
      CHANGING
        cs_ionra     = ls_ionra
      EXCEPTIONS
        not_possible = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
    ENDIF.

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-saknr
                                            IMPORTING
                                              ev_ouput = l_kstar ).

    SELECT kokrs, wogbtr
      INTO TABLE @DATA(lt_coep)
      FROM coep
      WHERE kokrs = @l_kokrs
      AND   perio BETWEEN '001' AND '012'
      AND   lednr = '00'
      AND   objnr = @ls_ionra-objnr
      AND   gjahr = @sy-datum(4)
      AND   wrttp = '04'
      AND   versn = '000'
      AND   kstar = @l_kstar.

    LOOP AT lt_coep INTO DATA(ls_coep).
      ADD ls_coep-wogbtr TO rv_real_cost.
    ENDLOOP.

  ENDMETHOD.

  METHOD if_check_budget~set_return_for_budget_value.

    DATA: l_saldo  TYPE c LENGTH 20,
          l_result TYPE p DECIMALS 2.

    l_result = iv_result.

    WRITE l_result TO l_saldo.
    CONDENSE l_saldo NO-GAPS.

    IF iv_result > 0.
      ev_status = 'approved'.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ELSE.
      MULTIPLY l_result BY -1.
      WRITE l_result TO l_saldo.
      CONDENSE l_saldo NO-GAPS.
      ev_status = 'rejected'.
      CONCATENATE '-' l_saldo INTO l_saldo.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento nao disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ENDIF.

    IF iv_no_valid EQ 'X'.
      ev_status = 'approved'.
    ENDIF.
    "Classificação contábil K sempre deve aprovar, mesmo que sem saldo.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_check_budget_fi IMPLEMENTATION. "Verificar orçamento: Ordem de investimento

  METHOD constructor.
    me->gs_parametros_orcamento = is_parametros_orcamento.
  ENDMETHOD.

  METHOD if_check_budget~check_budget_value.
    DATA: lt_acc_main TYPE STANDARD TABLE OF bbp_acc_r3,
          lt_acc      TYPE STANDARD TABLE OF bbp_acc_r3,
          lt_budget   TYPE STANDARD TABLE OF bbp_budget,
          lt_return   TYPE STANDARD TABLE OF bapiret2.

    DATA: l_code     TYPE t001-bukrs,
          l_order    TYPE aufk-aufnr,
          l_no_valid TYPE xflag.

    DATA: l_result TYPE p DECIMALS 2.

    l_code = me->gs_parametros_orcamento-bukrs.

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-cost_object
                                             IMPORTING
                                              ev_ouput = l_order ).

    SELECT COUNT(*)
      FROM coas
      WHERE aufnr = l_order
      AND auart   <> 'ZOAN'
      AND auart   <> 'ZSIN'.
    IF sy-subrc IS INITIAL.
      l_no_valid = 'X'.
    ENDIF.

    DATA(l_kokrs) = lcl_co_utilities=>get_controlling_area( EXPORTING
                                                             iv_bukrs = me->gs_parametros_orcamento-bukrs
                                                             iv_gsber = space ).

    APPEND VALUE #( order_no = l_order  co_code = l_code co_area = l_kokrs ) TO lt_acc_main.

    APPEND VALUE #( order_no = l_order  co_code = l_code co_area = l_kokrs ) TO lt_acc.

    CALL FUNCTION 'BBP_BUDGET_READ'
      EXPORTING
        co_code     = l_code
      TABLES
        it_acc_main = lt_acc_main
        it_acc      = lt_acc
        et_budget   = lt_budget
        return      = lt_return.

    READ TABLE lt_budget INTO DATA(ls_budget) INDEX 1.
    IF sy-subrc IS INITIAL.
      DATA(l_saldo_orcamento) = ls_budget-bdgt_curr - ls_budget-bdgt_assgn.
    ENDIF.

    IF me->gs_parametros_orcamento-currency = 'BRL'.
      l_result = l_saldo_orcamento - me->gs_parametros_orcamento-total.
    ELSE.
      l_result = l_saldo_orcamento - ( me->gs_parametros_orcamento-total / lcl_co_utilities=>get_currency_value_brl( iv_currency = me->gs_parametros_orcamento-currency ) ).
    ENDIF.

    me->if_check_budget~set_return_for_budget_value( EXPORTING
                                                      iv_result   = CONV #( l_result )
                                                      iv_no_valid = l_no_valid
                                                     IMPORTING
                                                      ev_status   = ev_status
                                                      ev_saldo    = ev_saldo
                                                      ev_mensagem = ev_mensagem ).

  ENDMETHOD.

  METHOD if_check_budget~set_return_for_budget_value.

    DATA: l_saldo  TYPE c LENGTH 20,
          l_result TYPE p DECIMALS 2.

    l_result = iv_result.

    WRITE l_result TO l_saldo.
    CONDENSE l_saldo NO-GAPS.

    IF iv_result > 0.
      ev_status = 'approved'.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ELSE.
      MULTIPLY l_result BY -1.
      WRITE l_result TO l_saldo.
      CONDENSE l_saldo NO-GAPS.
      ev_status = 'rejected'.
      CONCATENATE '-' l_saldo INTO l_saldo.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento nao disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ENDIF.

    IF iv_no_valid EQ 'X'.
      ev_status = 'approved'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_check_budget_fm IMPLEMENTATION. "Verificar orçamento: Ordem de Manutenção

  METHOD constructor.
    me->gs_parametros_orcamento = is_parametros_orcamento.
  ENDMETHOD.

  METHOD if_check_budget~check_budget_value.

    DATA: lt_acc_main TYPE STANDARD TABLE OF bbp_acc_r3,
          lt_acc      TYPE STANDARD TABLE OF bbp_acc_r3,
          lt_budget   TYPE STANDARD TABLE OF bbp_budget,
          lt_return   TYPE STANDARD TABLE OF bapiret2.

    DATA: l_code  TYPE t001-bukrs,
          l_order TYPE aufk-aufnr.

    DATA: l_result TYPE p DECIMALS 2.

    l_code = me->gs_parametros_orcamento-bukrs.

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = me->gs_parametros_orcamento-cost_object
                                             IMPORTING
                                              ev_ouput = l_order ).

    DATA(l_kokrs) = lcl_co_utilities=>get_controlling_area( EXPORTING
                                                             iv_bukrs = me->gs_parametros_orcamento-bukrs
                                                             iv_gsber = space ).

    APPEND VALUE #( order_no = l_order  co_code = l_code co_area = l_kokrs ) TO lt_acc_main.

    APPEND VALUE #( order_no = l_order  co_code = l_code co_area = l_kokrs ) TO lt_acc.

    CALL FUNCTION 'BBP_BUDGET_READ'
      EXPORTING
        co_code     = l_code
      TABLES
        it_acc_main = lt_acc_main
        it_acc      = lt_acc
        et_budget   = lt_budget
        return      = lt_return.

    READ TABLE lt_budget INTO DATA(ls_budget) INDEX 1.
    IF sy-subrc IS INITIAL.
      DATA(l_saldo_orcamento) = ls_budget-bdgt_curr - ls_budget-bdgt_assgn.
    ENDIF.

    IF me->gs_parametros_orcamento-currency = 'BRL'.
      l_result = l_saldo_orcamento - me->gs_parametros_orcamento-total.
    ELSE.
      l_result = l_saldo_orcamento - ( me->gs_parametros_orcamento-total / lcl_co_utilities=>get_currency_value_brl( iv_currency = me->gs_parametros_orcamento-currency ) ).
    ENDIF.

    me->if_check_budget~set_return_for_budget_value( EXPORTING
                                                      iv_result   = CONV #( l_result )
                                                      iv_no_valid = 'X'
                                                     IMPORTING
                                                      ev_status   = ev_status
                                                      ev_saldo    = ev_saldo
                                                      ev_mensagem = ev_mensagem ).

  ENDMETHOD.

  METHOD if_check_budget~set_return_for_budget_value.

    DATA: l_saldo  TYPE c LENGTH 20,
          l_result TYPE p DECIMALS 2.

    l_result = iv_result.

    WRITE l_result TO l_saldo.
    CONDENSE l_saldo NO-GAPS.

    IF iv_result > 0.
      ev_status = 'approved'.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ELSE.
      MULTIPLY l_result BY -1.
      WRITE l_result TO l_saldo.
      CONDENSE l_saldo NO-GAPS.
      ev_status = 'rejected'.
      CONCATENATE '-' l_saldo INTO l_saldo.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento nao disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ENDIF.

    IF iv_no_valid EQ 'X'.
      ev_status = 'approved'.
    ENDIF.
    "Classificação contábil FM sempre deve aprovar, mesmo que sem saldo.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_check_budget_a IMPLEMENTATION. "Responsabilidade -> Verificar orçamento: Imobilizado

  METHOD constructor.
    me->gs_parametros_orcamento = is_parametros_orcamento.
  ENDMETHOD.

  METHOD if_check_budget~check_budget_value.

    DATA: l_result TYPE p DECIMALS 2.

    DATA: l_anln1            TYPE anla-anln1,
          l_anln2            TYPE anla-anln2,
          l_saldo_orcamento  TYPE p DECIMALS 2,
          l_valor_orcamento  TYPE p DECIMALS 2,
          l_valor_disponivel TYPE p DECIMALS 2,
          l_tolerance_from   TYPE p DECIMALS 2.

    SPLIT me->gs_parametros_orcamento-cost_object AT '-' INTO l_anln1 l_anln2.

    CHECK l_anln1 IS NOT INITIAL.

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = l_anln1
                                            IMPORTING
                                              ev_ouput = l_anln1 ).

    lcl_utilities=>adicionar_zeros_esquerda( EXPORTING
                                              iv_input = l_anln2
                                            IMPORTING
                                              ev_ouput = l_anln2 ).

    SELECT valfrom, valto
      FROM setleaf
      INTO TABLE @DATA(lt_tolerance)
      WHERE setname = 'MAGGI_TOLIMOB_COUPA'.

    SELECT SINGLE bukrs, anln1, anln2, lkauf
      FROM anla
      INTO @DATA(ls_anla)
      WHERE bukrs = @me->gs_parametros_orcamento-bukrs
      AND   anln1 = @l_anln1
      AND   anln2 = @l_anln2.

    CHECK sy-subrc IS INITIAL.

    SELECT bukrs, anln1, anln2, kansw
      FROM anlc
      INTO TABLE @DATA(lt_anlc)
      WHERE bukrs = @ls_anla-bukrs
      AND   anln1 = @ls_anla-anln1
      AND   anln2 = @ls_anla-anln2
      AND   gjahr = @sy-datum(4)
      AND   afabe = '01'.

    READ TABLE lt_tolerance INTO DATA(ls_tolerance) INDEX 1.
    IF sy-subrc IS INITIAL.

      REPLACE ALL OCCURRENCES OF ',' IN ls_tolerance-valfrom WITH '.'.
      CONDENSE ls_tolerance-valfrom NO-GAPS.

      MOVE ls_tolerance-valfrom TO l_tolerance_from.

      l_valor_orcamento = ls_anla-lkauf + ( ls_anla-lkauf * ( l_tolerance_from / 100 ) ).
    ENDIF.

    LOOP AT lt_anlc INTO DATA(ls_anlc).
*---> 07/06/2023 - Migração S4 - JS
*            l_valor_disponivel = ls_anlc-kansw.
      l_valor_disponivel = CONV #( ls_anlc-kansw ).
*<--- 07/06/2023 - Migração S4 - JS

    ENDLOOP.

    l_saldo_orcamento = l_valor_orcamento - l_valor_disponivel.

    IF me->gs_parametros_orcamento-currency = 'BRL'.
      l_result = l_saldo_orcamento - me->gs_parametros_orcamento-total.
    ELSE.
      l_result = l_saldo_orcamento - ( me->gs_parametros_orcamento-total / lcl_co_utilities=>get_currency_value_brl( iv_currency = me->gs_parametros_orcamento-currency ) ).
    ENDIF.

    me->if_check_budget~set_return_for_budget_value( EXPORTING
                                                      iv_result = CONV #( l_result )
                                                     IMPORTING
                                                      ev_status   = ev_status
                                                      ev_saldo    = ev_saldo
                                                      ev_mensagem = ev_mensagem ).

    IF ev_mensagem IS NOT INITIAL.

      DATA(lv_imob) = me->gs_parametros_orcamento-cost_object.

      CONDENSE lv_imob NO-GAPS.

      ev_mensagem = ev_mensagem && ` - IMOB. ` && lv_imob.

    ENDIF.

  ENDMETHOD.

  METHOD if_check_budget~set_return_for_budget_value.

    DATA: l_saldo  TYPE c LENGTH 20,
          l_result TYPE p DECIMALS 2.

    l_result = iv_result.

    WRITE l_result TO l_saldo.
    CONDENSE l_saldo NO-GAPS.

    IF iv_result > 0.
      ev_status = 'approved'.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ELSE.
      MULTIPLY l_result BY -1.
      WRITE l_result TO l_saldo.
      CONDENSE l_saldo NO-GAPS.
      ev_status = 'rejected'.
      CONCATENATE '-' l_saldo INTO l_saldo.
      ev_saldo  = l_saldo.
      CONCATENATE 'Orcamento nao disponivel para aprovacao desta compra saldo em BRL' l_saldo INTO ev_mensagem SEPARATED BY space.
    ENDIF.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_utilities IMPLEMENTATION. "Utilidades para todos as clases.

  METHOD calculate_days_by_datum.
    rv_date = sy-datum - iv_number_of_days.
  ENDMETHOD.

  METHOD remover_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

  METHOD adicionar_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_co_utilities IMPLEMENTATION. "Utilidades módulo CO.

  METHOD get_controlling_area.

    SELECT SINGLE kokrs
      INTO rv_kokrs
      FROM tka02
      WHERE bukrs = iv_bukrs
      AND   gsber = iv_gsber.

  ENDMETHOD.

  METHOD get_currency_value_brl.

    SELECT SINGLE ukurs
      FROM tcurr
      INTO rv_taxa
      WHERE kurst = 'B'
      AND   fcurr = 'BRL'
      AND   tcurr = iv_currency
      AND   gdatu = sy-datum.
    IF sy-subrc IS NOT INITIAL.
      SELECT ukurs
        FROM tcurr
        INTO TABLE @DATA(lt_taxa)
        WHERE kurst = 'B'
        AND   fcurr = 'BRL'
        AND   tcurr = @iv_currency
        ORDER BY gdatu DESCENDING.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_taxa INTO DATA(ls_taxa) INDEX 1.
        IF sy-subrc IS INITIAL.
          rv_taxa = ls_taxa-ukurs.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
