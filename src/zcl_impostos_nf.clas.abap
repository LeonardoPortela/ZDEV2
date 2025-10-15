class ZCL_IMPOSTOS_NF definition
  public
  final
  create public .

public section.

  constants CT_AG type CHAR02 value 'AG' ##NO_TEXT.
  constants CT_LF type CHAR02 value 'LF' ##NO_TEXT.
  constants CT_BR type CHAR02 value 'BR' ##NO_TEXT.
  constants CT_D type CHAR01 value 'D' ##NO_TEXT.
  constants CT_F type CHAR01 value 'F' ##NO_TEXT.
  constants CT_43 type CHAR02 value '43' ##NO_TEXT.
  constants CT_31 type CHAR02 value '31' ##NO_TEXT.
  constants CT_71 type CHAR02 value '71' ##NO_TEXT.
  constants CT_72 type CHAR02 value '72' ##NO_TEXT.
  constants CT_76 type CHAR02 value '76' ##NO_TEXT.
  data AT_PARVW type J_1BPARVW .
  data AT_PARID type J_1BPARID .
  data AT_WERKS type WERKS_D .
  data AT_NFTYPE type J_1BNFTYPE .
  data AT_J_1BAA type J_1BAA .
  data AT_SHIPFROM type REGIO .
  data AT_SHIPTO type REGIO .
  data AT_T_IMPOSTOS type ZFIWRT0010_T .
  data AT_LEIS_FISCAIS type ZMMT0154 .

  methods SET_DEFINE_ORIGEM_DESTINO
    importing
      !I_PARVW type J_1BPARVW
      !I_PARID type J_1BPARID
      !I_WERKS type WERKS_D
      !I_NFTYPE type J_1BNFTYPE
    exporting
      !E_INDCOPER type ZFIWED002
      !E_TEXTO_FISCAL type STRING
      !E_SHIPFROM type REGIO
      !E_SHIPTO type REGIO .
  methods GET_BASE_IMPOSTO_ICMS
    importing
      !I_HEADER type J_1BNFDOC
      !I_ITENS type J_1BNFLIN
      !I_LEIS_FISCAIS type ZMMT0154
    returning
      value(E_CALC) type ZFIWRT0010 .
  methods GET_BASE_IMPOSTO_PIS
    importing
      !I_ITENS type J_1BNFLIN
      !I_LEIS_FISCAIS type ZMMT0154 optional
    returning
      value(E_CALC) type ZFIWRT0010 .
  methods GET_BASE_IMPOSTO_COFINS
    importing
      !I_ITENS type J_1BNFLIN
      !I_LEIS_FISCAIS type ZMMT0154
    returning
      value(E_CALC) type ZFIWRT0010 .
  methods GET_BASE_IMPOSTO_IPI
    importing
      !I_ITENS type J_1BNFLIN
      !I_LEIS_FISCAIS type ZMMT0154
    returning
      value(E_CALC) type ZFIWRT0010 .
  methods GET_LEIS_FISCAIS
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_MATNR type MATNR
    returning
      value(R_LEIS_FISCAIS) type ZMMT0154
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods CONSTRUCTOR .
  methods CALCULA_IMPOSTOS_BONIFICACAO
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_HEADER type J_1BNFDOC
      !I_ITENS type J_1BNFLIN
    exporting
      !E_IMPOSTOS type ZFIWRT0010_T
      !E_LEIS_FISCAIS type ZMMT0154 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IMPOSTOS_NF IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.


  METHOD get_base_imposto_cofins.

    DATA: w_1btxcof        TYPE j_1btxcof,
          w_itens          TYPE j_1bnflin,
          w_calculo_cofins TYPE zfiwrt0010.

    FREE: e_calc.

    w_itens = i_itens.

    SELECT SINGLE *
      FROM j_1btxcof
      INTO w_1btxcof
     WHERE country EQ me->ct_br
       AND gruop   EQ me->ct_71
       AND value   EQ w_itens-werks.

    SELECT SINGLE *
      FROM zi_mm_tax_code_conditions INTO @DATA(lwa_zi_mm_tax_code_cond)
     WHERE aland  = 'BR'
       AND mwskz  = @i_leis_fiscais-mwskz
       AND taxgrp = 'COFI'.

    e_calc-taxtyp   = 'ICOF'.

    IF w_1btxcof IS NOT INITIAL AND lwa_zi_mm_tax_code_cond IS NOT INITIAL.
      READ TABLE me->at_t_impostos INTO w_calculo_cofins WITH KEY taxtyp = 'ICM3'.
      e_calc-base   = ( w_itens-netwr - w_calculo_cofins-taxval ).
      e_calc-rate   = w_1btxcof-rate.
      IF  e_calc-base > 0 AND w_1btxcof-rate  > 0.
        e_calc-taxval = e_calc-base * ( w_1btxcof-rate / 100 ).
      ENDIF.
      e_calc-othbas = 0.
    ELSE.
      MOVE: w_itens-netwr TO e_calc-othbas.
    ENDIF.

  ENDMETHOD.


  METHOD get_base_imposto_icms.

    DATA: w_itens     TYPE j_1bnflin,
          lv_cst_icms TYPE c LENGTH 2.

    FREE: e_calc.

    w_itens             = i_itens.

    IF i_leis_fiscais-icms EQ abap_false.
      e_calc-taxtyp     = 'ICM3'.
      e_calc-othbas     = w_itens-netwr.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM j_1baa
      INTO @DATA(lwa_j_1baa)
     WHERE nftype EQ @i_header-nftype.

    CHECK sy-subrc EQ 0.


    CASE lwa_j_1baa-direct.
      WHEN '1'. "Entrada

        zcl_miro=>get_taxas_icms(
          EXPORTING
            i_data            = sy-datum
            i_shipfrom        = CONV #( me->at_shipfrom )
            i_shipto          = CONV #( me->at_shipto )
            i_matnr           = CONV #( w_itens-matnr )
            i_werks           = CONV #( i_header-branch )
            i_lifnr           = CONV #( me->at_parid  )
          IMPORTING
            e_rate_icms       = DATA(lva_rate_icms)
            e_base_icms       = DATA(lva_base_icms)
            e_lei_fiscal_icms = DATA(lva_lei_fiscal_icms)
            e_utiliza_base_nf = DATA(lva_utiliza_base_nf)
        ).

        IF lva_base_icms IS INITIAL.

          IF lva_utiliza_base_nf EQ abap_true.
            e_calc-base   = w_itens-netwr.
          ELSE.
            e_calc-base   = lva_base_icms.
          ENDIF.

          e_calc-taxval = ( e_calc-base * ( lva_rate_icms / 100 ) ).
          e_calc-othbas = 0.

        ELSE.

          e_calc-base   = w_itens-netwr * ( lva_base_icms / 100 ).
          e_calc-taxval = e_calc-base * ( lva_rate_icms / 100 ).

          CLEAR: lv_cst_icms.

          SELECT SINGLE *
            FROM j_1batl1 INTO @DATA(w_j_1batl1)
           WHERE taxlaw = @me->at_leis_fiscais-j_1btaxlw1.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
              EXPORTING
                input  = w_j_1batl1-taxsit
              IMPORTING
                output = lv_cst_icms.
          ENDIF.

          IF lv_cst_icms EQ '20'.
            e_calc-excbas = w_itens-netwr - e_calc-base.
          ELSE.
            e_calc-othbas = w_itens-netwr - e_calc-base.
          ENDIF.
        ENDIF.

        e_calc-taxtyp = 'ICM3'.
        e_calc-rate   = lva_rate_icms.

      WHEN '2'. "Saida


    ENDCASE.







  ENDMETHOD.


  METHOD get_base_imposto_ipi.

    DATA: w_itens          TYPE j_1bnflin.

    FREE: e_calc.

    w_itens = i_itens.

    e_calc-taxtyp   = 'IPI0'.
    e_calc-othbas   = w_itens-netwr.

  ENDMETHOD.


  METHOD get_base_imposto_pis.

    DATA: w_1btxpis     TYPE j_1btxpis,
          w_itens       TYPE j_1bnflin,
          w_calculo_pis TYPE zfiwrt0010.

    FREE: e_calc.

    w_itens = i_itens.

    SELECT SINGLE *
      FROM j_1btxpis
      INTO w_1btxpis
     WHERE country EQ me->ct_br
       AND gruop   EQ me->ct_72
       AND value   EQ w_itens-werks.

    SELECT SINGLE *
      FROM zi_mm_tax_code_conditions INTO @DATA(lwa_zi_mm_tax_code_cond)
     WHERE aland  = 'BR'
       AND mwskz  = @i_leis_fiscais-mwskz
       AND taxgrp = 'PIS'.

    e_calc-taxtyp   = 'IPIS'.

    IF w_1btxpis IS NOT INITIAL AND lwa_zi_mm_tax_code_cond IS NOT INITIAL.
      READ TABLE me->at_t_impostos INTO w_calculo_pis WITH KEY taxtyp = 'ICM3'.
      e_calc-base   = ( w_itens-netwr - w_calculo_pis-taxval ).
      e_calc-rate   = w_1btxpis-rate.
      e_calc-taxval = e_calc-base * ( w_1btxpis-rate / 100 ).
      e_calc-othbas = 0.
    ELSE.
      MOVE: w_itens-netwr TO e_calc-othbas.
    ENDIF.

  ENDMETHOD.


  METHOD get_leis_fiscais.

    DATA: lc_zcl_nfe  TYPE REF TO zcl_nfe_inbound.

    FREE: r_leis_fiscais.

    TRY.
        CREATE OBJECT lc_zcl_nfe
          EXPORTING
            i_chave_nfe    = i_chave_nfe
            i_sem_bloqueio = abap_true.
      CATCH zcx_nfe_inbound_exception.
      CATCH zcx_cadastro.
    ENDTRY.

    TRY.
        r_leis_fiscais = lc_zcl_nfe->get_leis_fiscais( i_matnr ).

      CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfe_inbound_exception).
        RAISE EXCEPTION TYPE zcx_nfe_inbound_exception
          EXPORTING
            textid = VALUE #( msgid = ex_nfe_inbound_exception->msgid
                              msgno = ex_nfe_inbound_exception->msgno
                              attr1 = ex_nfe_inbound_exception->msgv1
                              attr2 = ex_nfe_inbound_exception->msgv2
                              attr3 = ex_nfe_inbound_exception->msgv3
                              attr4 = ex_nfe_inbound_exception->msgv4 )
            msgid  = ex_nfe_inbound_exception->msgid
            msgno  = ex_nfe_inbound_exception->msgno
            msgv1  = ex_nfe_inbound_exception->msgv1
            msgv2  = ex_nfe_inbound_exception->msgv2
            msgv3  = ex_nfe_inbound_exception->msgv3
            msgv4  = ex_nfe_inbound_exception->msgv4
            msgty  = 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD set_define_origem_destino.

    FREE: e_indcoper, e_texto_fiscal, e_shipfrom, e_shipto, me->at_shipfrom, me->at_shipto.

    me->at_nftype = i_nftype.
    me->at_werks  = i_werks.
    me->at_parid  = i_parid.
    me->at_parvw  = i_parvw.

    SELECT SINGLE *
      FROM j_1baa
      INTO me->at_j_1baa
     WHERE nftype = i_nftype.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM t001w
      INTO @DATA(_t001w)
     WHERE werks = @i_werks.

    CHECK sy-subrc = 0.

    CASE i_parvw.
      WHEN ct_ag.
        SELECT SINGLE *
          FROM kna1
          INTO @DATA(_kna1)
         WHERE kunnr = @i_parid.

      WHEN ct_br OR ct_lf.
        SELECT SINGLE *
          FROM lfa1
          INTO @DATA(_lfa1)
         WHERE lifnr = @i_parid.
    ENDCASE.

    CHECK sy-subrc = 0.

    IF i_parvw EQ ct_ag.
      IF _kna1-regio = _t001w-regio.
        e_indcoper     = ct_d.
        e_texto_fiscal = 'Dentro do Estado'.
      ELSE.
        e_indcoper     = ct_f.
        e_texto_fiscal = 'Fora do Estado'.
      ENDIF.

      IF me->at_j_1baa-direct = 1.
        MOVE: _kna1-regio TO me->at_shipfrom.
      ELSE.
        MOVE: _kna1-regio TO me->at_shipto.
      ENDIF.

    ELSEIF i_parvw = ct_br
       OR  i_parvw = ct_lf.
      IF _lfa1-regio = _t001w-regio.
        e_indcoper     = ct_d.
        e_texto_fiscal = 'Dentro do Estado'.
      ELSE.
        e_indcoper     = ct_f.
        e_texto_fiscal = 'Fora do Estado'.
      ENDIF.

      IF me->at_j_1baa-direct = 1.
        MOVE: _lfa1-regio TO me->at_shipfrom.
      ELSE.
        MOVE: _lfa1-regio TO me->at_shipto.
      ENDIF.
    ENDIF.

    IF me->at_j_1baa-direct = 1.
      MOVE: _t001w-regio  TO me->at_shipto.
    ELSE.
      MOVE: _t001w-regio  TO me->at_shipfrom.
    ENDIF.

    e_shipto   = me->at_shipto.
    e_shipfrom = me->at_shipfrom.

  ENDMETHOD.


  METHOD CALCULA_IMPOSTOS_BONIFICACAO.

    DATA: lv_calc_icms   TYPE zfiwrt0010,
          lv_calc_pis    TYPE zfiwrt0010,
          lv_calc_cofins TYPE zfiwrt0010,
          lv_calc_ipi    TYPE zfiwrt0010.

    FREE: me->at_t_impostos, e_impostos, e_leis_fiscais.

*---------------------------------
*-- recuperar leis fiscais
*---------------------------------
    TRY.
        me->at_leis_fiscais = me->get_leis_fiscais( i_chave_nfe = i_chave_nfe
                                                    i_matnr     = i_itens-matnr ).
      CATCH zcx_nfe_inbound_exception.
        CLEAR: me->at_leis_fiscais.
    ENDTRY.

*---------------------------------
*-- define origem/destino
*---------------------------------
    me->set_define_origem_destino( EXPORTING i_parvw    = i_header-parvw
                                             i_parid    = i_header-parid
                                             i_werks    = i_itens-werks
                                             i_nftype   = i_header-nftype
                                   IMPORTING e_shipfrom = me->at_shipfrom
                                             e_shipto   = me->at_shipto ).

*---------------------------------
*-- calculo icms
*---------------------------------
    lv_calc_icms   = me->get_base_imposto_icms( i_header = i_header i_itens =  i_itens i_leis_fiscais = me->at_leis_fiscais ).
    APPEND lv_calc_icms    TO me->at_t_impostos.

*---------------------------------
*-- calculo pis
*---------------------------------
    lv_calc_pis    = me->get_base_imposto_pis( i_itens =  i_itens i_leis_fiscais = me->at_leis_fiscais  ).
    APPEND lv_calc_pis     TO me->at_t_impostos.

*---------------------------------
*-- calculo pis
*---------------------------------
    lv_calc_cofins = me->get_base_imposto_cofins( i_itens =  i_itens i_leis_fiscais = me->at_leis_fiscais ).
    APPEND lv_calc_cofins  TO me->at_t_impostos.

*---------------------------------
*-- calculo ipi
*---------------------------------
    lv_calc_ipi    = me->get_base_imposto_ipi( i_itens =  i_itens i_leis_fiscais = me->at_leis_fiscais ).
    APPEND lv_calc_ipi     TO me->at_t_impostos.

*---------------------------------
*-- retorno
*---------------------------------
    e_impostos[]   = me->at_t_impostos.
    e_leis_fiscais = me->at_leis_fiscais.

  ENDMETHOD.
ENDCLASS.
