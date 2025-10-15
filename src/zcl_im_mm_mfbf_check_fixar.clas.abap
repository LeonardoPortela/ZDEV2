class ZCL_IM_MM_MFBF_CHECK_FIXAR definition
  public
  final
  create public .

*"* public components of class ZCL_IM_MM_MFBF_CHECK_FIXAR
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_GOODSMVT .
protected section.
*"* protected components of class ZCL_IM_MM_MFBF_CHECK_FIXAR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_MM_MFBF_CHECK_FIXAR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_MM_MFBF_CHECK_FIXAR IMPLEMENTATION.


method IF_EX_WORKORDER_GOODSMVT~BACKFLUSH.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~COGI_AUTHORITY_CHECK.
endmethod.


method IF_EX_WORKORDER_GOODSMVT~COGI_POST.
endmethod.


  method IF_EX_WORKORDER_GOODSMVT~COMPLETE_GOODSMOVEMENT.
  endmethod.


METHOD if_ex_workorder_goodsmvt~gm_screen_line_check.
*$*********************************************************************
*$ Nota: Q/q alteração deverá ser refletida no método GOODS_RECEIPT
*$*********************************************************************
  CONSTANTS: cc_e         VALUE 'E',
             cc_bwart_261 TYPE bwart    VALUE '261'.

  DATA: lq_labst         TYPE labst,
        lc_werks_virtual TYPE werks_d,
        lq_quantidade    TYPE menge_d,
        wa_zmmt0017      TYPE zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

* Verifica transação e tipo de movimento
  CHECK: ( sy-tcode = 'MFBF' OR  sy-tcode(4) = 'MF42' )
    AND  i_cowb_comp-bwart  = cc_bwart_261.


* Verifica se o material está cadastrado em centros a fixar
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*  SELECT SINGLE centro_a_fixar
*    INTO lc_werks_virtual
*    FROM zmmt0017
*   WHERE matnr = i_cowb_comp-matnr
*     AND centro_fixo = i_cowb_comp-werks
*     AND lgort = i_cowb_comp-lgort.

  zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
           EXPORTING
             i_material        = i_cowb_comp-matnr
             i_centro_fixo     = i_cowb_comp-werks
             i_deposito        = i_cowb_comp-lgort
           IMPORTING
             e_single_depara   = wa_zmmt0017
         ).


  CHECK: wa_zmmt0017 IS NOT INITIAL.
  lc_werks_virtual = wa_zmmt0017-centro_a_fixar.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

* Análise dos centros reais (custo fixo)
  SELECT SINGLE labst
    INTO lq_labst
    FROM mard
   WHERE matnr = i_cowb_comp-matnr
     AND werks = i_cowb_comp-werks
     AND lgort = i_cowb_comp-lgort.

  CHECK: sy-subrc IS INITIAL AND  lq_labst < i_cowb_comp-erfmg.
  lq_quantidade = i_cowb_comp-erfmg - lq_labst.

*	Análise dos centros virtuais (custo a fixar)
  SELECT SINGLE labst
    INTO lq_labst
    FROM mard
   WHERE matnr = i_cowb_comp-matnr
     AND werks = lc_werks_virtual
     AND lgort = i_cowb_comp-lgort.

  CHECK: ( NOT sy-subrc IS INITIAL ) OR ( lq_labst < lq_quantidade ).

* Não é possível a saída do material (soja em grão).
  MESSAGE TEXT-m01 TYPE cc_e.

ENDMETHOD.


method IF_EX_WORKORDER_GOODSMVT~GM_SCREEN_OKCODE_CHECK.
endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_CHECK.
  endmethod.


  method IF_EX_WORKORDER_GOODSMVT~GM_WIPBATCH_PROPOSE.
  endmethod.


METHOD if_ex_workorder_goodsmvt~goods_receipt.
ENDMETHOD.


method IF_EX_WORKORDER_GOODSMVT~IM_CALLED.
endmethod.


METHOD if_ex_workorder_goodsmvt~manual_goods_receipt.
*$*********************************************************************
*$ Nota: Q/q alteração deverá ser refletida método GM_SCREEN_LINE_CHECK
*$*********************************************************************
  FIELD-SYMBOLS <saplcowb>  TYPE ANY TABLE.

  CONSTANTS: cc_gt_comp(20) TYPE c     VALUE '(SAPLCOWB)GT_COMP[]',
             cc_e                      VALUE 'E',
             cc_bwart_261   TYPE bwart VALUE '261'.


  DATA: lq_labst            TYPE labst,
        ls_cowb_comp        TYPE cowb_comp,
        lc_werks_virtual    TYPE werks_d,
        lq_quantidade       TYPE menge_d,
        WA_ZMMT0017         TYPE ZMMT0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

* Verifica transação
  CHECK: sy-tcode = 'MFBF' OR  sy-tcode(4) = 'MF42'.

  ASSIGN (cc_gt_comp) TO <saplcowb>.
  CHECK: <saplcowb> IS ASSIGNED.

  LOOP AT <saplcowb> INTO ls_cowb_comp.

*   Verifica movimento e cadastro de centro a fixar
    CHECK: ls_cowb_comp-bwart  = cc_bwart_261.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*    SELECT SINGLE centro_a_fixar
*      INTO lc_werks_virtual
*      FROM zmmt0017
*     WHERE matnr = ls_cowb_comp-matnr
*       AND centro_fixo = ls_cowb_comp-werks
*       AND lgort = ls_cowb_comp-lgort.

        ZCL_DEPARA_CENTRO_FIXO_AFIXAR=>ZIF_DEPARA_CENTRO_FIXO_AFIXAR~GET_DADOS_DEPARA(
                 EXPORTING
                   I_MATERIAL        = ls_cowb_comp-matnr
                   I_CENTRO_FIXO     = ls_cowb_comp-werks
                   I_DEPOSITO        = ls_cowb_comp-lgort
                  " I_EUDR            =
                 IMPORTING
                   E_SINGLE_DEPARA            = WA_ZMMT0017

               ).
    CHECK: WA_ZMMT0017 IS NOT INITIAL.
    lc_werks_virtual = WA_ZMMT0017-LGORT.
*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940  -    FIM

*   Análise dos centros reais (custo fixo)
    SELECT SINGLE labst
      INTO lq_labst
      FROM mard
     WHERE matnr = ls_cowb_comp-matnr
       AND werks = ls_cowb_comp-werks
       AND lgort = ls_cowb_comp-lgort.

    CHECK: sy-subrc IS INITIAL AND  lq_labst < ls_cowb_comp-erfmg.
    lq_quantidade = ls_cowb_comp-erfmg - lq_labst.

*   Análise de centro virtual
    SELECT SINGLE labst
      INTO lq_labst
      FROM mard
     WHERE matnr = ls_cowb_comp-matnr
       AND werks = lc_werks_virtuaL
       AND lgort = ls_cowb_comp-lgort.

    CHECK: ( NOT sy-subrc IS INITIAL ) OR ( lq_labst < lq_quantidade ).

*   Não há saldo nos centros a fixar para o material (soja em grão)
    MESSAGE text-m02 TYPE cc_e.
    EXIT.

  ENDLOOP.

ENDMETHOD.


method IF_EX_WORKORDER_GOODSMVT~PICKLIST.
endmethod.
ENDCLASS.
