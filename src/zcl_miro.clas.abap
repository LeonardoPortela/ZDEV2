class ZCL_MIRO definition
  public
  final
  create public .

public section.
  class ZCL_FISCAL definition load .

  constants ST_FORMA_PAGAMENTO_BOLETO type DZLSCH value 'E' ##NO_TEXT.
  constants ST_FORMA_PAGAMENTO_TED type DZLSCH value 'S' ##NO_TEXT.
  constants ST_FORMA_PAGAMENTO_TRANF type DZLSCH value 'U' ##NO_TEXT.
  constants ST_FORMA_PAGAMENTO_FT_CONSUMO type DZLSCH value 'D' ##NO_TEXT.
  data AT_CK_FPOL type ZZCK_FPOL .

  methods CRIAR
    importing
      !I_CABECALHO type ZDE_MIRO_CABECALHO
      !I_ITENS type ZDE_MIRO_ITENS_T
      !I_CONTAS type ZBAPI_INCINV_GL_ACCOUNT_T
      !I_LINES_MIRO_TEXTO type TLINE_T
      !I_BAPI_WAIT type BAPIWAIT default 'X'
    exporting
      !E_INVOICEDOCNUMBER type RE_BELNR
      !E_FISCALYEAR type GJAHR
      !E_RETORNO type BAPIRET2_T
      !E_J_1BNFDOC type J_1BNFDOC
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_MIRO_EXCEPTION .
  methods ESTORNAR
    importing
      !I_BAPI_WAIT type BAPIWAIT default 'X'
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E optional
      !I_GERAR_VIA_JOB type CHAR01 optional
    exporting
      !E_INVOICEDOCNUMBER_ESTORNO type RE_BELNR
      !E_FISCALYEAR_ESTORNO type GJAHR
      !E_RETORNO type BAPIRET2_T
    changing
      !I_INVOICEDOCNUMBER type RE_BELNR optional
      !I_FISCALYEAR type GJAHR optional
      !I_REASONREVERSAL type STGRD default 'Z1'
      !I_POSTINGDATE type BUDAT optional
    returning
      value(R_GEROU) type CHAR01
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_ESTORNO
    importing
      !I_BELNR type RE_BELNR
      !I_GJAHR type GJAHR
      !I_DATA type DATUM
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_CRIAR
    importing
      !I_DATA type DATUM
      !I_BUKRS type BUKRS
    returning
      value(R_DATA_VAL) type DATUM
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_PROXIMO_DIA_UTIL
    importing
      !I_DATA_BASE type DATUM default SY-DATUM
      !I_SIGNUM type T5A4A-SPLIT default '+'
      !I_CK_DATA_ZLES0145 type C optional
      !I_BUKRS type BUKRS default ' '
    returning
      value(R_DATA) type DATUM
    exceptions
      ERRO .
  class-methods GET_PROXIMO_VENC_FATURA
    importing
      !I_CK_REVISAO type CHAR01 optional
      !I_BUKRS type BUKRS optional
    exporting
      !E_DATA_VENCIMENTO type DATUM
    exceptions
      ERRO .
  class-methods VERIFICAR_VENCIMENTO_FATURA
    importing
      !I_DATA_VENCIMENTO type DATUM
      !I_DATA_SE type CHAR01 optional
      !I_PYMT_METH type DZLSCH optional
      !I_CK_REVISAO type CHAR01 optional
      !I_VALIDA_POLITICA type CHAR01 optional
      !I_CK_FPOL type ZZCK_FPOL optional
      !I_OBS_FINANCEIRA type ZDE_OBS_FINANCEIRA_CTR optional
      !I_BUKRS type BUKRS optional
    raising
      ZCX_MIRO_EXCEPTION
      ZCX_ERROR .
  class-methods GET_TAXAS_IVA
    importing
      !I_IVA type MWSKZ
      !I_DATA type DATUM
      !I_BBRANC type J_1BBRANC_
      !I_SHIPFROM type J_1BTXSHPF
      !I_SHIPTO type J_1BTXSHPT
      !I_MATNR type MATNR
      !I_ICMS_BASE type ZDE_VLR_BASE_ICMS default 0
      !I_VALOR_ICMS type ZDE_VLR_ICMS_TOTAL default 0
      !I_WERKS type WERKS_D optional
      !I_LIFNR type LIFNR optional
    exporting
      !E_RATE_ICMS type J_1BTXRATE
      !E_BASE_ICMS type J_1BTXBASE
      !E_RATE_PIS type J_1BTXRATE
      !E_RATE_COFINS type J_1BTXRATE
      !E_RATE_ICMS_DIFERENCIAL type J_1BTXRATE .
  class-methods GET_MIRO_PAGA
    importing
      !I_BELNR type RE_BELNR
      !I_GJAHR type GJAHR
    exporting
      !E_AUGDT type AUGDT
      !E_AUGBL type AUGBL
    returning
      value(R_MIRO_PAGA) type CHAR01 .
  class-methods VERIFICAR_TIPO_DOCUMENTO
    importing
      !I_BSART type ESART
      !I_BLART type BLART
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_CHAVE_BLOQUEIO
    importing
      !I_BSART type ESART
      !I_ZLSPR type DZLSPR .
  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_TIPO_PEDIDO
    importing
      !I_BSART type ESART
      !I_BUDAT type BUDAT
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_FORN_DOC_FISCAL
    importing
      !I_LIFNR type LIFNR
      !I_PARVW type J_1BPARVW optional
      !I_NFTYPE type J_1BNFTYPE
      !I_XBLNR type XBLNR1
      !I_DATA type INVDT
      !I_WERKS type WERKS_D
    exporting
      !E_NOTAS type ZIB_NFE_FORN_T
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_VALOR_NFE
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_WRBTR type ZDE_VLR_TOTAL
      !I_WAERS type WAERS
      !I_KURSF type KURSF
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_CHAVE_REFERENCIA
    importing
      !I_NF_NUMBER type J_1BNFDOC-NFNUM optional
      !I_SERIES type J_1BNFDOC-SERIES optional
      !I_SUBSERIES type J_1BNFDOC-SUBSER optional
      !I_NF_NUMBER9 type J_1BNFNUM9 optional
    returning
      value(R_REF_DOC_NO) type XBLNR .
  class-methods VERIFICAR_COD_BARRA
    importing
      !I_BOLETO type ZDE_NR_BOLETO
      !I_VALOR type BSEG-DMBTR
      !I_DT_VENCIMENTO type ZDE_DT_VENCIMENTO
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_PRIORIDADE_BANCO_FORNE
    importing
      !I_LIFNR type LIFNR
    returning
      value(R_BVTYP) type BVTYP
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods VERIFICAR_VENCIMENTO_PEDIDO
    importing
      !I_EBELN type EBELN
      !I_DATA_BASE type DATUM
      !I_DATA_VENCIMENTO type DATUM
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_FORMAPAG_BANCO_EMPRESA
    importing
      !I_BUKRS type BUKRS
      !I_LIFNR type LIFNR
      !I_BVTYP type BVTYP
      !I_ZLSCH type DZLSCH optional
    exporting
      !E_FORMA_PAGAMENTO type DZLSCH
      !E_BANCO_EMPRESA type HBKID .
  class-methods GET_DATA_VENCIMENTO_COND_PAG
    importing
      !I_ZTERM type DZTERM
      !I_DT_DOCUMENTO type BLDAT
      !I_DT_LANCAMENTO type BUDAT
      !I_DT_CONTABIL type CPUDT
    returning
      value(R_DT_VENCIMENTO) type VFDAT
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_BANCO_FORMA_PAGAMENTO
    importing
      !I_BUKRS type BUKRS
      !I_FORMA_PAGAMENTO type DZLSCH
    returning
      value(R_BANCO_EMPRESA) type HBKID
    raising
      ZCX_MIRO_EXCEPTION .
  class-methods GET_PROX_DATA_VENC_CSC_SE
    returning
      value(R_DATA) type DATUM
    raising
      ZCX_ERROR .
  class-methods GET_MES_PERMITIDO_FECHAMENTO
    importing
      !I_DATA type DATUM
      !I_USER type SY-UNAME optional
    returning
      value(R_DATA) type DATUM .
  methods SET_SIMULAR
    importing
      !I_SIMULAR type CHAR1 .
  methods SET_FORA_POLITICA
    importing
      !I_CK_FPOL type ZZCK_FPOL
      !I_OBS_FINANCEIRA type ZDE_OBS_FINANCEIRA_CTR optional .
  methods ESTORNAR_JOB
    importing
      !I_CHAVE_NFE type ZDE_CHAVE_DOC_E
      !I_INVOICEDOCNUMBER type RE_BELNR
      !I_FISCALYEAR type GJAHR
      !I_REASONREVERSAL type STGRD
      !I_POSTINGDATE type BUDAT
    exporting
      !E_INVOICEDOCNUMBER_ESTORNO type RE_BELNR
      !E_FISCALYEAR_ESTORNO type GJAHR .
  class-methods GET_TAXAS_ICMS
    importing
      !I_DATA type DATUM
      !I_SHIPFROM type J_1BTXSHPF
      !I_SHIPTO type J_1BTXSHPT
      !I_MATNR type MATNR
      !I_WERKS type WERKS_D optional
      !I_LIFNR type LIFNR optional
    exporting
      !E_RATE_ICMS type J_1BTXRATE
      !E_BASE_ICMS type J_1BTXBASE
      !E_LEI_FISCAL_ICMS type J_1BTAXLW1
      !E_UTILIZA_BASE_NF type CHAR01 .
protected section.
private section.

  data AT_INVOICEDOCNUMBER type RE_BELNR .
  data AT_FISCALYEAR type GJAHR .
  data AT_INVOICEDOCNUMBER_ESTORNO type RE_BELNR .
  data AT_FISCALYEAR_ESTORNO type GJAHR .
  data AT_RETORNO type BAPIRET2_T .
  data AT_RETORNO_ESTORNO type BAPIRET2_T .
  data AT_SIMULAR type CHAR1 .
  data AT_OB_FINANCEIRA type ZDE_OBS_FINANCEIRA_CTR .

  methods LIMPAR .
ENDCLASS.



CLASS ZCL_MIRO IMPLEMENTATION.


  method criar.
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data       |Request    | Autor         | Alteração                                                   *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 10/01/2025  |DEVK9A2C12 | NSEGATIN      | Automação Lançamento Fatura Energia. Chamado: 140931.       *
*--------------------------------------------------------------------------------------------------------*

    data: wa_headerdata        type bapi_incinv_create_header,
          it_itemdata          type table of bapi_incinv_create_item,
          wa_itemdata          type bapi_incinv_create_item,
          it_accountingdata    type table of bapi_incinv_create_account,
          wa_accountingdata    type bapi_incinv_create_account,
          v_aufpl              type ekkn-aufpl,
          v_menge              type ekpo-menge,
          v_aplzl              type ekkn-aplzl,
          it_contas            type table of bapi_incinv_create_gl_account,
          it_withtaxdata       type table of bapi_incinv_create_withtax,
          wa_withtaxdata       type bapi_incinv_create_withtax,
          invoicedocnumber     type re_belnr,
          v_sucesso            type zib_nfe_dist_ter-sucesso,
          it_materialdata      type table of bapi_incinv_create_material,
          wa_materialdata      type bapi_incinv_create_material,
          it_return            type table of  bapiret2,
          wa_return            type bapiret2,
          fiscalyear           type gjahr,
          wa_header            type thead,
          p_valor              type netwr_fp,
          p_desconto           type netwr_fp,
          p_forma_pagamento    type dzlsch,
          p_princ_bnc_emp      type hbkid,
          lt_lfbw              type table of lfbw,
          t_grupo              type standard table of  rgsb4,
          lwa_zib_nfe_dist_ter type zib_nfe_dist_ter,
          v_count              type i,
          vg_vlt_total         type ekpo-menge,
          vg_calc              type ekpo-menge,
          vg_vlt_item          type ekpo-menge,
          vg_item              type i.

    data: number           type tbtcjob-jobcount,
          name             type tbtcjob-jobname,
          print_parameters type pri_params.
    data: ck_erro type char01 value abap_false.
    "
    me->limpar( ).

    if i_cabecalho-doc_date_cal is not initial.
      zcl_miro=>verificar_vencimento_fatura( i_data_vencimento = i_cabecalho-doc_date_cal i_pymt_meth = i_cabecalho-pymt_meth i_ck_fpol = me->at_ck_fpol i_obs_financeira = me->at_ob_financeira ).
    else.
      zcl_miro=>verificar_vencimento_fatura( i_data_vencimento = i_cabecalho-doc_date_ven i_pymt_meth = i_cabecalho-pymt_meth ).
    endif.

    wa_headerdata-pstng_date = zcl_miro=>verificar_criar( i_data = i_cabecalho-doc_date_mov i_bukrs = i_cabecalho-comp_code ).

    move i_cabecalho-gross_amount to p_valor.
    move i_cabecalho-dsct_amount  to p_desconto.

    if i_cabecalho-pymt_meth is initial and i_cabecalho-housebankid is not initial.

      call function 'Z_RET_FORMA_PAGAMENTO'
        exporting
          p_bukrs           = i_cabecalho-comp_code
          p_lifnr           = i_cabecalho-lifnr
          p_valor           = p_valor
          p_bvtyp           = i_cabecalho-partner_bk
        importing
          p_forma_pagamento = wa_headerdata-pymt_meth
        exceptions
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          others            = 6.

      if sy-subrc is not initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = sy-msgid msgno = sy-msgno attr1 = conv #( sy-msgv1 ) attr2 = conv #( sy-msgv2 ) attr3 = conv #( sy-msgv3 ) attr4 = conv #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      endif.

      if wa_headerdata-pymt_meth is initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = zcx_miro_exception=>zcx_forma_pagamento-msgid
                              msgno = zcx_miro_exception=>zcx_forma_pagamento-msgno )
            msgid  = zcx_miro_exception=>zcx_forma_pagamento-msgid
            msgno  = zcx_miro_exception=>zcx_forma_pagamento-msgno
            msgty  = 'E'.
      else.
        wa_headerdata-pymt_meth   = wa_headerdata-pymt_meth.
        wa_headerdata-housebankid = i_cabecalho-housebankid.
      endif.

    elseif i_cabecalho-pymt_meth is not initial and i_cabecalho-housebankid is initial.

      call function 'Z_RET_FORMA_PAGAMENTO'
        exporting
          p_bukrs          = i_cabecalho-comp_code
          p_lifnr          = i_cabecalho-lifnr
          p_zlsch          = i_cabecalho-pymt_meth
          p_valor          = p_valor
          p_bvtyp          = i_cabecalho-partner_bk
        importing
          p_princ_bnc_emp  = wa_headerdata-housebankid
        exceptions
          nao_fornecedor   = 1
          fornecedor_conta = 2
          fornecedor_banco = 3
          faixa_valor      = 4
          banco_empresa    = 5
          others           = 6.

      if sy-subrc is not initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = sy-msgid msgno = sy-msgno attr1 = conv #( sy-msgv1 ) attr2 = conv #( sy-msgv2 ) attr3 = conv #( sy-msgv3 ) attr4 = conv #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      endif.

      if wa_headerdata-housebankid is initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = zcx_miro_exception=>zcx_banco_empresa-msgid
                              msgno = zcx_miro_exception=>zcx_banco_empresa-msgno )
            msgid  = zcx_miro_exception=>zcx_banco_empresa-msgid
            msgno  = zcx_miro_exception=>zcx_banco_empresa-msgno
            msgty  = 'E'.
      else.
        wa_headerdata-pymt_meth   = i_cabecalho-pymt_meth.
        wa_headerdata-housebankid = wa_headerdata-housebankid.
      endif.

    elseif i_cabecalho-pymt_meth is not initial and i_cabecalho-housebankid is not initial.
      wa_headerdata-pymt_meth   = i_cabecalho-pymt_meth.
      wa_headerdata-housebankid = i_cabecalho-housebankid.
    else.
      call function 'Z_RET_FORMA_PAGAMENTO'
        exporting
          p_bukrs           = i_cabecalho-comp_code
          p_lifnr           = i_cabecalho-lifnr
          p_zlsch           = i_cabecalho-pymt_meth
          p_valor           = p_valor
          p_bvtyp           = i_cabecalho-partner_bk
        importing
          p_forma_pagamento = wa_headerdata-pymt_meth
          p_princ_bnc_emp   = wa_headerdata-housebankid
        exceptions
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          others            = 6.

      if sy-subrc is not initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = sy-msgid msgno = sy-msgno attr1 = conv #( sy-msgv1 ) attr2 = conv #( sy-msgv2 ) attr3 = conv #( sy-msgv3 ) attr4 = conv #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      endif.
    endif.

    if i_cabecalho-pymt_meth eq zcl_miro=>st_forma_pagamento_boleto and i_cabecalho-boleto is initial.
      raise exception type zcx_miro_exception
        exporting
          textid = value #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto-msgid
                            msgno = zcx_miro_exception=>zcx_cod_barra_boleto-msgno )
          msgid  = zcx_miro_exception=>zcx_cod_barra_boleto-msgid
          msgno  = zcx_miro_exception=>zcx_cod_barra_boleto-msgno
          msgty  = 'E'.
    elseif i_cabecalho-pymt_meth eq zcl_miro=>st_forma_pagamento_boleto.

      if i_cabecalho-doc_date_cal is not initial.
        zcl_miro=>verificar_cod_barra(
          exporting
            i_boleto        = i_cabecalho-boleto
            i_valor         = conv #( i_cabecalho-gross_amount ) " Montante em moeda interna
            i_dt_vencimento = i_cabecalho-doc_date_cal    " Data Vencimento.
        ).
      else.
        zcl_miro=>verificar_cod_barra(
          exporting
            i_boleto        = i_cabecalho-boleto
            i_valor         = conv #( i_cabecalho-gross_amount ) " Montante em moeda interna
            i_dt_vencimento = i_cabecalho-doc_date_ven    " Data Vencimento.
        ).
      endif.

      data: lc_barcode type c length 47,
            lc_dmbtr   type dmbtr.

      call function 'CONVERSION_EXIT_ZBOLE_INPUT'
        exporting
          input  = i_cabecalho-boleto
        importing
          output = lc_barcode.

      lc_dmbtr = i_cabecalho-gross_amount.

      call function 'CONVERT_BARCODE'
        exporting
          barcode   = lc_barcode
          dmbtr     = lc_dmbtr
        importing
          esrre     = wa_headerdata-po_ref_no
          esrnr     = wa_headerdata-po_sub_no
        exceptions
          not_valid = 1
          others    = 2.

      if sy-subrc is not initial.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
                              msgno = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno )
            msgid  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
            msgno  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno
            msgty  = 'E'.
      endif.

    endif.
    "CS2020000586
    if i_cabecalho-chave_nfe is not initial.

      clear lwa_zib_nfe_dist_ter.

      select single * from zib_nfe_dist_ter into lwa_zib_nfe_dist_ter
        where chave_nfe eq  i_cabecalho-chave_nfe.

    endif.
    "CS2020000586

    "Registrar Fatura por default é 'X'
    wa_headerdata-invoice_ind    = i_cabecalho-invoice_ind.

    "Tipo do Documento
    wa_headerdata-doc_type       = i_cabecalho-doc_type.

    "Data do Documento
    wa_headerdata-doc_date       = i_cabecalho-doc_date.

    "Data de Movimento
    wa_headerdata-pstng_date     = i_cabecalho-doc_date_mov.

    "Data de Vencimento
    wa_headerdata-bline_date     = i_cabecalho-doc_date_ven.

    "Empresa Tomadora
    wa_headerdata-comp_code      = i_cabecalho-comp_code.
    "Filial Tomadora
    wa_headerdata-bus_area       = i_cabecalho-bus_area.

    "Fornecedor
    wa_headerdata-diff_inv       = i_cabecalho-lifnr.
    "Texto do Header do Cabeçalho
    wa_headerdata-header_txt     = i_cabecalho-header_txt.

    "Texto dados básico (Texto do Item)
    if lwa_zib_nfe_dist_ter-se_recordid is not initial.
      "CS2020000586
      wa_headerdata-item_text      = lwa_zib_nfe_dist_ter-se_recordid.
    else.
      wa_headerdata-item_text      = i_cabecalho-item_text.
    endif.

    "Chave p/ Bloqueio de Pagamento
    wa_headerdata-pmnt_block     = i_cabecalho-pmnt_block.

    if i_cabecalho-pmnttrms is not initial.

      select single * into @data(wa_t052)
        from t052
       where zterm eq @i_cabecalho-pmnttrms.

      "Chave de Condição de Pagamento
      wa_headerdata-pmnttrms   = i_cabecalho-pmnttrms.
      wa_headerdata-dsct_days1 = i_cabecalho-doc_date_cal - i_cabecalho-doc_date_ven.

      if wa_t052-ztag1 = wa_headerdata-dsct_days1.
        clear: wa_headerdata-dsct_days1.
      else.
        clear: wa_headerdata-pmnttrms.
      endif.

    else.
      "Dias de desconto 1
      wa_headerdata-dsct_days1 = i_cabecalho-doc_date_cal - i_cabecalho-doc_date_ven.
    endif.

    "Moeda
    wa_headerdata-currency       = i_cabecalho-currency.
    "Cotação
    wa_headerdata-exch_rate      = i_cabecalho-exch_rate.
    "Valor Total da Miro
    wa_headerdata-gross_amount   = i_cabecalho-gross_amount.
    "Valor de Desconto
    wa_headerdata-dsct_amount    = i_cabecalho-dsct_amount.
    "No é o Frete Pedido de Compra
    wa_headerdata-alloc_nmbr     = i_cabecalho-alloc_nmbr.

    "IVA para Calulo de Imposto
    wa_headerdata-del_costs_taxc = i_cabecalho-del_costs_taxc.
    "Calcular Taxa/Impostos
    wa_headerdata-calc_tax_ind   = i_cabecalho-calc_tax_ind.

    "Código: referência a bens de investimento ?
    wa_headerdata-goods_affected = i_cabecalho-goods_affected.

    "Banco Parceiro
    if i_cabecalho-pymt_meth ne zcl_miro=>st_forma_pagamento_boleto.
      wa_headerdata-partner_bk   = i_cabecalho-partner_bk.
    endif.

    "Tipo de Documento
    wa_headerdata-j_1bnftype     = i_cabecalho-j_1bnftype.
**<<<------"140931 - NMS - INI------>>>
    select single * into @data(wa_j_1baa) from j_1baa where nftype eq @i_cabecalho-j_1bnftype.

    if wa_j_1baa-nfe eq abap_true.
      zcl_miro=>verificar_valor_nfe( i_chave_nfe = i_cabecalho-chave_nfe
                                     i_wrbtr     = p_valor
                                     "I_DESCONTO  = P_DESCONTO
                                     i_waers     = i_cabecalho-currency
                                     i_kursf     = i_cabecalho-exch_rate ).
    endif.
**<<<------"140931 - NMS - FIM------>>>
    wa_headerdata-ref_doc_no = zcl_miro=>get_chave_referencia(
      i_nf_number  = i_cabecalho-nf_number
      i_series     = i_cabecalho-series
      i_subseries  = i_cabecalho-subseries
      i_nf_number9 = i_cabecalho-nf_number9 ).

    "Nota Fiscal Eletrônica - NF-e
    if i_cabecalho-nf_number9 is not initial.
      if wa_j_1baa-form is not initial or wa_j_1baa-model ne '55' and wa_j_1baa-doctyp ne '1'  and wa_j_1baa-direct ne '1' and wa_j_1baa-nfe  ne abap_true.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
                              msgno = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
                              attr1 = conv #( i_cabecalho-j_1bnftype ) )
            msgid  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
            msgno  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
            msgv1  = conv #( i_cabecalho-j_1bnftype )
            msgty  = 'E'.
      endif.
    else.
      if wa_j_1baa-form is not initial or wa_j_1baa-model ne '01' and wa_j_1baa-doctyp ne '1'  and wa_j_1baa-direct ne '1' and wa_j_1baa-nfe  ne abap_false.
        raise exception type zcx_miro_exception
          exporting
            textid = value #( msgid = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
                              msgno = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
                              attr1 = conv #( i_cabecalho-j_1bnftype ) )
            msgid  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
            msgno  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
            msgv1  = conv #( i_cabecalho-j_1bnftype )
            msgty  = 'E'.
      endif.
    endif.

    call method zcl_miro=>verificar_forn_doc_fiscal
      exporting
        i_lifnr  = i_cabecalho-lifnr
        i_nftype = i_cabecalho-j_1bnftype
        i_xblnr  = wa_headerdata-ref_doc_no
        i_data   = wa_headerdata-doc_date
        i_werks  = wa_headerdata-bus_area.

    loop at i_itens	into data(wa_itens).

      v_count = sy-tabix.

      if wa_itens-po_number is not initial.
        select single bsart
          from ekko
          into @data(v1_bsart)
         where ebeln eq @wa_itens-po_number.

        zcl_miro=>verificar_tipo_pedido( i_bsart = v1_bsart i_budat = i_cabecalho-doc_date_mov ).
        zcl_miro=>verificar_tipo_documento( i_bsart = v1_bsart i_blart = i_cabecalho-doc_type ).
        zcl_miro=>verificar_chave_bloqueio( i_bsart = v1_bsart i_zlspr = i_cabecalho-pmnt_block ).
        "
        if wa_headerdata-paymt_ref is initial.
          select single *
            into @data(_w0035)
            from zmmt0035
            where ebeln = @wa_itens-po_number.
          if sy-subrc = 0.
            wa_headerdata-paymt_ref = _w0035-nro_sol_cp.
          endif.
        endif.

      endif.

      wa_itemdata-invoice_doc_item = wa_itens-invoice_doc_item.
      wa_itemdata-po_number        = wa_itens-po_number.
      wa_itemdata-po_item          = wa_itens-po_item.
      wa_itemdata-tax_code         = wa_itens-tax_code.
      wa_itemdata-taxjurcode       = wa_itens-taxjurcode.
      wa_itemdata-ref_doc          = wa_itens-ref_doc.
      wa_itemdata-ref_doc_year     = wa_itens-ref_doc_year.
      wa_itemdata-ref_doc_it       = wa_itens-ref_doc_it.
      wa_itemdata-sheet_no         = wa_itens-sheet_no.
      wa_itemdata-po_unit          = wa_itens-po_unit.

      if wa_itemdata-po_number is not initial and wa_itemdata-po_item is not initial.

        select single * into @data(wa_ekpo)
          from ekpo
         where ebeln eq @wa_itemdata-po_number
           and ebelp eq @wa_itemdata-po_item.

        if sy-subrc is initial and
           wa_ekpo-pstyp <> '1' and
           wa_ekpo-pstyp <> '9'.
          wa_itemdata-po_pr_uom = wa_ekpo-bprme.
        endif.

        if v1_bsart eq 'ZDBP' and sy-tcode = 'ZMM0110'.

          clear: vg_vlt_total, vg_vlt_item, vg_calc.

          wa_itemdata-item_amount      = '0'.
          wa_itemdata-quantity         = '1'.

          select  *  from ekpo into table @data(it_ekpo)
            where ebeln eq @wa_ekpo-bednr
            and   knttp eq ' '.

          select * from mara  into table @data(it_mara)
            for all entries in @it_ekpo
              where matnr eq @it_ekpo-matnr.

          loop at it_ekpo into data(wl_ekpo).
            vg_calc = ( wl_ekpo-menge * wl_ekpo-netpr ).
            vg_vlt_total = vg_vlt_total + vg_calc.
          endloop.

          vg_item = 0.

          loop at it_ekpo into wl_ekpo.
            clear: vg_calc, vg_vlt_item.


            vg_calc = ( wl_ekpo-menge * wl_ekpo-netpr ).
            vg_vlt_item = vg_calc / vg_vlt_total.


            read table it_mara into data(wl_mara) with key matnr = wl_ekpo-matnr.

            wa_materialdata-invoice_doc_item = vg_item + 1.
            wa_materialdata-material         = wl_ekpo-matnr.
            wa_materialdata-val_area         = wl_ekpo-werks.
            wa_materialdata-valuation_type   = wl_ekpo-bwtar.
            wa_materialdata-db_cr_ind        = 'S'.
            wa_materialdata-item_amount      = wa_ekpo-netwr * vg_vlt_item.
            wa_materialdata-quantity         = 1.
            wa_materialdata-base_uom         = wl_mara-meins.
            wa_materialdata-tax_code         = wa_itens-tax_code.
            wa_materialdata-taxjurcode       = wa_itens-taxjurcode.

            append wa_materialdata to it_materialdata.
            clear wa_materialdata.

            clear wl_ekpo.
          endloop.

        else.
          wa_itemdata-item_amount      = wa_itens-item_amount.
          wa_itemdata-quantity         = wa_itens-quantity.
        endif.
      endif.

      append wa_itemdata to it_itemdata.

**** BUG 49216 (Inicio) - Erro classe ZCL_MIRO->CRIAR, não esta passando para a bapi a tabela ACCOUNTINGDATA
*---> 06/06/2023 - Migração S4 - JS
*      SELECT  sakto kostl vbeln vbelp anln1 anln2 dabrz
*                        ekkn~fistl ekkn~geber ekkn~grant_nbr gsber imkey kokrs kstrg paobjnr
*                        prctr ps_psp_pnr aufnr ekkn~menge zekkn aufpl aplzl ekpo~menge

      select  ekkn~sakto ekkn~kostl vbeln vbelp anln1 anln2 dabrz
                        ekkn~fistl ekkn~geber ekkn~grant_nbr gsber imkey kokrs kstrg paobjnr
                        prctr ekkn~ps_psp_pnr aufnr ekkn~menge zekkn aufpl aplzl ekpo~menge
*<--- 06/06/2023 - Migração S4 - JS
            from ekkn
            inner join ekpo
            on  ekpo~ebeln = ekkn~ebeln
            and ekpo~ebelp = ekkn~ebelp
            into (wa_accountingdata-gl_account, wa_accountingdata-costcenter,
                  wa_accountingdata-sd_doc, wa_accountingdata-sdoc_item,
                  wa_accountingdata-asset_no, wa_accountingdata-sub_number,
                  wa_accountingdata-ref_date, wa_accountingdata-funds_ctr,
                  wa_accountingdata-fund, wa_accountingdata-grant_nbr,
                  wa_accountingdata-bus_area, wa_accountingdata-rl_est_key,
                  wa_accountingdata-co_area, wa_accountingdata-costobject,
                  wa_accountingdata-profit_segm_no, wa_accountingdata-profit_ctr,
                  wa_accountingdata-wbs_elem, wa_accountingdata-orderid,
                  wa_accountingdata-quantity, wa_accountingdata-serial_no, v_aufpl , v_aplzl,  v_menge)
                where ekkn~ebeln eq wa_itemdata-po_number
                and   ekkn~ebelp eq wa_itemdata-po_item
                and   ekpo~twrkz ne ' '
                order by ekkn~ebelp ekkn~zekkn.

*                AND   EXISTS ( SELECT * FROM ekpo WHERE ekpo~ebeln = ekkn~ebeln AND ekpo~ebelp = ekkn~ebelp AND ekpo~vrtkz NE ' ').

        wa_accountingdata-invoice_doc_item = wa_itemdata-invoice_doc_item.
        wa_accountingdata-po_unit          = wa_itemdata-po_unit.
        wa_accountingdata-tax_code         = wa_itemdata-tax_code.
        if v_aufpl is not initial.
          select single vornr
            into wa_accountingdata-activity
            from afvc
            where aufpl = v_aufpl
            and   aplzl = v_aplzl.
        endif.
        if wa_accountingdata-quantity gt 0.
*          wa_accountingdata-item_amount      = wa_itemdata-item_amount * (  wa_accountingdata-quantity /  wa_itens-quantity  ) .
          wa_accountingdata-quantity         = (  ( wa_itens-quantity / v_menge ) * wa_accountingdata-quantity  ).
          wa_accountingdata-item_amount      =  wa_accountingdata-quantity * ( wa_itemdata-item_amount / wa_itens-quantity ). " ALRS 28.03.2023
          append wa_accountingdata to it_accountingdata.
        endif.
        clear: wa_accountingdata, v_aufpl.
      endselect.
**** BUG 49216 (Fim) - Erro classe ZCL_MIRO->CRIAR, não esta passando para a bapi a tabela ACCOUNTINGDATA

      loop at i_contas into data(wa_conta) where invoice_doc_item eq wa_itens-invoice_doc_item.
        append wa_conta to it_contas.
      endloop.

    endloop.

    delete it_accountingdata where gl_account = ' ' and  costcenter = ' ' and   asset_no = ' '.  "IR091638  lportelA

    p_valor = wa_headerdata-gross_amount.

    "Validar Forma de Pagamento e Banco Empresa
    call function 'Z_RET_FORMA_PAGAMENTO'
      exporting
        p_bukrs           = i_cabecalho-comp_code
        p_lifnr           = i_cabecalho-lifnr
        p_zlsch           = wa_headerdata-pymt_meth
        p_valor           = p_valor
        p_bvtyp           = i_cabecalho-partner_bk
      importing
        p_forma_pagamento = p_forma_pagamento "WA_HEADERDATA-PYMT_METH
        p_princ_bnc_emp   = p_princ_bnc_emp  "WA_HEADERDATA-HOUSEBANKID
      exceptions
        nao_fornecedor    = 1
        fornecedor_conta  = 2
        fornecedor_banco  = 3
        faixa_valor       = 4
        banco_empresa     = 5
        others            = 6.

    if sy-subrc is not initial.
      raise exception type zcx_miro_exception
        exporting
          textid = value #( msgid = sy-msgid msgno = sy-msgno attr1 = conv #( sy-msgv1 ) attr2 = conv #( sy-msgv2 ) attr3 = conv #( sy-msgv3 ) attr4 = conv #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4
          msgty  = 'E'.
    endif.

    if p_forma_pagamento ne wa_headerdata-pymt_meth or p_princ_bnc_emp ne wa_headerdata-housebankid.
      raise exception type zcx_miro_exception
        exporting
          textid = value #( msgid = zcx_miro_exception=>zcx_forma_paga_banco-msgid
                            msgno = zcx_miro_exception=>zcx_forma_paga_banco-msgno
                            attr1 = conv #( wa_headerdata-pymt_meth )
                            attr2 = conv #( wa_headerdata-housebankid ) )
          msgid  = zcx_miro_exception=>zcx_forma_paga_banco-msgid
          msgno  = zcx_miro_exception=>zcx_forma_paga_banco-msgno
          msgty  = 'E'
          msgv1  = conv #( wa_headerdata-pymt_meth )
          msgv2  = conv #( wa_headerdata-housebankid ).
    endif.

    refresh it_withtaxdata.
    if 'ZDEF_ZFTE_ZSEM_ZSON' cs v1_bsart and sy-tcode = 'ZMM0110'.
      "impostos retidos
      call function 'FI_WT_READ_LFBW'
        exporting
          i_lifnr   = i_cabecalho-lifnr
          i_bukrs   = i_cabecalho-comp_code
        tables
          t_lfbw    = lt_lfbw
        exceptions
          not_found = 1
          others    = 2.
      if sy-subrc eq 0.
        if lt_lfbw[] is not initial.
          call function 'G_SET_GET_ALL_VALUES'
            exporting
              class         = '0000'
              setnr         = 'MAGGI_ZMM0110_SEM'
            tables
              set_values    = t_grupo
            exceptions
              set_not_found = 1
              others        = 2.
          if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          endif.
          sort t_grupo by from.
          loop at i_itens  into data(wa_itens2).
            if wa_itens-po_number is not initial.
              select single mara~*
                from ekpo
                inner join mara
                on mara~matnr = ekpo~matnr
                into @data(vl_mara)
               where ebeln eq @wa_itens2-po_number
               and   ebelp eq @wa_itens2-po_item.

              if '02_03' cs vl_mara-spart. "Fertilizantes/Defensivos US148370
                refresh lt_lfbw.                            "US148370
              elseif  vl_mara-spart = '04'. "SEMENTE
                select single *
                  into @data(wa_lfa1)
                  from lfa1
                  where lifnr = @i_cabecalho-lifnr.
                if wa_lfa1-stkzn = 'X'.
                  delete lt_lfbw where witht ne  'SF'.
                else.
                  refresh lt_lfbw.                          "US148370
                endif.
              else.
                read table t_grupo into data(w_grupo) with key from = vl_mara-matkl.
                if sy-subrc = 0.
                  delete lt_lfbw where witht = 'IF'.
                  delete lt_lfbw where witht = 'FR'.
                  delete lt_lfbw where witht = 'IS'.
                  exit.
                endif.
              endif.
            endif.

          endloop.

          loop at lt_lfbw into data(w_lfbw).
            wa_withtaxdata-split_key   = '000001'.
            wa_withtaxdata-wi_tax_type = w_lfbw-witht.
            wa_withtaxdata-wi_tax_code = w_lfbw-wt_withcd.
            append wa_withtaxdata to it_withtaxdata.

          endloop.
        endif.
      endif.
    endif.

                                                            "US162279
    data: lv_json_head type string,
          lv_json_item type string,
          lv_json_acct type string,
          lv_json_cont type string,
          lv_json_mate type string,
          lv_json_with type string.

    lv_json_head = /ui2/cl_json=>serialize( data = wa_headerdata ).
    lv_json_item = /ui2/cl_json=>serialize( data = it_itemdata ).
    lv_json_acct = /ui2/cl_json=>serialize( data = it_accountingdata ).
    lv_json_cont = /ui2/cl_json=>serialize( data = it_contas ).
    lv_json_mate = /ui2/cl_json=>serialize( data = it_materialdata ).
    lv_json_with = /ui2/cl_json=>serialize( data = it_withtaxdata ).
    data(lc_user_job) = zcl_job=>get_user_job( ).

    if at_simular is initial.
      if sy-batch = 'X' or sy-tcode = 'ZPM0065' . "executado por job / Posto miriam
        call function 'BAPI_INCOMINGINVOICE_CREATE'
          exporting
            headerdata       = wa_headerdata
          importing
            invoicedocnumber = invoicedocnumber
            fiscalyear       = fiscalyear
          tables
            itemdata         = it_itemdata
            accountingdata   = it_accountingdata
            glaccountdata    = it_contas
            materialdata     = it_materialdata
            withtaxdata      = it_withtaxdata
            return           = it_return.
      else.
        concatenate 'JOB_MIRO' i_cabecalho-chave_nfe  into name separated by '_'.

        call function 'JOB_OPEN'
          exporting
            jobname          = name
          importing
            jobcount         = number
          exceptions
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            others           = 4.

        if sy-subrc is initial.
          submit zmmr208 to sap-spool spool parameters print_parameters without spool dynpro via job name number number
            with pchave  = i_cabecalho-chave_nfe
            with pjsonhd = lv_json_head
            with pjsonit = lv_json_item
            with pjsonac = lv_json_acct
            with pjsonct = lv_json_cont
            with pjsonmt = lv_json_mate
            with pjsonwi = lv_json_with
            with psimu   = ' '

            user lc_user_job
             and return.

          if sy-subrc is initial.
            call function 'JOB_CLOSE'
              exporting
                jobcount             = number
                jobname              = name
                strtimmed            = 'X'
              exceptions
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                others               = 8.

            if sy-subrc is not initial.
              ck_erro = abap_true.
              message id sy-msgid type sy-msgty number sy-msgno into data(mtext) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              call function 'BP_JOB_DELETE'
                exporting
                  jobcount                 = number
                  jobname                  = name
                exceptions
                  cant_delete_event_entry  = 1
                  cant_delete_job          = 2
                  cant_delete_joblog       = 3
                  cant_delete_steps        = 4
                  cant_delete_time_entry   = 5
                  cant_derelease_successor = 6
                  cant_enq_predecessor     = 7
                  cant_enq_successor       = 8
                  cant_enq_tbtco_entry     = 9
                  cant_update_predecessor  = 10
                  cant_update_successor    = 11
                  commit_failed            = 12
                  jobcount_missing         = 13
                  jobname_missing          = 14
                  job_does_not_exist       = 15
                  job_is_already_running   = 16
                  no_delete_authority      = 17
                  others                   = 18.
              if sy-subrc is not initial.
                ck_erro = abap_false.
              endif.
            endif.
          else.
            ck_erro = abap_true.
            message id sy-msgid type sy-msgty number sy-msgno into mtext with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            call function 'BP_JOB_DELETE'
              exporting
                jobcount                 = number
                jobname                  = name
              exceptions
                cant_delete_event_entry  = 1
                cant_delete_job          = 2
                cant_delete_joblog       = 3
                cant_delete_steps        = 4
                cant_delete_time_entry   = 5
                cant_derelease_successor = 6
                cant_enq_predecessor     = 7
                cant_enq_successor       = 8
                cant_enq_tbtco_entry     = 9
                cant_update_predecessor  = 10
                cant_update_successor    = 11
                commit_failed            = 12
                jobcount_missing         = 13
                jobname_missing          = 14
                job_does_not_exist       = 15
                job_is_already_running   = 16
                no_delete_authority      = 17
                others                   = 18.
            if sy-subrc is not initial.
              ck_erro = abap_false.
            endif.
          endif.
        endif.

        "Aguardar execução do job
        zcl_job=>get_instance(
         )->set_key_job( i_jobname = name i_jobcount = number
         )->get_wait_job_exec(
         ).
        "
        "recupera numero da MIRO
        select single belnr gjahr
          into ( invoicedocnumber, fiscalyear )
          from zib_nfe_dist_ter
          where chave_nfe = i_cabecalho-chave_nfe.
        if sy-subrc ne 0.
          select single belnr gjahr
            into ( invoicedocnumber, fiscalyear )
            from zfit0007
            where invoicekey = i_cabecalho-chave_nfe.

          data obj_nfeclass       type ref to zcl_repository_classes.
          data: it_logs type zde_nfe_dist_log_alv_t,
                wa_logs type zde_nfe_dist_log_alv.
          if obj_nfeclass is initial.
            create object obj_nfeclass.
            obj_nfeclass->nfe_inbound( ).
          endif.

          it_logs = obj_nfeclass->at_nfe_inbound->get_log_proc_nfe( p_chave = i_cabecalho-chave_nfe ).

          loop at it_logs into wa_logs.
            wa_return-type           = wa_logs-type.
            wa_return-id             = wa_logs-id.
            wa_return-number         = wa_logs-num.
            wa_return-message        = wa_logs-message.
            wa_return-log_no         = 0.
            wa_return-log_msg_no     = wa_logs-num.
            wa_return-message_v1     = wa_logs-message_v1.
            wa_return-message_v2     = wa_logs-message_v2.
            wa_return-message_v3     = wa_logs-message_v4.
            wa_return-message_v4     = wa_logs-message_v4.
            append  wa_return to it_return.
          endloop.

        endif.

      endif.

                                                            "US162279

      move it_return[] to e_retorno.
      move it_return[] to me->at_retorno.

      if ( invoicedocnumber is not initial ) and ( fiscalyear is not initial ).

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = i_bapi_wait.

        e_invoicedocnumber = invoicedocnumber.
        e_fiscalyear       = fiscalyear.

        r_gerou = abap_true.

        if i_lines_miro_texto is not initial.
          wa_header-tdobject = 'RBKP'.
          concatenate invoicedocnumber fiscalyear into wa_header-tdname.
          wa_header-tdid    = '0001'.
          wa_header-tdspras = sy-langu.

          call function 'SAVE_TEXT'
            exporting
              header          = wa_header
              savemode_direct = abap_true
            tables
              lines           = i_lines_miro_texto[]
            exceptions
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              others          = 5.
        endif.

        "Resgistro fiscal gerado pela MIRO
        if wa_headerdata-j_1bnftype is not initial.

          select single * into e_j_1bnfdoc
            from j_1bnfdoc
           where belnr eq e_invoicedocnumber
             and gjahr eq e_fiscalyear.

          if sy-subrc is not initial.

            wait up to 5 seconds.

            select single * into e_j_1bnfdoc
              from j_1bnfdoc
             where belnr eq e_invoicedocnumber
               and gjahr eq e_fiscalyear.

            data: lc_ciclos	type zde_qtd_ciclos,
                  lc_tempo  type zde_qtd_segundos_ciclo.

            lc_ciclos = 60.
            lc_tempo  = 5.

            while lc_ciclos is not initial and e_j_1bnfdoc is initial.

              "Tempo de Cadas Ciclo
              wait up to lc_tempo seconds.

              select single * into e_j_1bnfdoc
                from j_1bnfdoc
               where belnr eq e_invoicedocnumber
                 and gjahr eq e_fiscalyear.

              subtract 1 from lc_ciclos.

            endwhile.

          endif.
        endif.
        message s002 with e_invoicedocnumber e_fiscalyear.

      else.
        r_gerou = abap_false.
        call function 'BAPI_TRANSACTION_ROLLBACK'.
      endif.
    else.
      if sy-batch = 'X'.
        call function 'MRM_SRM_INVOICE_SIMULATE'
          exporting
            headerdata     = wa_headerdata
          importing
            return         = it_return
*           t_accit        = lt_accit
          tables
            itemdata       = it_itemdata
            accountingdata = it_accountingdata
            glaccountdata  = it_contas
            materialdata   = it_materialdata
            withtaxdata    = it_withtaxdata.

        move it_return[] to e_retorno.
        move it_return[] to me->at_retorno.
        read table it_return into data(ret) with key type = 'E'.
        if sy-subrc = 0.
          r_gerou = abap_false.
        else.
          r_gerou = abap_true.
        endif.
      else.
        if 1 = 2. "para teste debug
          submit zmmr208
           with pchave  = i_cabecalho-chave_nfe
           with pjsonhd = lv_json_head
           with pjsonit = lv_json_item
           with pjsonac = lv_json_acct
           with pjsonct = lv_json_cont
           with pjsonmt = lv_json_mate
           with pjsonwi = lv_json_with
           with psimu   = 'X'
           and return.
        endif.

        concatenate 'JOB_MIRO_SIM' i_cabecalho-chave_nfe  into name separated by '_'.

        call function 'JOB_OPEN'
          exporting
            jobname          = name
          importing
            jobcount         = number
          exceptions
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            others           = 4.

        if sy-subrc is initial.
          submit zmmr208 to sap-spool spool parameters print_parameters without spool dynpro via job name number number
            with pchave  = i_cabecalho-chave_nfe
            with pjsonhd = lv_json_head
            with pjsonit = lv_json_item
            with pjsonac = lv_json_acct
            with pjsonct = lv_json_cont
            with pjsonmt = lv_json_mate
            with pjsonwi = lv_json_with
            with psimu   = 'X'

            user lc_user_job
             and return.

          if sy-subrc is initial.
            call function 'JOB_CLOSE'
              exporting
                jobcount             = number
                jobname              = name
                strtimmed            = 'X'
              exceptions
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                others               = 8.

            if sy-subrc is not initial.
              ck_erro = abap_true.
              message id sy-msgid type sy-msgty number sy-msgno into mtext with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              call function 'BP_JOB_DELETE'
                exporting
                  jobcount                 = number
                  jobname                  = name
                exceptions
                  cant_delete_event_entry  = 1
                  cant_delete_job          = 2
                  cant_delete_joblog       = 3
                  cant_delete_steps        = 4
                  cant_delete_time_entry   = 5
                  cant_derelease_successor = 6
                  cant_enq_predecessor     = 7
                  cant_enq_successor       = 8
                  cant_enq_tbtco_entry     = 9
                  cant_update_predecessor  = 10
                  cant_update_successor    = 11
                  commit_failed            = 12
                  jobcount_missing         = 13
                  jobname_missing          = 14
                  job_does_not_exist       = 15
                  job_is_already_running   = 16
                  no_delete_authority      = 17
                  others                   = 18.
              if sy-subrc is not initial.
                ck_erro = abap_false.
              endif.
            endif.
          else.
            ck_erro = abap_true.
            message id sy-msgid type sy-msgty number sy-msgno into mtext with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            call function 'BP_JOB_DELETE'
              exporting
                jobcount                 = number
                jobname                  = name
              exceptions
                cant_delete_event_entry  = 1
                cant_delete_job          = 2
                cant_delete_joblog       = 3
                cant_delete_steps        = 4
                cant_delete_time_entry   = 5
                cant_derelease_successor = 6
                cant_enq_predecessor     = 7
                cant_enq_successor       = 8
                cant_enq_tbtco_entry     = 9
                cant_update_predecessor  = 10
                cant_update_successor    = 11
                commit_failed            = 12
                jobcount_missing         = 13
                jobname_missing          = 14
                job_does_not_exist       = 15
                job_is_already_running   = 16
                no_delete_authority      = 17
                others                   = 18.
            if sy-subrc is not initial.
              ck_erro = abap_false.
            endif.
          endif.
        endif.

        "Aguardar execução do job
        zcl_job=>get_instance(
         )->set_key_job( i_jobname = name i_jobcount = number
         )->get_wait_job_exec(
         ).
        "
        "recupera numero da MIRO
        select single sucesso
          into v_sucesso
          from zib_nfe_dist_ter
          where chave_nfe = i_cabecalho-chave_nfe.
        r_gerou = abap_false.
        if sy-subrc = 0.
          if v_sucesso is not initial.
            r_gerou = abap_true.
          endif.
        else.
          select single *
            into @data(w_0007)
            from zfit0007
            where invoicekey = @i_cabecalho-chave_nfe.
          if sy-subrc = 0.
            if w_0007-belnr is not initial.
              r_gerou = abap_true.
            endif.
          endif.
        endif.

      endif.
    endif.

    clear  me->at_simular.
  endmethod.


  METHOD estornar.
    "Compensação contabil estorno
    DATA: wa_bkpf_fat TYPE bkpf,
          wa_bkpf_est TYPE bkpf,
          v_lifnr     TYPE lfa1-lifnr,
          p_mode      TYPE rfpdo-allgazmd.

    DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
          l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
          l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

    DATA: lt_blntab  TYPE STANDARD TABLE OF blntab,
          lt_ftclear TYPE STANDARD TABLE OF ftclear,
          lt_ftpost  TYPE STANDARD TABLE OF ftpost,
          lt_fttax   TYPE STANDARD TABLE OF fttax,
          "
          wa_blntab  TYPE blntab,
          wa_ftclear TYPE ftclear,
          wa_ftpost  TYPE ftpost,
          wa_fttax   TYPE fttax,
          lds_return TYPE bapiret2.
    "Compensação contabil estorno

    DATA: vg_augdt  TYPE augdt,
          vg_augbl  TYPE augbl,
          it_return	TYPE TABLE OF	bapiret2.

    IF i_invoicedocnumber IS INITIAL.
      i_invoicedocnumber = me->at_invoicedocnumber.
    ENDIF.

    IF i_fiscalyear IS INITIAL.
      i_fiscalyear = me->at_fiscalyear.
    ENDIF.

    CALL METHOD zcl_miro=>verificar_estorno
      EXPORTING
        i_belnr = i_invoicedocnumber
        i_gjahr = i_fiscalyear
        i_data  = i_postingdate.

*-CS2025000249-17.04.2025-#173311-JT-inicio
    IF i_gerar_via_job = abap_true.
      me->estornar_job( EXPORTING i_chave_nfe                = i_chave_nfe
                                  i_invoicedocnumber         = i_invoicedocnumber
                                  i_fiscalyear               = i_fiscalyear
                                  i_reasonreversal           = i_reasonreversal
                                  i_postingdate              = i_postingdate
                        IMPORTING e_invoicedocnumber_estorno = e_invoicedocnumber_estorno
                                  e_fiscalyear_estorno       = e_fiscalyear_estorno ).
*-CS2025000249-17.04.2025-#173311-JT-fim
    ELSE.
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
        EXPORTING
          invoicedocnumber          = i_invoicedocnumber
          fiscalyear                = i_fiscalyear
          reasonreversal            = i_reasonreversal
          postingdate               = i_postingdate
        IMPORTING
          invoicedocnumber_reversal = e_invoicedocnumber_estorno
          fiscalyear_reversal       = e_fiscalyear_estorno
        TABLES
          return                    = it_return.
    ENDIF.

    MOVE it_return[] TO e_retorno.
    MOVE it_return[] TO me->at_retorno_estorno.

    IF e_invoicedocnumber_estorno IS NOT INITIAL.

      me->at_fiscalyear_estorno       = e_fiscalyear_estorno.
      me->at_invoicedocnumber_estorno = e_invoicedocnumber_estorno.

      r_gerou = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = i_bapi_wait.

      "Compensação
      IF i_gerar_via_job = abap_false.  "*-CS2025000249-17.04.2025-#173311-JT
        WAIT UP TO 10 SECONDS.
      ENDIF.

      SELECT SINGLE lifnr
        INTO v_lifnr
        FROM rbkp
        WHERE belnr = i_invoicedocnumber
        AND   gjahr = i_fiscalyear.

      CONCATENATE i_invoicedocnumber i_fiscalyear INTO wa_bkpf_fat-awkey.
      SELECT SINGLE *
        FROM bkpf
        INTO wa_bkpf_fat
        WHERE awkey = wa_bkpf_fat-awkey.
      "
      CONCATENATE e_invoicedocnumber_estorno e_fiscalyear_estorno INTO wa_bkpf_est-awkey.
      SELECT SINGLE *
        FROM bkpf
        INTO wa_bkpf_est
        WHERE awkey = wa_bkpf_est-awkey.

      IF sy-subrc = 0.
        p_mode = 'N'.
        CALL FUNCTION 'POSTING_INTERFACE_START'
          EXPORTING
            i_client           = sy-mandt
            i_function         = 'C'
            i_mode             = p_mode
            i_update           = 'S'
            i_user             = sy-uname
          EXCEPTIONS
            client_incorrect   = 1
            function_invalid   = 2
            group_name_missing = 3
            mode_invalid       = 4
            update_invalid     = 5
            OTHERS             = 6.

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        wa_ftpost-stype = 'K'."Header
        wa_ftpost-count = 1.  "number of Dynpro

        wa_ftpost-fnam = 'BKPF-BUKRS'.
        wa_ftpost-fval =  wa_bkpf_fat-bukrs.
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-WAERS'.
        wa_ftpost-fval = wa_bkpf_fat-waers.
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-BLDAT'.
        CONCATENATE wa_bkpf_est-budat+6(2) wa_bkpf_est-budat+4(2) wa_bkpf_est-budat(4) INTO wa_ftpost-fval SEPARATED BY '.'.
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-BUDAT'.
        CONCATENATE wa_bkpf_est-budat+6(2) wa_bkpf_est-budat+4(2) wa_bkpf_est-budat(4) INTO wa_ftpost-fval SEPARATED BY '.'.
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-MONAT'.
        wa_ftpost-fval =  wa_bkpf_est-budat+4(2).
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-BLART'.
        wa_ftpost-fval = wa_bkpf_est-blart. "'RE'. "CS2017000118
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftpost-fnam = 'BKPF-XBLNR'.
        wa_ftpost-fval = 'EST.MR8M' .
        APPEND wa_ftpost TO lt_ftpost.

        wa_ftclear-agkoa  = 'K'.
        wa_ftclear-agkon  = v_lifnr.
        wa_ftclear-agbuk  = wa_bkpf_fat-bukrs.
        wa_ftclear-xnops  = 'X'.
        wa_ftclear-selfd  = 'BELNR'.
        wa_ftclear-selvon = wa_bkpf_fat-belnr.
        APPEND wa_ftclear TO lt_ftclear.

        wa_ftclear-agkoa  = 'K'.
        wa_ftclear-agkon  = v_lifnr.
        wa_ftclear-agbuk  = wa_bkpf_fat-bukrs.
        wa_ftclear-xnops  = 'X'.
        wa_ftclear-selfd  = 'BELNR'.
        wa_ftclear-selvon = wa_bkpf_est-belnr.
        APPEND wa_ftclear TO lt_ftclear.

        CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
          EXPORTING
            i_auglv                    = l_auglv
            i_tcode                    = l_tcode
            i_sgfunct                  = l_sgfunct
            i_no_auth                  = 'X'
          IMPORTING
            e_msgid                    = lds_return-id
            e_msgno                    = lds_return-number
            e_msgty                    = lds_return-type
            e_msgv1                    = lds_return-message_v1
            e_msgv2                    = lds_return-message_v2
            e_msgv3                    = lds_return-message_v3
            e_msgv4                    = lds_return-message_v4
          TABLES
            t_blntab                   = lt_blntab
            t_ftclear                  = lt_ftclear
            t_ftpost                   = lt_ftpost
            t_fttax                    = lt_fttax
          EXCEPTIONS
            clearing_procedure_invalid = 1
            clearing_procedure_missing = 2
            table_t041a_empty          = 3
            transaction_code_invalid   = 4
            amount_format_error        = 5
            too_many_line_items        = 6
            company_code_invalid       = 7
            screen_not_found           = 8
            no_authorization           = 9
            OTHERS                     = 10.

        CALL FUNCTION 'POSTING_INTERFACE_END'
          EXPORTING
            i_bdcimmed              = 'X'
          EXCEPTIONS
            session_not_processable = 1
            OTHERS                  = 2.

        IF sy-subrc <> 0.
          EXIT.
*             RETURN.
        ENDIF.
      ENDIF.
      "Compensação
      MESSAGE s003 WITH i_invoicedocnumber i_fiscalyear e_invoicedocnumber_estorno e_fiscalyear_estorno.

    ELSE.
      r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.


  METHOD GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD GET_BANCO_FORMA_PAGAMENTO.

    SELECT SINGLE HBKID INTO @R_BANCO_EMPRESA
      FROM ZFIT0143
     WHERE BUKRS EQ @I_BUKRS
       AND ZLSCH EQ @I_FORMA_PAGAMENTO.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_SEM_BANCO_EMPRESA-MSGID
                          MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_SEM_BANCO_EMPRESA-MSGNO
                          ATTR1 = CONV #( I_FORMA_PAGAMENTO )
                          ATTR2 = CONV #( I_BUKRS ) )
        MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_SEM_BANCO_EMPRESA-MSGID
        MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_SEM_BANCO_EMPRESA-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_FORMA_PAGAMENTO )
        MSGV2  = CONV #( I_BUKRS ).

  ENDMETHOD.


  METHOD GET_CHAVE_REFERENCIA.

    DATA: L_SERIE TYPE C LENGTH 3.

    IF I_NF_NUMBER9 IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = I_NF_NUMBER9
        IMPORTING
          OUTPUT = R_REF_DOC_NO.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = I_SERIES
        IMPORTING
          OUTPUT = L_SERIE.

      CONCATENATE R_REF_DOC_NO L_SERIE INTO R_REF_DOC_NO SEPARATED BY '-'.

    ELSE.

      CALL FUNCTION 'J_1B_NF_NUMBER_CONDENSE'
        EXPORTING
          NF_NUMBER  = I_NF_NUMBER
          SERIES     = I_SERIES
          SUBSERIES  = I_SUBSERIES
          NF_NUMBER9 = I_NF_NUMBER9
        IMPORTING
          REF_NUMBER = R_REF_DOC_NO.

    ENDIF.

  ENDMETHOD.


  METHOD GET_DATA_VENCIMENTO_COND_PAG.

    DATA: TAG1          TYPE BSEG-ZBD1T, " help variables
          TAG2          TYPE BSEG-ZBD2T, " for due date
          TAG3          TYPE BSEG-ZBD3T,
          HELP_DUE_DATE TYPE BSEG-ZFBDT.

    SELECT SINGLE * INTO @DATA(WA_T052)
      FROM T052
     WHERE ZTERM EQ @I_ZTERM.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_COND_PAG_NOT_FOUND-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_COND_PAG_NOT_FOUND-MSGNO
                            ATTR1 = CONV #( I_ZTERM ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_COND_PAG_NOT_FOUND-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_COND_PAG_NOT_FOUND-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_ZTERM ).
    ENDIF.

    CASE WA_T052-ZDART.
      WHEN 'C'.
        R_DT_VENCIMENTO = I_DT_CONTABIL.
      WHEN 'D'.
        R_DT_VENCIMENTO = I_DT_LANCAMENTO.
      WHEN 'B'.
        R_DT_VENCIMENTO = I_DT_DOCUMENTO.
      WHEN OTHERS.
        R_DT_VENCIMENTO = I_DT_DOCUMENTO.
    ENDCASE.

    IF NOT WA_T052-ZFAEL IS INITIAL.
      CONCATENATE R_DT_VENCIMENTO(6) WA_T052-ZFAEL INTO R_DT_VENCIMENTO.
    ENDIF.

    IF NOT WA_T052-ZMONA IS INITIAL.
      CALL FUNCTION 'MONTH_PLUS_DETERMINE'
        EXPORTING
          MONTHS  = WA_T052-ZMONA
          OLDDATE = R_DT_VENCIMENTO
        IMPORTING
          NEWDATE = R_DT_VENCIMENTO.
    ENDIF.

    TAG1 = WA_T052-ZTAG1.
    TAG2 = WA_T052-ZTAG2.
    TAG3 = WA_T052-ZTAG3.

    CALL FUNCTION 'J_1B_FI_NETDUE'
      EXPORTING
        ZFBDT   = R_DT_VENCIMENTO
        ZBD1T   = TAG1
        ZBD2T   = TAG2
        ZBD3T   = TAG3
        ZSTG1   = WA_T052-ZSTG1
        ZSMN1   = WA_T052-ZSMN1
        ZSTG2   = WA_T052-ZSTG2
        ZSMN2   = WA_T052-ZSMN2
        ZSTG3   = WA_T052-ZSTG3
        ZSMN3   = WA_T052-ZSMN3
      IMPORTING
        DUEDATE = HELP_DUE_DATE
      EXCEPTIONS
        OTHERS  = 1.

    IF SY-SUBRC = 0.
      R_DT_VENCIMENTO  = HELP_DUE_DATE.
      "R_DT_VENCIMENTO  = ZCL_MIRO=>GET_PROXIMO_DIA_UTIL( EXPORTING I_DATA_BASE = R_DT_VENCIMENTO ).
      ZCL_MIRO=>GET_PROXIMO_VENC_FATURA( IMPORTING E_DATA_VENCIMENTO = DATA(R_DATA_PERMITIDA) ).
      IF R_DATA_PERMITIDA GT R_DT_VENCIMENTO.
        R_DT_VENCIMENTO = R_DATA_PERMITIDA.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_FORMAPAG_BANCO_EMPRESA.

    CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
      EXPORTING
        P_BUKRS           = I_BUKRS
        P_LIFNR           = I_LIFNR
        P_BVTYP           = I_BVTYP
        P_ZLSCH           = I_ZLSCH
      IMPORTING
        P_FORMA_PAGAMENTO = E_FORMA_PAGAMENTO
        P_PRINC_BNC_EMP   = E_BANCO_EMPRESA
      EXCEPTIONS
        NAO_FORNECEDOR    = 1
        FORNECEDOR_CONTA  = 2
        FORNECEDOR_BANCO  = 3
        FAIXA_VALOR       = 4
        BANCO_EMPRESA     = 5
        OTHERS            = 6.

    IF SY-SUBRC IS NOT INITIAL.
      TRY .
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO INTO DATA(I_TEXTO) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ZCL_MIRO=>GERA_ERRO_GERAL( EXPORTING I_TEXTO = I_TEXTO ).
        CATCH ZCX_MIRO_EXCEPTION .
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MES_PERMITIDO_FECHAMENTO.

    DATA: LC_MONAT TYPE MONAT,
          LC_GJAHR TYPE GJAHR.

    CLEAR: R_DATA.

    "Se Mes Atual for Maior
    IF SY-DATUM(6) GT I_DATA(6).

      DATA(LC_DATA_TESTE) = I_DATA.
      DATA(CK_PERMITIDO)  = ABAP_FALSE.
      LC_MONAT = I_DATA(4).
      LC_GJAHR = I_DATA+4(2).

      WHILE CK_PERMITIDO EQ ABAP_FALSE.

        SELECT SINGLE * FROM ZFIT0033 INTO @DATA(WA_ZFIT0033) WHERE MONAT EQ @LC_MONAT AND GJAHR EQ @LC_GJAHR AND USNAM EQ @I_USER.

        "Se Achou Permissão
        "Senão soma 1 mês
        IF SY-SUBRC IS INITIAL.
          CK_PERMITIDO = ABAP_TRUE.

          "Se data testada for a mesma data passada retorna ela mesma
          "Senão retorna primeiro dia do mês testado
          IF LC_DATA_TESTE EQ I_DATA.
            R_DATA = I_DATA.
          ELSE.
            R_DATA = LC_DATA_TESTE && '01'.
          ENDIF.
        ELSE.

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = LC_DATA_TESTE
              DAYS      = 0
              MONTHS    = 1
              YEARS     = 0
            IMPORTING
              CALC_DATE = LC_DATA_TESTE.

          IF SY-DATUM(6) EQ LC_DATA_TESTE(6).
            CK_PERMITIDO = ABAP_TRUE.
            R_DATA       = LC_DATA_TESTE && '01'.
          ENDIF.

        ENDIF.

      ENDWHILE.

    ELSE.
      R_DATA = I_DATA.
    ENDIF.

  ENDMETHOD.


  METHOD GET_MIRO_PAGA.

    DATA: VG_AWKEY TYPE AWKEY.

    CLEAR: R_MIRO_PAGA.

    CONCATENATE I_BELNR I_GJAHR INTO VG_AWKEY.

    SELECT SINGLE * INTO @DATA(WA_BKPF)
      FROM BKPF
     WHERE AWTYP EQ 'RMRP'
       AND AWKEY EQ @VG_AWKEY.

    CHECK SY-SUBRC IS INITIAL.

    SELECT SINGLE * INTO @DATA(WA_BSAK)
      FROM BSAK
     WHERE BUKRS EQ @WA_BKPF-BUKRS
       AND GJAHR EQ @WA_BKPF-GJAHR
       AND BELNR EQ @WA_BKPF-BELNR.

    CHECK SY-SUBRC IS INITIAL.

    E_AUGDT = WA_BSAK-AUGDT.
    E_AUGBL = WA_BSAK-AUGBL.
    R_MIRO_PAGA = ABAP_TRUE.


  ENDMETHOD.


  METHOD GET_PRIORIDADE_BANCO_FORNE.

    TRY .

        ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
          )->SET_PARCEIRO( I_PARCEIRO = I_LIFNR
          )->GET_DADOS_BANCARIOS( IMPORTING E_BANCOS = DATA(E_BANCOS)
          ).

        SORT E_BANCOS ASCENDING BY BVTYP.
        READ TABLE E_BANCOS INTO DATA(WA_BANCOS) INDEX 1.
        R_BVTYP = WA_BANCOS-BVTYP.

      CATCH ZCX_PARCEIROS INTO DATA(EX_PARCEIROS).
        MESSAGE ID EX_PARCEIROS->MSGID TYPE 'S' NUMBER EX_PARCEIROS->MSGNO INTO DATA(MTEXT) WITH EX_PARCEIROS->MSGV1 EX_PARCEIROS->MSGV2 EX_PARCEIROS->MSGV3 EX_PARCEIROS->MSGV4.
        ZCL_MIRO=>GERA_ERRO_GERAL( I_TEXTO = MTEXT ).
    ENDTRY.

  ENDMETHOD.


  METHOD GET_PROXIMO_DIA_UTIL.

    DATA: IT_DATAS               TYPE TABLE OF ISCAL_DAY,
          VDIAS                  TYPE I,
          R_CONSIDERAR_ZLEST0195 TYPE C,
          WA_ZLEST0195           TYPE ZLEST0195.

    R_DATA = I_DATA_BASE.
    R_CONSIDERAR_ZLEST0195 = I_CK_DATA_ZLES0145.
    VDIAS  = 0.

    WHILE VDIAS = 0.

      CLEAR: IT_DATAS[], IT_DATAS.

** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
*      "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
*      DATA: lv_hol_cal       TYPE scal-hcalid,
*            lv_fac_cal       TYPE scal-fcalid,
*            lv_nome_processo TYPE ze_nomep.
*
*      lv_nome_processo = sy-tcode.
*
*      zcl_calendario=>get_calendario(
*        EXPORTING
*          I_BUKRS            = i_bukrs
**          I_TIPO_PROCESSO    = 'T'
**          I_NOME_PROCESSO    = lv_nome_processo
*        IMPORTING
*          E_HOLIDAY_CALENDAR = lv_hol_cal
*          E_FACTORY_CALENDAR = lv_fac_cal ).
*
*      IF lv_hol_cal IS INITIAL.
*        lv_hol_cal = 'BR'.
*      ENDIF.
*      IF ( lv_fac_cal IS INITIAL ).
*        lv_fac_cal = 'ZF'.
*      ENDIF.
*      "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
*          HOLIDAY_CALENDAR = lv_hol_cal "USER STORY 158527 - MMSILVA - 17.01.2025
*          FACTORY_CALENDAR = lv_fac_cal "USER STORY 158527 - MMSILVA - 17.01.2025
          HOLIDAY_CALENDAR = 'MG'
          FACTORY_CALENDAR = 'ZT'
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
          DATE_FROM        = R_DATA
          DATE_TO          = R_DATA
        TABLES
          HOLIDAYS         = IT_DATAS
        EXCEPTIONS
          OTHERS           = 1.

      IF IT_DATAS[] IS NOT INITIAL.

        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
          EXPORTING
            DATE      = R_DATA
            DAYS      = 1
            MONTHS    = 0
            YEARS     = 0
            SIGNUM    = I_SIGNUM
          IMPORTING
            CALC_DATE = R_DATA.

      ELSEIF IT_DATAS[] IS INITIAL AND R_CONSIDERAR_ZLEST0195 = 'X'.
        CLEAR WA_ZLEST0195.
        SELECT SINGLE * FROM ZLEST0195 INTO WA_ZLEST0195 WHERE DT_PGTO = R_DATA.

        IF WA_ZLEST0195 IS NOT INITIAL.

          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
            EXPORTING
              DATE      = R_DATA
              DAYS      = 1
              MONTHS    = 0
              YEARS     = 0
              SIGNUM    = I_SIGNUM
            IMPORTING
              CALC_DATE = R_DATA.

        ELSE.
          VDIAS = 1.
        ENDIF.
      ELSE.
        VDIAS = 1.
      ENDIF.

*      IF IT_DATAS[] IS INITIAL. "AND R_CONSIDERAR_ZLEST0195 = 'X'.
*
*        SELECT SINGLE * FROM ZLEST0195 INTO WA_ZLEST0195 WHERE DT_PGTO = R_DATA.
*
*        IF WA_ZLEST0195 IS NOT INITIAL.
*
*          CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*          EXPORTING
*            DATE      = R_DATA
*            DAYS      = 1
*            MONTHS    = 0
*            YEARS     = 0
*            SIGNUM    = I_SIGNUM
*          IMPORTING
*            CALC_DATE = R_DATA.
*
*        ENDIF.
*
*        VDIAS = 1.
*
*      ELSE.
*        CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*          EXPORTING
*            DATE      = R_DATA
*            DAYS      = 1
*            MONTHS    = 0
*            YEARS     = 0
*            SIGNUM    = I_SIGNUM
*          IMPORTING
*            CALC_DATE = R_DATA.
*      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD GET_PROXIMO_VENC_FATURA.


    DATA: E_DATA_FINAL TYPE DATUM,
          IT_DATAS     TYPE TABLE OF ISCAL_DAY,
          WA_DATAS     TYPE ISCAL_DAY,
          VDIAS2       TYPE I,
          VDIAS        TYPE I.

    E_DATA_VENCIMENTO = SY-DATUM - 1.
    VDIAS             = 0.

    "72 Horas úteis (3 dias)
    VDIAS2 = 3.
    IF ( SY-TCODE = 'ZMM0116' OR SY-TCODE = 'ZFIS63' ) AND I_CK_REVISAO NE 'X'.
      VDIAS2 = 5.
    ENDIF.
*    WHILE VDIAS <= 3. "72 horas
    WHILE VDIAS <= VDIAS2.  "120 horas

      CLEAR: IT_DATAS[], IT_DATAS.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          DATE      = E_DATA_VENCIMENTO
          DAYS      = 1
          MONTHS    = 0
          YEARS     = 0
        IMPORTING
          CALC_DATE = E_DATA_VENCIMENTO.

** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
*      "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
*      DATA: lv_hol_cal       TYPE scal-hcalid,
*            lv_fac_cal       TYPE scal-fcalid,
*            lv_nome_processo TYPE ze_nomep.
*
*      lv_nome_processo = sy-tcode.
*
*      zcl_calendario=>get_calendario(
*        EXPORTING
*          I_BUKRS            = i_bukrs
**          I_TIPO_PROCESSO    = 'T'
**          I_NOME_PROCESSO    = lv_nome_processo
*        IMPORTING
*          E_HOLIDAY_CALENDAR = lv_hol_cal
*          E_FACTORY_CALENDAR = lv_fac_cal ).
*
*      IF lv_hol_cal IS INITIAL.
*        lv_hol_cal = 'BR'.
*      ENDIF.
*      IF lv_fac_cal IS INITIAL.
*        lv_fac_cal = 'ZF'.
*      ENDIF.
**      "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
*         holiday_calendar = lv_hol_cal "USER STORY 158527 - MMSILVA - 17.01.2025
*         factory_calendar = lv_fac_cal "USER STORY 158527 - MMSILVA - 17.01.2025
          HOLIDAY_CALENDAR = 'MG'
          FACTORY_CALENDAR = 'ZT'
** comentado para subir para PRD WBARBOSA 02/07/2025 Projeto Insumos
          DATE_FROM        = E_DATA_VENCIMENTO
          DATE_TO          = E_DATA_VENCIMENTO
        TABLES
          HOLIDAYS         = IT_DATAS
        EXCEPTIONS
          OTHERS           = 1.

      READ TABLE IT_DATAS INTO WA_DATAS WITH KEY DATE = E_DATA_VENCIMENTO.
      IF SY-SUBRC NE 0.
        "Feriados Extras
        SELECT SINGLE *
          FROM ZMMT0117
          INTO  @DATA(W_117)
          WHERE DATA = @E_DATA_VENCIMENTO.
        IF SY-SUBRC = 0.
          WA_DATAS-DATE       = E_DATA_VENCIMENTO.
          WA_DATAS-FREEDAY    = ' '.
          WA_DATAS-HOLIDAY    = 'X'.
          WA_DATAS-HOLIDAY_ID = ' '.
          WA_DATAS-TXT_SHORT  = W_117-DESCR_FER(10).
          WA_DATAS-TXT_LONG   = W_117-DESCR_FER(30).
          APPEND WA_DATAS TO IT_DATAS.
        ENDIF.
      ENDIF.

      IF IT_DATAS[] IS INITIAL.
        ADD 1 TO VDIAS.
      ELSE.
        CLEAR: IT_DATAS[].
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD GET_PROX_DATA_VENC_CSC_SE.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT,
          LC_XML_NODE TYPE REF TO IF_IXML_NODE,
          VAR_HTTP    TYPE REF TO IF_HTTP_CLIENT,
          XML_RETORNO TYPE STRING.

    DATA(LC_BODY) = '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">' &&
   '<soapenv:Header/>' &&
   '<soapenv:Body>' &&
      '<getMiroDeMaisUm soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>' &&
   '</soapenv:Body>' &&
   '</soapenv:Envelope>' .

    CL_HTTP_CLIENT=>CREATE_BY_URL(    EXPORTING URL    = 'https://csc.amaggi.com.br/se/api/webservicesoap'
                                      IMPORTING CLIENT = VAR_HTTP
                                      EXCEPTIONS ARGUMENT_NOT_FOUND = 1
                                                 PLUGIN_NOT_ACTIVE  = 2
                                                 INTERNAL_ERROR     = 3
                                    ).


    DATA(LC_TAMANHO) = ZCL_STRING=>LENGTH( TEXT = LC_BODY ).

    VAR_HTTP->REQUEST->SET_HEADER_FIELD(
      EXPORTING
        NAME  = 'Content-Length'
        VALUE = CONV #( LC_TAMANHO ) ).

    VAR_HTTP->REQUEST->SET_CONTENT_TYPE( CONTENT_TYPE = 'Content-Type: Content-Type: text/xml;charset=utf-8' ).

    VAR_HTTP->REQUEST->SET_METHOD( METHOD = 'POST' ).

    VAR_HTTP->REQUEST->SET_CDATA(
      EXPORTING
        DATA   = LC_BODY
        OFFSET = 0
        LENGTH = LC_TAMANHO ).

    CALL METHOD VAR_HTTP->SEND
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E028(ZSIMETRYA) WITH 'SoftExpert: Send WebService' INTO DATA(LC_MSG).
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
      WHEN 2.
        MESSAGE E029(ZSIMETRYA) WITH 'SoftExpert: Send WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
      WHEN 3.
        MESSAGE E030(ZSIMETRYA) WITH 'SoftExpert: Send WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
      WHEN 4.
        MESSAGE E031(ZSIMETRYA) WITH 'SoftExpert: Send WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
    ENDCASE.

    CALL METHOD VAR_HTTP->RECEIVE
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3.

    CASE SY-SUBRC.
      WHEN 1.
        MESSAGE E028(ZSIMETRYA) WITH 'SoftExpert: Receive WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
      WHEN 2.
        MESSAGE E029(ZSIMETRYA) WITH 'SoftExpert: Receive WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
      WHEN 3.
        MESSAGE E030(ZSIMETRYA) WITH 'SoftExpert: Receive WebService' INTO LC_MSG.
        ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = LC_MSG ).
    ENDCASE.

    XML_RETORNO = VAR_HTTP->RESPONSE->GET_CDATA( ).

    VAR_HTTP->CLOSE(
      EXCEPTIONS
        HTTP_INVALID_STATE = 1
        OTHERS             = 2
    ).

    CREATE OBJECT LC_XML_RET.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = XML_RETORNO
      RECEIVING
        RETCODE = LC_TAMANHO.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Envelope'
      RECEIVING
        NODE = LC_XML_NODE.

    IF ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ).
      DATA(LC_VALOR) = LC_XML_NODE->GET_VALUE( ).
      IF ZCL_STRING=>LENGTH( LC_VALOR ) GE 1.
        "Validar Qtd Caracteres
        IF ZCL_STRING=>LENGTH( LC_VALOR ) LT 10.
          ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = 'SoftExpert: Data Vencimento deve estar no padrão dd/mm/yyyy' ).
        ENDIF.
        "Validar Padrão de Data
        FIND REGEX '[0-9]{2}[/]{1}[0-9]{2}[/]{1}[0-9]{4}' IN LC_VALOR.
        IF SY-SUBRC IS NOT INITIAL.
          ZCX_ERROR=>ZIF_ERROR~GERA_ERRO_GERAL( I_TEXTO = 'SoftExpert: Data Vencimento deve estar no padrão dd/mm/yyyy' ).
        ENDIF.
      ENDIF.
      R_DATA = LC_VALOR+6(4) && LC_VALOR+3(2)  && LC_VALOR(2).
    ENDIF.

  ENDMETHOD.


  METHOD get_taxas_iva.

    DATA: lc_tax_icms   TYPE c LENGTH 2,
          lc_vencimento TYPE c LENGTH 08.

    DATA: ws_value2    TYPE j_1btxic3-value2,
          ws_value3    TYPE j_1btxic3-value3, "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
          ws_value     TYPE j_1btxic3-value,
          input(200)   TYPE c,
          vg_dt_inv(9) TYPE c.

    e_base_icms = 100.

    SELECT SINGLE * INTO @DATA(wa_iva)
      FROM t007a
     WHERE kalsm EQ 'TAXBRA'
       AND mwskz EQ @i_iva.

    "// WBARBOSA 29/09/2025 correção data invertida inicio
    DATA(lc_data) = 99999999 - i_data.
    input = i_data.
    vg_dt_inv     = '99999999' - input.
    "// WBARBOSA 29/09/2025 correção data invertida Fim

    "J_1BTAXLW1  J_1BTAXLW1  CHAR  3 Direito fiscal: ICMS
    "J_1BTAXLW2  J_1BTAXLW2  CHAR  3 Direito fiscal: IPI
    "J_1BTAXLW4  J_1BTAXLW4  CHAR  3 Lei tributária COFINS
    "J_1BTAXLW5  J_1BTAXLW5  CHAR  3 Lei tributária PIS

    "ICMS -----------------------------------------------------------------------------------------------------
    e_rate_icms = 0.
    e_rate_icms_diferencial = 0.
    CLEAR: ws_value2, ws_value.

    IF wa_iva-j_1btaxlw1 IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1batl1)
        FROM j_1batl1
       WHERE taxlaw EQ @wa_iva-j_1btaxlw1.

      CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
        EXPORTING
          input  = wa_j_1batl1-taxsit
        IMPORTING
          output = lc_tax_icms.

      CASE lc_tax_icms.
        WHEN '40' OR '41' OR '51' OR '50'.
          "40 - Isento
          "41 - Não tributada
          "51 - Diferimento
          "50 - Suspensão do ICMS
          e_rate_icms = 0.
        WHEN '00'.

          "SD-300925-ZMM0185-Ajustes Parametros Fiscais #191846 WPP ----->

          zcl_miro=>get_taxas_icms(
            EXPORTING
              i_data      =  i_data
              i_shipfrom  =  i_shipfrom
              i_shipto    =  i_shipto
              i_matnr     =  i_matnr
              i_werks     =  i_werks
              i_lifnr     =  i_lifnr
            IMPORTING
              e_rate_icms = e_rate_icms
              e_base_icms = e_base_icms   ).

*          00 - Tributada integralmente
*          e_rate_icms = 0.
*          SELECT * INTO TABLE @DATA(it_j_1btxic2)
*            FROM j_1btxic2
*           WHERE land1     EQ 'BR'
*             AND shipfrom  EQ @i_shipfrom
*             AND shipto    EQ @i_shipto
*             AND matnr     EQ @i_matnr
*             AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*             AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
**             AND validfrom GE @i_data "// WBARBOSA 29/09/2025 correção data invertida
**             AND validto   LE @i_data "// WBARBOSA 29/09/2025 correção data invertida
*            .
*
*          IF it_j_1btxic2 IS INITIAL.
*
*            ws_value  = i_werks.
*            ws_value2 = i_matnr.
*            ws_value3 = i_lifnr. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*
*            DATA(_achou_j_1btxic3) = abap_false.
*
*            "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
*            IF ws_value3 IS NOT INITIAL.
*
*              SELECT * FROM j_1btxic3
*                INTO TABLE @DATA(it_j_1btxic3)
*                 WHERE land1     EQ 'BR'
*                 AND shipfrom  EQ @i_shipfrom
*                 AND shipto    EQ @i_shipto
*                 AND value     EQ @ws_value
*                 AND value2    EQ @ws_value2
*                 AND value3    EQ @ws_value3
*                 AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*                 AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
**                 AND validfrom GE @i_data "// WBARBOSA 29/09/2025 correção data invertida
**                 AND validto   LE @i_data "// WBARBOSA 29/09/2025 correção data invertida
*                 AND gruop     EQ '31'.
*
*              IF it_j_1btxic3[] IS NOT INITIAL.
*                _achou_j_1btxic3 = abap_true.
*              ENDIF.
*
*            ELSE.
*
*              IF _achou_j_1btxic3 EQ abap_false. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*
*                SELECT * FROM j_1btxic3
*                  INTO TABLE it_j_1btxic3
*                   WHERE land1     EQ 'BR'
*                   AND shipfrom  EQ i_shipfrom
*                   AND shipto    EQ i_shipto
*                   AND value     EQ ws_value
*                   AND value2    EQ ws_value2 "i_matnr
*                   AND validfrom GE vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*                   AND validto   LE vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
**                   AND validfrom GE i_data "// WBARBOSA 29/09/2025 correção data invertida
**                   AND validto   LE i_data "// WBARBOSA 29/09/2025 correção data invertida
*                   AND gruop = '31'.
*
*                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
*                IF it_j_1btxic3[] IS NOT INITIAL.
*                  _achou_j_1btxic3 = abap_true.
*                ENDIF.
*                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--
*
*              ENDIF. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*
*              IF _achou_j_1btxic3 EQ abap_false. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*
*                SELECT * FROM j_1btxic3
*                 INTO TABLE it_j_1btxic3
*                  WHERE land1   EQ 'BR'
*                  AND shipfrom  EQ i_shipfrom
*                  AND shipto    EQ i_shipto
*                  AND value     EQ ws_value
*                  AND value2    EQ ws_value2
*                  AND validfrom GE vg_dt_inv
*                  AND validto   LE vg_dt_inv
*                  AND gruop = '31'.
*
*                IF it_j_1btxic3[] IS NOT INITIAL.
*                  _achou_j_1btxic3 = abap_true.
*                ENDIF.
*
*              ENDIF. "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
*
*
*            ENDIF.
*            "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--
*
*
*          ENDIF.
*
*          IF it_j_1btxic2 IS NOT INITIAL OR it_j_1btxic3 IS NOT INITIAL.
*            READ TABLE it_j_1btxic2 INTO DATA(wa_j_1btxic2) INDEX 1.
*            IF sy-subrc EQ 0.
*              e_rate_icms = wa_j_1btxic2-rate.
*              e_base_icms = wa_j_1btxic2-base.
*
*            ELSE.
*              READ TABLE it_j_1btxic3 INTO DATA(wa_j_1btxic3) INDEX 1.
*              IF sy-subrc EQ 0.
*                e_rate_icms = wa_j_1btxic3-rate.
*                e_base_icms = wa_j_1btxic3-base.
*              ENDIF.
*            ENDIF.
*
*          ELSE.
*            SELECT * INTO TABLE @DATA(it_j_1btxic1)
*              FROM j_1btxic1
*             WHERE land1    EQ 'BR'
*               AND shipfrom EQ @i_shipfrom
*               AND shipto   EQ @i_shipto.
*
*            IF sy-subrc IS INITIAL.
*              LOOP AT it_j_1btxic1 INTO DATA(wa_j_1btxic1).
*
*                lc_vencimento = 99999999 - wa_j_1btxic1-validfrom.
*
*                IF ( lc_vencimento LE i_data ) AND ( e_rate_icms EQ 0 ).
*                  e_rate_icms = wa_j_1btxic1-rate.
*                ENDIF.
*              ENDLOOP.
*            ENDIF.
*          ENDIF.

          "SD-300925-ZMM0185-Ajustes Parametros Fiscais #191846 WPP <<<<---------------

        WHEN  '10' OR '20' OR '30' OR '60' OR '70'.
          "10 - Tributada e com cobrança do ICMS por substituição tributária
          "20 - Com redução de base de cálculo
          "30 - Isenta ou não tributada e com cobrança do ICMS por substitui
          "60 - ICMS cobrado anteriormente por substituição tributária
          "70 - Com redução de base de cálculo e cobrança do ICMS por Sub.T.
          e_rate_icms = 0.
        WHEN '90'.

          IF i_valor_icms IS NOT INITIAL.

            SELECT * INTO TABLE @DATA(it_j_1btxic2)
              FROM j_1btxic2
             WHERE land1     EQ 'BR'
               AND shipfrom  EQ @i_shipfrom
               AND shipto    EQ @i_shipto
               AND matnr     EQ @i_matnr
               AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
               AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*               and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*               and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
              .

            IF it_j_1btxic2 IS INITIAL.
              SELECT * FROM j_1btxic3
              INTO TABLE @DATA(it_j_1btxic3)
               WHERE land1     EQ 'BR'
               AND shipfrom  EQ @i_shipfrom
               AND shipto    EQ @i_shipto
               AND value     EQ @i_werks
               AND value2    EQ @i_matnr
               AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
               AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*               and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*               and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
               AND gruop = '31'.
            ENDIF.

            IF it_j_1btxic2 IS NOT INITIAL OR it_j_1btxic3 IS NOT INITIAL.
              READ TABLE it_j_1btxic2 INDEX 1 INTO DATA(wa_j_1btxic2).
              IF sy-subrc EQ 0.
                e_rate_icms = wa_j_1btxic2-rate.
                e_base_icms = wa_j_1btxic2-base.
              ELSE.
                READ TABLE it_j_1btxic3 INDEX 1 INTO DATA(wa_j_1btxic3).
                IF sy-subrc EQ 0.
                  e_rate_icms = wa_j_1btxic3-rate.
                  e_base_icms = wa_j_1btxic3-base.
                ENDIF.
              ENDIF.
            ELSE.
              SELECT * INTO TABLE @DATA(it_j_1btxic1)
                FROM j_1btxic1
               WHERE land1    EQ 'BR'
                 AND shipfrom EQ @i_shipfrom
                 AND shipto   EQ @i_shipto.

              IF sy-subrc IS INITIAL.
                LOOP AT it_j_1btxic1 INTO DATA(wa_j_1btxic1).

                  lc_vencimento = 99999999 - wa_j_1btxic1-validfrom.

                  IF ( lc_vencimento LE i_data ) AND ( e_rate_icms EQ 0 ).
                    e_rate_icms = wa_j_1btxic1-rate.
                  ENDIF.

                ENDLOOP.
              ENDIF.
            ENDIF.

            SELECT * INTO TABLE @it_j_1btxic2
              FROM j_1btxic2
             WHERE land1     EQ 'BR'
               AND shipfrom  EQ @i_shipto
               AND shipto    EQ @i_shipto
               AND matnr     EQ @i_matnr
               AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
               AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*               and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*               and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
              .


            SELECT * FROM j_1btxic3
            INTO TABLE @it_j_1btxic3
             WHERE land1     EQ 'BR'
             AND shipfrom  EQ @i_shipfrom
             AND shipto    EQ @i_shipto
             AND value     EQ @i_werks
             AND value2    EQ @i_matnr
             AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
             AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*             and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*             and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
             AND gruop = '31'.


            IF it_j_1btxic2 IS NOT INITIAL OR it_j_1btxic2 IS NOT INITIAL.
              READ TABLE it_j_1btxic2 INDEX 1 INTO wa_j_1btxic2.
              IF sy-subrc EQ 0.
                e_rate_icms_diferencial = wa_j_1btxic2-rate.
                e_rate_icms_diferencial = wa_j_1btxic2-base.
              ELSE.
                READ TABLE it_j_1btxic3 INDEX 1 INTO wa_j_1btxic3.
                IF sy-subrc EQ 0.
                  e_rate_icms_diferencial = wa_j_1btxic3-rate.
                  e_rate_icms_diferencial = wa_j_1btxic3-base.
                ENDIF.
              ENDIF.

            ELSE.
              SELECT * INTO TABLE @it_j_1btxic1
                FROM j_1btxic1
               WHERE land1    EQ 'BR'
                 AND shipfrom EQ @i_shipto
                 AND shipto   EQ @i_shipto.

              IF sy-subrc IS INITIAL.
                LOOP AT it_j_1btxic1 INTO wa_j_1btxic1.

                  lc_vencimento = 99999999 - wa_j_1btxic1-validfrom.

                  IF ( lc_vencimento LE i_data ) AND ( e_rate_icms_diferencial EQ 0 ).
                    e_rate_icms_diferencial = wa_j_1btxic1-rate.
                  ENDIF.

                ENDLOOP.
              ENDIF.
            ENDIF.

            e_rate_icms_diferencial = e_rate_icms_diferencial - e_rate_icms.

          ENDIF.
          "IF I_ICMS_BASE IS NOT INITIAL AND I_VALOR_ICMS IS NOT INITIAL.
          "  E_RATE_ICMS = ( I_VALOR_ICMS / I_ICMS_BASE ) * 100.
          "ENDIF.
          "90 - Outros/as
      ENDCASE.

    ENDIF.

    "COFINS ----------------------------------------------------------------------------------------------------
    e_rate_cofins = 0.

    IF wa_iva-j_1btaxlw4 IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1batl4a)
        FROM j_1batl4a
       WHERE taxlaw EQ @wa_iva-j_1btaxlw4.

      CASE wa_j_1batl4a-taxsit.
        WHEN '70'. "Operação de Aquisição sem Direito a Crédito
        WHEN '98'. " Outras Operações de Entrada (Nestes CST serão informadas as entradas decorrentes de retorno de remessa para industrialização, conserto, demonstração...)
          "WHEN '56'. "Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno e de Exportação
        WHEN OTHERS.
          "COFINS
          SELECT * INTO TABLE @DATA(it_j_1btxcof)
            FROM j_1btxcof
           WHERE country   EQ 'BR'
             AND gruop     EQ '71'
             AND value     EQ @i_bbranc
             AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
             AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*             and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*             and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
            .

          IF sy-subrc IS INITIAL.
            READ TABLE it_j_1btxcof INTO DATA(wa_j_1btxcof) INDEX 1.
            e_rate_cofins = wa_j_1btxcof-rate.
          ENDIF.
      ENDCASE.

    ENDIF.

    "PIS -------------------------------------------------------------------------------------------------------
    e_rate_pis = 0.

    IF wa_iva-j_1btaxlw5 IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_j_1batl5)
        FROM j_1batl5
       WHERE taxlaw EQ @wa_iva-j_1btaxlw5.

      CASE wa_j_1batl5-taxsit.
        WHEN '70'. "Operação de Aquisição sem Direito a Crédito
        WHEN '98'. " Outras Operações de Entrada (Nestes CST serão informadas as entradas decorrentes de retorno de remessa para industrialização, conserto, demonstração...)
          "WHEN '56'. "Operação com Direito a Crédito - Vinculada a Receitas Tributadas e Não-Tributadas no Mercado Interno e de Exportação
        WHEN OTHERS.
          SELECT * INTO TABLE @DATA(it_j_1btxpis)
            FROM j_1btxpis
           WHERE country   EQ 'BR'
             AND gruop     EQ '72'
             AND value     EQ @i_bbranc
             AND validfrom GE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
             AND validto   LE @vg_dt_inv "// WBARBOSA 29/09/2025 correção data invertida
*             and validfrom ge @i_data "// WBARBOSA 29/09/2025 correção data invertida
*             and validto   le @i_data "// WBARBOSA 29/09/2025 correção data invertida
            .

          IF sy-subrc IS INITIAL.
            READ TABLE it_j_1btxpis INTO DATA(wa_j_1btxpis) INDEX 1.
            e_rate_pis = wa_j_1btxpis-rate.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD LIMPAR.

    CLEAR: ME->AT_FISCALYEAR,
           ME->AT_FISCALYEAR_ESTORNO,
           ME->AT_INVOICEDOCNUMBER,
           ME->AT_INVOICEDOCNUMBER_ESTORNO,
           ME->AT_RETORNO,
           ME->AT_RETORNO_ESTORNO.

  ENDMETHOD.


  METHOD set_simular.
    ME->at_simular = i_simular.
  ENDMETHOD.


  METHOD VERIFICAR_CHAVE_BLOQUEIO.

    IF ( I_BSART EQ 'ZSEM' OR I_BSART EQ 'ZFTE' ) AND ( I_ZLSPR NE 'I' ).
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_BLOQUEIO-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_BLOQUEIO-MSGNO
                            ATTR1 = CONV #( I_BSART ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_BLOQUEIO-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_BLOQUEIO-MSGNO
          MSGV1  = CONV #( I_BSART )
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  method verificar_cod_barra.

    data: lc_esrre           type bseg-esrre,
          lc_esrnr           type bseg-esrnr,
          lc_data_vencimento type zde_dt_vencimento,
          lc_qtd             type i,
          i_boleto_out       type c length 55.

    try .
        data(qtd_length) = strlen( i_boleto ).

        if qtd_length lt 47.
          raise exception type zcx_miro_exception
            exporting
              textid = value #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
                                msgno = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno )
              msgid  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
              msgno  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno
              msgty  = 'E'.
        endif.

        lc_data_vencimento = '19971007'.

        call function 'CONVERSION_EXIT_ZBOLE_OUTPUT'
          exporting
            input  = i_boleto
          importing
            output = i_boleto_out.

        lc_qtd = i_boleto_out+40(4).
        "US163807
        if i_dt_vencimento <= '20250221'.
          lc_data_vencimento = lc_data_vencimento + lc_qtd.
        else.
          lc_data_vencimento = '20250222'.
          lc_qtd = lc_qtd - 1000.
          lc_data_vencimento = lc_data_vencimento + lc_qtd.
        endif.
        "US163807


        "Jogar para o próximo dia útil
        zcl_miro=>get_proximo_dia_util(
          exporting
            i_data_base = lc_data_vencimento
          receiving
            r_data      = lc_data_vencimento
          exceptions
            erro        = 1
            others      = 2 ).

        if sy-subrc is not initial.
          raise exception type zcx_miro_exception
            exporting
              textid = value #( msgno = sy-msgno
                                msgid = sy-msgid )
              msgid  = sy-msgid
              msgno  = sy-msgno
              msgty  = 'E'
              msgv1  = sy-msgv1
              msgv2  = sy-msgv2
              msgv3  = sy-msgv3
              msgv4  = sy-msgv4.
        endif.

        if lc_data_vencimento lt i_dt_vencimento.
          raise exception type zcx_miro_exception
            exporting
              textid = value #( msgid = zcx_miro_exception=>zcx_data_barra_boleto_vcto-msgid
                                msgno = zcx_miro_exception=>zcx_data_barra_boleto_vcto-msgno
                                attr1 = conv #( i_dt_vencimento ) )
              msgid  = zcx_miro_exception=>zcx_data_barra_boleto_vcto-msgid
              msgno  = zcx_miro_exception=>zcx_data_barra_boleto_vcto-msgno
              msgv1  = conv #( i_dt_vencimento )
              msgty  = 'E'.
        endif.

        data: lc_barcode type c length 47.
        call function 'CONVERSION_EXIT_ZBOLE_INPUT'
          exporting
            input  = i_boleto
          importing
            output = lc_barcode.

        call function 'CONVERT_BARCODE'
          exporting
            barcode   = lc_barcode
            dmbtr     = i_valor
          importing
            esrre     = lc_esrre
            esrnr     = lc_esrnr
          exceptions
            not_valid = 1
            others    = 2.

        if sy-subrc is not initial.
          raise exception type zcx_miro_exception
            exporting
              textid = value #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
                                msgno = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno )
              msgid  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
              msgno  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno
              msgty  = 'E'.
        endif.

      catch cx_root into data(ex_root).
        zcl_miro=>gera_erro_geral( exporting i_texto = ex_root->if_message~get_text( ) ).
    endtry.


  endmethod.


  METHOD VERIFICAR_CRIAR.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        P_DATA_ENT     = I_DATA
        P_BUKRS        = I_BUKRS
        P_VAL_FI       = 'X'
        P_VAL_MM       = 'X'
      IMPORTING
        P_DATA_VAL     = R_DATA_VAL
      EXCEPTIONS
        DATA_FI_MM_NAO = 1
        OTHERS         = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_ESTORNO.

    DATA: VG_AUGDT   TYPE AUGDT,
          VG_AUGBL   TYPE AUGBL,
          R_DATA_VAL TYPE DATUM,
          LC_BUKRS   TYPE BUKRS,
          LC_DATA    TYPE DATUM.

    CALL METHOD ZCL_MIRO=>GET_MIRO_PAGA
      EXPORTING
        I_BELNR     = I_BELNR
        I_GJAHR     = I_GJAHR
      IMPORTING
        E_AUGDT     = DATA(E_AUGDT)
        E_AUGBL     = DATA(E_AUGBL)
      RECEIVING
        R_MIRO_PAGA = DATA(R_MIRO_PAGA).

    IF R_MIRO_PAGA EQ ABAP_TRUE.
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_MIRO_COMPENSADA-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_MIRO_COMPENSADA-MSGNO
                            ATTR1 = CONV #( I_BELNR )
                            ATTR2 = CONV #( I_GJAHR )
                            ATTR3 = CONV #( E_AUGDT )
                            ATTR4 = CONV #( E_AUGBL ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_MIRO_COMPENSADA-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_MIRO_COMPENSADA-MSGNO
          MSGV1  = CONV #( I_BELNR )
          MSGV2  = CONV #( I_GJAHR )
          MSGV3  = CONV #( E_AUGDT )
          MSGV4  = CONV #( E_AUGBL )
          MSGTY  = 'E'.
    ENDIF.

    SELECT SINGLE BUKRS INTO LC_BUKRS
      FROM RBKP
     WHERE BELNR EQ I_BELNR
       AND GJAHR EQ I_GJAHR.

    IF I_DATA IS INITIAL.
      LC_DATA = SY-DATUM.
    ELSE.
      LC_DATA = I_DATA.
    ENDIF.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        P_DATA_ENT     = LC_DATA
        P_BUKRS        = LC_BUKRS
        P_VAL_FI       = 'X'
        P_VAL_MM       = 'X'
      IMPORTING
        P_DATA_VAL     = R_DATA_VAL
      EXCEPTIONS
        DATA_FI_MM_NAO = 1
        OTHERS         = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_FI_MM_FECHADO-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_FORN_DOC_FISCAL.

    DATA: IT_NFE_FORN TYPE TABLE OF ZIB_NFE_FORN.

    CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
      EXPORTING
        P_LIFNR       = I_LIFNR
        P_PARVW       = I_PARVW
        P_NFTYPE      = I_NFTYPE
        P_XBLNR       = I_XBLNR
        P_DATA        = I_DATA
        P_WERKS       = I_WERKS
        P_RET_INF_XML = ABAP_TRUE
      TABLES
        T_NFE_FORN    = IT_NFE_FORN
      EXCEPTIONS
        ERROR         = 1
        OTHERS        = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGTY  = 'E'
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4
          MSGID  = SY-MSGID.
    ENDIF.

    MOVE IT_NFE_FORN[] TO E_NOTAS[].

  ENDMETHOD.


  METHOD VERIFICAR_TIPO_DOCUMENTO.

    IF ( I_BSART EQ 'ZDEF' OR I_BSART EQ 'ZSEM' OR I_BSART EQ 'ZFTE' ) AND ( I_BLART NE 'IN' ).
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_FATURA-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_FATURA-MSGNO
                            ATTR1 = CONV #( I_BSART ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_FATURA-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_TIPO_PEDIDO_FATURA-MSGNO
          MSGV1  = CONV #( I_BSART )
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_TIPO_PEDIDO.

    IF I_BSART = 'ZGR'.

      SELECT SINGLE * FROM SETLEAF INTO @DATA(WA_SETLEAF)
        WHERE SETNAME EQ 'MAGGI_PED_GRAOS'
         AND VALFROM  EQ @SY-UNAME.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_SOMENTE_VIA_SIGAM-MSGID
                              MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_SOMENTE_VIA_SIGAM-MSGNO
                              ATTR1 = CONV #( I_BSART ) )
            MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_SOMENTE_VIA_SIGAM-MSGID
            MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_SOMENTE_VIA_SIGAM-MSGNO
            MSGV1  = CONV #( I_BSART )
            MSGTY  = 'E'.
      ENDIF.

*    ELSEIF ( 'PCE_PCEI_PCS_PCSI_PCEF_PSEF' CS I_BSART ).
*      IF I_BUDAT NE SY-DATUM.
*        RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_DT_ATUAL-MSGID
*                              MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_DT_ATUAL-MSGNO )
*            MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_DT_ATUAL-MSGID
*            MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_DT_ATUAL-MSGNO
*            MSGTY  = 'E'.
*      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_VALOR_NFE.

    DATA: TX_TOTAL TYPE C LENGTH 30,
          TX_DIF   TYPE C LENGTH 30.

    SELECT SINGLE VL_TOTAL INTO @DATA(VL_TOTAL)
      FROM ZIB_NFE_DIST_TER
     WHERE CHAVE_NFE EQ @I_CHAVE_NFE.

    IF ( I_WAERS <> 'BRL' ) AND ( I_KURSF > 0 ) .
      DATA(LC_WRBTR) = I_WRBTR * I_KURSF.
      "DATA(LC_DESCONTO) = I_DESCONTO * I_KURSF.
    ELSE.
      LC_WRBTR    = I_WRBTR.
      "LC_DESCONTO = I_DESCONTO.
    ENDIF.

    "DATA(VL_DIF) = ( LC_WRBTR - LC_DESCONTO ) - VL_TOTAL.
    DATA(VL_DIF) = LC_WRBTR - VL_TOTAL.
    VL_DIF = ABS( VL_DIF ).

    IF VL_DIF > 2.

      WRITE VL_TOTAL TO TX_TOTAL.
      WRITE VL_DIF   TO TX_DIF.
      CONDENSE TX_TOTAL NO-GAPS.
      CONDENSE TX_DIF NO-GAPS.

      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_MIRO_EXCEPTION=>ZCX_VALOR_DIVERGENTE-MSGID
                            MSGNO = ZCX_MIRO_EXCEPTION=>ZCX_VALOR_DIVERGENTE-MSGNO
                            ATTR1 = CONV #( TX_TOTAL )
                            ATTR2 = CONV #( TX_DIF ) )
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_VALOR_DIVERGENTE-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_VALOR_DIVERGENTE-MSGNO
          MSGV1  = CONV #( TX_TOTAL )
          MSGV2  = CONV #( TX_DIF )
          MSGTY  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD verificar_vencimento_fatura.

    DATA: lc_proximo_vencimento TYPE datum,
          it_datas              TYPE TABLE OF iscal_day,
          wa_datas              TYPE iscal_day,
          var_hora(3).

    var_hora = '72'.
    IF i_ck_revisao NE 'X' .
      var_hora = '120'.
    ENDIF.
    DATA: lc_data TYPE c LENGTH 10.
    WRITE i_data_vencimento TO lc_data.

    CALL METHOD zcl_miro=>get_proximo_venc_fatura
      EXPORTING
        i_ck_revisao      = i_ck_revisao
        i_bukrs           = i_bukrs
      IMPORTING
        e_data_vencimento = lc_proximo_vencimento
      EXCEPTIONS
        erro              = 1
        OTHERS            = 2.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_miro_exception
        EXPORTING
          textid = VALUE #( msgid = sy-msgid
                            msgno = sy-msgno
                            attr1 = CONV #( sy-msgv1 )
                            attr2 = CONV #( sy-msgv2 )
                            attr3 = CONV #( sy-msgv3 )
                            attr4 = CONV #( sy-msgv4 ) )
          msgid  = sy-msgid
          msgno  = sy-msgno
          msgv1  = sy-msgv1
          msgv2  = sy-msgv2
          msgv3  = sy-msgv3
          msgv4  = sy-msgv4
          msgty  = 'E'.
    ENDIF.

*-CS2024000243-05.06.2024-#136397-JT-inicio
*   IF i_valida_politica = abap_false.
*    IF sy-tcode <> 'ZMM0116'.
*      IF i_data_vencimento LT lc_proximo_vencimento.
*        RAISE EXCEPTION TYPE zcx_miro_exception
*          EXPORTING
*            textid = VALUE #( msgid = zcx_miro_exception=>zcx_vencimento_72_horas-msgid
*                              msgno = zcx_miro_exception=>zcx_vencimento_72_horas-msgno
*                              attr1 = CONV #( lc_proximo_vencimento )
*                              attr2 = CONV #( var_hora ) )
*            msgid  = zcx_miro_exception=>zcx_vencimento_72_horas-msgid
*            msgno  = zcx_miro_exception=>zcx_vencimento_72_horas-msgno
*            msgv1  = CONV #( lc_proximo_vencimento )
*            msgv2  = CONV #( var_hora )
*            msgty  = 'E'.
*      ENDIF.
*    ELSE

     IF i_ck_fpol = abap_false.
      IF i_data_vencimento LT lc_proximo_vencimento.
        DATA(l_mesg1) = |Vencimento não pode ser menor que { lc_proximo_vencimento }|.
        DATA(l_mesg2) = |Regra de { var_hora } Horas!|.
        DATA(l_mesg3) = |Selecione Pagamento Fora da Política e |.
        DATA(l_mesg4) = |informe necessidade em Obs.Financ|.

        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_erro_geral-msgid
                              msgno = zcx_miro_exception=>zcx_erro_geral-msgno
                              attr1 = CONV #( l_mesg1 )
                              attr2 = CONV #( l_mesg2 )
                              attr3 = CONV #( l_mesg3 )
                              attr4 = CONV #( l_mesg4 ) )
            msgid  = zcx_miro_exception=>zcx_erro_geral-msgid
            msgno  = zcx_miro_exception=>zcx_erro_geral-msgno
            msgv1  = CONV #( l_mesg1 )
            msgv2  = CONV #( l_mesg2 )
            msgv3  = CONV #( l_mesg3 )
            msgv4  = CONV #( l_mesg4 )
            msgty  = 'E'.
      ENDIF.
    ELSEIF i_obs_financeira IS INITIAL.
      DATA(l_mesg_obs1) = |O campo Obs. Financeira é |.
      DATA(l_mesg_obs2) = |de preenchimento obrigatório!|.

      RAISE EXCEPTION TYPE zcx_miro_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_miro_exception=>zcx_erro_geral-msgid
                            msgno = zcx_miro_exception=>zcx_erro_geral-msgno
                            attr1 = CONV #( l_mesg_obs1 )
                            attr2 = CONV #( l_mesg_obs2 ) )
          msgid  = zcx_miro_exception=>zcx_erro_geral-msgid
          msgno  = zcx_miro_exception=>zcx_erro_geral-msgno
          msgv1  = CONV #( l_mesg_obs1 )
          msgv2  = CONV #( l_mesg_obs2 )
          msgty  = 'E'.
    ENDIF.
*-CS2024000243-05.06.2024-#136397-JT-fim

*** Comentado para Subir para PRD WBARBOSA Projeto Insumos 02/07/2025
**    "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
**    DATA: lv_hol_cal       TYPE scal-hcalid,
**          lv_fac_cal       TYPE scal-fcalid,
**          lv_nome_processo TYPE ze_nomep.
**
**    lv_nome_processo = sy-tcode.
**
**    zcl_calendario=>get_calendario(
**      EXPORTING
**        I_BUKRS            = i_bukrs
***        I_TIPO_PROCESSO    = 'T'
***        I_NOME_PROCESSO    = lv_nome_processo
**      IMPORTING
**        E_HOLIDAY_CALENDAR = lv_hol_cal
**        E_FACTORY_CALENDAR = lv_fac_cal ).
**
**    IF lv_hol_cal IS INITIAL.
**      lv_hol_cal = 'BR'.
**    ENDIF.
**    IF lv_fac_cal IS INITIAL.
**      lv_fac_cal = 'ZF'.
**    ENDIF.
**    "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim
*** Comentado para Subir para PRD WBARBOSA Projeto Insumos 02/07/2025

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
* Comentado para Subir para PRD WBARBOSA Projeto Insumos 02/07/2025
*        holiday_calendar = lv_hol_cal "USER STORY 158527 - MMSILVA - 17.01.2025
*        factory_calendar = lv_fac_cal "USER STORY 158527 - MMSILVA - 17.01.2025
        holiday_calendar = 'MG'
        factory_calendar = 'ZT'
* Comentado para Subir para PRD WBARBOSA Projeto Insumos 02/07/2025
        date_from        = i_data_vencimento
        date_to          = i_data_vencimento
      TABLES
        holidays         = it_datas
      EXCEPTIONS
        OTHERS           = 1.

    READ TABLE it_datas INTO wa_datas WITH KEY date = i_data_vencimento.
    IF sy-subrc NE 0.
      "Feriados Extras
      SELECT SINGLE *
        FROM zmmt0117
        INTO  @DATA(w_117)
        WHERE data = @i_data_vencimento.
      IF sy-subrc = 0.
        wa_datas-date       = i_data_vencimento.
        wa_datas-freeday    = ' '.
        wa_datas-holiday    = 'X'.
        wa_datas-holiday_id = ' '.
        wa_datas-txt_short  = w_117-descr_fer(10).
        wa_datas-txt_long   = w_117-descr_fer(30).
        APPEND wa_datas TO it_datas.
      ENDIF.
    ENDIF.

    READ TABLE it_datas INTO wa_datas WITH KEY date = i_data_vencimento.
    IF sy-subrc IS INITIAL.
      CASE wa_datas-holiday.
        WHEN abap_false.

          RAISE EXCEPTION TYPE zcx_miro_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_miro_exception=>zcx_vencimento_final_semana-msgid
                                msgno = zcx_miro_exception=>zcx_vencimento_final_semana-msgno
                                attr1 = CONV #( lc_data ) )
              msgid  = zcx_miro_exception=>zcx_vencimento_final_semana-msgid
              msgno  = zcx_miro_exception=>zcx_vencimento_final_semana-msgno
              msgv1  = CONV #( lc_data )
              msgty  = 'E'.

        WHEN abap_true.

          RAISE EXCEPTION TYPE zcx_miro_exception
            EXPORTING
              textid = VALUE #( msgid = zcx_miro_exception=>zcx_vencimento_feriado-msgid
                                msgno = zcx_miro_exception=>zcx_vencimento_feriado-msgno
                                attr1 = CONV #( lc_data )
                                attr2 = CONV #( wa_datas-txt_long ) )
              msgid  = zcx_miro_exception=>zcx_vencimento_feriado-msgid
              msgno  = zcx_miro_exception=>zcx_vencimento_feriado-msgno
              msgv1  = CONV #( lc_data )
              msgv2  = CONV #( wa_datas-txt_long )
              msgty  = 'E'.

      ENDCASE.
    ENDIF.

    CHECK i_data_se EQ abap_true.

    CHECK i_pymt_meth NE zcl_miro=>st_forma_pagamento_boleto.

    IF i_data_vencimento LT zcl_miro=>get_prox_data_venc_csc_se( ) and i_ck_fpol = abap_false.
      RAISE EXCEPTION TYPE zcx_miro_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_miro_exception=>zcx_data_venc_csc_finan-msgid
                            msgno = zcx_miro_exception=>zcx_data_venc_csc_finan-msgno
                            attr1 = lc_data )
          msgid  = zcx_miro_exception=>zcx_data_venc_csc_finan-msgid
          msgno  = zcx_miro_exception=>zcx_data_venc_csc_finan-msgno
          msgv1  = CONV #( lc_data )
          msgty  = 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD VERIFICAR_VENCIMENTO_PEDIDO.

    DATA: LC_DATA_BASE       TYPE C LENGTH 10,
          LC_DATA_VENCIMENTO TYPE C LENGTH 10,
          LC_DIAS            TYPE C LENGTH 10,
          LC_DAYS            TYPE VTBBEWE-ATAGE.


    ZCL_PEDIDO_COMPRA=>GET_PEDIDO(
      EXPORTING
        I_EBELN          = I_EBELN
      RECEIVING
        R_EKKO           = DATA(WA_EKKO)
      EXCEPTIONS
        NAO_ACHOU_PEDIDO = 1
        OTHERS           = 2 ).

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO DATA(LC_TEXTO).
      ZCL_MIRO=>GERA_ERRO_GERAL( I_TEXTO = LC_TEXTO ).
    ENDIF.

    DATA: E_FAEDT TYPE RFPOS-FAEDT.

    CALL FUNCTION 'NET_DUE_DATE_GET'
      EXPORTING
        I_ZFBDT = I_DATA_BASE
        I_ZBD1T = WA_EKKO-ZBD1T
        I_ZBD2T = WA_EKKO-ZBD2T
        I_ZBD3T = WA_EKKO-ZBD3T
        I_SHKZG = SPACE
        I_REBZG = SPACE
        I_KOART = SPACE
      IMPORTING
        E_FAEDT = E_FAEDT.

    "Jogar para o próximo dia útil
    ZCL_MIRO=>GET_PROXIMO_DIA_UTIL(
      EXPORTING
        I_DATA_BASE = E_FAEDT
      RECEIVING
        R_DATA      = E_FAEDT
      EXCEPTIONS
        ERRO        = 1
        OTHERS      = 2 ).

    IF I_DATA_VENCIMENTO LT E_FAEDT.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          INPUT  = E_FAEDT
        IMPORTING
          OUTPUT = LC_DATA_BASE.

      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          I_DATE_FROM = I_DATA_BASE
          I_DATE_TO   = I_DATA_VENCIMENTO
        IMPORTING
          E_DAYS      = LC_DAYS.

      WRITE LC_DAYS TO LC_DIAS.
      CONDENSE LC_DIAS NO-GAPS.

      CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
        EXPORTING
          INPUT  = I_DATA_VENCIMENTO
        IMPORTING
          OUTPUT = LC_DATA_VENCIMENTO.

      "Dt.Vencimento &MSGV1& não está conforme pedido &MSGV2&! Dt.Calculada: &MSGV3&!
      RAISE EXCEPTION TYPE ZCX_MIRO_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_DT_VENCIMENTO_CALCULADA-MSGID
                            MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_DT_VENCIMENTO_CALCULADA-MSGNO
                            ATTR1  = CONV #( LC_DATA_VENCIMENTO )
                            ATTR2  = CONV #( I_EBELN )
                            ATTR3  = CONV #( LC_DATA_BASE )
                            ATTR4  = CONV #( LC_DIAS ) )
          MSGTY  = 'E'
          MSGID  = ZCX_MIRO_EXCEPTION=>ZCX_DT_VENCIMENTO_CALCULADA-MSGID
          MSGNO  = ZCX_MIRO_EXCEPTION=>ZCX_DT_VENCIMENTO_CALCULADA-MSGNO
          MSGV1  = CONV #( LC_DATA_VENCIMENTO )
          MSGV2  = CONV #( I_EBELN )
          MSGV3  = CONV #( LC_DATA_BASE )
          MSGV4  = CONV #( LC_DIAS ).
    ENDIF.

  ENDMETHOD.


  METHOD estornar_job.

    DATA: lv_number TYPE tbtcjob-jobcount,
          lv_name   TYPE tbtcjob-jobname,
          w_bkpf    TYPE bkpf.

    FREE: e_invoicedocnumber_estorno,
          e_fiscalyear_estorno.


*--------------------------------------------------
*-- for debug
*--------------------------------------------------
    IF 1 = 2.
      SUBMIT zmmr0046 WITH p_chave  = i_chave_nfe
                      WITH p_docnum = i_invoicedocnumber
                      WITH p_year   = i_fiscalyear
                      WITH p_revers = i_reasonreversal
                      WITH p_pgdate = i_postingdate
                       AND RETURN.
    ENDIF.

*--------------------------------------------------
*-- criar JOB
*--------------------------------------------------
    DATA(lc_user_job) = zcl_job=>get_user_job( ).

    lv_name = 'JOB_ESTORNO_MIRO_' && i_invoicedocnumber && '_' && i_fiscalyear.

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_name
      IMPORTING
        jobcount         = lv_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.
      SUBMIT zmmr0046 VIA JOB lv_name
                       NUMBER lv_number
                         WITH p_chave  = i_chave_nfe
                         WITH p_docnum = i_invoicedocnumber
                         WITH p_year   = i_fiscalyear
                         WITH p_revers = i_reasonreversal
                         WITH p_pgdate = i_postingdate
                         USER lc_user_job
                          AND RETURN.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_number
            jobname              = lv_name
            strtimmed            = 'X'
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.

        IF sy-subrc IS NOT INITIAL.
          CALL FUNCTION 'BP_JOB_DELETE'
            EXPORTING
              jobcount                 = lv_number
              jobname                  = lv_name
            EXCEPTIONS
              cant_delete_event_entry  = 1
              cant_delete_job          = 2
              cant_delete_joblog       = 3
              cant_delete_steps        = 4
              cant_delete_time_entry   = 5
              cant_derelease_successor = 6
              cant_enq_predecessor     = 7
              cant_enq_successor       = 8
              cant_enq_tbtco_entry     = 9
              cant_update_predecessor  = 10
              cant_update_successor    = 11
              commit_failed            = 12
              jobcount_missing         = 13
              jobname_missing          = 14
              job_does_not_exist       = 15
              job_is_already_running   = 16
              no_delete_authority      = 17
              OTHERS                   = 18.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BP_JOB_DELETE'
          EXPORTING
            jobcount                 = lv_number
            jobname                  = lv_name
          EXCEPTIONS
            cant_delete_event_entry  = 1
            cant_delete_job          = 2
            cant_delete_joblog       = 3
            cant_delete_steps        = 4
            cant_delete_time_entry   = 5
            cant_derelease_successor = 6
            cant_enq_predecessor     = 7
            cant_enq_successor       = 8
            cant_enq_tbtco_entry     = 9
            cant_update_predecessor  = 10
            cant_update_successor    = 11
            commit_failed            = 12
            jobcount_missing         = 13
            jobname_missing          = 14
            job_does_not_exist       = 15
            job_is_already_running   = 16
            no_delete_authority      = 17
            OTHERS                   = 18.
      ENDIF.
    ENDIF.

*--------------------------------------------------
*-- aguardar execução do job
*--------------------------------------------------
    zcl_job=>get_instance(
      )->set_key_job( i_jobname = lv_name i_jobcount = lv_number
      )->get_wait_job_exec(
      ).

*--------------------------------------------------
*-- recuperar Numero doc estorno
*--------------------------------------------------
    w_bkpf-awkey = i_invoicedocnumber && i_fiscalyear.

    DO 5 TIMES.
      SELECT SINGLE *
        FROM bkpf
        INTO w_bkpf
       WHERE awkey = w_bkpf-awkey.

      IF sy-subrc = 0 AND w_bkpf-xreversed = abap_true.
        e_invoicedocnumber_estorno = w_bkpf-awref_rev.
        e_fiscalyear_estorno       = w_bkpf-aworg_rev.
        RETURN.
      ELSE.
        WAIT UP TO 3 SECONDS.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  method set_fora_politica.

    check i_ck_fpol is not initial.

    me->at_ck_fpol = i_ck_fpol.

    check i_obs_financeira is not initial.
    me->at_ob_financeira = i_obs_financeira.
  endmethod.


  METHOD get_taxas_icms.

    DATA: lc_tax_icms   TYPE c LENGTH 2,
          lc_vencimento TYPE c LENGTH 08.

    DATA: lit_j_1btxic3 TYPE TABLE OF j_1btxic3,
          lit_j_1btxic2 TYPE TABLE OF j_1btxic2,
          lit_j_1btxic1 TYPE TABLE OF j_1btxic1.

    DATA: ws_value2    TYPE j_1btxic3-value2,
          ws_value3    TYPE j_1btxic3-value3,
          ws_value     TYPE j_1btxic3-value,
          input(200)   TYPE c,
          vg_dt_inv(9) TYPE c.

    e_base_icms = 100.
    e_rate_icms = 0.
    e_lei_fiscal_icms = abap_false.
    e_utiliza_base_nf = abap_false.

    input     = i_data.
    vg_dt_inv = '99999999' - input.

    SELECT SINGLE matnr, extwg
      FROM mara INTO @DATA(lwa_mara)
     WHERE matnr EQ @i_matnr.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE matnr, mtorg
      FROM mbew INTO @DATA(lwa_mbew)
     WHERE matnr EQ @i_matnr
       AND bwkey EQ @i_werks.

    CHECK sy-subrc EQ 0.

*---------------------------------------------------------------------------------------------------------*
*  Read Table J_1BTXIC3
*---------------------------------------------------------------------------------------------------------*

    CLEAR: lit_j_1btxic3[].

    "-------------------------------------------------------->>>
    " Grupo 31
    "-------------------------------------------------------->>>

    ws_value  = i_werks.
    ws_value2 = i_matnr.
    ws_value3 = i_lifnr.

    SELECT *
      FROM j_1btxic3 INTO TABLE lit_j_1btxic3
     WHERE land1   EQ 'BR'
       AND shipfrom  EQ i_shipfrom
       AND shipto    EQ i_shipto
       AND value     EQ ws_value
       AND value2    EQ ws_value2
       AND value3    EQ ws_value3
       AND validfrom GE vg_dt_inv
       AND validto   LE vg_dt_inv
       AND gruop     EQ '31'.

    "-------------------------------------------------------->>>
    " Grupo 44
    "-------------------------------------------------------->>>

    IF ( lit_j_1btxic3[] IS INITIAL     ) AND
       ( lwa_mbew-mtorg  IS NOT INITIAL ).

      ws_value  = lwa_mbew-mtorg.
      ws_value2 = space.
      ws_value3 = space.

      SELECT *
        FROM j_1btxic3 INTO TABLE lit_j_1btxic3
       WHERE land1   EQ 'BR'
         AND shipfrom  EQ i_shipfrom
         AND shipto    EQ i_shipto
         AND value     EQ ws_value
         AND value2    EQ ws_value2
         AND value3    EQ ws_value3
         AND validfrom GE vg_dt_inv
         AND validto   LE vg_dt_inv
         AND gruop     EQ '44'.

    ENDIF.

    "-------------------------------------------------------->>>
    " Grupo 43
    "-------------------------------------------------------->>>

    IF ( lit_j_1btxic3[] IS INITIAL     ) AND
       ( i_werks         IS NOT INITIAL ) AND
       ( lwa_mara-extwg  IS NOT INITIAL ).

      ws_value  = lwa_mara-extwg.
      ws_value2 = i_werks.
      ws_value3 = space.

      SELECT *
        FROM j_1btxic3 INTO TABLE lit_j_1btxic3
       WHERE land1   EQ 'BR'
         AND shipfrom  EQ i_shipfrom
         AND shipto    EQ i_shipto
         AND value     EQ ws_value
         AND value2    EQ ws_value2
         AND value3    EQ ws_value3
         AND validfrom GE vg_dt_inv
         AND validto   LE vg_dt_inv
         AND gruop     EQ '43'.

    ENDIF.


    IF lit_j_1btxic3[] IS NOT INITIAL.
      READ TABLE lit_j_1btxic3 INTO DATA(wa_j_1btxic3) INDEX 1.
      IF sy-subrc EQ 0.
        e_rate_icms       = wa_j_1btxic3-rate.
        e_base_icms       = wa_j_1btxic3-base.
        e_lei_fiscal_icms = wa_j_1btxic3-taxlaw.
        RETURN.
      ENDIF.
    ENDIF.

*---------------------------------------------------------------------------------------------------------*
*  Read Table J_1BTXIC2
*---------------------------------------------------------------------------------------------------------*

    CLEAR: lit_j_1btxic2[].

    SELECT *
      FROM j_1btxic2 INTO TABLE lit_j_1btxic2
     WHERE land1     EQ 'BR'
       AND shipfrom  EQ i_shipfrom
       AND shipto    EQ i_shipto
       AND matnr     EQ i_matnr
       AND validfrom GE vg_dt_inv
       AND validto   LE vg_dt_inv.

    IF lit_j_1btxic2[] IS NOT INITIAL.
      READ TABLE lit_j_1btxic2 INTO DATA(wa_j_1btxic2) INDEX 1.
      IF sy-subrc EQ 0.
        e_rate_icms       = wa_j_1btxic2-rate.
        e_base_icms       = wa_j_1btxic2-base.
        e_lei_fiscal_icms = wa_j_1btxic2-taxlaw.
        RETURN.
      ENDIF.
    ENDIF.

*---------------------------------------------------------------------------------------------------------*
*  Read Table J_1BTXIC1
*---------------------------------------------------------------------------------------------------------*

    CLEAR: lit_j_1btxic1[].

    SELECT *
      FROM j_1btxic1 INTO TABLE lit_j_1btxic1
     WHERE land1    EQ 'BR'
       AND shipfrom EQ i_shipfrom
       AND shipto   EQ i_shipto.

    LOOP AT lit_j_1btxic1 INTO DATA(wa_j_1btxic1).
      lc_vencimento = 99999999 - wa_j_1btxic1-validfrom.
      IF ( lc_vencimento LE i_data ) AND ( wa_j_1btxic1-rate IS NOT INITIAL ).
        e_rate_icms       = wa_j_1btxic1-rate.
        e_utiliza_base_nf = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
