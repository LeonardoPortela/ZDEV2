*----------------------------------------------------------------------*
***INCLUDE Simulação de MIRO.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  Z_SIMULAR_DADOS
*&---------------------------------------------------------------------*
*       Simulação de Informações a serem enviadas para bapi
*----------------------------------------------------------------------*
FORM z_simular_dados USING visualizar TYPE c.

  DATA: it_dados_aux  TYPE TABLE OF zftte_dados,
        wa_dados_aux  TYPE zftte_dados,
        it_documentos TYPE TABLE OF zftte_dados,
        wa_documentos TYPE zftte_dados,
        vg_rblgp      TYPE rblgp,
        vg_tabix_item TYPE sy-tabix,
        p_bukrs       TYPE bukrs,
        p_lifnr       TYPE lifnr,
        p_valor       TYPE netwr_fp,
        p_bvtyp       TYPE bvtyp,
        p_cotacao     TYPE ukursm,
        lc_imp_ret    TYPE zles0043_imp_retidos,
        vl_retido     TYPE netwr_fp,
        p_nftype      TYPE j_1bnftype,
        p_xblnr       TYPE xblnr1,
        wa_setleaf    TYPE setleaf,
        wa_servico    TYPE asmd.

  IF NOT vg_alterou IS INITIAL.
    MESSAGE e000(zles) WITH TEXT-027.
  ENDIF.

  it_dados_aux[]    = ti_dados[].
  DELETE it_dados_aux    WHERE box IS INITIAL.

  " Processar somente quando houver linhas selecionadas
  IF it_dados_aux[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-022.
    CHECK it_dados_aux[] IS NOT INITIAL.
  ENDIF.

  it_documentos[] = it_dados_aux[].
  SORT it_documentos BY tdlnr nr_conhec series nfe.
  DELETE ADJACENT DUPLICATES FROM it_documentos COMPARING tdlnr nr_conhec series nfe.

  SORT it_dados_aux BY tdlnr nr_conhec series.

  CLEAR: it_headerdata[],
         it_headerdata,
         it_itemdata[],
         it_glaccountdata[],
         ti_dados_miro,
         ti_eventos[],
         it_impostos[],
         it_impostos_ret_b[].

  "Cabeçalhos
  LOOP AT it_documentos INTO wa_documentos.

    CONCATENATE wa_documentos-nr_conhec '-' wa_documentos-series INTO p_xblnr.

    IF wa_documentos-iva EQ 'S1'.
      IF wa_documentos-nfe NE abap_true. "IS INITIAL "IS INITIAL. / #89845 / Correção na codição da validação. / AOENNING
        p_nftype = c_nt.
      ELSE.
        p_nftype = c_ns.
      ENDIF.
    ELSE.
      IF wa_documentos-nfe IS INITIAL.
        IF wa_documentos-multimodal NE abap_true. "IS INITIAL. / #89845 / Correção na codição da validação. / AOENNING
          p_nftype = c_c1.
        ELSE.
          p_nftype = c_c6.
        ENDIF.
      ELSE.
        p_nftype = c_c2.
      ENDIF.

      SELECT SINGLE *
       FROM setleaf
       INTO wa_setleaf
     WHERE setname EQ 'MAGGI_EMPRESA_EXTERIOR'
       AND valfrom EQ wa_documentos-bukrs.

      IF ( sy-subrc NE 0 ).

        CALL FUNCTION 'Z_SD_VERIFICA_FORN_DOC_FISCAL'
          EXPORTING
            p_lifnr  = wa_documentos-tdlnr
            p_nftype = p_nftype
            p_xblnr  = p_xblnr
            p_data   = wa_documentos-zdt_conhec
            p_werks  = wa_documentos-werks
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDIF.
    ENDIF.

    "" Verificar se é serviço """""""""""""""""""""""""""
    SELECT SINGLE * INTO wa_servico
      FROM asmd
     WHERE asnum EQ wa_documentos-matns(18).

    IF sy-subrc IS INITIAL.
      p_nftype = c_nt.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_zlest0037)
      FROM zlest0037
     WHERE matnr      EQ @wa_documentos-matns
       AND ck_servico EQ @abap_true.

    IF sy-subrc IS INITIAL.
      p_nftype = c_nt.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""

    IF wa_documentos-bvtyp IS INITIAL.
      MESSAGE e057(zles) WITH wa_documentos-tknum.
    ENDIF.

    IF wa_documentos-nfe EQ abap_true AND wa_documentos-vsart NE '03'.
      MESSAGE TEXT-v19 TYPE 'E'.
    ENDIF.

    CALL METHOD zcl_cte_dist_g=>verifica_vencimento_fatura
      EXPORTING
        i_data_vencimento = wa_documentos-zdt_vencto
      EXCEPTIONS
        nao_valida        = 1
        OTHERS            = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR: wa_headerdata.
    wa_headerdata-invoice_ind    = c_x.
    wa_headerdata-doc_type       = c_ft.
    wa_headerdata-doc_date       = wa_documentos-zdt_conhec.
    wa_headerdata-pstng_date     = wa_documentos-zdt_mov.
    wa_headerdata-bline_date     = wa_documentos-zdt_vencto.
    CONCATENATE wa_documentos-nr_conhec '-' wa_documentos-series INTO wa_headerdata-ref_doc_no.
    wa_headerdata-nr_conhec      = wa_documentos-nr_conhec.
    wa_headerdata-series         = wa_documentos-series.
    wa_headerdata-comp_code      = wa_documentos-bukrs.
    wa_headerdata-diff_inv       = wa_documentos-tdlnr.
    wa_headerdata-currency       = wa_documentos-waers.
    wa_headerdata-header_txt     = 'Frete Terceiro'.
    IF p_nftype =  c_nt AND wa_documentos-vsart = '01'.
      CLEAR wa_headerdata-pmnt_block.
    ELSE.
      wa_headerdata-pmnt_block     = 'A'.
    ENDIF.
    wa_headerdata-pmnttrms       = c_0004.
    wa_headerdata-del_costs_taxc = wa_documentos-iva.
    wa_headerdata-gross_amount   = 0.
    wa_headerdata-alloc_nmbr     = wa_documentos-ebeln.
    wa_headerdata-bus_area       = wa_documentos-werks.
    wa_headerdata-calc_tax_ind   = c_x.
    "Usado para controle de documento eletrônico ou não
    wa_headerdata-goods_affected = wa_documentos-nfe.
    wa_headerdata-partner_bk     = wa_documentos-bvtyp.

    vg_rblgp = 1.
    wa_impostos-ref_doc_no       = wa_headerdata-ref_doc_no.
    wa_impostos-tax_code         = wa_documentos-iva.
    wa_impostos-tax_amount       = wa_documentos-valor_icms + wa_documentos-valor_pis + wa_documentos-valor_cofins.
    wa_impostos-tax_base_amount  = wa_headerdata-gross_amount.
    APPEND wa_impostos TO it_impostos.

    vl_retido = 0.

    "Itens do cabeçalho
    LOOP AT it_dados_aux INTO wa_dados_aux WHERE tdlnr = wa_documentos-tdlnr
                                             AND nr_conhec = wa_documentos-nr_conhec
                                             AND series    = wa_documentos-series
                                             AND nfe       = wa_documentos-nfe.
      CHECK wa_dados_aux-re_belnr IS INITIAL.
      vg_tabix_item = sy-tabix.
      APPEND wa_dados_aux TO ti_dados_miro.
      CLEAR: wa_itemdata.

      "Agrupadores
      CONCATENATE wa_documentos-nr_conhec '-' wa_documentos-series INTO wa_itemdata-ref_doc_no.
      wa_itemdata-diff_inv         = wa_documentos-tdlnr.
      wa_itemdata-zvlr_quebra      = wa_dados_aux-zvlr_quebra.
      wa_itemdata-zvlr_perda       = wa_dados_aux-zvlr_perda.
      wa_itemdata-valor_pedagio    = wa_dados_aux-valor_pedagio.
      wa_itemdata-taxjurcode       = wa_dados_aux-txjcd_emissor.
      wa_itemdata-invoice_doc_item = vg_rblgp.
      wa_itemdata-po_number        = wa_dados_aux-ebeln.
      wa_itemdata-po_item          = wa_dados_aux-ebelp.
      wa_itemdata-ref_doc          = wa_dados_aux-lblni.
      wa_itemdata-ref_doc_year     = wa_dados_aux-lfgja.
      wa_itemdata-tax_code         = wa_dados_aux-iva.
      wa_itemdata-dmbtr            = wa_dados_aux-dmbtr.
      wa_itemdata-item_amount      = wa_dados_aux-dmbtr_doc - ( wa_dados_aux-valor_icms + wa_dados_aux-valor_pis + wa_dados_aux-valor_cofins ) - wa_dados_aux-valor_pedagio.
      wa_itemdata-sheet_no         = wa_dados_aux-lblni.

      "Somatória de valores de documentos selecionados
      APPEND wa_itemdata TO it_itemdata.

      "Atualiza item do documento
      wa_dados_aux-re_item         = vg_rblgp.
      MODIFY it_dados_aux INDEX vg_tabix_item FROM wa_dados_aux TRANSPORTING re_item.

      wa_headerdata-gross_amount   = wa_headerdata-gross_amount + wa_dados_aux-zvlr_liq_pagar.

      "Incrementa 1 no item
      ADD n_1 TO vg_rblgp.

      LOOP AT wa_dados_aux-it_impostos_retidos INTO lc_imp_ret.
        READ TABLE it_impostos_ret_b INTO wa_impostos_ret_b WITH KEY ref_doc_no = wa_headerdata-ref_doc_no
                                                                     wi_tax_type = lc_imp_ret-witht
                                                                     wi_tax_code = lc_imp_ret-wt_withcd.
        IF sy-subrc IS INITIAL.
          wa_impostos_ret_b-wi_tax_base = wa_impostos_ret_b-wi_tax_base + lc_imp_ret-base.
          wa_impostos_ret_b-wi_tax_amt  = wa_impostos_ret_b-wi_tax_amt  + lc_imp_ret-taxval.
          MODIFY it_impostos_ret_b INDEX sy-tabix FROM wa_impostos_ret_b TRANSPORTING wi_tax_base wi_tax_amt.
        ELSE.
          wa_impostos_ret_b-ref_doc_no  = wa_headerdata-ref_doc_no.
          wa_impostos_ret_b-wi_tax_type = lc_imp_ret-witht.
          wa_impostos_ret_b-wi_tax_code = lc_imp_ret-wt_withcd.
          wa_impostos_ret_b-wi_tax_base = lc_imp_ret-base.
          wa_impostos_ret_b-wi_tax_amt  = lc_imp_ret-taxval.
          APPEND wa_impostos_ret_b TO it_impostos_ret_b.
        ENDIF.
        vl_retido = vl_retido + lc_imp_ret-taxval.
      ENDLOOP.

    ENDLOOP.

    IF wa_headerdata-gross_amount GT 0.

      "Tipo de Transporte
      CASE wa_documentos-vsart.
          "Aquaviario
        WHEN: '03'.

          IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.
            IF ( wa_documentos-waers EQ 'BRL' ).
              p_cotacao = 1.
            ELSEIF ( wa_documentos-waers EQ 'USD' ).

              p_cotacao = wa_documentos-tax_dolar.
              wa_headerdata-exch_rate = p_cotacao.
            ENDIF.
          ELSE.
            IF wa_documentos-waers EQ 'BRL'.
              p_cotacao = 1.
            ELSE.
              p_cotacao = wa_documentos-kurst.
            ENDIF.
          ENDIF.

          "Outros
        WHEN OTHERS.

          "O código abaixo é uma validação antiga.
          IF wa_documentos-waers EQ 'BRL'.
            p_cotacao = 1.
          ELSE.
            p_cotacao = wa_documentos-kurst.
          ENDIF.

      ENDCASE.

      p_valor = wa_headerdata-gross_amount.
      p_lifnr = wa_headerdata-diff_inv.
      p_bukrs = wa_documentos-bukrs.
      p_bvtyp = wa_documentos-bvtyp.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs           = p_bukrs
          p_lifnr           = p_lifnr
          p_valor           = p_valor
          p_cotacao         = p_cotacao
          p_bvtyp           = p_bvtyp
        IMPORTING
          p_forma_pagamento = wa_headerdata-pymt_meth
          p_princ_bnc_emp   = wa_headerdata-housebankid
        EXCEPTIONS
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          OTHERS            = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      PERFORM monta_conta_frete USING wa_headerdata wa_documentos.
      wa_headerdata-gross_amount = wa_headerdata-gross_amount + vl_retido.

      APPEND wa_headerdata TO it_headerdata.
    ENDIF.

  ENDLOOP.

  IF visualizar IS NOT INITIAL.
    CALL SCREEN 200.
  ENDIF.

ENDFORM.                    " Z_SIMULAR_DADOS


FORM seleciona_21 USING p_shtyp TYPE shtyp p_dt_referencia TYPE budat.

  CLEAR: it_zlest0021[], it_zlest0021.
  DATA: rgveicu TYPE RANGE OF zde_tp_prop_veiculo_ctb.
  rgveicu = VALUE #( sign = 'I' option = 'EQ' ( low = space high = space ) ( low = '0' high = '0' ) ).
  TRY .
      zcl_controle_conta_razao=>get_instance(
        )->get_conta_razao(
        EXPORTING
          i_shtyp                  = p_shtyp    " Tipo de transporte
          i_tcode                  = CONV #( c_miro )    " Código de transação
          i_fatura                 = c_t    " Emissor da fatura - Fretes
          i_tp_emissor             = c_t    " Tipo de emissor
          i_operfrete_range        = VALUE #( sign = 'I' option = 'EQ'
                                                ( low = '2'  high = '2'  )
                                                ( low = '15' high = '15' )
                                                ( low = '14' high = '14' )
                                                ( low = '16' high = '16' )
                                                ( low = '17' high = '17' )
                                                ( low = '18' high = '18' )
                                                ( low = '19' high = '19' )
                                                ( low = '20' high = '20' ) )    " Ranges Operação de lançamento no razão - Frete
          i_tp_veiculo             = rgveicu    " Tipo de Proprietário de Veículo para Contabilização
          i_dt_referencia          = p_dt_referencia    " Data de lançamento no documento
        IMPORTING
          e_it_zlest0021           = it_zlest0021[]  " Controle de desterminação conta razão
        ).
    CATCH zcx_controle_conta_razao.    " .
  ENDTRY.

*  "Busca parâmetros contábeis
*  SELECT *
*    INTO TABLE IT_ZLEST0021
*    FROM ZLEST0021
*     FOR ALL ENTRIES IN IT_DOCUMENTOS
*   WHERE SHTYP     = IT_DOCUMENTOS-SHTYP
*     AND TCODE      EQ C_MIRO
*     AND FATURA     EQ C_T
*     AND TP_EMISSOR EQ C_T
*     AND TP_VEICULO IN RGVEICU
*     AND OPERFRETE  IN (C_2,C_15,C_14,C_16,C_17,C_18,C_19,C_20).

* Processar somente se localizar parâmetros contábeis
  IF it_zlest0021[] IS INITIAL.
    MESSAGE w000(zles) WITH TEXT-024.
    CHECK it_zlest0021[] IS NOT INITIAL.
  ENDIF.
  SORT it_zlest0021 BY shtyp tcode operfrete. "Tp.transp, transação e oper.lancto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTA_CONTA_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM monta_conta_frete  USING    wa_cabec    TYPE ty_bapi_incinv_create_header
                                 wa_dados    TYPE zftte_dados.

  DATA: conta_razao  TYPE zlest0021-razaocred,
        vl_deb_cred  TYPE c,
        vl_contabil  TYPE bapiwrbtr,
        vl_dif_netwr TYPE bapiwrbtr.

  PERFORM seleciona_21 USING wa_dados-shtyp wa_dados-budat.

  LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no EQ wa_cabec-ref_doc_no.


* Alimentar a tabela com dados do conta razão - Transitória do Frete
    READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_2 BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    vl_dif_netwr = wa_itemdata-dmbtr.

    DO 2 TIMES.
      IF sy-index = 1.
        conta_razao = wa_zlest0021-razaocred.
        vl_deb_cred = c_h.
      ELSE.
        conta_razao = wa_zlest0021-razaodeb.
        vl_deb_cred = c_s.
      ENDIF.
      IF vl_deb_cred EQ c_s.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            wa_itemdata-item_amount
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-025
                                            wa_dados-exti1.
      ENDIF.
      IF vl_deb_cred EQ c_h.
        vl_contabil = wa_itemdata-dmbtr.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_contabil
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-034
                                            wa_dados-exti1.
      ENDIF.
    ENDDO.

    vl_dif_netwr = vl_dif_netwr - wa_itemdata-item_amount.

    IF vl_dif_netwr GT 0.
      READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_19 BINARY SEARCH.
      IF sy-subrc EQ 0.
        vl_deb_cred = c_s.
        conta_razao = wa_zlest0021-razaodeb.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_dif_netwr
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-035
                                            wa_dados-exti1.
      ENDIF.
    ELSEIF vl_dif_netwr LT 0.
      READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_20 BINARY SEARCH.
      IF sy-subrc EQ 0.
        vl_deb_cred = c_h.
        conta_razao = wa_zlest0021-razaocred.
        vl_dif_netwr = vl_dif_netwr * -1.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_dif_netwr
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-036
                                            wa_dados-exti1.
      ENDIF.
    ENDIF.

  ENDLOOP.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Pedágio
  READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_14 BINARY SEARCH.
  IF sy-subrc EQ 0.
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no EQ wa_cabec-ref_doc_no.
      IF wa_itemdata-valor_pedagio GT 0.
        vl_contabil = wa_itemdata-valor_pedagio.
        conta_razao = wa_zlest0021-razaodeb.
        vl_deb_cred = c_s.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_contabil
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-037
                                            wa_dados-exti1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Quebra
  READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_16 BINARY SEARCH.
  IF sy-subrc EQ 0.
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no EQ wa_cabec-ref_doc_no.
      IF wa_itemdata-zvlr_quebra GT 0.
        vl_contabil = wa_itemdata-zvlr_quebra.
        conta_razao = wa_zlest0021-razaocred.
        vl_deb_cred = c_h.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_contabil
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-031
                                            wa_dados-exti1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Sobra
  READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_17 BINARY SEARCH.
  IF sy-subrc EQ 0.
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no EQ wa_cabec-ref_doc_no.
      IF wa_itemdata-zvlr_quebra LT 0.
        vl_contabil = wa_itemdata-zvlr_quebra * -1.
        conta_razao = wa_zlest0021-razaodeb.
        vl_deb_cred = c_s.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_contabil
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-033
                                            wa_dados-exti1.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Alimentar a tabela com dados do conta razão - Transitória do Frete - Perda
  READ TABLE it_zlest0021 INTO wa_zlest0021 WITH KEY shtyp = wa_dados-shtyp tcode = c_miro operfrete = c_18 BINARY SEARCH.
  IF sy-subrc EQ 0.
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no EQ wa_cabec-ref_doc_no.
      IF wa_itemdata-zvlr_perda GT 0.
        vl_contabil = wa_itemdata-zvlr_perda.
        conta_razao = wa_zlest0021-razaocred.
        vl_deb_cred = c_h.
        PERFORM z_alimentar_glaccount USING wa_itemdata-ref_doc_no
                                            wa_itemdata-invoice_doc_item
                                            conta_razao
                                            vl_contabil
                                            vl_deb_cred
                                            wa_cabec-comp_code
                                            wa_cabec-bus_area
                                            wa_zlest0021-operfrete
                                            TEXT-032
                                            wa_dados-exti1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MONTA_CONTA_FRETE

*&---------------------------------------------------------------------*
*&      Form  Z_ALIMENTAR_GLACCOUNT
*&---------------------------------------------------------------------*
*       Gera registos
*----------------------------------------------------------------------*
FORM z_alimentar_glaccount  USING    ref_doc_no	 TYPE xblnr
                                     p_item      TYPE rblgp
                                     p_razao     TYPE saknr
                                     p_netwr     TYPE bapiwrbtr
                                     p_deb_cred  TYPE shkzg
                                     p_bukrs     TYPE bukrs
                                     p_bus_area  TYPE werks_d
                                     p_operfrete TYPE zoperfrete
                                     p_texto     TYPE sgtxt
                                     p_exti1     TYPE exti1.

  IF p_netwr GT 0.
    CLEAR wa_glaccountdata.
    wa_glaccountdata-ref_doc_no       = ref_doc_no.
    wa_glaccountdata-invoice_doc_item = p_item.
    wa_glaccountdata-gl_account       = p_razao.
    wa_glaccountdata-item_amount      = p_netwr.
    wa_glaccountdata-db_cr_ind        = p_deb_cred.
    wa_glaccountdata-comp_code        = p_bukrs.
    wa_glaccountdata-bus_area         = p_bus_area.
    CONCATENATE p_texto p_exti1 INTO wa_glaccountdata-item_text SEPARATED BY space.
    APPEND wa_glaccountdata TO it_glaccountdata.
  ENDIF.

ENDFORM.                    " Z_ALIMENTAR_GLACCOUNT

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_MIRO
*&---------------------------------------------------------------------*
*       Gerar Miro
*----------------------------------------------------------------------*
FORM z_gerar_miro .

  DATA: wa_cabecalho TYPE bapi_incinv_create_header,
        wa_itens     TYPE bapi_incinv_create_item,
        wa_imp_ret   TYPE bapi_incinv_create_withtax,
        it_itens     TYPE TABLE OF bapi_incinv_create_item,
        it_imp_ret   TYPE TABLE OF bapi_incinv_create_withtax,
        it_contas    TYPE TABLE OF bapi_incinv_create_gl_account,
        it_dados     TYPE TABLE OF zftte_dados,
        it_taxdata   TYPE TABLE OF bapi_incinv_create_tax,
        wa_taxdata   TYPE bapi_incinv_create_tax,
        wa_dados     TYPE zftte_dados,
        wa_zlest0034 TYPE zlest0034,
        nur_miro     TYPE bapi_incinv_fld-inv_doc_no,
        ano_miro     TYPE bapi_incinv_fld-fisc_year,
        wa_contas    TYPE bapi_incinv_create_gl_account,
        it_return    TYPE TABLE OF bapiret2,
        it_dados_aux TYPE TABLE OF zftte_dados,
        vg_tabix     TYPE sy-tabix.


  DATA: wa_lfa1_chv TYPE lfa1,
        wa_zib_nfe  TYPE zib_nfe_forn.

  CLEAR: ti_eventos[].

*  IF ( st_dados-zdt_mov NE sy-datum ).
*    MESSAGE e000(z01) WITH 'A Data do Movimento tem'
*                           'que ser igual a data do dia'.
*    STOP.
*  ENDIF.



  LOOP AT it_headerdata INTO wa_headerdata.

    MOVE-CORRESPONDING wa_headerdata TO wa_cabecalho.

    CLEAR: it_itens, it_contas, it_return, it_taxdata.
    CLEAR: it_itens[], it_contas[], it_return[], it_taxdata[], it_imp_ret[].

    it_dados[] = ti_dados_miro[].

    DELETE it_dados WHERE nr_conhec NE wa_headerdata-nr_conhec
                       OR series NE wa_headerdata-series
                       OR tdlnr NE wa_headerdata-diff_inv
                       OR nfe NE wa_headerdata-goods_affected.

    CLEAR: wa_cabecalho-goods_affected.

    "Seleciona os itens e as contas
    LOOP AT it_itemdata INTO wa_itemdata WHERE ref_doc_no	= wa_cabecalho-ref_doc_no AND diff_inv EQ wa_cabecalho-diff_inv.
      MOVE-CORRESPONDING wa_itemdata TO wa_itens.
      APPEND wa_itens TO it_itens.
      LOOP AT it_glaccountdata INTO wa_glaccountdata WHERE ref_doc_no	= wa_cabecalho-ref_doc_no
                                                       AND invoice_doc_item EQ wa_itemdata-invoice_doc_item.
        MOVE-CORRESPONDING wa_glaccountdata TO wa_contas.
        APPEND wa_contas TO it_contas.
      ENDLOOP.
    ENDLOOP.

    LOOP AT it_impostos_ret_b INTO wa_impostos_ret_b.
      MOVE-CORRESPONDING wa_impostos_ret_b TO wa_imp_ret.
      wa_imp_ret-split_key           = c_000001.
      wa_imp_ret-wi_tax_withheld_amt = wa_imp_ret-wi_tax_amt.
      APPEND wa_imp_ret TO it_imp_ret.
    ENDLOOP.

    "Criar o documento de revisão de fatura através da bapi da MIRO

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        headerdata       = wa_cabecalho
      IMPORTING
        invoicedocnumber = nur_miro
        fiscalyear       = ano_miro
      TABLES
        itemdata         = it_itens
        glaccountdata    = it_contas
        withtaxdata      = it_imp_ret
        return           = it_return.        "taxdata = it_taxdata

    IF it_return[] IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CLEAR st_eventos.
      st_eventos-icone    = icon_led_green.
      CONCATENATE '(Miro) Documento' nur_miro 'gerado para o ano de' ano_miro '!' INTO st_eventos-msg_text SEPARATED BY space.
      APPEND st_eventos TO ti_eventos.

      "Atualizar documento da miro.
      LOOP AT it_dados INTO wa_dados.
        LOOP AT ti_dados INTO st_dados WHERE re_belnr IS INITIAL
                                         AND re_gjahr IS INITIAL
                                         AND nr_conhec EQ wa_dados-nr_conhec
                                         AND series EQ wa_dados-series
                                         AND tdlnr EQ wa_dados-tdlnr.
          vg_tabix = sy-tabix.
          st_dados-re_belnr = nur_miro.
          st_dados-re_gjahr = ano_miro.
          "Atualizar ti_dados e salvar
          SELECT SINGLE * INTO wa_zlest0034
            FROM zlest0034
           WHERE tknum EQ st_dados-tknum.
          wa_zlest0034-re_belnr = st_dados-re_belnr.
          wa_zlest0034-re_gjahr = st_dados-re_gjahr.
          MODIFY zlest0034 FROM wa_zlest0034.

          CALL FUNCTION 'Z_LES_REVISAO_FATURA_TERC'
            EXPORTING
              p_zlest0034 = wa_zlest0034.

          COMMIT WORK.
          PERFORM verificar_status CHANGING st_dados.
          MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status re_belnr re_gjahr.
        ENDLOOP.
      ENDLOOP.

      PERFORM z_gerar_fiscal.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    PERFORM popula_log TABLES it_return.

    CLEAR: wa_cabecalho.

  ENDLOOP.



ENDFORM.                    " Z_GERAR_MIRO

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_FISCAL
*&---------------------------------------------------------------------*
*       Gera nota fiscal
*----------------------------------------------------------------------*
FORM z_gerar_fiscal .

  DATA: it_dados_aux       TYPE TABLE OF zftte_dados,
        wa_dados           TYPE zftte_dados,
        st_obj_header      TYPE bapi_j_1bnfdoc,
        st_obj_partner     TYPE bapi_j_1bnfnad,
        st_obj_item        TYPE bapi_j_1bnflin,
        st_obj_item_tax    TYPE bapi_j_1bnfstx,
        ti_obj_partner     TYPE TABLE OF bapi_j_1bnfnad,
        ti_obj_item        TYPE TABLE OF bapi_j_1bnflin,
        ti_obj_item_add    TYPE TABLE OF bapi_j_1bnflin_add,
        ti_obj_item_tax    TYPE TABLE OF bapi_j_1bnfstx,
        ti_obj_ot_partner  TYPE TABLE OF bapi_j_1bnfcpd,
        ti_return          TYPE TABLE OF bapiret2,
        wa_return          TYPE bapiret2,
        wa_zlest0034       TYPE zlest0034,
        wa_zlest0040       TYPE zlest0040,
        wa_mara            TYPE mara,
        wa_t007a           TYPE t007a,
        vg_tabix           TYPE sy-tabix,
        wa_j_1baa          TYPE j_1baa,
        wa_zib_nfe_forn    TYPE zib_nfe_forn,
        vg_nu_chave_cnpj   LIKE zib_nfe_forn-nu_chave_cnpj,
        vg_nu_chave_modelo LIKE zib_nfe_forn-nu_chave_modelo,
        vg_nu_chave_serie  LIKE zib_nfe_forn-nu_chave_serie,
        vg_nu_chave_numero LIKE zib_nfe_forn-nu_chave_numero,
        i_doc              TYPE j_1bnfdoc,
        wa_header_cte_aux  TYPE j_1bnfe_active,
        wl_zlest0061_brl   TYPE zlest0061.

  DATA: vl_dstcat               TYPE zlest0030-dstcat,
        vl_industry             TYPE zlest0030-industry,
        vl_cfop                 TYPE zlest0030-cfop,
        wa_zlest0037            TYPE zlest0037,
        vl_docnum               TYPE bapi_j_1bnfdoc-docnum,
        st_nfcheck              TYPE bapi_j_1bnfcheck,
        vl_parid                TYPE char10,
*==>>Inicio - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
        vl_nfesrv               TYPE c,
*<<==Fim - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
        wa_material_text_record TYPE makt,
        p_data_ent              TYPE datum,
        p_data_val              TYPE datum,
        wa_servico              TYPE asmd.


  DATA: sl_zlest0040 TYPE zlest0040.


  "Gerar Fiscal
  it_dados_aux[]    = ti_dados[].
  DELETE it_dados_aux    WHERE box IS INITIAL.
  DELETE it_dados_aux    WHERE re_belnr  IS INITIAL.
  DELETE it_dados_aux    WHERE en_docnum IS NOT INITIAL.
  "delete it_dados_aux    where iva eq 'S1'.

  " Processar somente quando houver linhas sem fiscal
  IF it_dados_aux[] IS INITIAL.
    CHECK it_dados_aux[] IS NOT INITIAL.
  ENDIF.

  SORT it_dados_aux BY re_belnr re_gjahr.
  DELETE ADJACENT DUPLICATES FROM it_dados_aux COMPARING re_belnr re_gjahr.

  LOOP AT it_dados_aux INTO wa_dados.

    vg_tabix = sy-tabix.
    wa_dados-dmbtr_doc      = 0.
    wa_dados-zvlr_liq_pagar = 0.
    wa_dados-valor_icms     = 0.
    wa_dados-valor_cofins   = 0.
    wa_dados-valor_pis      = 0.
    wa_dados-valor_pedagio  = 0.

    LOOP AT ti_dados INTO st_dados WHERE box IS NOT INITIAL
                                     AND re_belnr EQ wa_dados-re_belnr
                                     AND re_gjahr EQ wa_dados-re_gjahr.



      CASE st_dados-vsart.
        WHEN '03'.

          IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.

            SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061_brl WHERE tknum EQ st_dados-tknum
                                                                   AND fknum EQ st_dados-fknum
                                                                   AND ck_anulado EQ abap_false.

            IF ( sy-subrc EQ 0 ).

              CLEAR:   wa_dados-dmbtr_doc,
                       wa_dados-zvlr_liq_pagar,
                       wa_dados-valor_icms,
                       wa_dados-valor_cofins,
                       wa_dados-valor_pis,
                       wa_dados-valor_pedagio.

              "WA_DADOS-DMBTR_DOC      = WA_DADOS-DMBTR_DOC      + ST_DADOS-VLR_BRL.
              wa_dados-dmbtr_doc      = wl_zlest0061_brl-vlr_brl.
              wa_dados-zvlr_liq_pagar = wa_dados-zvlr_liq_pagar + st_dados-vlr_brl.

              wa_dados-valor_icms     = ( ( wl_zlest0061_brl-vlr_brl * st_dados-rate_icms ) / 100 ).
              wa_dados-valor_cofins   = ( ( wl_zlest0061_brl-vlr_brl * st_dados-rate_cofins ) / 100 ).
              wa_dados-valor_pis      = ( ( wl_zlest0061_brl-vlr_brl * st_dados-rate_pis ) / 100 ).

            ENDIF.
          ELSE.
            wa_dados-dmbtr_doc      = wa_dados-dmbtr_doc      + st_dados-dmbtr_doc.
            wa_dados-zvlr_liq_pagar = wa_dados-zvlr_liq_pagar + st_dados-zvlr_liq_pagar.
            wa_dados-valor_icms     = wa_dados-valor_icms     + st_dados-valor_icms.
            wa_dados-valor_cofins   = wa_dados-valor_cofins   + st_dados-valor_cofins.
            wa_dados-valor_pis      = wa_dados-valor_pis      + st_dados-valor_pis.
            wa_dados-valor_pedagio  = wa_dados-valor_pedagio  + st_dados-valor_pedagio.
          ENDIF.

*-CS2024000597-14.10.2024-#146076-JT-inicio
        WHEN '04'.
          SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061_brl WHERE tknum EQ st_dados-tknum
                                                                 AND fknum EQ st_dados-fknum
                                                                 AND ck_anulado EQ abap_false.
          IF sy-subrc = 0.
            wa_dados-operacao = wl_zlest0061_brl-operacao.
          ENDIF.
*-CS2024000597-14.10.2024-#146076-JT-fim

        WHEN OTHERS.
          wa_dados-dmbtr_doc      = wa_dados-dmbtr_doc      + st_dados-dmbtr_doc.
          wa_dados-zvlr_liq_pagar = wa_dados-zvlr_liq_pagar + st_dados-zvlr_liq_pagar.
          wa_dados-valor_icms     = wa_dados-valor_icms     + st_dados-valor_icms.
          wa_dados-valor_cofins   = wa_dados-valor_cofins   + st_dados-valor_cofins.
          wa_dados-valor_pis      = wa_dados-valor_pis      + st_dados-valor_pis.
          wa_dados-valor_pedagio  = wa_dados-valor_pedagio  + st_dados-valor_pedagio.

      ENDCASE.

    ENDLOOP.

    MODIFY it_dados_aux INDEX vg_tabix FROM wa_dados TRANSPORTING dmbtr_doc zvlr_liq_pagar valor_icms valor_cofins valor_pis operacao. "*-CS2024000597-14.10.2024-#146076-JT
  ENDLOOP.

  LOOP AT it_dados_aux INTO wa_dados.

* Limpar área de trabalho e tabelas internas.
    CLEAR: st_obj_header,
           st_obj_item  ,
           st_notas     ,
           st_header    ,
           vl_parid     ,
           wa_zib_nfe_forn.

    REFRESH: ti_obj_item,
             ti_obj_item_tax,
             ti_obj_partner,
             ti_obj_ot_partner,
             ti_return.

    IF wa_dados-iva EQ 'S1'.
      IF wa_dados-nfe NE abap_true. "IS INITIAL "IS INITIAL. / #89845 / Correção na codição da validação. / AOENNING
        st_obj_header-nftype = c_nt.
        st_obj_header-nfnum  = wa_dados-nr_conhec.
      ELSE.
        st_obj_header-nftype = c_ns.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_dados-nr_conhec
          IMPORTING
            output = st_obj_header-nfenum.
      ENDIF.
    ELSE.

      IF wa_dados-nfe IS INITIAL.
        IF wa_dados-multimodal IS INITIAL.
          st_obj_header-nftype  = c_c1.
        ELSE.
          st_obj_header-nftype  = c_c6.
        ENDIF.
        st_obj_header-nfnum   = wa_dados-nr_conhec.
      ELSE.
        st_obj_header-nftype  = c_c2.
        st_obj_header-xmlvers = ( 110 / 100 ).
      ENDIF.

    ENDIF.

    "" Verificar se é serviço """""""""""""""""""""""""""
    IF wa_dados-nfe IS INITIAL.

      SELECT SINGLE * INTO wa_servico
        FROM asmd
       WHERE asnum EQ wa_dados-matns.        "#EC CI_FLDEXT_OK[2215424]

      IF sy-subrc IS INITIAL.
        st_obj_header-nftype = c_nt.
        st_obj_header-nfesrv = abap_true.
*==>>Inicio - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
        vl_nfesrv            = abap_true.
*<<==Fim - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
      ENDIF.

      SELECT SINGLE * INTO wa_zlest0037
        FROM zlest0037
       WHERE matnr      EQ wa_dados-matns
         AND ck_servico EQ abap_true.

      IF sy-subrc IS INITIAL.
        st_obj_header-nftype = c_nt.
        st_obj_header-nfesrv = abap_true.
*==>>Inicio - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
        vl_nfesrv            = abap_true.
*<<==Fim - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
      ENDIF.

      CLEAR: wa_zlest0037.

    ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""

    IF ( NOT wa_dados-nfe IS INITIAL ) AND ( wa_dados-iva NE 'S1' ).

      CLEAR: st_obj_header-nfnum.

      st_obj_header-code = '100'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados-nr_conhec
        IMPORTING
          output = st_obj_header-nfenum.

      vg_nu_chave_modelo = '57'.
      vg_nu_chave_serie  = wa_dados-series.
      vg_nu_chave_numero = wa_dados-nr_conhec.

      SHIFT vg_nu_chave_serie  LEFT DELETING LEADING space.
      SHIFT vg_nu_chave_numero LEFT DELETING LEADING space.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_nu_chave_serie
        IMPORTING
          output = vg_nu_chave_serie.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_nu_chave_numero
        IMPORTING
          output = vg_nu_chave_numero.

      SELECT SINGLE stcd1 INTO vg_nu_chave_cnpj
        FROM lfa1
       WHERE lifnr EQ wa_dados-tdlnr.

      SELECT SINGLE * INTO wa_zib_nfe_forn
        FROM zib_nfe_forn
       WHERE nu_chave_cnpj   EQ vg_nu_chave_cnpj
         AND nu_chave_modelo EQ vg_nu_chave_modelo
         AND nu_chave_serie  EQ vg_nu_chave_serie
         AND nu_chave_numero EQ vg_nu_chave_numero.

    ENDIF.

    SELECT SINGLE * INTO wa_j_1baa
      FROM j_1baa
     WHERE nftype EQ st_obj_header-nftype.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING wa_j_1baa TO st_obj_header.
    ENDIF.

    st_obj_header-docstat = wa_zib_nfe_forn-st_nota.
    st_obj_header-parid   = wa_dados-tdlnr.
    st_obj_header-access_key = wa_zib_nfe_forn-nu_chave.

    st_obj_header-docdat  = wa_dados-zdt_conhec.

    p_data_ent = wa_dados-zdt_mov.

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = p_data_ent
        p_bukrs        = wa_dados-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      IMPORTING
        p_data_val     = p_data_val
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    st_obj_header-pstdat  = p_data_val.
    st_obj_header-bukrs   = wa_dados-bukrs.
    st_obj_header-branch  = wa_dados-werks.
    st_obj_header-waerk   = wa_dados-waers.
    st_obj_header-series  = wa_dados-series.

    "Item da parte fiscal
    st_obj_item-itmnum    = 10.
    st_obj_item-bwkey     = wa_dados-werks.
    st_obj_item-werks     = wa_dados-werks.
    st_obj_item-refkey    = wa_dados-re_belnr.
    "Logística: Revisão de Faturas
    st_obj_item-reftyp    = c_li.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*    st_obj_item-matnr     = wa_dados-matns.
    DATA(v_len) = strlen( wa_dados-matns ).
    IF v_len > 18.
      st_obj_item-matnr_long = wa_dados-matns.
    ELSE.
      st_obj_item-matnr      = wa_dados-matns.
    ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
    st_obj_item-itmtyp    = c_zh.
    st_obj_item-menge     = n_1.
    st_obj_item-netoth    = CONV #( wa_dados-valor_pedagio ).
    st_obj_item-netpr     = wa_dados-dmbtr_doc - ( wa_dados-valor_pedagio ).
    st_obj_item-netwr     = wa_dados-dmbtr_doc - ( wa_dados-valor_pedagio ).

    CALL FUNCTION 'J_1B_MATERIAL_READ'
      EXPORTING
        matnr                = wa_dados-matns
        val_area             = wa_dados-werks
        val_type             = space
        language             = sy-langu
        i_werks              = wa_dados-werks
      IMPORTING
        nbm                  = st_obj_item-nbm
        matuse               = st_obj_item-matuse
        matorg               = st_obj_item-matorg
        material_text_record = wa_material_text_record
        e_matkl              = st_obj_item-matkl
      EXCEPTIONS
        material_not_found   = 1
        valuation_not_found  = 2
        OTHERS               = 3.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    st_obj_item-maktx   = wa_material_text_record-maktx.

    SELECT SINGLE * INTO wa_zlest0040
      FROM zlest0040
     WHERE iva EQ wa_dados-iva.

    IF sy-subrc NE 0.
      CLEAR st_eventos.
      st_eventos-icone = icon_led_red.
      st_eventos-type  = c_e.
      CONCATENATE 'Código de IVA' wa_dados-iva 'não possui parâmetros! (ZLES0052)' INTO st_eventos-msg_text SEPARATED BY space.
      APPEND st_eventos TO ti_eventos.
      CONTINUE.
    ENDIF.

* Leis Fiscais para entrada de frete terceiro
    CASE wa_dados-add03+9(1).
      WHEN: '2'.

        SELECT SINGLE * FROM zlest0040 INTO sl_zlest0040 WHERE iva    EQ wa_dados-iva
                                                           AND fatura EQ 'T'.
        st_obj_item-taxlw1 = sl_zlest0040-icms.
        st_obj_item-taxlw2 = sl_zlest0040-ipi.
        st_obj_item-taxlw4 = sl_zlest0040-cofins.
        st_obj_item-taxlw5 = sl_zlest0040-pis.
    ENDCASE.


*    st_obj_item-taxlw1 = wa_zlest0040-icms.
*    st_obj_item-taxlw2 = wa_zlest0040-ipi.
*    st_obj_item-taxlw4 = wa_zlest0040-cofins.
*    st_obj_item-taxlw5 = wa_zlest0040-pis.

    SELECT SINGLE * INTO wa_mara FROM mara WHERE matnr EQ wa_dados-matns.
    st_obj_item-matkl = wa_mara-matkl.
    st_obj_item-meins = wa_mara-meins.

    SELECT SINGLE industry INTO vl_industry
      FROM j_1bbranch
     WHERE bukrs  EQ wa_dados-bukrs
       AND branch EQ wa_dados-werks.

    PERFORM busca_cat_destino USING wa_dados-tdlnr wa_dados-werks CHANGING vl_dstcat .

    SELECT SINGLE * INTO @DATA(wa_vttk)
      FROM vttk
     WHERE tknum EQ @wa_dados-tknum.

    CASE wa_vttk-vsart.
      WHEN '01' OR '03' OR '04'. "Rodoviário / Navegação fluvial
        DATA(cd_modal) = wa_vttk-vsart.
      WHEN '02'. "Ferroviário
        cd_modal = '04'.
      WHEN '04'. "Marítimo
        cd_modal = '03'.
      WHEN '05'. "Aéreo
        cd_modal = '02'.
      WHEN '06'. "Correio, serv.postal
      WHEN '07'. "Multimodal
        cd_modal = '06'.
    ENDCASE.

    SELECT SINGLE * INTO wa_zlest0037
      FROM zlest0037
     WHERE matnr    EQ wa_dados-matns
       AND bukrs    EQ wa_dados-bukrs
       AND cd_modal EQ cd_modal
       AND lifnr    EQ wa_dados-lifnr
       AND operacao EQ wa_dados-operacao. "*-CS2024000597-14.10.2024-#146076-JT-inicio

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE * INTO wa_zlest0037
        FROM zlest0037
       WHERE matnr    EQ wa_dados-matns
         AND bukrs    EQ wa_dados-bukrs
         AND cd_modal EQ cd_modal
         AND operacao EQ wa_dados-operacao. "*-CS2024000597-14.10.2024-#146076-JT-inicio
    ENDIF.

    IF wa_zlest0037-ck_servico EQ space.
      SELECT SINGLE cfop INTO vl_cfop
        FROM zlest0030
       WHERE direct     EQ c_1
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ c_1
         AND vkaus      NE 'V'
         AND tdlnr      EQ wa_dados-lifnr
         AND bukrs      EQ wa_dados-bukrs.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE cfop INTO vl_cfop
          FROM zlest0030
         WHERE direct     EQ c_1
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ c_1
           AND vkaus      NE 'V'
           AND tdlnr      EQ wa_dados-lifnr
           AND bukrs      EQ space.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE cfop INTO vl_cfop
            FROM zlest0030
           WHERE direct     EQ c_1
             AND dstcat     EQ vl_dstcat
             AND industry   EQ vl_industry
             AND tpparceiro EQ c_1
             AND vkaus      NE 'V'
             AND tdlnr      EQ space
             AND bukrs      EQ wa_dados-bukrs.

          IF sy-subrc IS NOT INITIAL.
            SELECT SINGLE cfop INTO vl_cfop
              FROM zlest0030
             WHERE direct     EQ c_1
               AND dstcat     EQ vl_dstcat
               AND industry   EQ vl_industry
               AND tpparceiro EQ c_1
               AND vkaus      NE 'V'
               AND tdlnr      EQ space
               AND bukrs      EQ space.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      SELECT SINGLE cfop INTO vl_cfop
        FROM zlest0030
       WHERE direct     EQ c_1
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ c_1
         AND vkaus      EQ 'V'
         AND tdlnr      EQ wa_dados-lifnr
         AND bukrs      EQ wa_dados-bukrs.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE cfop INTO vl_cfop
          FROM zlest0030
         WHERE direct     EQ c_1
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ c_1
           AND vkaus      EQ 'V'
           AND tdlnr      EQ wa_dados-lifnr
           AND bukrs      EQ space.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE cfop INTO vl_cfop
            FROM zlest0030
           WHERE direct     EQ c_1
             AND dstcat     EQ vl_dstcat
             AND industry   EQ vl_industry
             AND tpparceiro EQ c_1
             AND vkaus      EQ 'V'
             AND tdlnr      EQ space
             AND bukrs      EQ wa_dados-bukrs.

          IF sy-subrc IS NOT INITIAL.

            SELECT SINGLE cfop INTO vl_cfop
              FROM zlest0030
             WHERE direct     EQ c_1
               AND dstcat     EQ vl_dstcat
               AND industry   EQ vl_industry
               AND tpparceiro EQ c_1
               AND vkaus      EQ 'V'
               AND tdlnr      EQ space
               AND bukrs      EQ space.

          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    st_obj_item-cfop_10 = vl_cfop.

*==>>Inicio - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
    IF vl_nfesrv IS NOT INITIAL.
*    IF wa_zlest0037-ck_servico IS NOT INITIAL.
      st_obj_item-tmiss = abap_true.
    ENDIF.
*<<==Fim - SD-RMNI-CS0962061-Flag para nfe serviço 31.01.2022
    APPEND st_obj_item TO ti_obj_item.

    "Alimentar a estrutura do parceiros da nota (somente o parceiro LF é necessário)
    st_obj_partner-mandt  = sy-mandt.
    st_obj_partner-parvw  = c_lf.
    st_obj_partner-parid  = wa_dados-tdlnr.
    st_obj_partner-partyp = c_v.
    APPEND st_obj_partner TO ti_obj_partner.

    PERFORM calcula_impostos TABLES ti_obj_item ti_obj_item_tax USING wa_dados st_obj_header st_obj_partner.

    CLEAR vl_docnum.

    st_nfcheck-chekcon = c_x.

    st_obj_header-manual = 'X'.

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header     = st_obj_header
        nfcheck        = st_nfcheck
      IMPORTING
        e_docnum       = vl_docnum
      TABLES
        obj_partner    = ti_obj_partner
        obj_item       = ti_obj_item
        obj_item_tax   = ti_obj_item_tax
        obj_ot_partner = ti_obj_ot_partner
        return         = ti_return.

    IF NOT vl_docnum IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      IF ( NOT wa_dados-nfe IS INITIAL ) AND ( wa_zib_nfe_forn IS NOT INITIAL ).

        SELECT SINGLE * INTO i_doc
          FROM j_1bnfdoc
         WHERE docnum EQ vl_docnum.

        SELECT SINGLE * INTO wa_header_cte_aux
          FROM j_1bnfe_active
         WHERE docnum EQ vl_docnum.

        IF sy-subrc IS INITIAL.

          i_doc-authcod             = wa_zib_nfe_forn-nu_protocolo.
          i_doc-docstat             = '1'.
          wa_header_cte_aux-authcod = wa_zib_nfe_forn-nu_protocolo.
          wa_header_cte_aux-docnum9 = wa_zib_nfe_forn-nu_chave_aleator.
          wa_header_cte_aux-docsta  = '1'.
          wa_header_cte_aux-cdv     = wa_zib_nfe_forn-nu_chave_dv.
          wa_header_cte_aux-regio   = wa_zib_nfe_forn-nu_chave_regiao.

          CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
            EXPORTING
              i_doc     = i_doc
              i_acttab  = wa_header_cte_aux
              i_updmode = 'U'.

        ENDIF.

      ENDIF.

      LOOP AT ti_dados INTO st_dados WHERE re_belnr EQ wa_dados-re_belnr
                                       AND re_gjahr EQ wa_dados-re_gjahr.

        vg_tabix = sy-tabix.

        SELECT SINGLE * INTO wa_zlest0034
          FROM zlest0034
         WHERE tknum EQ st_dados-tknum.

        wa_zlest0034-en_docnum = vl_docnum.
        MODIFY zlest0034 FROM wa_zlest0034.

        CALL FUNCTION 'Z_LES_REVISAO_FATURA_TERC'
          EXPORTING
            p_zlest0034 = wa_zlest0034.

        COMMIT WORK.
        st_dados-en_docnum = vl_docnum.
        PERFORM verificar_status CHANGING st_dados.
        MODIFY ti_dados INDEX vg_tabix FROM st_dados TRANSPORTING status en_docnum.
      ENDLOOP.

      LOOP AT ti_return INTO wa_return.
        IF wa_return-type EQ c_e.
          wa_return-type = c_w.
          MODIFY ti_return INDEX sy-tabix FROM wa_return TRANSPORTING type.
        ENDIF.
      ENDLOOP.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM: z_estorno_miro_fiscal .
    ENDIF.

    PERFORM popula_log TABLES ti_return.

  ENDLOOP.

  READ TABLE ti_eventos INTO st_eventos WITH KEY type = c_e.
  IF sy-subrc IS INITIAL.
    MESSAGE s000(zles) WITH TEXT-029.
  ELSE.
    READ TABLE ti_eventos INTO st_eventos WITH KEY type = c_w.
    IF sy-subrc IS INITIAL.
      MESSAGE s000(zles) WITH TEXT-030.
    ELSE.
      MESSAGE s000(zles) WITH TEXT-028.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_GERAR_FISCAL

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CAT_DESTINO
*&---------------------------------------------------------------------*
*       Busca categoria de CFOP baseado nas UFs
*----------------------------------------------------------------------*
FORM busca_cat_destino  USING  p_lifnr  TYPE lifnr
                               p_werks  TYPE werks_d
                      CHANGING p_dstcat TYPE j_1bdstcat.

  DATA: wk_txjcd TYPE txjcd,
        lf_txjcd TYPE txjcd.

  PERFORM ufs USING p_werks p_lifnr CHANGING wk_txjcd lf_txjcd.

  IF wk_txjcd(2) EQ lf_txjcd(2).
    p_dstcat = '0'.
  ELSE.
    p_dstcat = '1'.
  ENDIF.

ENDFORM.                    " BUSCA_CAT_DESTINO

*&---------------------------------------------------------------------*
*&      Form  UFS
*&---------------------------------------------------------------------*
*       Seleciona UF de Cliente e Fornecedor
*----------------------------------------------------------------------*
FORM ufs  USING    p_werks  TYPE werks_d
                   p_lifnr  TYPE lifnr
          CHANGING wk_txjcd TYPE txjcd
                   lf_txjcd TYPE txjcd.

  SELECT SINGLE txjcd INTO wk_txjcd
    FROM t001w
   WHERE werks EQ p_werks.

  SELECT SINGLE txjcd INTO lf_txjcd
    FROM lfa1
   WHERE lifnr EQ p_lifnr.

ENDFORM.                    " UFS

*&---------------------------------------------------------------------*
*&      Form  POPULA_LOG
*&---------------------------------------------------------------------*
*       Popula tabela de log de processamento
*----------------------------------------------------------------------*
FORM popula_log  TABLES   p_it_return STRUCTURE bapiret2.

  DATA: wa_return TYPE bapiret2,
        wa_numero TYPE t100-msgnr.

  LOOP AT p_it_return INTO wa_return.

    CLEAR st_eventos.

    CASE wa_return-type.
      WHEN c_s.
        st_eventos-icone    = icon_led_green.
      WHEN c_w.
        st_eventos-icone    = icon_led_yellow.
      WHEN c_e.
        st_eventos-icone    = icon_led_red.
    ENDCASE.

    st_eventos-type     = wa_return-type.
    st_eventos-msg_id   = wa_return-id.
    st_eventos-msg_no   = wa_return-number.
    st_eventos-msg_var1 = wa_return-message_v1.
    st_eventos-msg_var2 = wa_return-message_v2.
    st_eventos-msg_var3 = wa_return-message_v3.
    st_eventos-msg_var4 = wa_return-message_v4.

    wa_numero = wa_return-number.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = wa_return-id
        msg_no                 = wa_numero
        msg_var1               = wa_return-message_v1
        msg_var2               = wa_return-message_v2
        msg_var3               = wa_return-message_v3
        msg_var4               = wa_return-message_v4
      IMPORTING
        msg_text               = st_eventos-msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    IF sy-subrc EQ 0.
      APPEND st_eventos TO ti_eventos.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " POPULA_LOG

*&---------------------------------------------------------------------*
*&      Form  CALCULA_IMPOSTOS
*&---------------------------------------------------------------------*
*       Calcula Impostos
*----------------------------------------------------------------------*
FORM calcula_impostos  TABLES   itens     STRUCTURE bapi_j_1bnflin
                                impostos  STRUCTURE bapi_j_1bnfstx
                        USING   wa_dados  TYPE zftte_dados
                                cabecalho TYPE bapi_j_1bnfdoc
                                parceiro  TYPE bapi_j_1bnfnad.

  DATA: wa_itens TYPE bapi_j_1bnflin,
        vg_tabix TYPE sy-tabix.

  LOOP AT itens INTO wa_itens.

    vg_tabix = sy-tabix.

    PERFORM calcular_icms   TABLES impostos USING wa_dados wa_itens cabecalho parceiro space.
    PERFORM calcular_ipi    TABLES impostos USING wa_dados wa_itens cabecalho parceiro space.
    PERFORM calcular_cofins TABLES impostos USING wa_dados wa_itens cabecalho parceiro space.
    PERFORM calcular_pis    TABLES impostos USING wa_dados wa_itens cabecalho parceiro space.

    MODIFY itens INDEX sy-tabix FROM wa_itens TRANSPORTING netpr netwr.

    PERFORM calcular_icms   TABLES impostos USING wa_dados wa_itens cabecalho parceiro c_x.
    PERFORM calcular_ipi    TABLES impostos USING wa_dados wa_itens cabecalho parceiro c_x.
    PERFORM calcular_cofins TABLES impostos USING wa_dados wa_itens cabecalho parceiro c_x.
    PERFORM calcular_pis    TABLES impostos USING wa_dados wa_itens cabecalho parceiro c_x.

    wa_itens-netpr = wa_itens-netpr - wa_dados-valor_icms - wa_dados-valor_cofins - wa_dados-valor_pis.
    wa_itens-netwr = wa_itens-netwr - wa_dados-valor_icms - wa_dados-valor_cofins - wa_dados-valor_pis.

    MODIFY itens INDEX vg_tabix FROM wa_itens TRANSPORTING netpr netwr.

  ENDLOOP.

ENDFORM.                    " CALCULA_IMPOSTOS

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_ICMS
*&---------------------------------------------------------------------*
*       Calcula Valores do ICMS
*----------------------------------------------------------------------*
FORM calcular_icms  TABLES   impostos  STRUCTURE bapi_j_1bnfstx
                     USING   wa_dados  TYPE zftte_dados
                             itens     TYPE bapi_j_1bnflin
                             cabecalho TYPE bapi_j_1bnfdoc
                             parceiro  TYPE bapi_j_1bnfnad
                             calcular  TYPE c.

  DATA: wk_txjcd    TYPE txjcd,
        lf_txjcd    TYPE txjcd,
        wa_impostos TYPE bapi_j_1bnfstx,
        wa_j_1batl1 TYPE j_1batl1,
        st_icms     TYPE c LENGTH 2.

  IF itens-taxlw1 IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1batl1 FROM j_1batl1
     WHERE taxlaw = itens-taxlw1.

    itens-taxsit = wa_j_1batl1-taxsit.
    PERFORM ufs USING cabecalho-branch parceiro-parid CHANGING wk_txjcd lf_txjcd.

    IF wa_dados-valor_icms GT 0.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.
      wa_impostos-rate   = wa_dados-rate_icms.
      wa_impostos-taxval = wa_dados-valor_icms.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = CONV #( wa_dados-valor_pedagio ).
    ELSE.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = 0.
      wa_impostos-rate   = 0.
      wa_impostos-taxval = 0.
      wa_impostos-excbas = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.
      wa_impostos-othbas = CONV #( wa_dados-valor_pedagio ).
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl1-taxsit
      IMPORTING
        output = st_icms.

    "PERFORM busca_valores_icms USING itens wa_impostos wk_txjcd lf_txjcd calcular st_icms.

    "Selecionar Tipo de imposto
    PERFORM selecionar_tipo_imposto USING c_icms CHANGING wa_impostos-taxtyp.

*STATTX J_1BSTATTX  CHAR  01  0 Código: item estatístico de imposto
*RECTYPE  J_1BTXRT  CHAR  01  0 Nota Fiscal tipo de taxa de imposto
*FACTOR J_1BTXIPF   DEC   06  2 Núm.unidades IPI
*UNIT   J_1BTXIPU   UNIT  03  0 IPI Pauta: unidade de referência
    IF calcular IS NOT INITIAL.
      APPEND wa_impostos TO impostos.
    ENDIF.

  ENDIF.

ENDFORM.                    " CALCULAR_ICMS

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_TIPO_IMPOSTO
*&---------------------------------------------------------------------*
*       Seleciona tipo do imposto baseado no IVA e Tipo de Cond. Ativa
*----------------------------------------------------------------------*
FORM selecionar_tipo_imposto  USING    c_tp_imposto TYPE c
                              CHANGING tipo_imposto.

  DATA: it_j_1baj TYPE TABLE OF j_1baj,
        wa_j_1baj TYPE j_1baj,
        it_t683s  TYPE TABLE OF t683s,
        wa_t683s  TYPE t683s.

  SELECT * INTO TABLE it_j_1baj FROM j_1baj WHERE taxgrp EQ c_tp_imposto.

  SELECT * INTO TABLE it_t683s
    FROM t683s
     FOR ALL ENTRIES IN it_j_1baj
   WHERE kvewe EQ c_a
     AND kappl EQ c_tx
     AND kalsm EQ c_taxbra
     AND kschl EQ it_j_1baj-taxtyp
     AND kstat EQ c_x.

  READ TABLE it_t683s INDEX 1 INTO wa_t683s.
  tipo_imposto = wa_t683s-kschl.

ENDFORM.                    " SELECIONAR_TIPO_IMPOSTO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALORES_ICMS
*&---------------------------------------------------------------------*
*       Busca ICMS por situação tributária
*----------------------------------------------------------------------*
FORM busca_valores_icms USING itens       TYPE bapi_j_1bnflin
                              wa_impostos TYPE bapi_j_1bnfstx
                              wk_txjcd    TYPE txjcd
                              lf_txjcd    TYPE txjcd
                              calcular    TYPE c
                              st_icms     TYPE c.

  DATA: wa_j_1btxic1 TYPE j_1btxic1,
        wa_j_1btxic2 TYPE j_1btxic2.

  CASE st_icms.
    WHEN '00'. "Sujeito a ICMS

      "ICMS Normal
      SELECT SINGLE * INTO wa_j_1btxic1
        FROM j_1btxic1
       WHERE land1    EQ c_br
         AND shipfrom EQ wk_txjcd(3)
         AND shipto   EQ lf_txjcd(3).

      IF sy-subrc IS INITIAL.
        IF calcular IS INITIAL.
          itens-netpr = itens-netpr / ( ( 100 + wa_j_1btxic1-rate_f ) / 100 ).
        ENDIF.
        wa_impostos-rate   = wa_j_1btxic1-rate_f.
        wa_impostos-base   = itens-netpr.
        wa_impostos-taxval = ( wa_impostos-base * wa_impostos-rate ) / 100.
        wa_impostos-excbas = itens-netpr - wa_impostos-base.
      ENDIF.

    WHEN '10'. "Sujeito a ICMS + Substituição Tributária
      EXIT.
    WHEN '20'. "Sujeito a ICMS (base do imposto reduzida)

      "ICMS Exceções
      SELECT SINGLE * INTO wa_j_1btxic2
        FROM j_1btxic2
       WHERE land1    EQ c_br
         AND shipfrom EQ wk_txjcd(3)
         AND shipto   EQ lf_txjcd(3)
         AND matnr    EQ itens-matnr.

      IF sy-subrc IS INITIAL.
        "Adcionar impostos
        IF calcular IS INITIAL.
          itens-netpr = itens-netpr - ( ( ( ( itens-netpr * wa_j_1btxic2-base / 100 ) ) * wa_j_1btxic2-rate ) / 100 ).
        ENDIF.
        wa_impostos-rate   = wa_j_1btxic2-rate.
        wa_impostos-base   = ( itens-netpr * wa_j_1btxic2-base ) / 100.
        wa_impostos-taxval = ( wa_impostos-base * wa_impostos-rate ) / 100.
        wa_impostos-excbas = itens-netpr - wa_impostos-base.
      ENDIF.

    WHEN '30'. "Isento ou não sujeito a imposto + Substituição Tributária
      EXIT.
    WHEN '40'. "Isento
      wa_impostos-excbas = itens-netpr.
    WHEN '41'. "Não sujeito a imposto
      wa_impostos-excbas = itens-netpr.
    WHEN '50'. "Suspenso
      wa_impostos-othbas = itens-netpr.
    WHEN '51'. "Diferido
      wa_impostos-othbas = itens-netpr.
    WHEN '60'. "ICMS já cobrado por meio de Substituição Tributária
      EXIT.
    WHEN '70'. "Sujeito a ICMS (base imposto reduzida) + Sub.Trib.
      EXIT.
    WHEN '90'. "Outros/as
      EXIT.
  ENDCASE.

ENDFORM.                    " BUSCA_VALORES_ICMS

*&---------------------------------------------------------------------*
*&      Form  BUSCA_TAXA_ICMS
*&---------------------------------------------------------------------*
*       Busca taxa de ICMS de transporte de uma região para outra
*----------------------------------------------------------------------*
FORM busca_taxa_icms_pis_cofins CHANGING wa_dados TYPE zftte_dados.

  DATA: it_j_1btxic1  TYPE TABLE OF j_1btxic1 INITIAL SIZE 0 WITH HEADER LINE,
        it_j_1btxpis  TYPE TABLE OF j_1btxpis INITIAL SIZE 0 WITH HEADER LINE,
        it_j_1btxcof  TYPE TABLE OF j_1btxcof INITIAL SIZE 0 WITH HEADER LINE,
        wa_j_1btxic1  TYPE j_1btxic1,
        wa_j_1btxpis  TYPE j_1btxpis,
        wa_j_1btxcof  TYPE j_1btxcof,
        vg_inicio_val TYPE c LENGTH 10,
        vg_vencimento TYPE c LENGTH 10,
        dt_inicio_val TYPE zdt_vencto,
        dt_vencimento TYPE zdt_vencto,
        p_lifnr       TYPE lifnr,
        valor         TYPE j_1bvalue,
        vw_lfa1_1     TYPE lfa1,
        vw_kna1_1     TYPE kna1,
        vw_lfa1_2     TYPE lfa1.


  DATA: lt_zlest0061 TYPE TABLE OF zlest0061,
        ls_zlest0061 TYPE zlest0061,
        lt_zlest0056 TYPE TABLE OF zlest0056,
        ls_zlest0056 TYPE zlest0056.

  wa_dados-rate_icms    = 0.
  wa_dados-rate_pis     = 0.
  wa_dados-rate_cofins  = 0.

  wa_dados-base_icms    = 0.
  wa_dados-base_pis     = 0.
  wa_dados-base_cofins  = 0.

  wa_dados-valor_icms   = 0.
  wa_dados-valor_pis    = 0.
  wa_dados-valor_cofins = 0.


  CASE wa_dados-vsart.
    WHEN: '03'.

      IF zcl_parceiro=>get_parceiro_local_negocio( i_partiner = st_dados-lifnr ) EQ abap_true.
        SELECT SINGLE * FROM zlest0061
          INTO ls_zlest0061
         WHERE tknum  EQ wa_dados-tknum
           AND fknum  EQ wa_dados-fknum
           AND ck_anulado EQ abap_false.

        IF ( sy-subrc EQ 0 ).
          SELECT SINGLE * FROM zlest0056
            INTO ls_zlest0056
           WHERE bukrs      EQ ls_zlest0061-bukrs
             AND werks      EQ ls_zlest0061-werks
             AND nr_viagem  EQ ls_zlest0061-nr_viagem
             AND ano_viagem EQ ls_zlest0061-ano_viagem.

          SELECT SINGLE * INTO vw_lfa1_1
            FROM lfa1
           WHERE lifnr EQ ls_zlest0056-po_embarque.

          wa_dados-regio_emissor = vw_lfa1_1-regio.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_dados-werks
            IMPORTING
              output = p_lifnr.

          SELECT SINGLE * INTO vw_lfa1_2
            FROM lfa1
           WHERE lifnr EQ p_lifnr.

          SELECT SINGLE * INTO vw_kna1_1
            FROM kna1
           WHERE kunnr EQ ls_zlest0056-po_destino.

          wa_dados-regio_receptor  = vw_kna1_1-regio.

          IF vw_kna1_1-regio NE vw_lfa1_2-regio.
            wa_dados-txjcd_emissor = vw_kna1_1-txjcd.
          ELSE.
            CLEAR: wa_dados-txjcd_emissor.
          ENDIF.

        ENDIF.
      ELSE.
        SELECT SINGLE regio INTO wa_dados-regio_emissor
          FROM lfa1
         WHERE lifnr EQ wa_dados-lifnr.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_dados-werks
          IMPORTING
            output = p_lifnr.

        SELECT SINGLE regio INTO wa_dados-regio_receptor
          FROM lfa1
         WHERE lifnr EQ p_lifnr.
      ENDIF.

    WHEN OTHERS.

      SELECT SINGLE regio INTO wa_dados-regio_emissor
        FROM lfa1
       WHERE lifnr EQ wa_dados-lifnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_dados-werks
        IMPORTING
          output = p_lifnr.

      SELECT SINGLE regio INTO wa_dados-regio_receptor
        FROM lfa1
       WHERE lifnr EQ p_lifnr.
  ENDCASE.

  IF ( wa_dados-iva EQ 'I8' ) OR ( wa_dados-iva EQ 'I1' ) .

    wa_dados-base_icms = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.

    SELECT * INTO TABLE it_j_1btxic1
      FROM j_1btxic1
     WHERE land1    EQ 'BR'
       AND shipfrom EQ wa_dados-regio_emissor
       AND shipto   EQ wa_dados-regio_receptor.

    SORT it_j_1btxic1 BY validfrom.

    LOOP AT it_j_1btxic1 INTO wa_j_1btxic1.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_j_1btxic1-validfrom
        IMPORTING
          output = vg_vencimento.

      WRITE vg_vencimento TO dt_vencimento.

      IF ( dt_vencimento LE wa_dados-zdt_conhec ) AND ( wa_dados-rate_icms EQ 0 ).
        wa_dados-rate_icms = wa_j_1btxic1-rate.
        IF wa_j_1btxic1-rate GT 0.
          wa_dados-valor_icms = wa_dados-base_icms * ( wa_j_1btxic1-rate / 100 ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

  IF ( wa_dados-iva EQ 'I1' ) OR ( wa_dados-iva EQ 'I0' ) OR ( wa_dados-iva EQ 'S1' ).

    CLEAR: vg_vencimento, dt_vencimento,
           vg_inicio_val, dt_inicio_val.

    wa_dados-base_pis    = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.
    wa_dados-base_cofins = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.


    valor = wa_dados-werks.

    SELECT * INTO TABLE it_j_1btxpis
      FROM j_1btxpis
     WHERE country EQ 'BR'
       AND gruop   EQ '72'
       AND value   EQ valor.

    LOOP AT it_j_1btxpis INTO wa_j_1btxpis.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_j_1btxpis-validfrom
        IMPORTING
          output = vg_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_j_1btxpis-validto
        IMPORTING
          output = vg_vencimento.

      WRITE vg_vencimento TO dt_vencimento.
      WRITE vg_inicio_val TO dt_inicio_val.

      IF ( dt_vencimento GE wa_dados-zdt_conhec ) AND ( dt_inicio_val LE wa_dados-zdt_conhec ).
        wa_dados-rate_pis = wa_j_1btxpis-rate.
        IF wa_j_1btxpis-rate GT 0.
          wa_dados-valor_pis = wa_dados-base_pis * ( wa_j_1btxpis-rate / 100 ).
        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR: vg_vencimento, dt_vencimento,
           vg_inicio_val, dt_inicio_val.

    SELECT * INTO TABLE it_j_1btxcof
      FROM j_1btxcof
     WHERE country EQ 'BR'
       AND gruop   EQ '71'
       AND value   EQ valor.

    LOOP AT it_j_1btxcof INTO wa_j_1btxcof.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_j_1btxcof-validfrom
        IMPORTING
          output = vg_inicio_val.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_j_1btxcof-validto
        IMPORTING
          output = vg_vencimento.

      WRITE vg_vencimento TO dt_vencimento.
      WRITE vg_inicio_val TO dt_inicio_val.

      IF ( dt_vencimento GE wa_dados-zdt_conhec ) AND ( dt_inicio_val LE wa_dados-zdt_conhec ).
        wa_dados-rate_cofins = wa_j_1btxcof-rate.
        IF wa_j_1btxcof-rate GT 0.
          wa_dados-valor_cofins = wa_dados-base_cofins * ( wa_j_1btxcof-rate / 100 ).
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " BUSCA_TAXA_ICMS

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_IPI
*&---------------------------------------------------------------------*
*       Calcula Valores do PIS
*----------------------------------------------------------------------*
FORM calcular_pis  TABLES impostos  STRUCTURE bapi_j_1bnfstx
                    USING wa_dados  TYPE zftte_dados
                          itens     TYPE bapi_j_1bnflin
                          cabecalho TYPE bapi_j_1bnfdoc
                          parceiro  TYPE bapi_j_1bnfnad
                          calcular  TYPE c.


  DATA: wk_txjcd    TYPE txjcd,
        lf_txjcd    TYPE txjcd,
        wa_impostos TYPE bapi_j_1bnfstx,
        wa_j_1batl5 TYPE j_1batl5,
        st_pis      TYPE c LENGTH 2,
        lw_pis      TYPE c LENGTH 3.

  IF itens-taxlw5 IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1batl5 FROM j_1batl5
     WHERE taxlaw EQ itens-taxlw5.

    itens-taxsi5 = wa_j_1batl5-taxsit.

    IF wa_dados-valor_pis GT 0.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.
      wa_impostos-rate   = wa_dados-rate_pis.
      wa_impostos-taxval = wa_dados-valor_pis.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = CONV #( wa_dados-valor_pedagio ).
    ELSE.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = 0.
      wa_impostos-rate   = 0.
      wa_impostos-taxval = 0.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = CONV #( wa_dados-dmbtr_doc ).
    ENDIF.


    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl5-taxsit
      IMPORTING
        output = st_pis.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl5-taxlaw
      IMPORTING
        output = lw_pis.

    "PERFORM busca_valores_pis USING itens wa_impostos wk_txjcd lf_txjcd calcular st_pis lw_pis.

    "Selecionar Tipo de imposto
    PERFORM selecionar_tipo_imposto USING c_pis CHANGING wa_impostos-taxtyp.

    IF calcular IS NOT INITIAL.
      APPEND wa_impostos TO impostos.
    ENDIF.

  ENDIF.

ENDFORM.                    " CALCULAR_IPI

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALORES_PIS
*&---------------------------------------------------------------------*
*       Busca IPI por situação tributária
*----------------------------------------------------------------------*
FORM busca_valores_pis  USING itens       TYPE bapi_j_1bnflin
                              wa_impostos TYPE bapi_j_1bnfstx
                              wk_txjcd    TYPE txjcd
                              lf_txjcd    TYPE txjcd
                              calcular    TYPE c
                              st_pis      TYPE c
                              lw_pis      TYPE c.

*  DATA:  wa_j_1btxip1 TYPE j_1btxip1,
*         wa_j_1btxip2 TYPE j_1btxip2.

  CASE st_pis.
*    WHEN '01'. "Sujeito a ICMS
*
**      "ICMS Normal
**      SELECT SINGLE * INTO wa_j_1btxic1
**        FROM j_1btxic1
**       WHERE land1    EQ c_br
**         AND shipfrom EQ wk_txjcd(3)
**         AND shipto   EQ lf_txjcd(3).
**
**      IF sy-subrc IS INITIAL.
**        IF calcular IS INITIAL.
**          itens-netpr = itens-netpr / ( ( 100 + wa_j_1btxic1-rate_f ) / 100 ).
**        ENDIF.
**        wa_impostos-rate   = wa_j_1btxic1-rate_f.
**        wa_impostos-base   = itens-netpr.
**        wa_impostos-taxval = ( wa_impostos-base * wa_impostos-rate ) / 100.
**        wa_impostos-excbas = itens-netpr - wa_impostos-base.
**      ENDIF.
*
*    WHEN '10'. "Sujeito a ICMS + Substituição Tributária
*      EXIT.
*    WHEN '20'. "Sujeito a ICMS (base do imposto reduzida)
*
**      "ICMS Exceções
**      SELECT SINGLE * INTO wa_j_1btxic2
**        FROM j_1btxic2
**       WHERE land1    EQ c_br
**         AND shipfrom EQ wk_txjcd(3)
**         AND shipto   EQ lf_txjcd(3)
**         AND matnr    EQ itens-matnr.
**
**      IF sy-subrc IS INITIAL.
**        "Adcionar impostos
**        IF calcular IS INITIAL.
**          itens-netpr = itens-netpr - ( ( ( ( itens-netpr * wa_j_1btxic2-base / 100 ) ) * wa_j_1btxic2-rate ) / 100 ).
**        ENDIF.
**        wa_impostos-rate   = wa_j_1btxic2-rate.
**        wa_impostos-base   = ( itens-netpr * wa_j_1btxic2-base ) / 100.
**        wa_impostos-taxval = ( wa_impostos-base * wa_impostos-rate ) / 100.
**        wa_impostos-excbas = itens-netpr - wa_impostos-base.
**      ENDIF.
*
*    WHEN '30'. "Isento ou não sujeito a imposto + Substituição Tributária
*      EXIT.
*    WHEN '40'. "Isento
*      wa_impostos-excbas = itens-netpr.
*    WHEN '41'. "Não sujeito a imposto
*      wa_impostos-excbas = itens-netpr.
*    WHEN '50'. "Suspenso
*      wa_impostos-othbas = itens-netpr.
*    WHEN '51'. "Diferido
*      wa_impostos-othbas = itens-netpr.
*    WHEN '60'. "ICMS já cobrado por meio de Substituição Tributária
*      EXIT.
*    WHEN '70'. "Sujeito a ICMS (base imposto reduzida) + Sub.Trib.
*      EXIT.
*    WHEN '90'. "Outros/as
*      EXIT.
    WHEN '98'. "Outros/as
      wa_impostos-excbas = itens-netpr.
  ENDCASE.

ENDFORM.                    " BUSCA_VALORES_PIS

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_COFINS
*&---------------------------------------------------------------------*
*       Calcula Valores do COFINS
*----------------------------------------------------------------------*
FORM calcular_cofins  TABLES impostos  STRUCTURE bapi_j_1bnfstx
                       USING wa_dados  TYPE zftte_dados
                             itens     TYPE bapi_j_1bnflin
                             cabecalho TYPE bapi_j_1bnfdoc
                             parceiro  TYPE bapi_j_1bnfnad
                             calcular  TYPE c.

  DATA: wk_txjcd     TYPE txjcd,
        lf_txjcd     TYPE txjcd,
        wa_impostos  TYPE bapi_j_1bnfstx,
        wa_j_1batl4a TYPE j_1batl4a,
        st_cofins    TYPE c LENGTH 2,
        lw_cofins    TYPE c LENGTH 3.

  IF itens-taxlw4 IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1batl4a FROM j_1batl4a
     WHERE taxlaw EQ itens-taxlw4.

    itens-taxsi4 = wa_j_1batl4a-taxsit.

    IF wa_dados-valor_cofins GT 0.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = wa_dados-dmbtr_doc - wa_dados-valor_pedagio.
      wa_impostos-rate   = wa_dados-rate_cofins.
      wa_impostos-taxval = wa_dados-valor_cofins.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = CONV #( wa_dados-valor_pedagio ).
    ELSE.
      wa_impostos-itmnum = 10.
      wa_impostos-base   = 0.
      wa_impostos-rate   = 0.
      wa_impostos-taxval = 0.
      wa_impostos-excbas = 0.
      wa_impostos-othbas = CONV #( wa_dados-dmbtr_doc ).
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl4a-taxsit
      IMPORTING
        output = st_cofins.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl4a-taxlaw
      IMPORTING
        output = lw_cofins.

    "PERFORM busca_valores_pis USING itens wa_impostos wk_txjcd lf_txjcd calcular st_cofins lw_cofins.

    "Selecionar Tipo de imposto
    PERFORM selecionar_tipo_imposto USING c_cofins CHANGING wa_impostos-taxtyp.

    IF calcular IS NOT INITIAL.
      APPEND wa_impostos TO impostos.
    ENDIF.

  ENDIF.

ENDFORM.                    " CALCULAR_COFINS

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_IPI
*&---------------------------------------------------------------------*
*       Calcula Valores do IPI
*----------------------------------------------------------------------*
FORM calcular_ipi  TABLES impostos  STRUCTURE bapi_j_1bnfstx
                    USING wa_dados  TYPE zftte_dados
                          itens     TYPE bapi_j_1bnflin
                          cabecalho TYPE bapi_j_1bnfdoc
                          parceiro  TYPE bapi_j_1bnfnad
                          calcular  TYPE c.

  DATA: wk_txjcd    TYPE txjcd,
        lf_txjcd    TYPE txjcd,
        wa_impostos TYPE bapi_j_1bnfstx,
        wa_j_1batl2 TYPE j_1batl2,
        st_ipi      TYPE n LENGTH 5,
        lw_ipi      TYPE c LENGTH 3.

  IF itens-taxlw2 IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1batl2 FROM j_1batl2
     WHERE taxlaw EQ itens-taxlw2.

    wa_impostos-itmnum = 10.
    wa_impostos-base   = 0.
    wa_impostos-rate   = 0.
    wa_impostos-taxval = 0.
    wa_impostos-excbas = 0.
    wa_impostos-othbas = CONV #( wa_dados-dmbtr_doc ).

    st_ipi = wa_j_1batl2-taxsit.

    CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
      EXPORTING
        input  = wa_j_1batl2-taxlaw
      IMPORTING
        output = lw_ipi.

    "PERFORM busca_valores_ipi USING itens wa_impostos itens-matnr calcular st_ipi lw_ipi.

    "Selecionar Tipo de imposto
    PERFORM selecionar_tipo_imposto USING c_ipi CHANGING wa_impostos-taxtyp.

    IF calcular IS NOT INITIAL.
      APPEND wa_impostos TO impostos.
    ENDIF.

  ENDIF.

ENDFORM.                    " CALCULAR_IPI

*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALORES_IPI
*&---------------------------------------------------------------------*
*       Busca IPI por situação tributária
*----------------------------------------------------------------------*
FORM busca_valores_ipi  USING itens       TYPE bapi_j_1bnflin
                              wa_impostos TYPE bapi_j_1bnfstx
                              cd_material TYPE matnr
                              calcular    TYPE c
                              st_ipi      TYPE n
                              lw_ipi      TYPE c.

  CASE st_ipi.
    WHEN 49. "Outras Entradas
      wa_impostos-excbas = itens-netpr.
    WHEN 99. "Outras Saídas
      wa_impostos-excbas = itens-netpr.
  ENDCASE.


ENDFORM.                    " BUSCA_VALORES_IPI
