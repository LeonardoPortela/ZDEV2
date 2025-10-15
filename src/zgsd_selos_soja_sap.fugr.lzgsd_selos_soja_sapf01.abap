*----------------------------------------------------------------------*
***INCLUDE LZGSD_SELOS_SOJA_SAPF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_RECUPERA_XML
*&---------------------------------------------------------------------*

FORM f_recupera_xml CHANGING p_xml_doc.

  CLEAR p_xml_doc.

  TRY .
      zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum =  l_docnum
        )->set_registro(      EXPORTING i_docnum        =  l_docnum
                                        i_sem_bloqueio  =  abap_true
        )->get_ck_autorizado_uso(
        )->get_xml_grc(       IMPORTING e_xml_string    = DATA(_xml_doc) ).
    CATCH zcx_doc_eletronico  INTO DATA(ex_doc).
      ex_doc->published_erro( EXPORTING i_msgty         = 'S'
                                        i_msgty_display = 'W' ).
      RETURN.
  ENDTRY.

  p_xml_doc = _xml_doc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_IMPRESSAO
*&---------------------------------------------------------------------*
FORM f_saida_impressao .

  g_output_options-tddest        = 'LOCL'.
  g_output_options-tdnewid       = 'X'.
  g_output_options-tdimmed       = 'X'.
  g_output_options-tdreceiver    = sy-uname.
  g_output_options-tddelete      = 'X'.
*
  g_control_parameters-device    = 'PRINTER'.
  g_control_parameters-preview   = ''.
  g_control_parameters-getotf    = 'X'.
  g_control_parameters-langu     = sy-langu.
  g_control_parameters-no_dialog = 'X'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_IMPRESSAO_SELO
*&---------------------------------------------------------------------*
FORM f_monta_estrutura_xml.

  FREE: wg_xml_sefaz,
        t_element_array.

  APPEND 'det'       TO t_element_array.
  APPEND 'detPag'    TO t_element_array.
  APPEND 'NFref'     TO t_element_array.
  APPEND 'DI'        TO t_element_array.
  APPEND 'adi'       TO t_element_array.
  APPEND 'detExport' TO t_element_array.
  APPEND 'med'       TO t_element_array.
  APPEND 'arma'      TO t_element_array.
  APPEND 'comb'      TO t_element_array.
  APPEND 'lacres'    TO t_element_array.
  APPEND 'dup'       TO t_element_array.
  APPEND 'pag'       TO t_element_array.
  APPEND 'procRef'   TO t_element_array.
  APPEND 'obsCont'   TO t_element_array.
  APPEND 'obsFisco'  TO t_element_array.
  APPEND 'vol'       TO t_element_array.

*------------------------------------------
* elementos XML
*------------------------------------------
  DATA(_json) = zcl_string=>xml_to_json( i_xml           =  l_xml_doc
                                         i_element_array =  t_element_array ).

*------------------------------------------
* descerializacao XML
*------------------------------------------
  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = _json
    CHANGING
      data = wg_xml_sefaz.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_IMPRESSAO_SELO
*&---------------------------------------------------------------------*
FORM f_monta_dados.

  PERFORM f_limpa_variaveis.
  PERFORM f_ins_dados_cabecalho.
  PERFORM f_ins_dados_imposto.
  PERFORM f_ins_dados_transporte.
  PERFORM f_ins_dados_itens.
  PERFORM f_ins_dados_adicionais.

ENDFORM.

*&---------------------------------------------------------------------*
*& calcular lote
*&---------------------------------------------------------------------*
FORM f_recupera_lote.

  l_data_ref = sy-datum.

  CALL FUNCTION 'ZSD_CALCULAR_LOTE_SELO'
    EXPORTING
      i_werks                  = w_j1bnflin-werks
      i_safra                  = l_safra
      i_matnr                  = w_j1bnflin-matnr
      i_data_ref               = l_data_ref
      i_docnum                 = l_docnum
    IMPORTING
      e_lote                   = l_numero_lote
      e_data_fabricacao        = l_data_fabricacao
      e_data_validade          = l_data_validade
      e_selo                   = l_selo
    EXCEPTIONS
      documento_nao_autorizado = 1
      documento_nao_imprimir   = 2
      OTHERS                   = 3.

  IF sy-subrc  = 1.
    RAISE documento_nao_autorizado.
    EXIT.
  ELSEIF sy-subrc  = 2.
    RAISE documento_nao_imprimir.
    EXIT.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* Chamada do Smart Form da DANFE
*----------------------------------------------------------------------*
FORM f_saida_formulario CHANGING p_document_output_info TYPE  ssfcrespd
                                 p_job_output_info      TYPE  ssfcrescl
                                 p_job_output_options   TYPE  ssfcresop.

  DATA: it_otf  TYPE TABLE OF ssfcrescl.

* Ordena as tabelas output
  SORT t_zbrnfe_danfe_item.
  SORT t_zbrnfe_danfe_item_desc.
  SORT t_zbrnfe_danfe_fatura.
  SORT t_zbrnfe_danfe_dados_adic.

*-----------------------------------------
* Busca nome da função de chamada do formulário
*-----------------------------------------
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZBRNFE_SELO_SAP'
    IMPORTING
      fm_name            = ls_funcname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*-----------------------------------------
* executa
*-----------------------------------------
  CALL FUNCTION ls_funcname
    EXPORTING
      control_parameters   = g_control_parameters
      output_options       = g_output_options
      user_settings        = space
      i_cabecalho          = t_danfe_cabecalho
      i_selo               = l_selo
      i_data_fabricacao    = l_data_fabricacao
      i_data_validade      = l_data_validade
      i_numero_lote        = l_numero_lote
    IMPORTING
      document_output_info = p_document_output_info
      job_output_info      = p_job_output_info
      job_output_options   = p_job_output_options
    TABLES
      it_itens             = t_zbrnfe_danfe_item
      it_itens_desc        = t_zbrnfe_danfe_item_desc
      it_fatura            = t_zbrnfe_danfe_fatura
      it_dados_adic        = t_zbrnfe_danfe_dados_adic
      it_cfop              = t_zbrnfe_danfe_cfop
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "zf_chama_smart_form

*&---------------------------------------------------------------------*
*&      Form  F_IMPRESSAO_SELO
*&---------------------------------------------------------------------*
FORM f_limpa_variaveis.

  CLEAR   t_danfe_cabecalho.
  REFRESH t_zbrnfe_danfe_item.
  REFRESH t_zbrnfe_danfe_item_desc.
  REFRESH t_zbrnfe_danfe_fatura.
  REFRESH t_zbrnfe_danfe_dados_adic.
  REFRESH t_zbrnfe_danfe_cfop.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_CABECALHO
*&---------------------------------------------------------------------*
FORM f_ins_dados_cabecalho .

  g_sadr_branch-name1             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-xnome.
  g_sadr_branch-stras             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xlgr.
  g_sadr_branch-strs2             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xcpl.
  g_sadr_branch-hausn             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-nro.
  g_sadr_branch-ort02             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xbairro.
  g_sadr_branch-pstlz             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-cep.
  g_sadr_branch-ort01             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xmun.
  g_sadr_branch-telf1             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-fone.
  g_sadr_branch-regio             = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-uf.

  t_danfe_cabecalho-sadr          = g_sadr_branch.
  t_danfe_cabecalho-tipo          = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-tpnf. "Tipo (1 - Entrada; 2 - Saída)
  t_danfe_cabecalho-serie         = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-serie.
  t_danfe_cabecalho-numero        = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.
  t_danfe_cabecalho-inscest       = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  t_danfe_cabecalho-inscestsubst  = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-iest.
  t_danfe_cabecalho-cnpj          = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.

  IF wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient IS INITIAL.
    wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi.
  ENDIF.

  PERFORM f_formata_cgc       USING t_danfe_cabecalho-cnpj
                           CHANGING t_danfe_cabecalho-cnpj.

  t_danfe_cabecalho-chave         = wg_xml_sefaz-nfeproc-nfe-infnfe-a_id+3(44).
  t_danfe_cabecalho-protocolo     = wg_xml_sefaz-nfeproc-protnfe-infprot-nprot. "Protocolo de autorização

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                           CHANGING t_danfe_cabecalho-data_aut
                                    t_danfe_cabecalho-hora_aut.
*-----------------------------------------------------------------------
*** Dados de emissão
*-----------------------------------------------------------------------
  PERFORM f_get_data_utc      USING wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                           CHANGING t_danfe_cabecalho-dtemi.

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                           CHANGING t_danfe_cabecalho-dtmov
                                    t_danfe_cabecalho-hrmov.

*-----------------------------------------------------------------------
*** Dados Destinatário
*-----------------------------------------------------------------------
  t_danfe_cabecalho-dest_nome = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-xnome.

  IF     wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj IS NOT INITIAL.
    PERFORM f_formata_cgc     USING wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj
                           CHANGING t_danfe_cabecalho-dest_cnpj.
  ELSEIF wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cpf IS NOT INITIAL.
    t_danfe_cabecalho-dest_cnpj   = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cpf.
  ENDIF.

  t_danfe_cabecalho-dest_endereco = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xlgr.
  t_danfe_cabecalho-dest_bairro   = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xbairro.
  t_danfe_cabecalho-dest_cep      = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-cep.
  t_danfe_cabecalho-dest_cidade   = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xmun.

  IF wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-uf = 'EX'.
    CONCATENATE t_danfe_cabecalho-dest_cidade '-' wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xpais
           INTO t_danfe_cabecalho-dest_cidade SEPARATED BY space.
    TRANSLATE t_danfe_cabecalho-dest_cidade TO UPPER CASE.
  ENDIF.

  t_danfe_cabecalho-dest_fone     = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-fone.
  t_danfe_cabecalho-dest_uf       = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-uf.
  t_danfe_cabecalho-dest_insc_est = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-ie.

ENDFORM.                    " ZF_INS_DADOS_CABECALHO

*&---------------------------------------------------------------------*
* dados imposto
*----------------------------------------------------------------------*
FORM f_ins_dados_imposto.

  t_danfe_cabecalho-imp_baseicms         = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vbc.          "Imposto - Base Cálculo ICMS
  t_danfe_cabecalho-imp_valicms          = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vicms.        "Imposto - Valor do ICMS
  t_danfe_cabecalho-imp_valicmsdeson     = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vicmsdeson.   "Imposto - Valor do ICMS Desonerado
  t_danfe_cabecalho-imp_baseicmssubst    = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vbcst.        "Imposto - Base Cálculo ICMS Subst
  t_danfe_cabecalho-imp_valoricmssubst   = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vst.          "Imposto - Valor do ICMS Subst
  t_danfe_cabecalho-imp_valtot           = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vprod.        "Imposto - Valor Total dos Produtos
  t_danfe_cabecalho-imp_valfrete         = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vfrete.       "Imposto - Valor do Frete
  t_danfe_cabecalho-imp_valseguro        = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vseg.         "Imposto - Valor do Seguro
  t_danfe_cabecalho-imp_desconto         = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vdesc.        "Imposto - Valor do Desconto
  t_danfe_cabecalho-imp_outros           = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-voutro.       "Imposto - Outras despesas
  t_danfe_cabecalho-imp_valnota          = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vnf.          "Imposto - Valor Total NF-e
  t_danfe_cabecalho-imp_valipi           = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vipi.
  t_danfe_cabecalho-imp_valipidevol      = wg_xml_sefaz-nfeproc-nfe-infnfe-total-icmstot-vipidevol.

  IF t_danfe_cabecalho-imp_valipidevol > 0.
    t_danfe_cabecalho-imp_valipi         = t_danfe_cabecalho-imp_valipidevol.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* dados transporte
*----------------------------------------------------------------------*
FORM f_ins_dados_transporte.

  CASE wg_xml_sefaz-nfeproc-nfe-infnfe-transp-modfrete.
    WHEN '0'.
      t_danfe_cabecalho-trans_tipofrete = 'Emitente'.
    WHEN '1'.
      t_danfe_cabecalho-trans_tipofrete = 'Destinatário/Remetente'.
    WHEN '2'.
      t_danfe_cabecalho-trans_tipofrete = 'Terceiros'.
    WHEN '9'.
      t_danfe_cabecalho-trans_tipofrete = 'Sem Frete'.
  ENDCASE.

  READ TABLE wg_xml_sefaz-nfeproc-nfe-infnfe-transp-vol INTO DATA(wl_vol) INDEX 1.
  IF ( sy-subrc EQ 0 ) AND ( wg_xml_sefaz-nfeproc-nfe-infnfe-transp-vol[] IS NOT INITIAL ).
    t_danfe_cabecalho-trans_pesoliquido  = wl_vol-pesol.
    t_danfe_cabecalho-trans_pesobruto    = wl_vol-pesob.
    t_danfe_cabecalho-trans_qtde         = wl_vol-qvol.
    t_danfe_cabecalho-trans_marca        = wl_vol-marca.
    t_danfe_cabecalho-trans_numero       = wl_vol-nvol.
    t_danfe_cabecalho-trans_um_desc      = wl_vol-esp.
  ENDIF.

  t_danfe_cabecalho-trans_nome           = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xnome.
  t_danfe_cabecalho-trans_inscest        = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-ie.
  t_danfe_cabecalho-trans_uf2            = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-uf.
  t_danfe_cabecalho-trans_cidade         = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xmun.
  t_danfe_cabecalho-trans_endereco       = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xender.

  IF wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cnpj IS NOT INITIAL.
    t_danfe_cabecalho-trans_cnpjf        = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cnpj.
  ELSE.
    t_danfe_cabecalho-trans_cnpjf        = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cpf.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* dados itens
*----------------------------------------------------------------------*
FORM f_ins_dados_itens.

  DATA: lt_item_desc  TYPE TABLE OF zbrnfe_danfe_item_desc  WITH HEADER LINE,
        l_valor_dummy TYPE j_1bnfstx-base,
        vl_lote(6)    TYPE c VALUE 'Lote: '.

  LOOP AT wg_xml_sefaz-nfeproc-nfe-infnfe-det INTO DATA(wl_det_item).

    CLEAR t_zbrnfe_danfe_item.

    t_zbrnfe_danfe_item-num    = wl_det_item-a_nitem.
    t_zbrnfe_danfe_item-codigo = wl_det_item-prod-cprod.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = t_zbrnfe_danfe_item-codigo
      IMPORTING
        output = t_zbrnfe_danfe_item-codigo.

    t_zbrnfe_danfe_item-tipodesc = '3'.

*--------------------------------------------------------------------*
*   DESCRICAO  - Descrição do Produto
*--------------------------------------------------------------------*

    t_zbrnfe_danfe_item-descricao      = wl_det_item-prod-xprod.
    t_zbrnfe_danfe_item-infadprod      = wl_det_item-infadprod.

    CLEAR: t_zbrnfe_danfe_item_desc.
    t_zbrnfe_danfe_item_desc-num       = wl_det_item-a_nitem.
    t_zbrnfe_danfe_item_desc-linha     = 1.
    t_zbrnfe_danfe_item_desc-tipodesc  = '1'.
    t_zbrnfe_danfe_item_desc-descricao = t_zbrnfe_danfe_item-descricao.
    APPEND t_zbrnfe_danfe_item_desc.

    WRITE wl_det_item-prod-ncm        TO t_zbrnfe_danfe_item-ncm.

    t_zbrnfe_danfe_item-cfop           = wl_det_item-prod-cfop.
    t_zbrnfe_danfe_item-unidade        = wl_det_item-prod-ucom.
    t_zbrnfe_danfe_item-quantidade     = wl_det_item-prod-qcom.
    t_zbrnfe_danfe_item-qtde_char      = t_zbrnfe_danfe_item-quantidade.
    REPLACE '.' WITH ','            INTO t_zbrnfe_danfe_item-qtde_char.

    t_zbrnfe_danfe_item-und_trib       = wl_det_item-prod-utrib.
    t_zbrnfe_danfe_item-qtde_trib      = wl_det_item-prod-qtrib.
    t_zbrnfe_danfe_item-vlr_unitario   = wl_det_item-prod-vuncom.
    t_zbrnfe_danfe_item-vlr_total      = wl_det_item-prod-vprod.

    IF t_zbrnfe_danfe_cfop[] IS INITIAL.
      t_zbrnfe_danfe_cfop-cfop         = t_zbrnfe_danfe_item-cfop.
      t_zbrnfe_danfe_cfop-cfotxt       = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-natop.
      APPEND t_zbrnfe_danfe_cfop.
    ENDIF.

    IF wl_det_item-imposto-icms-icms00-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms00-orig && wl_det_item-imposto-icms-icms00-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms00-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms00-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms00-picms.
    ELSEIF wl_det_item-imposto-icms-icms10-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms10-orig && wl_det_item-imposto-icms-icms10-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms10-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms10-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms10-picms.
    ELSEIF wl_det_item-imposto-icms-icms20-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms20-orig && wl_det_item-imposto-icms-icms20-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms20-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms20-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms20-picms.
    ELSEIF wl_det_item-imposto-icms-icms30-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms30-orig && wl_det_item-imposto-icms-icms30-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms30-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms30-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms30-picms.
    ELSEIF wl_det_item-imposto-icms-icms40-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms40-orig && wl_det_item-imposto-icms-icms40-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms40-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms40-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms40-picms.
    ELSEIF wl_det_item-imposto-icms-icms51-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms51-orig && wl_det_item-imposto-icms-icms51-cst.

      CLEAR: t_danfe_cabecalho-imp_baseicms, t_danfe_cabecalho-imp_valicms.

    ELSEIF wl_det_item-imposto-icms-icms60-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms60-orig && wl_det_item-imposto-icms-icms60-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms60-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms60-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms60-picms.
    ELSEIF wl_det_item-imposto-icms-icms70-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms70-orig && wl_det_item-imposto-icms-icms70-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms70-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms70-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms70-picms.
    ELSEIF wl_det_item-imposto-icms-icms90-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-cst          = wl_det_item-imposto-icms-icms90-orig && wl_det_item-imposto-icms-icms90-cst.
      t_zbrnfe_danfe_item-vlr_baseicms = wl_det_item-imposto-icms-icms90-vbc.
      t_zbrnfe_danfe_item-vlr_icms     = wl_det_item-imposto-icms-icms90-vicms.
      t_zbrnfe_danfe_item-aliq_icms    = wl_det_item-imposto-icms-icms90-picms.
    ENDIF.

    IF wl_det_item-impostodevol-ipi-vipidevol IS NOT INITIAL.
      t_zbrnfe_danfe_item-vlr_ipi      = wl_det_item-impostodevol-ipi-vipidevol.
    ELSEIF wl_det_item-imposto-ipi-ipitrib-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-vlr_ipi      = wl_det_item-imposto-ipi-ipitrib-vipi.
      t_zbrnfe_danfe_item-aliq_ipi     = wl_det_item-imposto-ipi-ipitrib-pipi.
    ELSEIF wl_det_item-imposto-ipi-ipint-cst IS NOT INITIAL.
    ENDIF.

    APPEND t_zbrnfe_danfe_item.

  ENDLOOP.

  PERFORM f_formata_itens TABLES t_zbrnfe_danfe_item
                                 t_zbrnfe_danfe_item_desc.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  dados adicionais
*&---------------------------------------------------------------------*
FORM f_ins_dados_adicionais.

  t_zbrnfe_danfe_dados_adic-linha      = 1.
  t_zbrnfe_danfe_dados_adic-tipodesc   = '1'.
  t_zbrnfe_danfe_dados_adic-descricao  = wg_xml_sefaz-nfeproc-nfe-infnfe-infadic-infcpl.

  APPEND t_zbrnfe_danfe_dados_adic.

  t_danfe_cabecalho-reservado_fisco    = wg_xml_sefaz-nfeproc-nfe-infnfe-infadic-infadfisco.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATA_ITENS
*&---------------------------------------------------------------------*
FORM f_formata_itens TABLES  p_danfe_item STRUCTURE zbrnfe_danfe_item
                             p_item_desc  STRUCTURE zbrnfe_danfe_item_desc.

  DATA t_konv            TYPE STANDARD TABLE OF konv WITH HEADER LINE.
  DATA v_ipifrete        TYPE p DECIMALS 2.
  DATA v_ipifrete2       TYPE p DECIMALS 2 VALUE '0.00'.
  DATA v_vlr_unit        TYPE p DECIMALS 6.
  DATA v_txtipifrete(20) TYPE c.
  DATA lin_danfe_item TYPE i. "Numero de linhas

  LOOP AT p_danfe_item.
    WRITE p_danfe_item-quantidade TO p_danfe_item-quantidade_desc
      DECIMALS 3
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-qtde_trib TO p_danfe_item-qtde_trib_desc
     DECIMALS 4
     LEFT-JUSTIFIED
     NO-GAP.

    v_vlr_unit = p_danfe_item-vlr_unitario.
    WRITE v_vlr_unit TO p_danfe_item-vlr_unitario_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-vlr_total    TO p_danfe_item-vlr_total_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-vlr_baseicms TO p_danfe_item-vlr_baseicms_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-vlr_icms     TO p_danfe_item-vlr_icms_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-vlr_ipi      TO p_danfe_item-vlr_ipi_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-aliq_icms    TO p_danfe_item-aliq_icms_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    WRITE p_danfe_item-aliq_ipi     TO p_danfe_item-aliq_ipi_desc
      DECIMALS 2
      LEFT-JUSTIFIED
      NO-GAP.

    MODIFY p_danfe_item INDEX sy-tabix.
  ENDLOOP.


ENDFORM.                    " ZF_FORMATA_ITENS

*---------------------------------------------------------------------*
* Formata cgc
*----------------------------------------------------------------------*
FORM f_formata_cgc  USING    p_cgc
                    CHANGING p_cgc_format.

  DATA lc_cgc_aux TYPE pbr99_cgc.

  CHECK NOT p_cgc IS INITIAL.

  CHECK p_cgc > 0.

  lc_cgc_aux = p_cgc.

  CALL FUNCTION 'HR_BR_CHECK_CGC_FORMAT'
    EXPORTING
      cgc_number               = lc_cgc_aux
    IMPORTING
      cgc_number_formatted     = lc_cgc_aux
    EXCEPTIONS
      cgc_format_not_supported = 1
      cgc_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cgc_aux TO p_cgc_format.

ENDFORM.                    " ZF_FORMATA_CGC

*---------------------------------------------------------------------*
* data utc
*----------------------------------------------------------------------*
FORM f_get_data_hora_utc  USING p_data_hora
                       CHANGING c_data
                                c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  PERFORM f_get_data_utc USING p_data_hora
                      CHANGING c_data.

  PERFORM f_get_hora_utc USING p_data_hora
                      CHANGING c_hora.

ENDFORM.

*---------------------------------------------------------------------*
* data utc
*----------------------------------------------------------------------*
FORM f_get_data_utc USING p_data_hora
                 CHANGING c_data.

  CHECK p_data_hora IS NOT INITIAL.

  c_data = p_data_hora(4) &&
           p_data_hora+05(02) &&
           p_data_hora+08(02).

ENDFORM.

*---------------------------------------------------------------------*
* data utc
*----------------------------------------------------------------------*
FORM f_get_hora_utc USING p_data_hora
                 CHANGING c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  c_hora  = p_data_hora+11(02) &&
            p_data_hora+14(02) &&
            p_data_hora+17(02).

ENDFORM.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
