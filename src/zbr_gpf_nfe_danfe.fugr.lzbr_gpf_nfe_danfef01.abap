
FORM zf_insere_dados.

  PERFORM zf_limpa_variaveis.

  PERFORM zf_ins_dados_cabecalho. "OK
  PERFORM zf_ins_dados_fatura. "pendente
  PERFORM zf_ins_dados_imposto. "ok
  PERFORM zf_ins_dados_transporte."ok
  PERFORM zf_ins_dados_itens. "ok
  PERFORM zf_ins_dados_adicionais. "OK


ENDFORM.                    "seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  zf_chama_smart_form
*&---------------------------------------------------------------------*
* Chamada do Smart Form da DANFE
*----------------------------------------------------------------------*
FORM zf_chama_smart_form CHANGING p_document_output_info TYPE  ssfcrespd
                                  p_job_output_info      TYPE  ssfcrescl
                                  p_job_output_options   TYPE  ssfcresop.

  DATA: it_otf  TYPE TABLE OF ssfcrescl.

* Ordena as tabelas output
  SORT t_zbrnfe_danfe_item.
  SORT t_zbrnfe_danfe_item_desc.
  SORT t_zbrnfe_danfe_fatura.
  SORT t_zbrnfe_danfe_dados_adic.


*Busca nome da função de chamada do formulário
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZBRNFE_DANFE'
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

  CALL FUNCTION ls_funcname
    EXPORTING
      control_parameters   = g_control_parameters
      output_options       = g_output_options
      user_settings        = space
      i_cabecalho          = t_danfe_cabecalho
      "i_devolucao          = l_devolucao
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

  "append p_job_output_info to it_otf.



ENDFORM.                    "zf_chama_smart_form

*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATA_CGC
*&---------------------------------------------------------------------*
* Formata cgc
*----------------------------------------------------------------------*
FORM zf_formata_cgc  USING    p_cgc
                     CHANGING p_cgc_format.


  DATA lc_cgc_aux TYPE pbr99_cgc.

  "CLEAR p_cgc_format.

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


*&---------------------------------------------------------------------*
*&      Form  zf_formata_cpf
*&---------------------------------------------------------------------*
* Formata CPF
*----------------------------------------------------------------------*
FORM zf_formata_cpf USING    p_cpf
                    CHANGING p_cpf_format.

  DATA lc_cpf_aux TYPE pbr99_cpf.

  CLEAR p_cpf_format.

  CHECK NOT p_cpf IS INITIAL.

  lc_cpf_aux = p_cpf.

  CALL FUNCTION 'HR_BR_CHECK_CPF_FORMAT'
    EXPORTING
      cpf_number               = lc_cpf_aux
    IMPORTING
      cpf_number_formatted     = lc_cpf_aux
    EXCEPTIONS
      cpf_format_not_supported = 1
      cpf_check_digit          = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lc_cpf_aux TO p_cpf_format.

ENDFORM.                    "zf_formata_cpf
*&---------------------------------------------------------------------*
*&      Form  ZF_ACUMULA_VALORES
*&---------------------------------------------------------------------*
* Acumula valores
*----------------------------------------------------------------------*
FORM zf_acumula_valores
  TABLES p_j_1bnflin STRUCTURE j_1bnflin
         p_j_1bnfstx STRUCTURE j_1bnfstx
         p_j_1bnfnad STRUCTURE j_1bnfnad
  USING  p_j_1bnfdoc TYPE j_1bnfdoc
  CHANGING p_j_1bindocdx TYPE j_1bindocdx.

  CONSTANTS c_prefixo(14)    VALUE 'P_J_1BINDOCDX-'.
  CONSTANTS c_sufixo_base(4) VALUE 'BASE'.
  CONSTANTS c_sufixo_val(3)  VALUE 'VAL'.

  FIELD-SYMBOLS <f_base> TYPE any.
  FIELD-SYMBOLS <f_val>  TYPE any.

  DATA l_aux(50).
  DATA l_valor       TYPE j_1bnflin-nfnet.
  DATA l_vlr_unit(8) TYPE c. "TYPE j_1bnflin-netpr.
  DATA l_vlr_desc    TYPE j_1bnflin-netdis.
  DATA l_vlr_frete   TYPE j_1bnflin-netfre.
  DATA l_vlr_seguro  TYPE j_1bnflin-netins.
  DATA l_vlr_desp    TYPE j_1bnflin-netoth.
  DATA l_vlr_total   TYPE j_1bnflin-netwrt.

* Valdeci/FH - 11.09.2013 - INC0065015 <<
  DATA l_base        TYPE j_1bbase.
  DATA l_ii_val      TYPE j_1bnfstx-taxval.
* Valdeci/FH - 11.09.2013 - INC0065015 >>

  CLEAR: p_j_1bindocdx, g_netdis.

* Verifica e aplica reducao de base
  PERFORM reducao_base
    TABLES  p_j_1bnfnad
            p_j_1bnflin
            p_j_1bnfstx
    USING   p_j_1bnfdoc.

  LOOP AT p_j_1bnflin.

    IF NOT p_j_1bnflin-nfnet IS INITIAL.
      l_vlr_unit = p_j_1bnflin-nfpri.
      l_valor = p_j_1bnflin-nfnet.
*      l_vlr_desc = p_j_1bnflin-nfdis.
      l_vlr_frete = p_j_1bnflin-nffre.
      l_vlr_seguro = p_j_1bnflin-nfins.
      l_vlr_desp = p_j_1bnflin-nfoth.
      l_vlr_total = p_j_1bnflin-nfnett.
    ELSE.
*      l_vlr_unit = p_j_1bnflin-netpr.
*     Preço unitário.
      IF NOT p_j_1bnflin-menge IS INITIAL AND
         NOT p_j_1bnflin-netwr IS INITIAL.
        l_vlr_unit = p_j_1bnflin-netwr / p_j_1bnflin-menge.
      ENDIF.

      l_valor = p_j_1bnflin-netwr.
*      l_vlr_desc = p_j_1bnflin-netdis.
      l_vlr_frete = p_j_1bnflin-netfre.
      l_vlr_seguro = p_j_1bnflin-netins.
      l_vlr_desp = p_j_1bnflin-netoth.
      l_vlr_total = p_j_1bnflin-netwrt.
    ENDIF.

    l_vlr_desc = p_j_1bnflin-nfdis.

*   NFNETT     - valor líquido/frete/seguro/despesas/desconto
*    ADD t_j_1bnflin-nfnett TO t_j_1bindocdx-nfnett.
    p_j_1bindocdx-nfnett = p_j_1bindocdx-nfnett +
      l_valor +
      l_vlr_frete +
      l_vlr_seguro +
      l_vlr_desp -
      l_vlr_desc.

*   NFNET     - Valor líquido incluindo impostos
    ADD l_valor TO p_j_1bindocdx-nfnet.
*   NFFRE     - Valor do frete incluíndo imposto
    ADD l_vlr_frete TO p_j_1bindocdx-nffre.
*   NFINS     - Montante do seguro incluíndo imposto
    ADD l_vlr_seguro TO p_j_1bindocdx-nfins.
*   NFOTH     - Despesas incluindo impostos
    ADD l_vlr_desp TO p_j_1bindocdx-nfoth.
*   NFDIS     - Montante do desconto incl.impostos
    ADD l_vlr_desc TO p_j_1bindocdx-nfdis.
*   Total de desconto incondicional
    ADD p_j_1bnflin-netdis TO g_netdis.

*   Impostos
    LOOP AT p_j_1bnfstx
      WHERE docnum = t_j_1bnflin-docnum
      AND   itmnum = t_j_1bnflin-itmnum.

      CLEAR t_j_1baj.
      READ TABLE t_j_1baj WITH KEY taxtyp = p_j_1bnfstx-taxtyp
        BINARY SEARCH.

*     Descobre dinamicamente a qual imposto se refere
      CONCATENATE c_prefixo t_j_1baj-taxgrp c_sufixo_base
        INTO l_aux.
      ASSIGN (l_aux) TO <f_base>.

      CONCATENATE c_prefixo t_j_1baj-taxgrp c_sufixo_val
        INTO l_aux.
      ASSIGN (l_aux) TO <f_val>.

      IF sy-subrc = 0.
        PERFORM zf_adiciona_imposto
          USING   p_j_1bnfstx
         CHANGING <f_base> <f_val>.
      ENDIF.

* Valdeci/FH - 11.09.2013 - INC0065015 <<
* Incluir imposto de importação no total da nota
      IF t_j_1baj-taxgrp EQ 'II'.
        PERFORM zf_adiciona_imposto
          USING   p_j_1bnfstx
         CHANGING l_base l_ii_val.
      ENDIF.
* Valdeci/FH - 11.09.2013 - INC0065015 >>
    ENDLOOP.

  ENDLOOP.

  ADD p_j_1bindocdx-ipival TO p_j_1bindocdx-nfnett.
  ADD g_netdis TO p_j_1bindocdx-nfnett.

* Valdeci/FH - 11.09.2013 - INC0065015 <<
* Incluir imposto de importação no total da nota
  ADD l_ii_val TO p_j_1bindocdx-nfnett.
* Valdeci/FH - 11.09.2013 - INC0065015 >>

  IF p_j_1bnfdoc-nftype = '11'.
    p_j_1bindocdx-nfoth = p_j_1bindocdx-nfoth - p_j_1bindocdx-icmsval.
  ENDIF.

ENDFORM.                    " ZF_ACUMULA_VALORES
*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_DADOS
*&---------------------------------------------------------------------*
* Carrega dados da NF
*----------------------------------------------------------------------*
FORM zf_carrega_dados USING p_docnum TYPE j_1bnfdoc-docnum.

  CLEAR:  t_j_1bnfdoc,
          t_j_1bnfe_active,
          t_j_1bbranch,
          g_sadr_branch,
          t_parc_recebedor,
          t_doc_ref,
          g_netdis.

  REFRESH: gt_bkpf[],
           gt_bseg[],
           gt_awkey[],
           gt_vbrp[],
           t_j_1bnfnad[],
           t_j_1bnflin[],
           t_j_1bnfstx[],
           t_j_1bnfftx[],
           t_j_1bnfref[],
           t_parceiro[].

  CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
    EXPORTING
      doc_number         = p_docnum
    IMPORTING
      doc_header         = t_j_1bnfdoc
    TABLES
      doc_partner        = t_j_1bnfnad
      doc_item           = t_j_1bnflin
      doc_item_tax       = t_j_1bnfstx
      doc_header_msg     = t_j_1bnfftx
      doc_refer_msg      = t_j_1bnfref
*     DOC_OT_PARTNER     =
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Documento de referencia
  IF NOT t_j_1bnfdoc-docref IS INITIAL.

    SELECT SINGLE *
      FROM j_1bnfdoc
      INTO t_doc_ref
      WHERE docnum = t_j_1bnfdoc-docref.

    SELECT SINGLE *
      FROM j_1bnfe_active
      INTO t_nfe_ref
      WHERE docnum = t_j_1bnfdoc-docref.

  ENDIF.

* Nota fiscal eletrônica: status atual
  SELECT SINGLE *
    FROM j_1bnfe_active
    INTO t_j_1bnfe_active
   WHERE docnum EQ p_docnum.

* Dados do Centro
  SELECT SINGLE *
    FROM j_1bbranch
    INTO t_j_1bbranch
   WHERE bukrs  EQ t_j_1bnfdoc-bukrs
     AND branch EQ t_j_1bnfdoc-branch.

* Carrega dados do endereco da filial
  PERFORM zf_carrega_endereco
    USING t_j_1bbranch-adrnr
    CHANGING g_sadr_branch
             g_addr_branch.

* Parceiro Recebedor
  CALL FUNCTION 'J_1B_NF_PARTNER_READ'
    EXPORTING
      partner_type           = t_j_1bnfdoc-partyp
      partner_id             = t_j_1bnfdoc-parid
      doc_number             = t_j_1bnfdoc-docnum
      partner_function       = t_j_1bnfdoc-parvw
*     OBJ_ITEM               =
      read_address           = 'X'
    IMPORTING
      parnad                 = t_parc_recebedor
    EXCEPTIONS
      partner_not_found      = 1
      partner_type_not_found = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Outros parceiros Parceiros
  LOOP AT t_j_1bnfnad.
    CLEAR t_parceiro.

    MOVE-CORRESPONDING t_j_1bnfnad TO t_parceiro.

    CALL FUNCTION 'J_1B_NF_PARTNER_READ'
      EXPORTING
        partner_type           = t_j_1bnfnad-partyp
        partner_id             = t_j_1bnfnad-parid
        doc_number             = t_j_1bnfnad-docnum
        partner_function       = t_j_1bnfnad-parvw
*       OBJ_ITEM               =
        read_address           = 'X'
      IMPORTING
        parnad                 = t_parceiro-dados
      EXCEPTIONS
        partner_not_found      = 1
        partner_type_not_found = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    APPEND t_parceiro.

  ENDLOOP.

* Carrega grupos de impostos para acumulacao.
  PERFORM zf_carrega_conf_imposto.

  SORT t_j_1baj BY taxtyp.

  IF NOT t_j_1bnflin[] IS INITIAL.

    REFRESH gt_awkey.

    LOOP AT t_j_1bnflin.
      CLEAR gt_awkey.
      gt_awkey-awkey = t_j_1bnflin-refkey.
      gt_awkey-vbeln = t_j_1bnflin-refkey.
      COLLECT gt_awkey.
    ENDLOOP.

    SELECT *
      FROM bkpf
      INTO TABLE gt_bkpf
      FOR ALL ENTRIES IN gt_awkey
     WHERE awtyp = 'VBRK'
       AND awkey = gt_awkey-awkey.

  ENDIF.

  IF NOT gt_bkpf[] IS INITIAL.


* ---> S4 Migration - 06/07/2023 - DG
*    SELECT *
*      FROM bseg
*      INTO TABLE gt_bseg
*      FOR ALL ENTRIES IN gt_bkpf
*    WHERE bukrs EQ gt_bkpf-bukrs
*    AND   belnr EQ gt_bkpf-belnr
*    AND   gjahr EQ gt_bkpf-gjahr
*    AND   koart EQ 'D'.

    DATA lt_bseg_aux TYPE TABLE OF bseg.

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = gt_bkpf[]
        i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
      IMPORTING
        et_bseg            = lt_bseg_aux
      EXCEPTIONS
        not_found          = 1.

    delete lt_bseg_aux where koart ne 'D'.

    IF sy-subrc = 0 AND lines( lt_bseg_aux ) > 0.
      MOVE-CORRESPONDING lt_bseg_aux TO gt_bseg[].
      sy-dbcnt = lines( lt_bseg_aux ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.
* <--- S4 Migration - 06/07/2023 - DG

    SORT gt_bseg.

    DELETE ADJACENT DUPLICATES FROM gt_bseg.
  ENDIF.

  IF NOT gt_awkey[] IS INITIAL.
* Pedidos de referencia
    SELECT vbeln posnr aubel aupos vgbel
      INTO TABLE gt_vbrp
      FROM vbrp
      FOR ALL ENTRIES IN gt_awkey
      WHERE vbeln = gt_awkey-vbeln.

    SORT gt_vbrp.

    DELETE ADJACENT DUPLICATES FROM gt_vbrp.

    SELECT kunnr
      INTO TABLE gt_vbpa
      FROM vbpa
      FOR ALL ENTRIES IN gt_awkey
      WHERE  vbeln EQ gt_awkey-vbeln
        AND  posnr EQ 0
        AND  parvw EQ 'RG'.

    SORT gt_vbpa.

    DELETE ADJACENT DUPLICATES FROM gt_vbpa.
  ENDIF.

  LOOP AT t_j_1bnflin.
    PERFORM zf_carrega_pis_cofins
      TABLES  t_j_1bnfstx
      USING t_j_1bnflin.
  ENDLOOP.

  PERFORM zf_carrega_transporte_frete.

  PERFORM zf_verifica_contingencia.
ENDFORM.                    " ZF_CARREGA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_IMPOSTO
*&---------------------------------------------------------------------*
* Ajusta o valor do imposto
*----------------------------------------------------------------------*
FORM zf_adiciona_imposto  USING    p_j_1bnfstx TYPE j_1bnfstx
                          CHANGING p_impbase
                                   p_impval.
  DATA l_base TYPE j_1bbase.

  IF t_j_1bnfstx-rate IS NOT INITIAL.

*   Valor base
    PERFORM fill_base USING  p_j_1bnfstx-base
                             p_j_1bnfstx-othbas
                             p_j_1bnfstx-excbas
                       CHANGING l_base.
    ADD l_base TO p_impbase.
* Valor Imposto
    ADD p_j_1bnfstx-taxval TO p_impval.

  ENDIF.

ENDFORM.                    " ZF_ADICIONA_IMPOSTO

*&---------------------------------------------------------------------*
*&      Form  ZF_DATA_VENCTO
*&---------------------------------------------------------------------*
* Importado do programa ZBRJ_1BNFPR, perform fatura
*----------------------------------------------------------------------*
FORM zf_data_vencto CHANGING p_data TYPE sy-datum.

*-----------------------Determinação da data de vencimento-------------*
  DATA data_base TYPE sy-datum.
  DATA l_t052 TYPE t052.
  DATA condtyp(2).

* Move a data base de pagamento para variável interna
  data_base = t_j_1bnfdoc-pstdat.

* Determina qual será a condição de pagamento
* 01 -> S/ condição pagto e sem dias a pagar na cotação
* 02 -> S/ condição pagto e com dias a pagar na cotação
* 03 -> Condição pagto com data fixa
* 04 -> Condição pagto com dias a pagar
* 05 -> Condição pagto com parcelas

* 01 -> S/ condição pagto e sem dias a pagar
*    if vbrk-zterm is initial and p_dia is initial.
*      condtyp = '01'.
*    endif.

* 02 -> S/ condição pagto e com dias a pagar
*    if vbrk-zterm is initial  and not p_dia is initial.
*      condtyp = '02'.
*    endif.

* Condição pagto foi informada
  IF NOT t_j_1bnfdoc-zterm IS INITIAL.
    CLEAR l_t052.
    SELECT SINGLE xsplt zfael ztag1
      INTO (l_t052-xsplt, l_t052-zfael, l_t052-ztag1)
      FROM t052
     WHERE zterm EQ t_j_1bnfdoc-zterm
       AND ztagg EQ '00'.

    IF sy-subrc EQ 0.
* 03 -> Condição pagto com data fixa
      IF NOT l_t052-zfael IS INITIAL.
        condtyp = '03'.
      ENDIF.
* 04 -> Condição pagto com dias a pagar
      IF NOT l_t052-ztag1 IS INITIAL.
        condtyp = '04'.
      ENDIF.
* 05 -> Condição pagto com parcelas
      IF NOT l_t052-xsplt IS INITIAL.
        condtyp = '05'.
      ENDIF.
    ENDIF.                             "sy-subrc da t052
  ENDIF.                               "if de existência da condição

  CASE condtyp.

    WHEN '01'.      "01 -> S/ condição pagto e sem dias a pagar
*          datavenc = data_base.        "Data base
*          perform data_vencto.

    WHEN '02'.      "02 -> S/ condição pagto e com dias a pagar
*          datavenc = data_base.        " + p_dia
*          perform data_vencto.

    WHEN '03'.                     "03 -> Condição pagto com data fixa

      CALL FUNCTION 'HR_BR_ADD_MONTH_TO_DATE'
        EXPORTING
          dmm_datin = data_base
          dmm_count = '1'
          dmm_oper  = '+'
          dmm_pos   = ''
        IMPORTING
          dmm_daout = p_data
        EXCEPTIONS
          unknown   = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*      p_data = data_base.
*      IF p_data+6(2) GE l_t052-zfael.  "Soma um mês
*        p_data+4(2) = p_data+4(2) + 1.
*      ENDIF.
      p_data+6(2) = l_t052-zfael.      "Define o dia

    WHEN '04'.                  "04 -> Condição pagto com dias a pagar
      p_data = data_base.
      p_data = p_data + l_t052-ztag1.

    WHEN '05'.                      "05 -> Condição pagto com parcelas
      p_data = data_base.
  ENDCASE.

ENDFORM.                    "ZF_DATA_VENCTO
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_CABECALHO
*&---------------------------------------------------------------------*
FORM zf_ins_dados_cabecalho .


*-----------------------------------------------------------------------
*** Dados do cabeçalho
*-----------------------------------------------------------------------


* ADRNR - Endereço
  g_sadr_branch-name1 = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-xnome.
  g_sadr_branch-stras = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xlgr.
  g_sadr_branch-strs2 = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xcpl.
  g_sadr_branch-hausn = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-nro.
  g_sadr_branch-ort02 = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xbairro.
  g_sadr_branch-pstlz = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-cep.
  g_sadr_branch-ort01 = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-xmun.
  g_sadr_branch-telf1 = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-fone.
  g_sadr_branch-regio = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-enderemit-uf.

  t_danfe_cabecalho-sadr = g_sadr_branch.

  t_danfe_cabecalho-tipo          = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-tpnf. "Tipo (1 - Entrada; 2 - Saída)
  t_danfe_cabecalho-serie         = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-serie.
  t_danfe_cabecalho-numero        = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-nnf.
  t_danfe_cabecalho-inscest       = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-ie.
  t_danfe_cabecalho-inscestsubst  = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-iest.
  t_danfe_cabecalho-cnpj          = wg_xml_sefaz-nfeproc-nfe-infnfe-emit-cnpj.

  IF wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient IS INITIAL.
    wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi.
  ENDIF.


  PERFORM zf_formata_cgc USING t_danfe_cabecalho-cnpj CHANGING t_danfe_cabecalho-cnpj.

  t_danfe_cabecalho-chave         = wg_xml_sefaz-nfeproc-nfe-infnfe-a_id+3(44).
  t_danfe_cabecalho-protocolo     = wg_xml_sefaz-nfeproc-protnfe-infprot-nprot. "Protocolo de autorização

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-nfeproc-protnfe-infprot-dhrecbto
                           CHANGING t_danfe_cabecalho-data_aut
                                    t_danfe_cabecalho-hora_aut.
*-----------------------------------------------------------------------
*** Dados de emissão
*-----------------------------------------------------------------------

  PERFORM f_get_data_utc USING wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhemi
                      CHANGING t_danfe_cabecalho-dtemi.

  PERFORM f_get_data_hora_utc USING wg_xml_sefaz-nfeproc-nfe-infnfe-ide-dhsaient
                           CHANGING t_danfe_cabecalho-dtmov
                                    t_danfe_cabecalho-hrmov.

*-----------------------------------------------------------------------
*** Dados Destinatário
*-----------------------------------------------------------------------

  "DEST_NOME - Destinatário - Nome/Razão Social
  t_danfe_cabecalho-dest_nome = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-xnome.

  "DEST_CNPJ - Destinatário - CNPJ/CPF (Formatado)
  IF wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj IS NOT INITIAL.
    PERFORM zf_formata_cgc USING wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cnpj CHANGING t_danfe_cabecalho-dest_cnpj.
  ELSEIF wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cpf IS NOT INITIAL.
    t_danfe_cabecalho-dest_cnpj = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-cpf.
  ENDIF.

* DEST_ENDERECO - Destinatário - Endereço
  t_danfe_cabecalho-dest_endereco = |{ wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xlgr }, { wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-nro }|.

* DEST_BAIRRO - Destinatário - Bairro/Distrito
  t_danfe_cabecalho-dest_bairro = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xbairro.

* DEST_CEP - Destinatário - CEP
  t_danfe_cabecalho-dest_cep = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-cep.

* DEST_CIDADE - Destinatário - Cidade
  t_danfe_cabecalho-dest_cidade = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xmun.

* Incluir o nome do pais quando for estrangeiro
  IF wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-uf = 'EX'.
    CONCATENATE t_danfe_cabecalho-dest_cidade '-' wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-xpais
           INTO t_danfe_cabecalho-dest_cidade SEPARATED BY space.

    TRANSLATE t_danfe_cabecalho-dest_cidade TO UPPER CASE.
  ENDIF.

* DEST_FONE - Destinatário - Fone/Fax (formatado)
  t_danfe_cabecalho-dest_fone = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-fone.

* DEST_UF - Destinatário - UF
  t_danfe_cabecalho-dest_uf = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-enderdest-uf.

* DEST_INSC_EST - Destinatário - Inscrição Estadual
  t_danfe_cabecalho-dest_insc_est = wg_xml_sefaz-nfeproc-nfe-infnfe-dest-ie.



ENDFORM.                    " ZF_INS_DADOS_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_FATURA
*&---------------------------------------------------------------------*
FORM zf_ins_dados_fatura .
**----------------------------------------------------------------------
**** Fatura
**----------------------------------------------------------------------
*  DATA T_SPELL  TYPE SPELL.
*  DATA L_NUM(2) TYPE N.
*  DATA L_VBELV  TYPE VBFA-VBELV.
*  DATA LV_AUART TYPE VBAK-AUART.
*  DATA VL_COUNT_VALORES TYPE I.
*
*  CLEAR L_NUM.
*
*  SELECT VBELV
*    FROM VBFA
*    INTO (L_VBELV)
*      UP TO 1 ROWS
*   WHERE VBELN = T_J_1BNFLIN-REFKEY
*     AND POSNN = T_J_1BNFLIN-REFITM.
*  ENDSELECT.
*
*  IF SY-SUBRC = 0.
*    SELECT SINGLE AUART
*      FROM VBAK
*      INTO LV_AUART
*     WHERE VBELN = L_VBELV.
*  ENDIF.
*
*  LOOP AT GT_BSEG.
*    ADD 1 TO VL_COUNT_VALORES.
*
*    IF GT_BSEG-DMBTR IS NOT INITIAL.
*
*      ADD 1 TO L_NUM.
*
*      CLEAR T_ZBRNFE_DANFE_FATURA.
*
*      CONCATENATE T_J_1BNFDOC-NFENUM "gt_bseg-belnr
*                  '-'
*                  GT_BSEG-BUZEI
*        INTO T_ZBRNFE_DANFE_FATURA-FAT_NRO.
*
*      CALL FUNCTION 'J_1B_FI_NETDUE'
*        EXPORTING
*          ZFBDT   = GT_BSEG-ZFBDT
*          ZBD1T   = GT_BSEG-ZBD1T
*          ZBD2T   = GT_BSEG-ZBD2T
*          ZBD3T   = GT_BSEG-ZBD3T
*        IMPORTING
*          DUEDATE = T_ZBRNFE_DANFE_FATURA-FAT_VENCTO.
*
*      T_ZBRNFE_DANFE_FATURA-FAT_VALOR = GT_BSEG-DMBTR.
*
*      T_ZBRNFE_DANFE_FATURA-FAT_MOEDA = GT_BSEG-PSWSL.
*
*      IF VL_COUNT_VALORES = 1.
*        CALL FUNCTION 'SPELL_AMOUNT'
*          EXPORTING
*            AMOUNT    = T_ZBRNFE_DANFE_FATURA-FAT_VALOR
*            CURRENCY  = T_ZBRNFE_DANFE_FATURA-FAT_MOEDA
**           FILLER    = ' '
*            LANGUAGE  = SY-LANGU
*          IMPORTING
*            IN_WORDS  = T_SPELL
*          EXCEPTIONS
*            NOT_FOUND = 1
*            TOO_LARGE = 2
*            OTHERS    = 3.
*
*        IF SY-SUBRC <> 0.
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
*
** FAT_VALOREXT - Fatura - Valor por extenso
*        IF T_SPELL-NUMBER = '1'.
*          CONCATENATE T_SPELL-WORD 'REAL'
*            INTO T_ZBRNFE_DANFE_FATURA-FAT_VALOREXT
*            SEPARATED BY SPACE.
*
*        ELSE.
*          CONCATENATE T_SPELL-WORD 'REAIS'
*            INTO T_ZBRNFE_DANFE_FATURA-FAT_VALOREXT
*            SEPARATED BY SPACE.
*
*        ENDIF.
*
*        IF NOT T_SPELL-DECIMAL IS INITIAL.
*          CONCATENATE
*            T_ZBRNFE_DANFE_FATURA-FAT_VALOREXT
*            'E'
*            T_SPELL-DECWORD
*            'CENTAVOS'
*            INTO T_ZBRNFE_DANFE_FATURA-FAT_VALOREXT
*            SEPARATED BY SPACE.
*        ENDIF.
*
*      ENDIF.
*
*      APPEND T_ZBRNFE_DANFE_FATURA.
*      CLEAR T_ZBRNFE_DANFE_FATURA-FAT_VALOREXT.
*
*    ENDIF.
*  ENDLOOP.

  LOOP AT wg_xml_sefaz-nfeproc-nfe-infnfe-cobr-dup INTO DATA(wl_dup_item).

    t_zbrnfe_danfe_fatura-fat_nro        =     wl_dup_item-ndup.
    t_zbrnfe_danfe_fatura-fat_vencto  =     wl_dup_item-dvenc.
    t_zbrnfe_danfe_fatura-fat_valor     =    wl_dup_item-vdup.

    APPEND t_zbrnfe_danfe_fatura.

  ENDLOOP.
ENDFORM.                    " ZF_INS_DADOS_FATURA
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_IMPOSTO
*&---------------------------------------------------------------------*
FORM zf_ins_dados_imposto .

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
    t_danfe_cabecalho-imp_valipi = t_danfe_cabecalho-imp_valipidevol.
  ENDIF.

ENDFORM.                    " ZF_INS_DADOS_IMPOSTO
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_TRANSPORTE
*&---------------------------------------------------------------------*
FORM zf_ins_dados_transporte .

  CASE wg_xml_sefaz-nfeproc-nfe-infnfe-transp-modfrete.
    WHEN '0'.
      t_danfe_cabecalho-trans_tipofrete = 'Remetente(CIF)'.
    WHEN '1'.
      t_danfe_cabecalho-trans_tipofrete = 'Destinatário(FOB)'.
    WHEN '2'.
      t_danfe_cabecalho-trans_tipofrete = 'Terceiros'.
    WHEN '3'.
      t_danfe_cabecalho-trans_tipofrete = 'Próprio-Remetente'.
    WHEN '4'.
      t_danfe_cabecalho-trans_tipofrete = 'Próprio-Destinatário'.
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

  t_danfe_cabecalho-trans_nome         = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xnome.
  t_danfe_cabecalho-trans_inscest      = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-ie.
  t_danfe_cabecalho-trans_uf2          = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-uf.
  t_danfe_cabecalho-trans_cidade       = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xmun.
  t_danfe_cabecalho-trans_endereco     = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-xender.

  IF wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cnpj IS NOT INITIAL.
    t_danfe_cabecalho-trans_cnpjf        = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cnpj.
  ELSE.
    t_danfe_cabecalho-trans_cnpjf        = wg_xml_sefaz-nfeproc-nfe-infnfe-transp-transporta-cpf.
  ENDIF.

ENDFORM.                    " ZF_INS_DADOS_TRANSPORTE
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_ITENS
*&---------------------------------------------------------------------*
FORM zf_ins_dados_itens .
*----------------------------------------------------------------------*
*** Itens da Nota
*----------------------------------------------------------------------*

*--------------------------------------------------------------------*
* TABLES
*--------------------------------------------------------------------*
  DATA lt_item_desc TYPE TABLE OF zbrnfe_danfe_item_desc
                    WITH HEADER LINE.

  "AD.MAGGI"
*  DATA tl_flapu     TYPE STANDARD TABLE OF flapu_types
*                    WITH HEADER LINE.

*--------------------------------------------------------------------*
* VARIÁVEIS
*--------------------------------------------------------------------*
  DATA l_valor_dummy TYPE j_1bnfstx-base.
  DATA vl_lote(6)   TYPE c VALUE 'Lote: '.

  LOOP AT wg_xml_sefaz-nfeproc-nfe-infnfe-det INTO DATA(wl_det_item).

    CLEAR t_zbrnfe_danfe_item.

*   NUM - Sequencial do item (chave)
    t_zbrnfe_danfe_item-num = wl_det_item-a_nitem.
*   CODIGO - Código do Produto
    t_zbrnfe_danfe_item-codigo = wl_det_item-prod-cprod.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = t_zbrnfe_danfe_item-codigo
      IMPORTING
        output = t_zbrnfe_danfe_item-codigo.

*   TIPODESC - Tp.Desc (1: Simples; 2: Elemento Texto; 3: Mult. Linhas)
    t_zbrnfe_danfe_item-tipodesc = '3'.

*--------------------------------------------------------------------*
*   DESCRICAO  - Descrição do Produto
*--------------------------------------------------------------------*

    t_zbrnfe_danfe_item-descricao = wl_det_item-prod-xprod.
    t_zbrnfe_danfe_item-infadprod = wl_det_item-infadprod.

    CLEAR: t_zbrnfe_danfe_item_desc.
    t_zbrnfe_danfe_item_desc-num       = wl_det_item-a_nitem.
    t_zbrnfe_danfe_item_desc-linha     = 1.
    t_zbrnfe_danfe_item_desc-tipodesc  = '1'.
    t_zbrnfe_danfe_item_desc-descricao = t_zbrnfe_danfe_item-descricao.
    APPEND t_zbrnfe_danfe_item_desc.

*   Descricoes
*    PERFORM zf_dados_itens_desc
*     TABLES lt_item_desc t_j_1bnfstx
*      USING t_j_1bnfdoc t_j_1bnflin.
*    APPEND LINES OF lt_item_desc TO t_zbrnfe_danfe_item_desc.

*   NCM  - NCM
    WRITE wl_det_item-prod-ncm TO t_zbrnfe_danfe_item-ncm.


*   CFOP - CFOP
    t_zbrnfe_danfe_item-cfop = wl_det_item-prod-cfop.

*   UNIDADE - Unidade de Medida - Comercializada
    t_zbrnfe_danfe_item-unidade = wl_det_item-prod-ucom.

*   QUANTIDADE - Quantidade - Comercializada
    t_zbrnfe_danfe_item-quantidade = wl_det_item-prod-qcom.

    t_zbrnfe_danfe_item-qtde_char = t_zbrnfe_danfe_item-quantidade.
    REPLACE '.' WITH ',' INTO t_zbrnfe_danfe_item-qtde_char.


    "UND/QTDE Trib.
    t_zbrnfe_danfe_item-und_trib  = wl_det_item-prod-utrib.
    t_zbrnfe_danfe_item-qtde_trib = wl_det_item-prod-qtrib.

*   VLR_UNITARIO - Valor Unitário
    t_zbrnfe_danfe_item-vlr_unitario = wl_det_item-prod-vuncom.

*   VLR_TOTAL - Valor Total
    t_zbrnfe_danfe_item-vlr_total = wl_det_item-prod-vprod.

    "CFOP
    IF t_zbrnfe_danfe_cfop[] IS INITIAL.
      t_zbrnfe_danfe_cfop-cfop   = t_zbrnfe_danfe_item-cfop.
      t_zbrnfe_danfe_cfop-cfotxt = wg_xml_sefaz-nfeproc-nfe-infnfe-ide-natop.
      APPEND t_zbrnfe_danfe_cfop.
    ENDIF.

    "ICMS Item
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

      IF t_danfe_cabecalho-imp_valicms EQ 0.
        CLEAR: t_danfe_cabecalho-imp_baseicms, t_danfe_cabecalho-imp_valicms.
      ENDIF.

      "T_ZBRNFE_DANFE_ITEM-VLR_BASEICMS = WL_DET_ITEM-IMPOSTO-ICMS-ICMS51-VBC.
      "T_ZBRNFE_DANFE_ITEM-VLR_ICMS     = WL_DET_ITEM-IMPOSTO-ICMS-ICMS51-VICMS.
      "T_ZBRNFE_DANFE_ITEM-ALIQ_ICMS    = WL_DET_ITEM-IMPOSTO-ICMS-ICMS51-PICMS.
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

    "IPI
    IF wl_det_item-impostodevol-ipi-vipidevol IS NOT INITIAL.
      t_zbrnfe_danfe_item-vlr_ipi    = wl_det_item-impostodevol-ipi-vipidevol.
    ELSEIF wl_det_item-imposto-ipi-ipitrib-cst IS NOT INITIAL.
      t_zbrnfe_danfe_item-vlr_ipi    = wl_det_item-imposto-ipi-ipitrib-vipi.
      t_zbrnfe_danfe_item-aliq_ipi   = wl_det_item-imposto-ipi-ipitrib-pipi.
    ELSEIF wl_det_item-imposto-ipi-ipint-cst IS NOT INITIAL.
    ENDIF.

    APPEND t_zbrnfe_danfe_item.

  ENDLOOP.

  PERFORM zf_formata_itens TABLES t_zbrnfe_danfe_item
                                  t_zbrnfe_danfe_item_desc.


ENDFORM.                    " ZF_INS_DADOS_ITENS

*&---------------------------------------------------------------------*
*&      Form  ZF_INS_DADOS_ADICIONAIS
*&---------------------------------------------------------------------*
FORM zf_ins_dados_adicionais .


  t_zbrnfe_danfe_dados_adic-linha      = 1.
  t_zbrnfe_danfe_dados_adic-tipodesc   = '1'.
  t_zbrnfe_danfe_dados_adic-descricao  = wg_xml_sefaz-nfeproc-nfe-infnfe-infadic-infcpl.

  APPEND t_zbrnfe_danfe_dados_adic.

  t_danfe_cabecalho-reservado_fisco = wg_xml_sefaz-nfeproc-nfe-infnfe-infadic-infadfisco.

ENDFORM.                    " ZF_INS_DADOS_ADICIONAIS

*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_ENDERECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_carrega_endereco
    USING p_adrnr TYPE addr1_sel-addrnumber
    CHANGING p_sadr TYPE sadr
             p_addr TYPE addr1_val.

  DATA addr1_sel TYPE addr1_sel.

  addr1_sel-addrnumber = p_adrnr.
  CALL FUNCTION 'ADDR_GET'
    EXPORTING
      address_selection = addr1_sel
    IMPORTING
      sadr              = p_sadr
      address_value     = p_addr
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      version_not_exist = 3
      internal_error    = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ZF_CARREGA_ENDERECO
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_CONTINGENCIA
*&---------------------------------------------------------------------*
FORM zf_verifica_contingencia.

  DATA lv_regio TYPE regio.
  DATA lv_land1 TYPE land1.
  DATA es_set_cont TYPE j_1bnfe_contin.
  DATA ev_contingency TYPE j_1bcontingency.
  CLEAR g_contingencia.

* Verifica a regiao
  CALL FUNCTION 'J_1B_NFE_FILL_MONITOR_TABLE'
    EXPORTING
      i_doc    = t_j_1bnfdoc
      i_docnum = t_j_1bnfdoc-docnum
    IMPORTING
*     e_active = ls_nfeactive
      e_regio  = lv_regio
      e_land1  = lv_land1.

  CALL FUNCTION 'J_1B_NFE_CONTINGENCY_READ'
    EXPORTING
      iv_land1                = lv_land1
      iv_regio                = lv_regio
      iv_bukrs                = t_j_1bnfdoc-bukrs
      iv_branch               = t_j_1bnfdoc-branch
    IMPORTING
      es_set_cont             = es_set_cont
      ev_contingency          = ev_contingency
    EXCEPTIONS
      region_in_contingency   = 1
      no_parameters_specified = 2
      incomplete_call         = 3
      OTHERS                  = 4.

  IF sy-subrc = 1 OR NOT ev_contingency IS INITIAL.

    g_contingencia = 'X'.

  ELSE.

*   Verifica o documento
    IF t_j_1bnfe_active-conting_s = 'X'
      OR t_j_1bnfe_active-conting = 'X'.
      g_contingencia = 'X'.
    ENDIF.

  ENDIF.

ENDFORM.                    " ZF_VERIFICA_CONTINGENCIA
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_ADIC_MSG_NOTA
*&---------------------------------------------------------------------*
* Mensagens da nota
*----------------------------------------------------------------------*
FORM zf_ins_adic_msg_nota.
* [Alteração - Pedro Sbais/FH - 19.06.2018 - Proj NFe-4.00 Outbound
  TYPES: BEGIN OF tp_j_1bnfstx,
           docnum TYPE j_1bnfstx-docnum,
           itmnum TYPE j_1bnfstx-itmnum,
           taxtyp TYPE j_1bnfstx-taxtyp,
           base   TYPE j_1bnfstx-base,
           rate   TYPE j_1bnfstx-rate,
           taxval TYPE j_1bnfstx-taxval,
         END OF tp_j_1bnfstx.

  DATA: lt_j_1bnfstx   TYPE TABLE OF tp_j_1bnfstx,
        lw_j_1bnfstx   TYPE tp_j_1bnfstx,
        lr_taxtyp      TYPE RANGE OF j_1bnfstx-taxtyp,
        lw_taxtyp      LIKE LINE OF lr_taxtyp,
        lv_base_icfp   TYPE j_1bnfstx-base,
        lv_base_icsp   TYPE j_1bnfstx-base,
        lv_taxval_icfp TYPE j_1bnfstx-taxval,
        lv_taxval_icsp TYPE j_1bnfstx-taxval,
        lv_rate_icfp   TYPE j_1bnfstx-rate,
        lv_rate_icsp   TYPE j_1bnfstx-rate,
        lv_base        TYPE c LENGTH 18,
        lv_taxval      TYPE c LENGTH 18,
        lv_rate        TYPE c LENGTH 9.
* ]Alteração - Pedro Sbais/FH - 19.06.2018 - Proj NFe-4.00 Outbound

  DATA l_regio_recebedor TYPE regio.

* Região do recebedor
  SELECT SINGLE regio
  FROM kna1
  INTO l_regio_recebedor
 WHERE kunnr EQ t_j_1bnfdoc-parid.

* Mensgens Nota Fiscal
  CALL FUNCTION 'ZBRNFE_MSG_DANFE_CALAMO'
    EXPORTING
      i_doc              = t_j_1bnfdoc
      i_header_add       = wk_header_add
      i_uf_e1            = l_regio_recebedor
      i_uf_c1            = lc_address1-region
    TABLES
      it_header_item_add = wk_item_add
      it_j1bnfstx        = t_j_1bnfstx
      it_j1bnflin        = t_j_1bnflin
      it_j1bnfftx        = t_j_1bnfftx.

  SORT t_j_1bnfftx BY docnum seqnum linnum.

  LOOP AT t_j_1bnfftx.

    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
    MOVE t_j_1bnfftx-message
      TO t_zbrnfe_danfe_dados_adic-descricao.
* [Alteração - André/FHC - 08.03.2010 - Proj NFe-Cálamo
*Faz tratamento p/ número do pedido do ecommerece ficarem destacados no
*DANFE
    IF t_j_1bnfdoc-bukrs = '1003' AND t_j_1bnfdoc-branch = '0010' AND
        NOT t_j_1bnfftx-message IS INITIAL AND
       ( t_j_1bnfftx-linnum = '99' OR t_j_1bnfftx-linnum = '98' ).
      MOVE t_j_1bnfftx-message
            TO t_zbrnfe_danfe_dados_adic-descricao.
      t_zbrnfe_danfe_dados_adic-objeto = 'PEDIDOECOM'.
    ENDIF.
* ]Alteração - André/FHC - 08.03.2010 - Proj NFe-Cálamo

    APPEND t_zbrnfe_danfe_dados_adic.

  ENDLOOP.

* [Alteração - Pedro Sbais/FH - 19.06.2018 - Proj NFe-4.00 Outbound
  "insere informações a respeito do imposto ICFP e ICSP
  lw_taxtyp-sign = 'I'.
  lw_taxtyp-option = 'EQ'.
  MOVE 'ICFP' TO lw_taxtyp-low.
  APPEND lw_taxtyp TO lr_taxtyp.

  MOVE 'ICSC' TO lw_taxtyp-low.
  APPEND lw_taxtyp TO lr_taxtyp.

  SELECT docnum itmnum taxtyp base rate taxval
    FROM j_1bnfstx
    INTO TABLE lt_j_1bnfstx
    WHERE docnum = t_j_1bnfdoc-docnum
    AND taxtyp IN lr_taxtyp.
  IF sy-subrc EQ 0.

    SORT lt_j_1bnfstx BY taxtyp.
    LOOP AT lt_j_1bnfstx INTO lw_j_1bnfstx.
      CASE lw_j_1bnfstx-taxtyp.
        WHEN 'ICFP'.
          ADD lw_j_1bnfstx-base   TO lv_base_icfp.
          ADD lw_j_1bnfstx-taxval TO lv_taxval_icfp.
          lv_rate_icfp = lw_j_1bnfstx-rate.
        WHEN 'ICSC'.
          ADD lw_j_1bnfstx-base   TO lv_base_icsp.
          ADD lw_j_1bnfstx-taxval TO lv_taxval_icsp.
          lv_rate_icsp = lw_j_1bnfstx-rate.
      ENDCASE.
    ENDLOOP.

    CLEAR: t_zbrnfe_danfe_dados_adic.

    IF lv_base_icsp IS NOT INITIAL AND lv_taxval_icsp IS NOT INITIAL AND lv_rate_icsp IS NOT INITIAL.
      WRITE lv_base_icsp   TO lv_base.
      WRITE lv_taxval_icsp TO lv_taxval.
      WRITE lv_rate_icsp   TO lv_rate.
      CONDENSE: lv_base, lv_taxval, lv_rate NO-GAPS.
      t_zbrnfe_danfe_dados_adic-linha = lines( t_zbrnfe_danfe_dados_adic[] ) + 1.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
      CONCATENATE 'Base FCP:'     lv_base
                  'Alíquota FCP:' lv_rate
                  'Valor FCP:'    lv_taxval
                  INTO t_zbrnfe_danfe_dados_adic-descricao SEPARATED BY space.
      APPEND t_zbrnfe_danfe_dados_adic.
    ENDIF.

    IF lv_base_icfp IS NOT INITIAL AND lv_taxval_icfp IS NOT INITIAL AND lv_rate_icfp IS NOT INITIAL.
      WRITE lv_base_icfp   TO lv_base.
      WRITE lv_taxval_icfp TO lv_taxval.
      WRITE lv_rate_icfp   TO lv_rate.
      CONDENSE: lv_base, lv_taxval, lv_rate NO-GAPS.
      t_zbrnfe_danfe_dados_adic-linha = lines( t_zbrnfe_danfe_dados_adic[] ) + 1.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
      CONCATENATE 'Base FCP-ST:'     lv_base
                  'Alíquota FCP-ST:' lv_rate
                  'Valor FCP-ST:'    lv_taxval
                  INTO t_zbrnfe_danfe_dados_adic-descricao SEPARATED BY space.
      APPEND t_zbrnfe_danfe_dados_adic.
    ENDIF.

  ENDIF.
* ]Alteração - Pedro Sbais/FH - 19.06.2018 - Proj NFe-4.00 Outbound


ENDFORM.                    " ZF_INS_ADIC_MSG_NOTA

*&---------------------------------------------------------------------*
*&      Form  ZF_INS_ADIC_CERT_QUALIDADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_ins_adic_cert_qualidade .
  DATA lt_vbrp TYPE TABLE OF tp_vbrp WITH HEADER LINE.
  DATA l_vbgel TYPE vbrp-vgbel.

  DATA l_aux(20).

  CHECK NOT gt_vbrp[] IS INITIAL.

  lt_vbrp[] = gt_vbrp[].

  SORT lt_vbrp BY vgbel.

  LOOP AT lt_vbrp.
    IF l_vbgel <> lt_vbrp-vgbel
       AND NOT lt_vbrp-vgbel IS INITIAL.

      ADD 1 TO g_linha.
      t_zbrnfe_danfe_dados_adic-linha = g_linha.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

      CONCATENATE lt_vbrp-vgbel '001' INTO l_aux.

      CONCATENATE TEXT-s02 l_aux
        INTO t_zbrnfe_danfe_dados_adic-descricao
        SEPARATED BY space.

      APPEND t_zbrnfe_danfe_dados_adic.
    ENDIF.
    l_vbgel = lt_vbrp-vgbel.
  ENDLOOP.
ENDFORM.                    " ZF_INS_ADIC_CERT_QUALIDADE
*&---------------------------------------------------------------------*
*&      Form  ZF_DADOS_ITENS_DESC
*&---------------------------------------------------------------------*
* Descricao dos itens
*----------------------------------------------------------------------*
FORM zf_dados_itens_desc
  TABLES   p_item_desc STRUCTURE zbrnfe_danfe_item_desc
           p_j_1bnfstx STRUCTURE j_1bnfstx
  USING    p_j_1bnfdoc TYPE j_1bnfdoc
           p_j_1bnflin TYPE j_1bnflin.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
  TYPES: BEGIN OF lt_aufm_main,
           mblnr TYPE aufm-mblnr,
           matnr TYPE aufm-matnr,
           aufnr TYPE aufm-aufnr,
           werks TYPE aufm-werks,
           charg TYPE aufm-charg,
           menge TYPE aufm-menge,
           meins TYPE aufm-meins,
           bwart TYPE aufm-bwart,
         END OF lt_aufm_main.

  TYPES: BEGIN OF gt_aufk,
           aufnr TYPE aufk-aufnr,
           werks TYPE aufk-werks,
         END OF gt_aufk.

  "AD.MAGGI"
*  TYPES: BEGIN OF flapu_types,
*            z_matnr TYPE zmmt008-z_matnr,
*            z_flapu TYPE zmmt008-z_flapu,
*         END OF flapu_types.
  "AD.MAGGI"

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
  DATA l_linha      TYPE zbrnfe_danfe_item_desc-linha.
  DATA lw_item_desc TYPE zbrnfe_danfe_item_desc.
  DATA lw_vbap      TYPE vbap.

*----------------------------------------------------------------------*
* VARIÁVEIS
*----------------------------------------------------------------------*
  DATA l_aux1(20).
  DATA l_aux2(20).
  DATA l_aux3(100).
  DATA l_aux4(50).
  DATA l_pis             TYPE j_1bnfstx-taxval.
  DATA l_cofins          TYPE j_1bnfstx-taxval.
  DATA l_vbeln           TYPE vbak-vbeln.
  DATA l_bstnk           TYPE vbak-bstnk.
  DATA l_rate_aux        TYPE j_1bnfstx-rate.
  DATA l_bstkd_e         TYPE vbkd-bstkd_e.
  DATA l_vbelv           TYPE vbfa-vbelv.
  DATA l_posnv           TYPE vbfa-posnv.
  DATA l_vgbel           TYPE vbrp-vgbel.
  DATA l_posnr           TYPE vbrp-posnr.
  DATA it_configuracao   TYPE ibco2_instance_tab2.
  DATA v_tipon           TYPE ibvalue0-atinn.
  DATA v_tipoc(15)       TYPE c.
  DATA lv_compr(30)      TYPE c.
  DATA lv_largura(30)    TYPE c.
  DATA lv_qtd_val(30)    TYPE c.
  DATA lv_lfimg          TYPE lips-lfimg.
  DATA lv_meins_c        TYPE lips-meins.
  DATA lv_val_carac      TYPE vbap-cuobj.
  DATA lv_peso_aux(30)   TYPE c.
  DATA lw_aufm           TYPE lt_aufm_main.
  DATA lv_auart          TYPE vbak-auart.
  DATA l_vbrk            TYPE vbrk.
  DATA v_ipifrete        TYPE p DECIMALS 2.
  DATA v_ipifrete2       TYPE p DECIMALS 2 VALUE '0.00'.
  DATA v_txtipifrete(20) TYPE c.
  DATA lin_danfe_item TYPE i. "Numero de linhas

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
  DATA lt_aufm   TYPE TABLE OF lt_aufm_main.
  DATA lt_aufk   TYPE TABLE OF gt_aufk.
  DATA it_values TYPE ibco2_value_tab               WITH HEADER LINE.
  DATA t_konv    TYPE STANDARD TABLE OF konv        WITH HEADER LINE.
  "DATA tl_flapu  TYPE STANDARD TABLE OF flapu_types WITH HEADER LINE. "AD.MAGGI"
  DATA vl_lote(6) TYPE c VALUE 'Lote: '.
*----------------------------------------------------------------------*
* PROCESSAMENTO
*----------------------------------------------------------------------*
  REFRESH p_item_desc.
  CLEAR l_linha.

  SELECT SINGLE *
    FROM vbfa
   WHERE vbeln   EQ p_j_1bnflin-refkey
     AND vbtyp_v EQ 'C'.

  IF sy-subrc = 0.

    SELECT SINGLE *
      FROM vbak
     WHERE vbeln = vbfa-vbelv.

  ENDIF.

  IF sy-subrc EQ 0 AND vbak-vkorg NE 'FU01'.

    SELECT SINGLE kdmat
      FROM vbap
      INTO vbap-kdmat
     WHERE vbeln EQ vbfa-vbelv
       AND posnr EQ p_j_1bnflin-itmnum.

  ENDIF.
* Descricao
  ADD 1 TO l_linha.
  CLEAR lw_item_desc.
  lw_item_desc-num       = p_j_1bnflin-itmnum.
  lw_item_desc-linha     = l_linha.
  lw_item_desc-tipodesc  = '1'.

* Informação do Lote
  IF ( t_j_1bnfdoc-nftype = 'G1'
    OR t_j_1bnfdoc-nftype = '10'
    OR t_j_1bnfdoc-nftype = '61' )
   AND t_j_1bnflin-itmtyp = '32'.
    CONCATENATE p_j_1bnflin-maktx
                vl_lote
                t_j_1bnflin-charg
           INTO lw_item_desc-descricao
      SEPARATED BY space.
  ELSE.
    lw_item_desc-descricao = p_j_1bnflin-maktx.
  ENDIF.


*--------------------------------------------------------------------*
* Busca caracteristica do material para sair na nota fiscal.
* Documento de faturamento: dados de item
  CLEAR vbrp.
  SELECT SINGLE *
    FROM vbrp
   WHERE vbeln EQ t_j_1bnflin-refkey
     AND posnr EQ t_j_1bnflin-refitm.

  IF sy-subrc EQ 0.
    CLEAR vbco3.
    vbco3-mandt = sy-mandt.
    vbco3-spras = sy-langu.
    vbco3-vbeln = vbrp-aubel.

* Não Imprimir caracteristicas para categoria de item MTO
    IF ( vbrp-pstyv = 'ZAMO' ) OR ( vbrp-pstyv = 'ZAMA' )
    OR ( vbrp-pstyv = 'ZDMA' ) OR ( vbrp-pstyv = 'ZDVO' )
    OR ( vbrp-pstyv = 'ZGOC' ) OR ( vbrp-pstyv = 'ZRI0' )
    OR ( vbrp-pstyv = 'ZVEO' ) OR ( vbrp-pstyv = 'ZVF0' )
    OR ( vbrp-pstyv = 'ZVFO' ) OR ( vbrp-pstyv = 'ZVIO' )
    OR ( vbrp-pstyv = 'ZVNO' ).

      CALL FUNCTION 'RV_DOCUMENT_PRINT_VIEW'
        EXPORTING
          comwa                       = vbco3
        IMPORTING
          kopf                        = vbdka
        TABLES
          pos                         = tvbdpa
          mess                        = da_mess
        EXCEPTIONS
          fehler_bei_datenbeschaffung = 1.

      IF sy-subrc EQ 0.
        READ TABLE tvbdpa WITH KEY posnr = vbrp-aupos
                                   matnr = vbrp-matnr.
        IF sy-subrc EQ 0.
          CLEAR: vbdpa, tkomcon.
          REFRESH: tkomcon.
          vbdpa = tvbdpa.
          CALL FUNCTION 'VC_I_GET_CONFIGURATION'
            EXPORTING
              instance              = vbdpa-cuobj
              language              = sy-langu
              iv_max_massprocessing = 0
              iv_no_value_check     = 'X'
            TABLES
              configuration         = tkomcon
            EXCEPTIONS
              OTHERS                = 4.
          IF sy-subrc EQ 0.

            LOOP AT tkomcon WHERE atnam EQ 'CA_LARG'
                               OR atnam EQ 'CA_MATCLNT'
                               OR atnam EQ 'CA_ARTE'.

              CASE tkomcon-atnam.
                WHEN 'CA_MATCLNT'."Número Material do Cliente
                  CHECK NOT tkomcon-atwrt IS INITIAL.

                  CONCATENATE lw_item_desc-descricao
                              tkomcon-atwrt
                         INTO lw_item_desc-descricao
                    SEPARATED BY space.

                WHEN 'CA_LARG'. "Largura do Material
                  CHECK NOT tkomcon-atwrt IS INITIAL.

                  CONCATENATE lw_item_desc-descricao
                              tkomcon-atwrt
                         INTO lw_item_desc-descricao
                    SEPARATED BY space.

                WHEN 'CA_ARTE'. "Código da Arte do Material
                  IF tkomcon-atwrt <> '0'.
                    CHECK NOT tkomcon-atwrt IS INITIAL.

                    CONCATENATE lw_item_desc-descricao
                                tkomcon-atwrt
                           INTO lw_item_desc-descricao
                      SEPARATED BY space.

                  ENDIF.
              ENDCASE.

            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*--------------------------------------------------------------------*

  APPEND lw_item_desc TO p_item_desc.

* Informação do Lote
  IF ( p_j_1bnfdoc-nftype = 'G1'
    OR p_j_1bnfdoc-nftype = '10'
    OR p_j_1bnfdoc-nftype = '61' )
   AND p_j_1bnflin-itmtyp = '32'.
    CONCATENATE lw_item_desc-descricao
                'Lote:'
                p_j_1bnflin-charg
           INTO lw_item_desc-descricao
      SEPARATED BY space.
  ENDIF.

  ADD 1 TO l_linha.
  CLEAR lw_item_desc.
  lw_item_desc-num = p_j_1bnflin-itmnum.
  lw_item_desc-linha = l_linha.
  lw_item_desc-tipodesc = '1'.

  IF NOT lw_item_desc-descricao IS INITIAL.
    APPEND lw_item_desc TO p_item_desc.
  ENDIF.

ENDFORM.                    " ZF_DADOS_ITENS_DESC
" ZF_INSC_ADIC_COND_VENDA
*&---------------------------------------------------------------------*
*&      Form  ZF_FORMATA_ITENS
*&---------------------------------------------------------------------*
FORM zf_formata_itens
  TABLES   p_danfe_item STRUCTURE zbrnfe_danfe_item
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

*    WRITE p_danfe_item-vlr_desc     TO
*    p_danfe_item-vlr_desc_desc
*      DECIMALS 2
*      LEFT-JUSTIFIED
*      NO-GAP.
*
*    WRITE p_danfe_item-vlr_liq     TO p_danfe_item-vlr_liq_desc
*      DECIMALS 2
*      LEFT-JUSTIFIED
*      NO-GAP.

    MODIFY p_danfe_item INDEX sy-tabix.
  ENDLOOP.


ENDFORM.                    " ZF_FORMATA_ITENS
*&---------------------------------------------------------------------*
*&      Form  ZF_INSC_ADIC_TP_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_insc_adic_tp_trans .

  CHECK NOT t_j_1bnfdoc-inco1 IS INITIAL.

  ADD 1 TO g_linha.

  t_zbrnfe_danfe_dados_adic-linha = g_linha.

  t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

  CONCATENATE TEXT-s06 t_j_1bnfdoc-inco1
    INTO t_zbrnfe_danfe_dados_adic-descricao
    SEPARATED BY space.

  APPEND t_zbrnfe_danfe_dados_adic.

ENDFORM.                    " ZF_INSC_ADIC_TP_TRANS

*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_PIS_COFINS
*&---------------------------------------------------------------------*
* Carrega impostos PIS e COFINS nas tabelas de impostos
*----------------------------------------------------------------------*
FORM zf_carrega_pis_cofins
   TABLES p_j_1bnfstx STRUCTURE j_1bnfstx
   USING  p_j_1bnflin TYPE j_1bnflin.

  PERFORM zf_recupera_pis_cofins
    TABLES p_j_1bnfstx
    USING  p_j_1bnflin.

  IF p_j_1bnfstx[] IS INITIAL.
    PERFORM zf_recupera_pis_cofins_memoria
      TABLES p_j_1bnfstx
      USING  p_j_1bnflin.
  ENDIF.

ENDFORM.                    "zf_carrega_pis_cofins

*&---------------------------------------------------------------------*
*&      Form  ZF_RECUPERA_PIS_COFINS
*&---------------------------------------------------------------------*
* Carrega impostos PIS e COFINS nas tabelas de impostos
*----------------------------------------------------------------------*
FORM zf_recupera_pis_cofins
   TABLES p_j_1bnfstx STRUCTURE j_1bnfstx
   USING  p_j_1bnflin TYPE j_1bnflin.

  DATA l_knumv TYPE vbrk-knumv.
  DATA lt_konv TYPE TABLE OF konv WITH HEADER LINE.
  DATA lw_stx TYPE j_1bnfstx.

  SELECT SINGLE knumv
    INTO l_knumv
    FROM vbrk
   WHERE vbeln = p_j_1bnflin-refkey.

*---> 05/07/2022 - Migração S4 - DG
** Condicoes
*  SELECT *
*    INTO TABLE lt_konv
*    FROM konv
*   WHERE knumv = l_knumv
*     AND kposn = p_j_1bnflin-refitm.

  SELECT *
    INTO TABLE @DATA(lt_konv_aux)
    FROM v_konv
   WHERE knumv = @l_knumv
     AND kposn = @p_j_1bnflin-refitm.

  MOVE-CORRESPONDING lt_konv_aux[] TO lt_konv[].
*<--- 05/07/2022 - Migração S4 - DG


* PIS
  LOOP AT lt_konv WHERE kschl = c_imp_pis.
    EXIT.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
    CLEAR lw_stx.
    lw_stx-docnum = p_j_1bnflin-docnum.
    lw_stx-itmnum = p_j_1bnflin-itmnum.
    lw_stx-basered1 = 100.
    lw_stx-taxtyp = lt_konv-kschl.
    lw_stx-base   = lt_konv-kawrt.
    lw_stx-taxval = lt_konv-kwert.
    lw_stx-rate   = lt_konv-kbetr / 10.
    APPEND lw_stx TO p_j_1bnfstx.
  ENDIF.

* COFINS
  LOOP AT lt_konv WHERE kschl = c_imp_cofins.
    EXIT.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
    CLEAR lw_stx.
    lw_stx-docnum = p_j_1bnflin-docnum.
    lw_stx-itmnum = p_j_1bnflin-itmnum.
    lw_stx-basered1 = 100.
    lw_stx-taxtyp = lt_konv-kschl.
    lw_stx-base   = lt_konv-kawrt.
    lw_stx-taxval = lt_konv-kwert.
    lw_stx-rate   = lt_konv-kbetr / 10.
    APPEND lw_stx TO p_j_1bnfstx.
  ENDIF.

ENDFORM.                    " ZF_RECUPERA_PIS_COFINS
*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_CONF_IMPOSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_carrega_conf_imposto.

  SELECT *
    FROM j_1baj
    INTO TABLE t_j_1baj.

* PIS e COFINS
  CLEAR t_j_1baj.
  t_j_1baj-taxtyp = c_imp_pis.
  t_j_1baj-taxgrp = c_pis.
  APPEND t_j_1baj.

  CLEAR t_j_1baj.
  t_j_1baj-taxtyp = c_imp_cofins.
  t_j_1baj-taxgrp = c_cofi.
  APPEND t_j_1baj.

ENDFORM.                    " ZF_CARREGA_CONF_IMPOSTO

*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE_ITEM_TOT
*&---------------------------------------------------------------------*
FORM zf_insere_item_tot .

  READ TABLE t_zbrnfe_danfe_item WITH KEY num = c_num_tot.
  IF sy-subrc NE 0.
    CLEAR t_zbrnfe_danfe_item.
*   NUM - Sequencial do item (chave)
    t_zbrnfe_danfe_item-num = c_num_tot.
*   TIPODESC - Tp.Desc (1: Simples; 2: Elemento Texto; 3: Mult. Linhas)
    t_zbrnfe_danfe_item-tipodesc = '3'.

    APPEND t_zbrnfe_danfe_item.

*   Insere Linha em branco
    t_zbrnfe_danfe_item_desc-num = c_num_tot.
    t_zbrnfe_danfe_item_desc-linha = '1'.
    t_zbrnfe_danfe_item_desc-tipodesc = '1'.
    APPEND t_zbrnfe_danfe_item_desc.
  ENDIF.

ENDFORM.                    " ZF_INSERE_ITEM_TOT

*&---------------------------------------------------------------------*
*&      Form  ZF_INS_ADIC_MSG_FATURA
*&---------------------------------------------------------------------*
FORM zf_ins_adic_msg_fatura .

  DATA lt_lines TYPE TABLE OF tline WITH HEADER LINE.
  DATA lw_lines TYPE tline.
  DATA l_tabix TYPE sy-tabix.

  LOOP AT gt_awkey.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '2'.

    MOVE gt_awkey-vbeln TO t_zbrnfe_danfe_dados_adic-nome.

    t_zbrnfe_danfe_dados_adic-objeto = 'VBBK'.
    t_zbrnfe_danfe_dados_adic-id = 'ZEXP'.
    t_zbrnfe_danfe_dados_adic-idioma = sy-langu.

* ]Alteração 22.09.09 - André Fachin/FH
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = t_zbrnfe_danfe_dados_adic-id
        language                = t_zbrnfe_danfe_dados_adic-idioma
        name                    = t_zbrnfe_danfe_dados_adic-nome
        object                  = t_zbrnfe_danfe_dados_adic-objeto
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

* Validar se tem dados na linha
    LOOP AT lt_lines INTO lw_lines.
      l_tabix = sy-tabix.
      IF lw_lines-tdline IS INITIAL.
        DELETE lt_lines INDEX  l_tabix.
      ENDIF.
    ENDLOOP.

    IF NOT lt_lines[] IS INITIAL.
      APPEND t_zbrnfe_danfe_dados_adic.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " ZF_INS_ADIC_MSG_FATURA

*&---------------------------------------------------------------------*
*&      Form  block_p PIS
*&---------------------------------------------------------------------*
*       Codigo importado e adaptado de LJ_1B_NFEF26; Form block_p
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM block_p
  USING    p_lin TYPE j_1bnflin
           p_stx TYPE j_1bnfstx
  CHANGING p_xml TYPE j1b_nf_xml_badi_item.

* Fields for PIS
  DATA: lv_taxsi5   TYPE char1,
        p_cst       TYPE char20 VALUE 'PX_CST',
        p_vbc       TYPE char20 VALUE 'PX_VBC',
        p_ppis      TYPE char20 VALUE 'PX_PPIS',
        p_vpis      TYPE char20 VALUE 'PX_VPIS',
        p_qbcprod   TYPE char20 VALUE 'PX_QBCPROD',
        p_valiqprod TYPE char20 VALUE 'PX_VALIQPROD'.

* P01
* is tag <PIS>
*
*----------------------------------------------------------------
* possible values for PIS CST:
*
* 01 ? Operação Tributável (base de cálculo = valor da operação
*      alíquota
*      normal  (cumulativo/não cumulativo));
* 02 - Operação Tributável (base de cálculo = valor da operação
*      (alíquota diferenciada));
* 03 - Operação Tributável (base de cálculo = quantidade vendida x
*      alíquota por unidade de produto);
* 04 - Operação Tributável (tributação monofásica (alíquota zero));
* 05 - Operação Tributável (substituição tributária);
* 06 - Operação Tributável (alíquota zero);
* 07 - Operação Isenta da Contribuição;
* 08 - Operação Sem Incidência da Contribuição;
* 09 - Operação com Suspensão da Contribuição;
* 99 - Outras Operações;
*

  CASE p_lin-taxsi5.
    WHEN '01' OR '02'.
      lv_taxsi5 = '1'.
    WHEN '03'.
      lv_taxsi5 = '2'.
    WHEN '04' OR '06' OR '07' OR '08' OR '09'.
      lv_taxsi5 = '3'.
    WHEN OTHERS.
      lv_taxsi5 = '4'.
* P5 is used PIS subtrib calculation for selling to Zona Franca
*    not supported in SAP ERP systems
  ENDCASE.

  MOVE: lv_taxsi5 TO p_cst+1(1),
        lv_taxsi5 TO p_vbc+1(1),
        lv_taxsi5 TO p_ppis+1(1),
        lv_taxsi5 TO p_vpis+1(1),
        lv_taxsi5 TO p_qbcprod+1(1),
        lv_taxsi5 TO p_valiqprod+1(1).

*P02
  ASSIGN COMPONENT p_cst OF STRUCTURE p_xml TO <f2>.
  IF sy-subrc IS INITIAL.
    <f2> = p_lin-taxsi5.
  ENDIF.

* when p_flag is set routine is called from final round     "1149585
* -> no taxes involved therefore it´s not necessary         "1149585
* to fill the xml file field only CST is required           "1149585
*  check p_flag is initial.                                  "1149585
*
*----------------------------------------------------------------------
*
* set base amounts
* P03
  ASSIGN COMPONENT p_vbc OF STRUCTURE p_xml TO <f4>.
  IF sy-subrc IS INITIAL.
    PERFORM fill_base USING p_stx-base
                           p_stx-othbas
                           p_stx-excbas
                     CHANGING <f4>.
  ENDIF.

*----------------------------------------------------------------------
* P04
  IF p_stx-rectype IS INITIAL.                 "No Pauta
    ASSIGN COMPONENT p_ppis OF STRUCTURE p_xml TO <f5>.
    IF sy-subrc IS INITIAL.
      <f5> = p_stx-rate.
    ENDIF.
  ENDIF.
* P05
  ASSIGN COMPONENT p_qbcprod OF STRUCTURE p_xml TO <f3>.
  IF sy-subrc IS INITIAL.
    <f3> = p_lin-menge.
  ENDIF.
* P06  is the rate when PAUTA
  IF p_stx-rectype IS NOT INITIAL.              "Pauta
    ASSIGN COMPONENT p_valiqprod OF STRUCTURE p_xml TO <f4>.
    IF sy-subrc IS INITIAL.
      <f4> = p_stx-rate.
    ENDIF.
  ENDIF.
* P07
  ASSIGN COMPONENT p_vpis OF STRUCTURE p_xml TO <f6>.
  IF sy-subrc IS INITIAL.
    <f6> = p_stx-taxval.
  ENDIF.
*
ENDFORM.                    " block_p

FORM block_q
  USING    p_lin TYPE j_1bnflin
           p_stx TYPE j_1bnfstx
  CHANGING p_xml TYPE j1b_nf_xml_badi_item.
*
* Fields for PIS
  DATA: lv_taxsi4   TYPE char1,
        q_cst       TYPE char20 VALUE 'QX_CST',
        q_vbc       TYPE char20 VALUE 'QX_VBC',
        q_pcofins   TYPE char20 VALUE 'QX_PCOFINS',
        q_vcofins   TYPE char20 VALUE 'QX_VCOFINS',
        q_qbcprod   TYPE char20 VALUE 'QX_QBCPROD',
        q_valiqprod TYPE char20 VALUE 'QX_VALIQPROD'.

* Q01
* is tag <COFINS>
*
*----------------------------------------------------------------------

* possible values for COFINS CST:
*
* 01 ? Operação Tributável (base de cálculo = valor da operação
*      alíquota
*      normal  (cumulativo/não cumulativo));
* 02 - Operação Tributável (base de cálculo = valor da operação
*      (alíquota diferenciada));
* 03 - Operação Tributável (base de cálculo = quantidade vendida x
*      alíquota por unidade de produto);
* 04 - Operação Tributável (tributação monofásica (alíquota zero));
* 05 - Operação Tributável (substituição tributária);
* 06 - Operação Tributável (alíquota zero);
* 07 - Operação Isenta da Contribuição;
* 08 - Operação Sem Incidência da Contribuição;
* 09 - Operação com Suspensão da Contribuição;
* 99 - Outras Operações;
*
  CASE p_lin-taxsi4.
    WHEN '01' OR '02'.
      lv_taxsi4 = '1'.
    WHEN '03'.
      lv_taxsi4 = '2'.
    WHEN '04' OR '06' OR '07' OR '08' OR '09'.
      lv_taxsi4 = '3'.
    WHEN OTHERS.
      lv_taxsi4 = '4'.
* 5 is used Cofins subtrib calculation for selling to Zona Franca
*    not supported in SAP ERP systems
  ENDCASE.

  MOVE: lv_taxsi4 TO q_cst+1(1),
        lv_taxsi4 TO q_vbc+1(1),
        lv_taxsi4 TO q_pcofins+1(1),
        lv_taxsi4 TO q_vcofins+1(1),
        lv_taxsi4 TO q_qbcprod+1(1),
        lv_taxsi4 TO q_valiqprod+1(1).

* Q02
  ASSIGN COMPONENT q_cst OF STRUCTURE p_xml TO <f2>.
  IF sy-subrc IS INITIAL.
    <f2> = p_lin-taxsi4.
  ENDIF.

* when p_flag is set routine is called from final round     "1149585
* -> no taxes involved therefore it´s not necessary         "1149585
* to fill the xml file field only CST is required           "1149585
*  check p_flag is initial.                                 "1149585
*
*----------------------------------------------------------------------
*
* set base amounts
* Q03
  ASSIGN COMPONENT q_vbc OF STRUCTURE p_xml TO <f4>.
  IF sy-subrc IS INITIAL.
    PERFORM fill_base USING p_stx-base
                           p_stx-othbas
                           p_stx-excbas
                     CHANGING <f4>.
  ENDIF.

*----------------------------------------------------------------------
* Q04
  IF p_stx-rectype IS INITIAL.                 "No Pauta
    ASSIGN COMPONENT q_pcofins OF STRUCTURE p_xml TO <f5>.
    IF sy-subrc IS INITIAL.
      <f5> = p_stx-rate.
    ENDIF.
  ENDIF.
* Q05
  ASSIGN COMPONENT q_qbcprod OF STRUCTURE p_xml TO <f3>.
  IF sy-subrc IS INITIAL.
    <f3> = p_lin-menge.
  ENDIF.
* Q06  is the rate when Pauta
  IF p_stx-rectype IS NOT INITIAL.              "Pauta
    ASSIGN COMPONENT q_valiqprod OF STRUCTURE p_xml TO <f4>.
    IF sy-subrc IS INITIAL.
      <f4> = p_stx-rate.
    ENDIF.
  ENDIF.
* Q07
  ASSIGN COMPONENT q_vcofins OF STRUCTURE p_xml TO <f6>.
  IF sy-subrc IS INITIAL.
    <f6> = p_stx-taxval.
  ENDIF.

ENDFORM.                    " block_q

*&---------------------------------------------------------------------*
*&      Form  fill_base
*&---------------------------------------------------------------------*
FORM fill_base USING    p_base   TYPE j_1bbase
                        p_othbas TYPE j_1bbase
                        p_excbas TYPE j_1bbase
               CHANGING p_vbc    TYPE j_1bbase.

* normal base filled take base amount from there
  IF NOT p_base IS INITIAL.
    p_vbc = p_base.
* normal base empty - other based filled -> take other base
  ELSEIF NOT p_othbas IS INITIAL.
    p_vbc = p_othbas.
* exclude base
  ELSE.
    p_vbc = p_excbas.
  ENDIF.
ENDFORM.                    "fill_base
*&---------------------------------------------------------------------*
*&      Form  ZF_VERIFICA_STATUS
*&---------------------------------------------------------------------*
FORM zf_verifica_status .
  DATA imprime.

  CLEAR imprime.

  IF t_j_1bnfdoc-xmlvers < '2'.
    IF t_j_1bnfdoc-docstat = '1'.
      imprime = 'X'.
      g_output_options-tdcopies = '1'.
    ELSEIF g_contingencia = 'X' AND t_j_1bnfe_active-tpemis = '5'.
      imprime = 'X'.
      g_output_options-tdcopies = '2'.
    ENDIF.
  ELSE.
    IF t_j_1bnfdoc-docstat = '1' OR t_j_1bnfe_active-docsta = '1'.

      imprime = 'X'.
      g_output_options-tdcopies = '1'.
    ELSEIF g_contingencia = 'X' AND t_j_1bnfe_active-tpemis = '5'.
      imprime = 'X'.
      g_output_options-tdcopies = '2'.
    ENDIF.
  ENDIF.


  IF imprime IS INITIAL.
    MESSAGE e000 WITH t_j_1bnfe_active-nfnum9
      RAISING nfe_nao_aprovada.
  ENDIF.
ENDFORM.                    " ZF_VERIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_ADIC_CONTINGENCIA
*&---------------------------------------------------------------------*
FORM zf_ins_adic_contingencia .

  DATA t_zbrnfe_cont TYPE zbrnfe_cont.
  DATA l_data(10) TYPE c.
  DATA l_hora(10) TYPE c.

* Inclui Data, Hora e Justificativa para entrada em contingência.
  IF t_j_1bnfdoc-conting = 'X'.
    IF t_j_1bnfe_active-tpemis = '5'.
      ADD 1 TO g_linha.
      t_zbrnfe_danfe_dados_adic-linha = g_linha.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
      MOVE TEXT-s18 TO t_zbrnfe_danfe_dados_adic-descricao.
      APPEND t_zbrnfe_danfe_dados_adic.
    ENDIF.
* Chave de Contingência para SEFAZ do PARANÁ
    SELECT SINGLE *
             FROM zbrnfe_cont "Tabela de dados de contingência
             INTO t_zbrnfe_cont
             WHERE bukrs  EQ t_j_1bnfdoc-bukrs
               AND branch EQ t_j_1bnfdoc-branch
               AND zcontin_ativo EQ 'X'.
    IF sy-subrc = 0.
      ADD 1 TO g_linha.
      CLEAR t_zbrnfe_danfe_dados_adic.
      t_zbrnfe_danfe_dados_adic-linha = g_linha.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
      WRITE t_zbrnfe_cont-zcontin_data TO l_data.
      CONCATENATE TEXT-s38 "Data Entrada em Contingência :
                  l_data
             INTO t_zbrnfe_danfe_dados_adic-descricao
        SEPARATED BY space.
      APPEND t_zbrnfe_danfe_dados_adic.
      ADD 1 TO g_linha.
      CLEAR t_zbrnfe_danfe_dados_adic.
      t_zbrnfe_danfe_dados_adic-linha = g_linha.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
      WRITE t_zbrnfe_cont-zcontin_hora TO l_hora.
      CONCATENATE TEXT-s39 "Hora Entrada em Contingência :
                  l_hora
             INTO t_zbrnfe_danfe_dados_adic-descricao
        SEPARATED BY space.
      APPEND t_zbrnfe_danfe_dados_adic.

      ADD 1 TO g_linha.
      CLEAR t_zbrnfe_danfe_dados_adic.
      t_zbrnfe_danfe_dados_adic-linha = g_linha.
      t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

      IF NOT t_zbrnfe_cont-zcontin_just IS INITIAL.

        CONCATENATE TEXT-s40 "Motivo Entrada em Contingência :
                    t_zbrnfe_cont-zcontin_just
               INTO t_zbrnfe_danfe_dados_adic-descricao
          SEPARATED BY space.
        APPEND t_zbrnfe_danfe_dados_adic.

      ELSEIF NOT t_j_1bnfe_active-reason_conting1 IS INITIAL.

        CONCATENATE TEXT-s40 "Motivo Entrada em Contingência :
                    t_j_1bnfe_active-reason_conting1
               INTO t_zbrnfe_danfe_dados_adic-descricao
          SEPARATED BY space.
        APPEND t_zbrnfe_danfe_dados_adic.

      ENDIF.
*     16.03.2010. Inclui Data, Hora e Justificativa para entrada em contingência.
    ELSE.

      IF NOT t_j_1bnfe_active-reason_conting1 IS INITIAL.
        ADD 1 TO g_linha.
        CLEAR t_zbrnfe_danfe_dados_adic.
        t_zbrnfe_danfe_dados_adic-linha = g_linha.
        t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
        CONCATENATE TEXT-s40 "Motivo Entrada em Contingência :
                    t_j_1bnfe_active-reason_conting1
               INTO t_zbrnfe_danfe_dados_adic-descricao
          SEPARATED BY space.
        APPEND t_zbrnfe_danfe_dados_adic.

        ADD 1 TO g_linha.
        CLEAR t_zbrnfe_danfe_dados_adic.
        CLEAR l_data.
        t_zbrnfe_danfe_dados_adic-linha = g_linha.
        t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
        WRITE t_j_1bnfe_active-conting_date TO l_data.
        CONCATENATE TEXT-s38 "Data Entrada em Contingência :
                    l_data
               INTO t_zbrnfe_danfe_dados_adic-descricao
          SEPARATED BY space.
        APPEND t_zbrnfe_danfe_dados_adic.

        ADD 1 TO g_linha.
        CLEAR t_zbrnfe_danfe_dados_adic.
        CLEAR l_hora.
        t_zbrnfe_danfe_dados_adic-linha = g_linha.
        t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
        WRITE t_j_1bnfe_active-conting_time TO l_hora.
        CONCATENATE TEXT-s39 "Hora Entrada em Contingência :
                    l_hora
               INTO t_zbrnfe_danfe_dados_adic-descricao
          SEPARATED BY space.
        APPEND t_zbrnfe_danfe_dados_adic.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM zf_ins_adic_docref .

  DATA v_data(10).

  SELECT SINGLE *
    FROM vbfa
   WHERE vbeln = t_j_1bnflin-refkey
     AND ( vbtyp_v = 'C'
      OR   vbtyp_v = 'I' ).

  IF sy-subrc = 0.

    SELECT SINGLE zuonr
             FROM vbak
             INTO vbak-zuonr
            WHERE vbeln EQ vbfa-vbelv.

    IF sy-subrc EQ 0.

      CHECK vbak-zuonr IS NOT INITIAL.

      SELECT *
        FROM j_1bnflin
          UP TO 1 ROWS
       WHERE refkey = vbak-zuonr.
      ENDSELECT.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM j_1bnfdoc
         WHERE docnum = j_1bnflin-docnum.

        IF sy-subrc EQ 0.

          ADD 1 TO g_linha.

          t_zbrnfe_danfe_dados_adic-linha = g_linha.
          t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

          CONCATENATE j_1bnfdoc-docdat+6(2)
                      j_1bnfdoc-docdat+4(2)
                      j_1bnfdoc-docdat(4)
                 INTO v_data SEPARATED BY '.'.

          IF j_1bnfdoc-nfnum IS NOT INITIAL.
            CONCATENATE 'Ref. a NF:'
                        j_1bnfdoc-nfnum
                        'de'
                        v_data
                   INTO t_zbrnfe_danfe_dados_adic-descricao
              SEPARATED BY space.

            APPEND t_zbrnfe_danfe_dados_adic.
          ELSEIF j_1bnfdoc-nfenum IS NOT INITIAL.
            CONCATENATE 'Ref. a NF-e:'
                      j_1bnfdoc-nfenum
                      'de'
                      v_data
                 INTO t_zbrnfe_danfe_dados_adic-descricao
            SEPARATED BY space.

            APPEND t_zbrnfe_danfe_dados_adic.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_INS_ADIC_DOCREF
*&---------------------------------------------------------------------*
*&      Form  ZF_RECUPERA_PIS_COFINS_MEMORIA
*&---------------------------------------------------------------------*
FORM zf_recupera_pis_cofins_memoria
   TABLES p_j_1bnfstx STRUCTURE j_1bnfstx
   USING  p_j_1bnflin TYPE j_1bnflin.

  DATA lt_komv TYPE TABLE OF komv WITH HEADER LINE.
  DATA lw_stx TYPE j_1bnfstx.
  FIELD-SYMBOLS <f> TYPE any.

  ASSIGN ('(SAPMV60A)XKOMV[]') TO <f>.

  IF sy-subrc IS NOT INITIAL.

    ASSIGN ('(SAPLV60A)XKOMV[]') TO <f>.

  ENDIF.

  CHECK sy-subrc IS INITIAL.

  lt_komv[] = <f>.

  LOOP AT lt_komv
    WHERE kposn = p_j_1bnflin-refitm
    AND ( kschl = c_imp_pis ). " OR kschl = c_imp_pis2 ).
    EXIT.
  ENDLOOP.

  IF sy-subrc IS INITIAL.

    CLEAR lw_stx.
    lw_stx-docnum = p_j_1bnflin-docnum.
    lw_stx-itmnum = p_j_1bnflin-itmnum.
    lw_stx-basered1 = 100.
    lw_stx-taxtyp = lt_komv-kschl.
    lw_stx-base   = lt_komv-kawrt.
    lw_stx-taxval = lt_komv-kwert.
    lw_stx-rate   = lt_komv-kbetr / 10.
    APPEND lw_stx TO p_j_1bnfstx.

  ENDIF.

  LOOP AT lt_komv
    WHERE kposn = p_j_1bnflin-refitm
    AND ( kschl = c_imp_cofins ). " OR kschl = c_imp_cofins2 ).
    EXIT.
  ENDLOOP.

  IF sy-subrc IS INITIAL.

    CLEAR lw_stx.
    lw_stx-docnum = p_j_1bnflin-docnum.
    lw_stx-itmnum = p_j_1bnflin-itmnum.
    lw_stx-basered1 = 100.
    lw_stx-taxtyp = lt_komv-kschl.
    lw_stx-base   = lt_komv-kawrt.
    lw_stx-taxval = lt_komv-kwert.
* [Alteração - André/FHC - Proj. NF2 2G
*    IF NOT lt_komv-kawrt IS INITIAL.
*      lw_stx-rate   = lt_komv-kwert / lt_komv-kawrt * 100.
*    ENDIF.
* ]Alteração - André/FHC - Proj. NF2 2G
    lw_stx-rate   = lt_komv-kbetr / 10.
    APPEND lw_stx TO p_j_1bnfstx.

  ENDIF.

ENDFORM.                    " ZF_RECUPERA_PIS_COFINS_MEMORIA

FORM zf_parametros_nast

  USING    p_nast TYPE nast
           p_screen TYPE char1
  CHANGING p_docnum TYPE j_1bnfdoc-docnum
           p_control_parameters TYPE ssfctrlop
           p_output_options TYPE ssfcompop.


  DATA pe_returncode TYPE  sysubrc.
  DATA pe_itcpo TYPE  itcpo.
  DATA pe_device TYPE  tddevice.
  DATA pe_recipient TYPE  swotobjid.
  DATA pe_sender TYPE  swotobjid.

  CLEAR p_control_parameters.
  CLEAR p_output_options.

  MOVE p_nast-objky TO p_docnum.

  CALL FUNCTION 'WFMC_PREPARE_SMART_FORM'
    EXPORTING
      pi_nast       = p_nast
      pi_repid      = ' '
      pi_screen     = p_screen
    IMPORTING
      pe_returncode = pe_returncode
      pe_itcpo      = pe_itcpo
      pe_device     = pe_device
      pe_recipient  = pe_recipient
      pe_sender     = pe_sender.

  IF pe_returncode = 0.
    MOVE-CORRESPONDING pe_itcpo TO p_output_options.
    p_control_parameters-device = pe_device.
    p_control_parameters-preview = p_screen.
    p_control_parameters-getotf = pe_itcpo-tdgetotf.
    p_control_parameters-langu = p_nast-spras.
    p_control_parameters-no_dialog = 'X'.
  ENDIF.

ENDFORM.                    " ZF_PARAMETROS_NAST
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
FORM read_text USING p_lin    TYPE j_1bnflin
                     p_id     TYPE stxh-tdid
                     p_object TYPE stxh-tdobject
            CHANGING p_tdline.

  DATA  BEGIN OF tlinetab OCCURS 10.
  INCLUDE STRUCTURE tline.
  DATA: END OF tlinetab.

  DATA  BEGIN OF tstxh.
  INCLUDE STRUCTURE stxh.
  DATA: END OF tstxh.

  DATA: theader LIKE thead.

  CLEAR tstxh.
* Utilizacao wk_item-refitm ao inves de wk_item-itmnum
  CONCATENATE p_lin-refkey p_lin-refitm INTO tstxh-tdname.
  tstxh-tdspras        = sy-langu.                          "Idioma
  tstxh-tdid           = p_id.                                "ID
  tstxh-tdobject       = p_object.       "Objeto de texto

  SELECT SINGLE * FROM stxh WHERE tdobject = tstxh-tdobject
                              AND tdname   = tstxh-tdname
                              AND tdid     = tstxh-tdid
                              AND tdspras  = tstxh-tdspras.
  IF sy-subrc EQ 0.
    REFRESH tlinetab.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = tstxh-tdid
        language                = tstxh-tdspras
        name                    = tstxh-tdname
        object                  = tstxh-tdobject
      IMPORTING
        header                  = theader
      TABLES
        lines                   = tlinetab
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 0.
      READ TABLE tlinetab  INDEX 1.
      MOVE tlinetab-tdline TO p_tdline.
    ENDIF.
  ENDIF.

ENDFORM.                                                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  ZF_RECUPERA_PESO_AUX
*&---------------------------------------------------------------------*
FORM zf_recupera_peso_aux
  CHANGING p_ntgew TYPE j_1bnfdoc-ntgew
           p_brgew TYPE j_1bnfdoc-brgew.

  CLEAR: p_ntgew, p_brgew.

  LOOP AT t_j_1bnflin.

    ADD t_j_1bnflin-menge TO p_brgew.

    ADD t_j_1bnflin-menge TO p_ntgew.

  ENDLOOP.

ENDFORM.                    " ZF_RECUPERA_PESO_AUX

*&---------------------------------------------------------------------*
*&      Form  ZF_INSERE_ITEM_CAB
*&---------------------------------------------------------------------*
FORM zf_insere_item_cab .

  READ TABLE t_zbrnfe_danfe_item WITH KEY num = c_num_cab.
  IF sy-subrc NE 0.
    CLEAR t_zbrnfe_danfe_item.
*   NUM - Sequencial do item (chave)
    t_zbrnfe_danfe_item-num = c_num_cab.
*   TIPODESC - Tp.Desc (1: Simples; 2: Elemento Texto; 3: Mult. Linhas)
    t_zbrnfe_danfe_item-tipodesc = '3'.

    APPEND t_zbrnfe_danfe_item.

  ENDIF.

ENDFORM.                    " ZF_INSERE_ITEM_TOT
*&---------------------------------------------------------------------*
*&      Form  reducao_base
*&---------------------------------------------------------------------*
* Problema de reducao de base PR/PR
*----------------------------------------------------------------------*
FORM reducao_base
  TABLES  p_partner STRUCTURE j_1bnfnad
          p_item    STRUCTURE j_1bnflin
          p_tax     STRUCTURE j_1bnfstx
  USING   p_header  TYPE j_1bnfdoc.

* Tipos locais
  TYPES:
    BEGIN OF tp_vbrp,
      vbeln     TYPE vbrp-vbeln,
      posnr     TYPE vbrp-posnr,
      auart     TYPE vbak-auart,
      j_1btxsdc TYPE vbrp-j_1btxsdc,
    END OF tp_vbrp.

* Variáveis locais
  DATA:
    vl_diftemp   TYPE j_1bnfstx-othbas,
    vl_emissor   TYPE j_1bnfnad-parid,
    vl_recebedor TYPE j_1bnfnad-parid,
    vl_regio     TYPE kna1-regio.

  RANGES: r_j_1btxsdc FOR vbap-j_1btxsdc.

* Tabelas internas locais
  DATA tl_item_ndx TYPE HASHED TABLE OF j_1bnflin
    WITH UNIQUE KEY docnum itmnum.
  DATA lw_t001w TYPE t001w.
  DATA l_tabix TYPE sy-tabix.
  DATA lw_item_tax TYPE j_1bnfstx.
  DATA lw_item     TYPE j_1bnflin.
  DATA lw_partner  TYPE j_1bnfnad.
  DATA lw_vbrp TYPE tp_vbrp.
  DATA lw_vbrk TYPE vbrk.
  DATA lt_xvbrp TYPE TABLE OF vbrpvb WITH HEADER LINE.
  FIELD-SYMBOLS <f> TYPE any.

  DATA: vl_fkdat LIKE vbrk-fkdat.

  r_j_1btxsdc-sign = 'I'.
  r_j_1btxsdc-option = 'EQ'.
  r_j_1btxsdc-low = 'I1'.
  APPEND r_j_1btxsdc.
  r_j_1btxsdc-low = 'I2'.
  APPEND r_j_1btxsdc.
  r_j_1btxsdc-low = 'I3'.
  APPEND r_j_1btxsdc.
  r_j_1btxsdc-low = 'I4'.
  APPEND r_j_1btxsdc.

  CHECK p_header-docdat > '20060701'.

* Obtém parceiro emissor
  READ TABLE p_partner INTO lw_partner
    WITH KEY parvw = 'AG'.
  IF sy-subrc = 0.
    vl_emissor = lw_partner-parid.
  ELSE.
    EXIT.
  ENDIF.

* Valida região do emissor (tem que ser PR)
  SELECT SINGLE regio
    INTO vl_regio
    FROM kna1
    WHERE kunnr = vl_emissor.
  CHECK vl_regio = 'PR'.

* Obtém parceiro recebedor
  READ TABLE p_partner INTO lw_partner
    WITH KEY parvw = 'WE'.
  IF sy-subrc = 0.
    vl_recebedor = lw_partner-parid.
  ENDIF.

  tl_item_ndx[] = p_item[].

* Verifica itens de imposto com "outras bases"
  LOOP AT p_tax INTO lw_item_tax
    WHERE taxtyp = 'ICM3'.

    l_tabix = sy-tabix.

*   Obtém item da nota
    READ TABLE tl_item_ndx INTO lw_item
      WITH TABLE KEY docnum = lw_item_tax-docnum
                     itmnum = lw_item_tax-itmnum.
    CHECK sy-subrc = 0.
    CHECK lw_item-werks = 'BR01'.

*   Obtém itens documento faturamento
    SELECT SINGLE vbrp~vbeln vbrp~posnr
                  vbak~auart vbrp~j_1btxsdc
      INTO lw_vbrp
      FROM vbrp
        JOIN vbak ON vbrp~aubel = vbak~vbeln
      WHERE vbrp~vbeln = lw_item-refkey(10)
        AND vbrp~posnr = lw_item-refitm.

    IF sy-subrc <> 0.
      ASSIGN ('(SAPMV60A)XVBRP[]') TO <f>.
      IF sy-subrc IS INITIAL.
        lt_xvbrp[] = <f>.
        READ TABLE lt_xvbrp WITH KEY
          posnr = lw_item-refitm.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING lt_xvbrp TO lw_vbrp.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK NOT lw_vbrp IS INITIAL.

    CLEAR: lw_t001w, vl_fkdat, vl_regio.

    SELECT SINGLE land1 regio
      FROM t001w
      INTO (lw_t001w-land1,lw_t001w-regio)
      WHERE werks = lw_item-werks.

    SELECT SINGLE fkdat
      FROM vbrk
      INTO vl_fkdat
      WHERE vbeln = lw_vbrp-vbeln.

    IF sy-subrc <> 0.
      ASSIGN ('(SAPMV60A)VBRK') TO <f>.
      IF sy-subrc IS INITIAL.
        lw_vbrk = <f>.
        vl_fkdat = lw_vbrk-fkdat.
      ENDIF.
    ENDIF.

    SELECT SINGLE regio
      FROM kna1
      INTO vl_regio
      WHERE kunnr = vl_recebedor.

    SELECT * UP TO 1 ROWS
      FROM j_1btxic3
      WHERE land1     = lw_t001w-land1
        AND shipfrom  = lw_t001w-regio
        AND shipto    = vl_regio
        AND gruop     = '10'
        AND value     = vl_recebedor
        AND value2    = ' '
        AND value3    = ' '
        AND validfrom >= vl_fkdat
        AND validto   <= vl_fkdat.
    ENDSELECT.

    IF sy-subrc = 0.

      IF lw_item_tax-othbas <> 0
        AND lw_vbrp-j_1btxsdc IN r_j_1btxsdc.

        ADD lw_item_tax-othbas TO lw_item_tax-base.

*        ADD lw_item_tax-othbas TO p_docdx-icmsbase.

        CLEAR lw_item_tax-othbas.

        MODIFY p_tax FROM lw_item_tax INDEX l_tabix
          TRANSPORTING base othbas.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " REDUCAO_BASE
*&---------------------------------------------------------------------*
*&      Form  ZF_INS_ADIC_MERC_REMETIDA
*&---------------------------------------------------------------------*
FORM zf_ins_adic_merc_remetida .
  DATA lw_kna1 TYPE kna1.
  DATA v_nempresa(200)   TYPE c.
  DATA v_cadastro(16).

  DATA l_itmtyp TYPE j_1bnflin.

  LOOP AT t_j_1bnflin INTO l_itmtyp
    WHERE itmtyp EQ '62' OR
          itmtyp EQ 'Z8' OR
          itmtyp EQ 'Z9'.
  ENDLOOP.

  IF l_itmtyp IS NOT INITIAL.

* Mensagem para tipo de item 62 ou Z8 ou Z9 (ZRCC ou ZFC3 ou ZRC3)
    SELECT SINGLE *
      FROM j_1bnfnad
     WHERE docnum = t_j_1bnfdoc-docnum
       AND parvw  = 'ZC'.

    IF sy-subrc NE 0.
      SELECT SINGLE *
        FROM j_1bnfnad
       WHERE docnum = t_j_1bnfdoc-docnum
         AND parvw  = 'WE'.
    ENDIF.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE *
      FROM kna1
      INTO lw_kna1
     WHERE kunnr = j_1bnfnad-parid.

    IF lw_kna1-name2 IS INITIAL.
      v_nempresa = lw_kna1-name1.
    ELSE.
      CONCATENATE lw_kna1-name1 lw_kna1-name2 INTO v_nempresa.
    ENDIF.

    IF lw_kna1-stcd1 IS INITIAL.
      v_cadastro = lw_kna1-stcd2.
    ELSE.
      v_cadastro = lw_kna1-stcd1.
    ENDIF.

    CONDENSE: v_cadastro, v_nempresa.

    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

    CONCATENATE 'Mercadoria Remetida p/ Conta e Ordem p/ Empresa '
    v_nempresa ', CNPJ ' v_cadastro
    ', I.E. ' lw_kna1-stcd3 ', Cidade ' lw_kna1-mcod3 ', Vila '
    lw_kna1-ort02
    INTO t_zbrnfe_danfe_dados_adic-descricao SEPARATED BY space.

    APPEND t_zbrnfe_danfe_dados_adic.

    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.

    CONCATENATE space
                sy-datum+6(2)
                '/'
                sy-datum+4(2)
                '/'
                sy-datum(4)
           INTO t_zbrnfe_danfe_dados_adic-descricao.

    CONCATENATE 'Conforme NF ___________ de'
    t_zbrnfe_danfe_dados_adic-descricao
    INTO t_zbrnfe_danfe_dados_adic-descricao SEPARATED BY space.

    CONDENSE t_zbrnfe_danfe_dados_adic-descricao.

    APPEND t_zbrnfe_danfe_dados_adic.
  ENDIF.

* Venda Para Consumidor Final
  SELECT SINGLE *
    FROM vbrp
   WHERE vbeln EQ t_j_1bnflin-refkey.

  IF vbrp-j_1btxsdc(1) EQ 'C'.
    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha     = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc  = '1'.
    t_zbrnfe_danfe_dados_adic-descricao = TEXT-s35. "Venda para Consumo
    APPEND t_zbrnfe_danfe_dados_adic.
  ENDIF.

ENDFORM.                    " ZF_INS_ADIC_MERC_REMETIDA
*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_TRANSPORTE
*&---------------------------------------------------------------------*
* Carrega transportadora de frete
*----------------------------------------------------------------------*
FORM zf_carrega_transporte_frete.

  DATA:
    BEGIN OF lw_vbfa,
      vbelv TYPE vbfa-vbelv,
      posnv TYPE vbfa-posnv,
      vbeln TYPE vbfa-vbeln,
      posnn TYPE vbfa-posnn,
    END OF lw_vbfa.

  DATA vl_fkart      TYPE vbrk-fkart.
  DATA vl_partner_id TYPE j_1bnfnad-parid.
  DATA zvbpa         TYPE vbpa.

* Reinaldo/FH - 20/08/2012 - Início - Novo fluxo de SD
  DATA: lv_tknum TYPE vttk-tknum.
  DATA: vl_vbelv TYPE vbfa-vbelv.
* Reinaldo/FH - 20/08/2012 - Fim - Novo fluxo de SD

  IF gt_vbrp[] IS NOT INITIAL.

    READ TABLE gt_vbrp INDEX 1.

    SELECT vbelv
           posnv
           vbeln
           posnn
      INTO lw_vbfa
      FROM vbfa
       FOR ALL ENTRIES IN gt_vbrp
     WHERE vbelv   = gt_vbrp-vgbel
       AND vbtyp_v = 'J'
       AND vbtyp_n = '8'.
    ENDSELECT.

    CLEAR: lv_tknum.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM vttk
        INTO lw_vttk
        WHERE tknum = lw_vbfa-vbeln.

      IF sy-subrc IS INITIAL.

        CLEAR t_parceiro.
        t_parceiro-docnum = t_j_1bnfdoc-docnum.
        t_parceiro-parvw = c_transp_frete.
        t_parceiro-parid = lw_vttk-tdlnr.

        CALL FUNCTION 'J_1B_NF_VENDOR_READ'
          EXPORTING
            partner_id        = lw_vttk-tdlnr
            read_address      = ''
          IMPORTING
            parnad            = t_parceiro-dados
          EXCEPTIONS
            partner_not_found = 1
            address_not_found = 2
            OTHERS            = 3.

        IF sy-subrc EQ 0.
          APPEND t_parceiro.
        ENDIF.

      ENDIF.
    ELSE.

      SELECT vbelv posnv vbeln posnn
        INTO lw_vbfa
        FROM vbfa
        UP TO 1 ROWS
        WHERE vbeln   = t_j_1bnflin-refkey
          AND vbtyp_v = 'C'.
      ENDSELECT.

      IF sy-subrc = 0.

        CLEAR vl_vbelv.
        vl_vbelv = lw_vbfa-vbelv.

        SELECT vbelv posnv vbeln posnn
          INTO lw_vbfa
          FROM vbfa
          UP TO 1 ROWS
          WHERE vbeln   = lw_vbfa-vbelv
            AND vbtyp_v = 'M'.
        ENDSELECT.

        IF sy-subrc = 0.

          SELECT vbelv posnv vbeln posnn
            INTO lw_vbfa
            FROM vbfa
            UP TO 1 ROWS
            WHERE vbeln   = lw_vbfa-vbelv
              AND vbtyp_v = 'J'.
          ENDSELECT.

        ENDIF.

        IF sy-subrc = 0 AND lv_tknum IS INITIAL.

          SELECT vbelv posnv vbeln posnn
            INTO lw_vbfa
            FROM vbfa
            UP TO 1 ROWS
            WHERE vbelv   = lw_vbfa-vbelv
              AND vbtyp_n = '8'.
          ENDSELECT.

          lv_tknum = lw_vbfa-vbeln.

        ENDIF.

        IF sy-subrc = 0.

          SELECT SINGLE *
            FROM vttk
            INTO lw_vttk
           WHERE tknum = lv_tknum.

          IF sy-subrc IS INITIAL.

            CLEAR t_parceiro.
            t_parceiro-docnum = t_j_1bnfdoc-docnum.
            t_parceiro-parvw = c_transp_frete.
            t_parceiro-parid = lw_vttk-tdlnr.

            CALL FUNCTION 'J_1B_NF_VENDOR_READ'
              EXPORTING
                partner_id        = lw_vttk-tdlnr
                read_address      = ''
              IMPORTING
                parnad            = t_parceiro-dados
              EXCEPTIONS
                partner_not_found = 1
                address_not_found = 2
                OTHERS            = 3.

            IF sy-subrc EQ 0.
              APPEND t_parceiro.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.




* Para Transportadores para NF SEM REMESSA
    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE fkart
        FROM vbrk
        INTO vl_fkart
       WHERE vbeln EQ t_j_1bnflin-refkey.

      IF sy-subrc EQ 0.

        IF vl_fkart NE 'ZBBB'.
          SELECT *
            FROM vbpa
            INTO zvbpa
             FOR ALL ENTRIES IN gt_vbrp
           WHERE vbeln = gt_vbrp-vgbel
             AND parvw IN ('BA','EP').
          ENDSELECT.

          IF sy-subrc IS INITIAL.
            CLEAR t_parceiro.
            t_parceiro-docnum = t_j_1bnfdoc-docnum.
            t_parceiro-parvw  = c_transp_frete.
            vl_partner_id     = zvbpa-lifnr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vl_partner_id
              IMPORTING
                output = vl_partner_id.

            CALL FUNCTION 'J_1B_NF_VENDOR_READ'
              EXPORTING
                partner_id        = vl_partner_id
                read_address      = ''
              IMPORTING
                parnad            = t_parceiro-dados
              EXCEPTIONS
                partner_not_found = 1
                address_not_found = 2
                OTHERS            = 3.

            IF sy-subrc EQ 0.
              APPEND t_parceiro.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.
*{Inicio 03.03.2016 INC795942 - Não considera na categ nf = 63 - Importação
    IF t_j_1bnfdoc-nftype <> '63'.
*} fim INC795942 - Não considera na categ nf = 63 - Importação
      CLEAR t_parceiro.
      t_parceiro-docnum = t_j_1bnfdoc-docnum.
      t_parceiro-parvw  = c_transp_frete.
      vl_partner_id     = t_j_1bnfdoc-shpmrk.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vl_partner_id
        IMPORTING
          output = vl_partner_id.

      CALL FUNCTION 'J_1B_NF_VENDOR_READ'
        EXPORTING
          partner_id        = vl_partner_id
          read_address      = ''
        IMPORTING
          parnad            = t_parceiro-dados
        EXCEPTIONS
          partner_not_found = 1
          address_not_found = 2
          OTHERS            = 3.

      IF sy-subrc EQ 0.
        APPEND t_parceiro.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_CARREGA_TRANSPORTE
*&---------------------------------------------------------------------*
*&      Form  ZF_CRIA_COD_DADOS_DANFE
*&---------------------------------------------------------------------*
*       Compor código de Dados da NF-e
*----------------------------------------------------------------------*
FORM f_dados_nfe USING ps_j_1bnfdoc TYPE j_1bnfdoc
                       ps_j_1binnad TYPE j_1binnad "Dados do recebedor
                       ps_j_1bnfe_active TYPE j_1bnfe_active
              CHANGING ps_danfe_cabecalho TYPE zbrnfe_danfe_cabecalho.

  DATA: l_valnota TYPE char15,
        l_icmsp   TYPE char1,
        l_icmss   TYPE char1.

  DATA: lv_index TYPE i VALUE 35,
        lv_step  TYPE i VALUE 1,
        lv_value TYPE i,
        lv_total TYPE i,
        lv_cd    TYPE i.

  DATA vl_cuf(2)   TYPE c.
  DATA vl_cnpj(14) TYPE n.

  DATA: lw_location_data    TYPE com_jur,
        lw_location_results TYPE com_jur,
        lt_location_results TYPE TABLE OF com_jur.



  l_valnota = ps_danfe_cabecalho-imp_valnota.

  REPLACE ALL OCCURRENCES OF '.' IN l_valnota WITH space.

  CONDENSE l_valnota NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_valnota
    IMPORTING
      output = l_valnota.

* Destaque de ICMS
  IF ps_danfe_cabecalho-imp_valicms IS INITIAL.
    l_icmsp = '2'. "Não há destaque de ICMS próprio
  ELSE.
    l_icmsp = '1'. "Há destaque de ICMS próprio
  ENDIF.

  IF ps_danfe_cabecalho-imp_valoricmssubst IS INITIAL.
    l_icmss = '2'. "Não há destaque de ICMS por substituição tributária
  ELSE.
    l_icmss = '1'. "Há destaque de ICMS por substituição tributária
  ENDIF.

* Domicílio Fiscal do Parceiro Recebedor
  IF ps_j_1binnad-land1 EQ 'BR'.
    IF ps_j_1binnad-txjcd+3(2) IS INITIAL.

      lw_location_data-country = 'BR'.              "Pais
      lw_location_data-state =  ps_j_1binnad-regio. "Regiao
      lw_location_data-county = ps_j_1binnad-ort02. "Bairro
      lw_location_data-city =  ps_j_1binnad-ort01.  "Cidade
      lw_location_data-zipcode = ps_j_1binnad-pstlz."Cep

      CALL FUNCTION 'J_1BTAXJUR_DETERMINE_NEW'
        EXPORTING
          location_data    = lw_location_data
* IMPORTING
*         LOCATION_ERR     =
        TABLES
          location_results = lt_location_results.

      READ TABLE lt_location_results INDEX 1 INTO lw_location_results.
      vl_cuf = lw_location_results-txjcd+3(2).

    ELSE.
      vl_cuf = ps_j_1binnad-txjcd+3(2).
    ENDIF.
    IF ps_j_1binnad-cgc IS INITIAL.
      vl_cnpj = ps_j_1binnad-cpf.
    ELSE.
      vl_cnpj = ps_j_1binnad-cgc.
    ENDIF.
  ELSE.
    vl_cuf = '99'.
    vl_cnpj = '00000000000000'.
  ENDIF.


  CONCATENATE vl_cuf "dom fiscal do parceiro recebedor
              '5'     "Tipo de emissão
              vl_cnpj "CNPJ Dados do recebedor
              l_valnota+1(14)
              l_icmsp
              l_icmss
              ps_danfe_cabecalho-dtemi+6(2)
         INTO ps_danfe_cabecalho-dados_nfe.

* Calcular dígito verificador
  DO 35 TIMES.
    ADD 1 TO lv_step.
    IF lv_step > 9.
      lv_step = 2.
    ENDIF.

    lv_index = lv_index - 1.

    lv_value = ps_danfe_cabecalho-dados_nfe+lv_index(1).

    lv_value = lv_value * lv_step.

    lv_total = lv_total + lv_value.
  ENDDO.

  lv_value = lv_total MOD 11.

  IF lv_value = 0 OR
     lv_value = 1.
    lv_cd = 0.
  ELSE.
    lv_cd = 11 - lv_value.
  ENDIF.

  ps_danfe_cabecalho-dados_nfe+35(1) = lv_cd.

ENDFORM.                    " F_DADOS_NFE

*&---------------------------------------------------------------------*
*&      Form  DATA_PE
*&---------------------------------------------------------------------*
FORM data_pe .

  CHECK t_j_1bnfdoc-bukrs = '1003' AND t_j_1bnfdoc-branch EQ '0008'.

  CLEAR t_zbrnfe_danfe_dados_adic.
  ADD 1 TO g_linha.
  t_zbrnfe_danfe_dados_adic-linha = g_linha.
  t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
  t_zbrnfe_danfe_dados_adic-descricao = TEXT-020.
  APPEND t_zbrnfe_danfe_dados_adic.


ENDFORM.                    " DATA_PE

*&---------------------------------------------------------------------*
*&      Form  MSG_NCM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM msg_ncm .

  CHECK t_j_1bnfdoc-bukrs = '1003'
  AND t_j_1bnfdoc-branch EQ '1000'
  AND t_parc_recebedor-txjcd(2) = 'EX'.

  DATA: l_qtdade(20) TYPE c,
        l_und(3)     TYPE c,
        l_ncm(16)    TYPE c.
  REFRESH t_msg_ncm.

  LOOP AT t_zbrnfe_danfe_item.
    CLEAR lw_msg_ncm.
    lw_msg_ncm-ncm = t_zbrnfe_danfe_item-ncm.
    lw_msg_ncm-und = t_zbrnfe_danfe_item-unidade.
    lw_msg_ncm-qtdade = t_zbrnfe_danfe_item-quantidade.

    COLLECT lw_msg_ncm INTO t_msg_ncm.
  ENDLOOP.

ENDFORM.                    " MSG_NCM
*&---------------------------------------------------------------------*
*&      Form  LOCAL_ENTREGA_PRESENTE
*&---------------------------------------------------------------------*
*       Local de entrega p/ presente
*----------------------------------------------------------------------*
FORM local_entrega_presente .

  DATA l_recebedor  TYPE j_1bnfcpd.

  READ TABLE t_j_1bnflin INDEX 1.

* Notas do ecommerce
  CHECK t_j_1bnfdoc-bukrs = '1003'
  AND t_j_1bnfdoc-branch EQ '0010'
  AND  t_j_1bnflin-itmtyp = 'ZH'.

* Recebedor da Mercadoria
  SELECT SINGLE *
         FROM j_1bnfcpd
         INTO l_recebedor
         WHERE docnum = t_j_1bnfdoc-docnum
           AND parvw = 'WE'.

  IF sy-subrc = 0.

* Local de entrega
    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
    t_zbrnfe_danfe_dados_adic-descricao = TEXT-047.
    APPEND t_zbrnfe_danfe_dados_adic.

* Recebedor
    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
    CONCATENATE TEXT-048  l_recebedor-name1 INTO
    t_zbrnfe_danfe_dados_adic-descricao.
    APPEND t_zbrnfe_danfe_dados_adic.

* Endereço
    CLEAR t_zbrnfe_danfe_dados_adic.
    ADD 1 TO g_linha.
    t_zbrnfe_danfe_dados_adic-linha = g_linha.
    t_zbrnfe_danfe_dados_adic-tipodesc = '1'.
    CONCATENATE 'Rua:' l_recebedor-street ', N° ' l_recebedor-house_num1
    ', ' l_recebedor-ort01 '-' l_recebedor-regio ', CEP:'
    l_recebedor-pstlz INTO t_zbrnfe_danfe_dados_adic-descricao.
    APPEND t_zbrnfe_danfe_dados_adic.
  ENDIF.


ENDFORM.                    " LOCAL_ENTREGA_PRESENTE
*{Alter - Jeferson Evangelista - 14.03.2011 - Projeto MV
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_IMPRESSAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_IMPRIMIR  text
*----------------------------------------------------------------------*
FORM verifica_impressao CHANGING p_imprimir.
* Alteração necessária para evitar que em caso de Pré Venda Presente
* (ZVPR) a segunda NFe(Danfe) não saia na impressora. Isso porque :
* 1 - A primeira nota é enviada via correio (com preço)
* 2 - A segunda nota é apenas para destaque de ICMS ( no momento da
*     circulação da mercadoria.
* 3 - A terceira nota é sem preço, já que é para presente, e deve
*     acompanhar a mercadoria.
* Essa alteração foi requisitada pela área de negócio
*  ( Expedição/Logística - Antonio Fernandes dos Santos Neto ).
* Isso pelo fato da geração das 3 notas poder ocasionar erros no momento
* da embalagem, enviando uma Danfe errada para o cliente e também o
* desperdício de papel.

* Constante
  CONSTANTS:
    c_fatura        TYPE vbfa-vbtyp_v VALUE 'M',
    c_ordem         TYPE vbfa-vbtyp_v VALUE 'C',
    c_ndebito       TYPE vbfa-vbtyp_v VALUE 'L',
    c_origem_fatura TYPE vbak-auart   VALUE 'ZVEF',
    c_origem_ov     TYPE vbak-auart   VALUE 'ZVPR'.

  "Declaração
  DATA:
    lw_lin TYPE j_1bnflin,
    BEGIN OF lt_vbfa OCCURS 0,
      vbeln TYPE vbak-vbeln,
      auart TYPE vbak-auart,
      vbtyp TYPE vbfa-vbtyp_v,
    END OF lt_vbfa.

  " Por default, imprime
  p_imprimir = 'X'.

  " Encontra o número da fatura
  READ TABLE t_j_1bnflin WITH KEY docnum = t_j_1bnfdoc-docnum
                          INTO lw_lin.
  CHECK sy-subrc EQ 0.

  " Encontra Ordens Origem da fatura
  SELECT vbak~vbeln vbak~auart vbfa~vbtyp_v
    FROM vbfa INNER JOIN vbak
    ON vbfa~vbelv EQ vbak~vbeln
    INTO TABLE lt_vbfa
    WHERE vbfa~vbeln EQ lw_lin-refkey
      AND vbfa~vbtyp_n EQ c_fatura      "Fatura
      AND vbfa~vbtyp_v IN (c_ordem,     " Ordem
                         c_ndebito ).   "Solicitação de Nota de Débito
  CHECK sy-subrc EQ 0.

  "Verifica se ordem origem da fatura é uma ZVEF
  READ TABLE lt_vbfa WITH KEY
                   auart = c_origem_fatura
                   vbtyp = c_ndebito TRANSPORTING NO FIELDS.
  CHECK sy-subrc EQ 0.

  "Chegou aqui, é origem de ZVEF, agora identifica se origem da ZVEF
  "é ZVPR
  READ TABLE lt_vbfa WITH KEY
                   auart = c_origem_ov
                   vbtyp = c_ordem TRANSPORTING NO FIELDS.
  CHECK sy-subrc EQ 0.

  "Chegou aqui, é de origem ZVPR, logo fatura não deve ser impressa
  CLEAR p_imprimir.


ENDFORM.                    " VERIFICA_IMPRESSAO

FORM zf_limpa_variaveis .

  CLEAR   t_danfe_cabecalho.
  REFRESH t_zbrnfe_danfe_item.
  REFRESH t_zbrnfe_danfe_item_desc.
  REFRESH t_zbrnfe_danfe_fatura.
  REFRESH t_zbrnfe_danfe_dados_adic.
  REFRESH t_zbrnfe_danfe_cfop.

ENDFORM.

FORM f_get_data_hora_utc  USING p_data_hora
                       CHANGING c_data
                                c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  PERFORM f_get_data_utc USING p_data_hora
                      CHANGING c_data.

  PERFORM f_get_hora_utc USING p_data_hora
                      CHANGING c_hora.

ENDFORM.


FORM f_get_data_utc USING p_data_hora
                 CHANGING c_data.

  CHECK p_data_hora IS NOT INITIAL.

  c_data = p_data_hora(4) &&
           p_data_hora+05(02) &&
           p_data_hora+08(02).

ENDFORM.

FORM f_get_hora_utc USING p_data_hora
                 CHANGING c_hora.

  CHECK p_data_hora IS NOT INITIAL.

  c_hora  = p_data_hora+11(02) &&
            p_data_hora+14(02) &&
            p_data_hora+17(02).

ENDFORM.
