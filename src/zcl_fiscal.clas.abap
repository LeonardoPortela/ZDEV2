class ZCL_FISCAL definition
  public
  create public .

public section.

  data AT_RETORNO type BAPIRET2_T .
  data AT_RETORNO_ESTORNO type BAPIRET2_T .

  methods CRIAR
    importing
      !I_CABECALHO type ZDE_FISCAL_CABECALHO
      !I_ITENS type ZDE_FISCAL_ITENS_T
      !I_IMPOSTOS type ZDE_FISCAL_IMPOSTOS_T
      !I_PARCEIROS type ZDE_FISCAL_PARCEIROS_T
      !I_BAPI_WAIT type BAPIWAIT default 'X'
      !I_OBS_FISCAL type STRING optional
      !I_OBS_CONTRIBUINTE type STRING optional
    exporting
      !E_DOCNUM type J_1BDOCNUM
      !E_RETORNO type BAPIRET2_T
    returning
      value(R_GEROU) type CHAR01
    exceptions
      DATA_FI_MM_NAO
      NAO_CTE_FORN
      DOCUMENTO_EXISTE
      ERRO .
  methods CRIAR_MDFE
    importing
      !I_CABECALHO type ZDE_FISCAL_CABECALHO
      !I_ITENS type ZDE_FISCAL_ITENS_T
      !I_IMPOSTOS type ZDE_FISCAL_IMPOSTOS_T optional
      !I_PARCEIROS type ZDE_FISCAL_PARCEIROS_T optional
      !I_BAPI_WAIT type BAPIWAIT default 'X'
      !I_OBS_FISCAL type STRING optional
      !I_OBS_CONTRIBUINTE type STRING optional
      !I_TRAILER_INFO type BAPI_J_1BNFTRAILER_TAB optional
    exporting
      !E_DOCNUM type J_1BDOCNUM
      !E_RETORNO type BAPIRET2_T
    returning
      value(R_GEROU) type CHAR01
    exceptions
      DATA_FI_MM_NAO
      NAO_CTE_FORN
      DOCUMENTO_EXISTE
      ERRO .
  methods ESTORNAR
    importing
      !I_DOC_NUMBER type J_1BNFDOC-DOCNUM
      !I_REF_TYPE type J_1BNFLIN-REFTYP
      !I_REF_KEY type J_1BNFLIN-REFKEY
      !I_CAN_DAT type J_1BNFDOC-CANDAT optional
      !I_CRE_NAM type J_1BNFDOC-CRENAM optional
      !I_DOC_DAT type J_1BNFDOC-DOCDAT optional
      !I_PST_DAT type J_1BNFDOC-PSTDAT optional
      !I_BEL_NR type J_1BNFDOC-BELNR optional
      !I_G_JAHR type J_1BNFDOC-GJAHR optional
      !I_BAPI_WAIT type CHAR01 default ABAP_FALSE
    exporting
      !E_DOCNUM type J_1BDOCNUM
      !E_RETORNO type BAPIRET2_T
    returning
      value(R_GEROU) type CHAR01 .
  class-methods VERIFICAR_CRIAR
    importing
      !I_DATA type DATUM
      !I_BUKRS type BUKRS
      !I_CHAVE type J_1B_NFE_ACCESS_KEY_DTEL44 optional
      !I_FORM type J_1BFORM
    exporting
      !E_ZIB_NFE_FORN type ZIB_NFE_FORN
    returning
      value(R_DATA_VAL) type DATUM
    exceptions
      DATA_FI_MM_NAO
      NAO_CTE_FORN
      DOCUMENTO_EXISTE .
  class-methods GET_TIPO_IMPOSTO
    importing
      !I_TAXGRP type J_1BTAXGRP
    returning
      value(E_KSCHL) type KSCHL
    exceptions
      ERRO .
  class-methods GET_DOCUMENTO_CHAVE
    importing
      !I_CHAVE type J_1B_NFE_ACCESS_KEY_DTEL44
      !I_PROPRIA type CHAR01
    exporting
      !E_J_1BNFE_ACTIVE type J_1BNFE_ACTIVE
    returning
      value(R_DOCNUM) type J_1BDOCNUM
    exceptions
      ERRO .
  PROTECTED SECTION.
private section.

  data AT_DOCNUM type J_1BDOCNUM .
  data AT_DOCNUM_ESTORNO type J_1BDOCNUM .

  methods LIMPAR .
ENDCLASS.



CLASS ZCL_FISCAL IMPLEMENTATION.


  METHOD criar.

    TYPES BEGIN OF ty_export_lines.
    TYPES: linha TYPE c LENGTH 72.
    TYPES END OF ty_export_lines.

    DATA: wa_obj_header   TYPE bapi_j_1bnfdoc,
          wa_obj_partner  TYPE bapi_j_1bnfnad,
          it_obj_partner  TYPE TABLE OF bapi_j_1bnfnad,
          wa_obj_item     TYPE bapi_j_1bnflin,
          it_obj_item     TYPE TABLE OF bapi_j_1bnflin,
          wa_obj_item_tax TYPE bapi_j_1bnfstx,
          it_obj_item_tax TYPE TABLE OF bapi_j_1bnfstx,
          wa_header_msg   TYPE bapi_j_1bnfftx,
          it_header_msg   TYPE TABLE OF bapi_j_1bnfftx,
          it_return	      TYPE TABLE OF	bapiret2,
          wa_nfcheck      TYPE bapi_j_1bnfcheck,
          wa_mara         TYPE mara,
          wa_makt         TYPE makt,
          longtext_tab    TYPE catsxt_longtext_itab.
    "IT_OBSERV_FIS   TYPE TABLE OF TY_EXPORT_LINES.

    me->limpar( ).

    SELECT SINGLE * INTO @DATA(wa_j_1baa)
      FROM j_1baa
     WHERE nftype EQ @i_cabecalho-nftype.

    wa_obj_header-nftype      = i_cabecalho-nftype.

    "Caracteristica do tipo do documento para transferir para o novo documento
    MOVE-CORRESPONDING wa_j_1baa TO wa_obj_header.

    "Documento de Referência Complementado/Anulado
    wa_obj_header-docref      = i_cabecalho-docref.

    "Tipo Finalidade de Emissão
    "WA_OBJ_HEADER-DOCTYP      = I_CABECALHO-DOCTYP.

    "Status SAP do Documento
    wa_obj_header-docstat     = i_cabecalho-docstat.
    "Parceiro
    wa_obj_header-parid       = i_cabecalho-parid.
    "Data de Emissão
    wa_obj_header-docdat      = i_cabecalho-docdat.
    "Data de Movimento
    wa_obj_header-pstdat      = i_cabecalho-pstdat.
    "Empresa
    wa_obj_header-bukrs       = i_cabecalho-bukrs.
    "Local de Negócio
    wa_obj_header-branch      = i_cabecalho-branch.
    "Moeda
    wa_obj_header-waerk       = i_cabecalho-waerk.
    "Série
    wa_obj_header-series      = i_cabecalho-series.
    "Lançamento Manual
    wa_obj_header-manual      = i_cabecalho-manual.

    "Unidade de Medida do Peso Líquido e Bruto
    wa_obj_header-gewei       = i_cabecalho-gewei.
    "Peso Bruto
    wa_obj_header-brgew       = i_cabecalho-brgew.
    "Peso Líquido
    wa_obj_header-ntgew       = i_cabecalho-ntgew.

    IF wa_j_1baa-nfe EQ abap_true.
      "Número Eletrônico
      wa_obj_header-nfenum      = i_cabecalho-nfenum.
      "Chave de Acesso
      wa_obj_header-access_key  = i_cabecalho-access_key .
      "Versão da XML do Documento
      wa_obj_header-xmlvers     = i_cabecalho-xmlvers.
      "Status SEFAZ Código de Sucesso/Erro
      wa_obj_header-code        = i_cabecalho-code.
      "Modal do Transporte
      wa_obj_header-transp_mode = i_cabecalho-transp_mode.
    ENDIF.

    LOOP AT i_itens INTO DATA(wa_itens).

      "Número do Item
      wa_obj_item-itmnum  = wa_itens-itmnum.
      "Tipo do Item da Nota Fiscal
      wa_obj_item-itmtyp  = wa_itens-itmtyp.
      "Área de Avaliação
      wa_obj_item-bwkey   = wa_itens-bwkey.
      "Centro
      wa_obj_item-werks   = wa_itens-werks.
      "Quantidade
      wa_obj_item-menge   = wa_itens-menge.
      "Unidade
      wa_obj_item-meins   = wa_itens-meins.
      "Despesas líquidas em moeda do documento
      wa_obj_item-netoth  = wa_itens-netoth.
      "Preço Líquido
      wa_obj_item-netpr   = wa_itens-netpr.
      "Valor Líquido
      wa_obj_item-netwr   = wa_itens-netwr.

      "WA_OBJ_ITEM-NFPRI   = WA_ITENS-NFPRI.
      "WA_OBJ_ITEM-NFNET   = WA_ITENS-NFNET.

      "CFOP
      wa_obj_item-cfop_10 = wa_itens-cfop_10.

      "ICMS
      wa_obj_item-taxlw1  = wa_itens-taxlw1.
      wa_obj_item-taxsit  = wa_itens-taxsit.

      "IPI
      wa_obj_item-taxlw2  = wa_itens-taxlw2.
      wa_obj_item-taxsi2  = wa_itens-taxsi2.

      "COFINS
      wa_obj_item-taxlw4  = wa_itens-taxlw4.
      wa_obj_item-taxsi4  = wa_itens-taxsi4.

      "PIS
      wa_obj_item-taxlw5  = wa_itens-taxlw5.
      wa_obj_item-taxsi5  = wa_itens-taxsi5.

      "Material
      "NCM
      "Utilização do Material
      "Origem do Material
      "Grupo do Material
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*      wa_obj_item-matnr   = wa_itens-matnr.
      DATA(v_len) = strlen( wa_itens-matnr ).
      IF v_len > 18.
        wa_obj_item-matnr_long = wa_itens-matnr.
      ELSE.
        wa_obj_item-matnr      = wa_itens-matnr.
      ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
      CALL FUNCTION 'J_1B_MATERIAL_READ' "#EC CI_FLDEXT_OK[2215424]
        EXPORTING
          matnr                = wa_obj_item-matnr
          val_area             = wa_obj_item-werks
          val_type             = space
          language             = sy-langu
          i_werks              = wa_obj_item-werks
        IMPORTING
          nbm                  = wa_obj_item-nbm
          matuse               = wa_obj_item-matuse
          matorg               = wa_obj_item-matorg
          material_record      = wa_mara
          material_text_record = wa_makt
          e_matkl              = wa_obj_item-matkl
        EXCEPTIONS
          material_not_found   = 1
          valuation_not_found  = 2
          OTHERS               = 3.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
      ENDIF.
      "Descrição do Material
      wa_obj_item-maktx = wa_makt-maktx.
      "Grupo do Material
      wa_obj_item-matkl = wa_mara-matkl.

      IF wa_itens-meins IS INITIAL.
        "Unidade de Estoque do Material
        wa_obj_item-meins = wa_mara-meins.
      ENDIF.

      "Tipo do Documento de Referência
      wa_obj_item-reftyp  = wa_itens-reftyp.
      "Item do Documento de Referência
      wa_obj_item-refitm  = wa_itens-refitm.
      "Documento do Documento de Referência
      wa_obj_item-refkey  = wa_itens-refkey.
      "Valor e preço incluíndo ICMS/ISS
      wa_obj_item-incltx  = wa_itens-incltx.

      "pedido
      wa_obj_item-xped      = wa_itens-xped.
      wa_obj_item-nitemped  = wa_itens-nitemped.

      APPEND wa_obj_item TO it_obj_item.

      "Impostos por item
      LOOP AT i_impostos INTO DATA(wa_impostos) WHERE itmnum EQ wa_itens-itmnum.
        "Número do Item
        wa_obj_item_tax-itmnum = wa_impostos-itmnum.
        "Tipo de imposto
        wa_obj_item_tax-taxtyp = wa_impostos-taxtyp.
        "Base de calculo
        wa_obj_item_tax-base   = wa_impostos-base.
        "Taxa
        wa_obj_item_tax-rate   = wa_impostos-rate.
        "Valor do Imposto
        wa_obj_item_tax-taxval = wa_impostos-taxval.
        "Exclusão da Base de Calculo
        wa_obj_item_tax-excbas = wa_impostos-excbas.
        "Outros Montante da Base
        wa_obj_item_tax-othbas = wa_impostos-othbas.
        APPEND wa_obj_item_tax TO it_obj_item_tax.
      ENDLOOP.

    ENDLOOP.

    LOOP AT i_parceiros INTO DATA(wa_parceiros).
      "Função Parceira
      wa_obj_partner-parvw  = wa_parceiros-parvw.
      "Código do Parceiro
      wa_obj_partner-parid  = wa_parceiros-parid.
      "Tipo do Parceiro (Fornecedor/Comprador/Local de Negócio)
      wa_obj_partner-partyp = wa_parceiros-partyp.
      APPEND wa_obj_partner TO it_obj_partner.
    ENDLOOP.

    IF wa_j_1baa-form IS NOT INITIAL.
      CLEAR: wa_obj_header-access_key.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_j_1bb2)
      FROM j_1bb2
     WHERE bukrs  EQ @i_cabecalho-bukrs
       AND branch EQ @i_cabecalho-branch
       AND form   EQ @wa_j_1baa-form.

    IF sy-subrc IS INITIAL AND wa_j_1bb2-series IS NOT INITIAL.
      wa_obj_header-series = wa_j_1bb2-series.
    ELSE.
      wa_obj_header-series = i_cabecalho-series.
    ENDIF.

    CALL METHOD zcl_fiscal=>verificar_criar
      EXPORTING
        i_data           = wa_obj_header-pstdat
        i_bukrs          = wa_obj_header-bukrs
        i_chave          = wa_obj_header-access_key
        i_form           = wa_obj_header-form
      IMPORTING
        e_zib_nfe_forn   = DATA(wa_zib_nfe_forn)
      RECEIVING
        r_data_val       = wa_obj_header-pstdat
      EXCEPTIONS
        data_fi_mm_nao   = 1
        nao_cte_forn     = 2
        documento_existe = 3
        OTHERS           = 4.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING data_fi_mm_nao.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING documento_existe.
    ENDCASE.

    wa_nfcheck-chekcon = abap_true.

    IF wa_obj_header-nfenum IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_obj_header-nfenum
        IMPORTING
          output = wa_obj_header-nfenum.
    ENDIF.

    DATA: lc_seqnum	TYPE j_1bseqnum.
    lc_seqnum = 0.

    IF i_obs_fiscal IS NOT INITIAL.
      ADD 1 TO lc_seqnum.
      zcl_string=>string_to_table( EXPORTING i_string = i_obs_fiscal i_line_length = 72 IMPORTING e_table = longtext_tab ).

      LOOP AT longtext_tab INTO DATA(wa_observ_fis).
        CLEAR: wa_header_msg.
        wa_header_msg-seqnum  = lc_seqnum.
        wa_header_msg-linnum  = sy-tabix.
        wa_header_msg-message = wa_observ_fis.
        APPEND wa_header_msg TO it_header_msg.
      ENDLOOP.
    ENDIF.

    IF i_obs_contribuinte IS NOT INITIAL.
      ADD 1 TO lc_seqnum.
      zcl_string=>string_to_table( EXPORTING i_string = i_obs_contribuinte i_line_length = 72 IMPORTING e_table = longtext_tab ).

      LOOP AT longtext_tab INTO wa_observ_fis.
        CLEAR: wa_header_msg.
        wa_header_msg-seqnum  = lc_seqnum.
        wa_header_msg-linnum  = sy-tabix.
        wa_header_msg-message = wa_observ_fis.
        wa_header_msg-manual  = abap_true.
        APPEND wa_header_msg TO it_header_msg.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header     = wa_obj_header
        nfcheck        = wa_nfcheck
      IMPORTING
        e_docnum       = e_docnum
      TABLES
        obj_partner    = it_obj_partner
        obj_item       = it_obj_item
        obj_item_tax   = it_obj_item_tax
        obj_header_msg = it_header_msg
        return         = it_return.

    MOVE it_return[] TO e_retorno.
    MOVE it_return[] TO me->at_retorno.

    IF ( e_docnum IS NOT INITIAL ) .

      me->at_docnum = e_docnum.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = i_bapi_wait.

      r_gerou = abap_true.

      IF wa_j_1baa-form IS INITIAL AND i_cabecalho-access_key IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
          FROM j_1bnfdoc
         WHERE docnum EQ @me->at_docnum.

        SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
          FROM j_1bnfe_active
         WHERE docnum EQ @me->at_docnum.

        wa_j_1bnfdoc-authcod      = wa_zib_nfe_forn-nu_protocolo.
        wa_j_1bnfdoc-docstat      = '1'.
        wa_j_1bnfe_active-authcod = wa_zib_nfe_forn-nu_protocolo.
        wa_j_1bnfe_active-docnum9 = wa_zib_nfe_forn-nu_chave_aleator.
        wa_j_1bnfe_active-docsta  = '1'.
        wa_j_1bnfe_active-cdv     = wa_zib_nfe_forn-nu_chave_dv.
        wa_j_1bnfe_active-regio   = wa_zib_nfe_forn-nu_chave_regiao.

        CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
          EXPORTING
            i_doc     = wa_j_1bnfdoc
            i_acttab  = wa_j_1bnfe_active
            i_updmode = 'U'.

        COMMIT WORK.

      ENDIF.

    ELSE.
      r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.


  METHOD criar_mdfe.

    DATA: es_cust3 TYPE  j_1bnfe_cust3.

    TYPES BEGIN OF ty_export_lines.
    TYPES: linha TYPE c LENGTH 72.
    TYPES END OF ty_export_lines.

    DATA: wa_obj_header   TYPE bapi_j_1bnfdoc,
          wa_obj_partner  TYPE bapi_j_1bnfnad,
          it_obj_partner  TYPE TABLE OF bapi_j_1bnfnad,
          wa_obj_item     TYPE bapi_j_1bnflin,
          it_obj_item     TYPE TABLE OF bapi_j_1bnflin,
          wa_obj_item_tax TYPE bapi_j_1bnfstx,
          it_obj_item_tax TYPE TABLE OF bapi_j_1bnfstx,
          wa_header_msg   TYPE bapi_j_1bnfftx,
          it_header_msg   TYPE TABLE OF bapi_j_1bnfftx,
          it_return	      TYPE TABLE OF	bapiret2,
          wa_nfcheck      TYPE bapi_j_1bnfcheck,
          wa_mara         TYPE mara,
          wa_makt         TYPE makt,
          longtext_tab    TYPE catsxt_longtext_itab.
    "IT_OBSERV_FIS   TYPE TABLE OF TY_EXPORT_LINES.

    me->limpar( ).

    SELECT SINGLE * INTO @DATA(wa_j_1baa)
      FROM j_1baa
     WHERE nftype EQ @i_cabecalho-nftype.

    wa_obj_header-nftype      = i_cabecalho-nftype.

    "Caracteristica do tipo do documento para transferir para o novo documento
    MOVE-CORRESPONDING wa_j_1baa TO wa_obj_header.

    "Documento de Referência Complementado/Anulado
    wa_obj_header-docref      = i_cabecalho-docref.
    "Status SAP do Documento
    wa_obj_header-docstat     = i_cabecalho-docstat.
    "Parceiro
    wa_obj_header-parid       = i_cabecalho-parid.
    "Tipo do Parceiro
    wa_obj_header-parvw       = i_cabecalho-parvw.
    "Data de Emissão
    wa_obj_header-docdat      = i_cabecalho-docdat.
    "Data de Movimento
    wa_obj_header-pstdat      = i_cabecalho-pstdat.
    "Empresa
    wa_obj_header-bukrs       = i_cabecalho-bukrs.
    "Local de Negócio
    wa_obj_header-branch      = i_cabecalho-branch.
    "Moeda
    wa_obj_header-waerk       = i_cabecalho-waerk.
    "Série
    wa_obj_header-series      = i_cabecalho-series.
    "Manual
    wa_obj_header-manual      = i_cabecalho-manual.
    "NF-e
    wa_obj_header-nfe         = i_cabecalho-nfe.

    wa_obj_header-anzpk       = i_cabecalho-anzpk.
    wa_obj_header-brgew       = i_cabecalho-brgew.
    wa_obj_header-ntgew       = i_cabecalho-ntgew.
    wa_obj_header-gewei       = i_cabecalho-gewei.
    wa_obj_header-rntc        = i_cabecalho-rntrc.
    wa_obj_header-modfrete    = i_cabecalho-modfrete.
    wa_obj_header-ind_final   = i_cabecalho-ind_final.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*    wa_obj_header-tpemis      = i_cabecalho-tpemis.
*    IF i_cabecalho-tpemis = '2'.
*      wa_obj_header-conting   = abap_true.
*    ENDIF.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    "Lançamento Manual
    wa_obj_header-manual      = i_cabecalho-manual.

    IF wa_j_1baa-nfe EQ abap_true.
      "Número Eletrônico
      wa_obj_header-nfenum      = i_cabecalho-nfenum.
      "Chave de Acesso
      wa_obj_header-access_key  = i_cabecalho-access_key .

      CALL FUNCTION 'J_1BNFE_CUST3_READ'
        EXPORTING
          iv_bukrs       = i_cabecalho-bukrs
          iv_branch      = i_cabecalho-branch
          iv_model       = wa_j_1baa-model
        IMPORTING
          es_cust3       = es_cust3
        EXCEPTIONS
          no_entry_found = 1
          OTHERS         = 2.

      IF sy-subrc IS INITIAL.
        "Versão da XML do Documento
        wa_obj_header-xmlvers = es_cust3-version.
      ENDIF.

      "Status SEFAZ Código de Sucesso/Erro
      wa_obj_header-code        = i_cabecalho-code.
      "Modal do Transporte
      wa_obj_header-transp_mode = i_cabecalho-transp_mode.
    ENDIF.

    LOOP AT i_itens INTO DATA(wa_itens).

      "Número do Item
      wa_obj_item-itmnum  = wa_itens-itmnum.
      "Tipo do Item da Nota Fiscal
      wa_obj_item-itmtyp  = wa_itens-itmtyp.
      "Área de Avaliação
      wa_obj_item-bwkey   = wa_itens-bwkey.
      "Centro
      wa_obj_item-werks   = wa_itens-werks.
      "Quantidade
      wa_obj_item-menge   = wa_itens-menge.
      "Unidade
      wa_obj_item-meins   = wa_itens-meins.
      "Despesas líquidas em moeda do documento
      wa_obj_item-netoth  = wa_itens-netoth.
      "Preço Líquido
      wa_obj_item-netpr   = wa_itens-netpr.
      "Valor Líquido
      wa_obj_item-netwr   = wa_itens-netwr.

      "CFOP
      wa_obj_item-cfop    = wa_itens-cfop.
      wa_obj_item-cfop_10 = wa_itens-cfop_10.

      "ICMS
      wa_obj_item-taxlw1  = wa_itens-taxlw1.
      wa_obj_item-taxsit  = wa_itens-taxsit.

      "IPI
      wa_obj_item-taxlw2  = wa_itens-taxlw2.
      wa_obj_item-taxsi2  = wa_itens-taxsi2.

      "COFINS
      wa_obj_item-taxlw4  = wa_itens-taxlw4.
      wa_obj_item-taxsi4  = wa_itens-taxsi4.

      "PIS
      wa_obj_item-taxlw5  = wa_itens-taxlw5.
      wa_obj_item-taxsi5  = wa_itens-taxsi5.

      wa_obj_item-nbm     = wa_itens-nbm.
      wa_obj_item-matkl   = wa_itens-matkl.

      "Material
      "NCM
      "Utilização do Material
      "Origem do Material
      "Grupo do Material
      IF wa_itens-matnr IS NOT INITIAL.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
*        WA_OBJ_ITEM-MATNR   = WA_ITENS-MATNR.
        DATA(v_len) = strlen( wa_itens-matnr ).
        IF v_len > 18.
          wa_obj_item-matnr_long = wa_itens-matnr.
        ELSE.
          wa_obj_item-matnr      = wa_itens-matnr.
        ENDIF.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
        CALL FUNCTION 'J_1B_MATERIAL_READ' "#EC CI_FLDEXT_OK[2215424]
          EXPORTING
            matnr                = wa_obj_item-matnr
            val_area             = wa_obj_item-werks
            val_type             = space
            language             = sy-langu
            i_werks              = wa_obj_item-werks
          IMPORTING
            nbm                  = wa_obj_item-nbm
            matuse               = wa_obj_item-matuse
            matorg               = wa_obj_item-matorg
            material_record      = wa_mara
            material_text_record = wa_makt
            e_matkl              = wa_obj_item-matkl
          EXCEPTIONS
            material_not_found   = 1
            valuation_not_found  = 2
            OTHERS               = 3.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
        ENDIF.

        "Descrição do Material
        wa_obj_item-maktx = wa_makt-maktx.

        IF wa_itens-matkl IS INITIAL.
          "Grupo do Material
          wa_obj_item-matkl = wa_mara-matkl.
        ELSE.
          wa_obj_item-matkl = wa_itens-matkl.
        ENDIF.

        IF wa_itens-meins IS INITIAL.
          "Unidade de Estoque do Material
          wa_obj_item-meins = wa_mara-meins.
        ENDIF.

      ENDIF.

      "Tipo do Documento de Referência
      wa_obj_item-reftyp  = wa_itens-reftyp.
      "Item do Documento de Referência
      wa_obj_item-refitm  = wa_itens-refitm.
      "Documento do Documento de Referência
      wa_obj_item-refkey  = wa_itens-refkey.

      APPEND wa_obj_item TO it_obj_item.

      "Impostos por item
      LOOP AT i_impostos INTO DATA(wa_impostos) WHERE itmnum EQ wa_itens-itmnum.
        "Número do Item
        wa_obj_item_tax-itmnum = wa_impostos-itmnum.
        "Tipo de imposto
        wa_obj_item_tax-taxtyp = wa_impostos-taxtyp.
        "Base de calculo
        wa_obj_item_tax-base   = wa_impostos-base.
        "Taxa
        wa_obj_item_tax-rate   = wa_impostos-rate.
        "Valor do Imposto
        wa_obj_item_tax-taxval = wa_impostos-taxval.
        "Exclusão da Base de Calculo
        wa_obj_item_tax-excbas = wa_impostos-excbas.
        "Outros Montante da Base
        wa_obj_item_tax-othbas = wa_impostos-othbas.
        APPEND wa_obj_item_tax TO it_obj_item_tax.
      ENDLOOP.

    ENDLOOP.

    LOOP AT i_parceiros INTO DATA(wa_parceiros).
      "Função Parceira
      wa_obj_partner-parvw  = wa_parceiros-parvw.
      "Código do Parceiro
      wa_obj_partner-parid  = wa_parceiros-parid.
      "Tipo do Parceiro (Fornecedor/Comprador/Local de Negócio)
      wa_obj_partner-partyp = wa_parceiros-partyp.
      APPEND wa_obj_partner TO it_obj_partner.
    ENDLOOP.

    IF wa_j_1baa-form IS NOT INITIAL.
      CLEAR: wa_obj_header-access_key.
    ENDIF.

    SELECT SINGLE * INTO @DATA(wa_j_1bb2)
      FROM j_1bb2
     WHERE bukrs  EQ @i_cabecalho-bukrs
       AND branch EQ @i_cabecalho-branch
       AND form   EQ @wa_j_1baa-form.

    IF sy-subrc IS INITIAL AND wa_j_1bb2-series IS NOT INITIAL.
      wa_obj_header-series = wa_j_1bb2-series.
    ELSE.
      wa_obj_header-series = i_cabecalho-series.
    ENDIF.

    CALL METHOD zcl_fiscal=>verificar_criar
      EXPORTING
        i_data           = wa_obj_header-pstdat
        i_bukrs          = wa_obj_header-bukrs
        i_chave          = wa_obj_header-access_key
        i_form           = wa_obj_header-form
      IMPORTING
        e_zib_nfe_forn   = DATA(wa_zib_nfe_forn)
      RECEIVING
        r_data_val       = wa_obj_header-pstdat
      EXCEPTIONS
        data_fi_mm_nao   = 1
        nao_cte_forn     = 2
        documento_existe = 3
        OTHERS           = 4.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING data_fi_mm_nao.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING documento_existe.
    ENDCASE.

    wa_nfcheck-chekcon = abap_true.

    IF wa_obj_header-nfenum IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_obj_header-nfenum
        IMPORTING
          output = wa_obj_header-nfenum.
    ENDIF.

    DATA: lc_seqnum	TYPE j_1bseqnum.
    lc_seqnum = 0.

    IF i_obs_fiscal IS NOT INITIAL.
      ADD 1 TO lc_seqnum.
      zcl_string=>string_to_table( EXPORTING i_string = i_obs_fiscal i_line_length = 72 IMPORTING e_table = longtext_tab ).

      LOOP AT longtext_tab INTO DATA(wa_observ_fis).
        CLEAR: wa_header_msg.
        wa_header_msg-seqnum  = lc_seqnum.
        wa_header_msg-linnum  = sy-tabix.
        wa_header_msg-message = wa_observ_fis.
        APPEND wa_header_msg TO it_header_msg.
      ENDLOOP.
    ENDIF.

    IF i_obs_contribuinte IS NOT INITIAL.
      ADD 1 TO lc_seqnum.
      zcl_string=>string_to_table( EXPORTING i_string = i_obs_contribuinte i_line_length = 72 IMPORTING e_table = longtext_tab ).

      LOOP AT longtext_tab INTO wa_observ_fis.
        CLEAR: wa_header_msg.
        wa_header_msg-seqnum  = lc_seqnum.
        wa_header_msg-linnum  = sy-tabix.
        wa_header_msg-message = wa_observ_fis.
        wa_header_msg-manual  = abap_true.
        APPEND wa_header_msg TO it_header_msg.
      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header       = wa_obj_header
        nfcheck          = wa_nfcheck
      IMPORTING
        e_docnum         = e_docnum
      TABLES
        obj_partner      = it_obj_partner
        obj_item         = it_obj_item
        obj_item_tax     = it_obj_item_tax
        obj_header_msg   = it_header_msg
        obj_trailer_info = i_trailer_info
        return           = it_return.

    MOVE it_return[] TO e_retorno.
    MOVE it_return[] TO me->at_retorno.

    IF ( e_docnum IS NOT INITIAL ) .

      me->at_docnum = e_docnum.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = i_bapi_wait.

      r_gerou = abap_true.

      WAIT UP TO 2 SECONDS.

      UPDATE j_1bnfdoc
         SET xmlvers = 3
       WHERE docnum EQ e_docnum
         AND docnum NE space.

      COMMIT WORK AND WAIT.

    ELSE.
      r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.


  METHOD ESTORNAR.

    DATA: WA_J_1BNFE_ACTIVE TYPE J_1BNFE_ACTIVE.

    CLEAR: E_DOCNUM, ME->AT_RETORNO_ESTORNO[], E_RETORNO, WA_J_1BNFE_ACTIVE.

    SELECT SINGLE * INTO @WA_J_1BNFE_ACTIVE
      FROM J_1BNFE_ACTIVE
     WHERE DOCNUM EQ @I_DOC_NUMBER.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @I_DOC_NUMBER.

    IF WA_J_1BNFDOC-NFE EQ ABAP_TRUE AND WA_J_1BNFDOC-NFENUM IS NOT INITIAL AND WA_J_1BNFE_ACTIVE-CODE NE '101'.

      IF WA_J_1BNFE_ACTIVE-ACTION_REQU EQ SPACE.
        MESSAGE S005(ZOB_FISCAL) WITH WA_J_1BNFE_ACTIVE-DOCNUM.
        APPEND VALUE #( TYPE = SY-MSGTY
                        ID   = SY-MSGID
                        NUMBER = SY-MSGNO
                        MESSAGE_V1 = SY-MSGV1
                        MESSAGE_V2 = SY-MSGV2
                        MESSAGE_V3 = SY-MSGV3
                        MESSAGE_V4 = SY-MSGV4 )
            TO ME->AT_RETORNO_ESTORNO.
        E_RETORNO = ME->AT_RETORNO_ESTORNO[].
        EXIT.
      ENDIF.

*      IF WA_J_1BNFE_ACTIVE-AUTHCOD IS INITIAL AND
*         WA_J_1BNFE_ACTIVE-ACTION_REQU NE '2' AND
*         WA_J_1BNFE_ACTIVE-SCSSTA NE '5' AND
*         WA_J_1BNFE_ACTIVE-ACTION_REQU NE '9'.
*
*        IF WA_J_1BNFE_ACTIVE-ACTION_REQU EQ '8'.
*          CALL FUNCTION 'J_1B_NFE_RESET_REJECT_STATUS'
*            EXPORTING
*              I_DOCNUM           = I_DOC_NUMBER
*            EXCEPTIONS
*              DOCUMENT_NOT_FOUND = 1
*              ENQUEUE_ERROR      = 2
*              INVALID_STATUS     = 3
*              OTHERS             = 4.
*
*          IF SY-SUBRC IS NOT INITIAL.
*
*            APPEND VALUE #( TYPE = SY-MSGTY
*                            ID   = SY-MSGID
*                            NUMBER = SY-MSGNO
*                            MESSAGE_V1 = SY-MSGV1
*                            MESSAGE_V2 = SY-MSGV2
*                            MESSAGE_V3 = SY-MSGV3
*                            MESSAGE_V4 = SY-MSGV4 )
*                TO ME->AT_RETORNO_ESTORNO.
*            E_RETORNO = ME->AT_RETORNO_ESTORNO[].
*            EXIT.
*          ENDIF.
*        ENDIF.
*
*        IF WA_J_1BNFE_ACTIVE-ACTION_REQU NE '1'.
*          "Estorno antes da autorização
*          CALL FUNCTION 'J_1B_NFE_CHECK_CANCEL_PR_AUTH'
*            EXPORTING
*              IV_DOCNUM          = I_DOC_NUMBER
*            IMPORTING
*              ES_ACTTAB_MOD      = WA_J_1BNFE_ACTIVE
*            EXCEPTIONS
*              STATUS_NOT_ALLOWED = 1
*              NO_ENTRY           = 2
*              OTHERS             = 3.
*
*          IF SY-SUBRC IS NOT INITIAL.
*            APPEND VALUE #( TYPE = SY-MSGTY
*                            ID   = SY-MSGID
*                            NUMBER = SY-MSGNO
*                            MESSAGE_V1 = SY-MSGV1
*                            MESSAGE_V2 = SY-MSGV2
*                            MESSAGE_V3 = SY-MSGV3
*                            MESSAGE_V4 = SY-MSGV4 )
*                TO ME->AT_RETORNO_ESTORNO.
*            E_RETORNO = ME->AT_RETORNO_ESTORNO[].
*            EXIT.
*          ENDIF.
*
*          CALL FUNCTION 'J_1B_NFE_CANCEL_PRIOR_AUTH'
*            EXPORTING
*              IS_ACTTAB      = WA_J_1BNFE_ACTIVE
*            EXCEPTIONS
*              PROCESS_ERRORS = 1
*              OTHERS         = 2.
*
*          IF SY-SUBRC IS NOT INITIAL.
*            APPEND VALUE #( TYPE = SY-MSGTY
*                            ID   = SY-MSGID
*                            NUMBER = SY-MSGNO
*                            MESSAGE_V1 = SY-MSGV1
*                            MESSAGE_V2 = SY-MSGV2
*                            MESSAGE_V3 = SY-MSGV3
*                            MESSAGE_V4 = SY-MSGV4 )
*                TO ME->AT_RETORNO_ESTORNO.
*            E_RETORNO = ME->AT_RETORNO_ESTORNO[].
*            EXIT.
*          ENDIF.
*        ELSE

*      IF  NOT ( WA_J_1BNFE_ACTIVE-ACTION_REQU CA 'C' ) AND ( WA_J_1BNFE_ACTIVE-ACTION_REQU CA '145' ).
*
*        CALL FUNCTION 'J_1B_NFE_RESET_REJECT_STATUS'
*          EXPORTING
*            I_DOCNUM           = I_DOC_NUMBER
*          IMPORTING
*            ES_ACTIVE_MOD      = WA_J_1BNFE_ACTIVE
*          EXCEPTIONS
*            DOCUMENT_NOT_FOUND = 1
*            ENQUEUE_ERROR      = 2
*            INVALID_STATUS     = 3
*            OTHERS             = 4.
*
*        IF SY-SUBRC IS NOT INITIAL.
*
*          CALL FUNCTION 'DEQUEUE_EJ_1BNFS'
*            EXPORTING
*              MODE_J_1BNFDOC = 'E'
*              MANDT          = SY-MANDT
*              DOCNUM         = I_DOC_NUMBER.
*
*          CALL FUNCTION 'DEQUEUE_E_J1BNFE'
*            EXPORTING
*              MODE_J_1BNFE_ACTIVE = 'E'
*              MANDT               = SY-MANDT
*              DOCNUM              = I_DOC_NUMBER.
*
*          CALL FUNCTION 'DEQUEUE_E_J1B_INVALID'
*            EXPORTING
*              MODE_J_1BNFE_INVALID = 'E'
*              MANDT                = SY-MANDT
*              DOCNUM               = I_DOC_NUMBER.
*        ENDIF.
*
*      ENDIF.

      IF WA_J_1BNFE_ACTIVE-AUTHCOD IS NOT INITIAL.
        MESSAGE S004(ZOB_FISCAL) WITH I_DOC_NUMBER.
        APPEND VALUE #( TYPE = SY-MSGTY
                        ID   = SY-MSGID
                        NUMBER = SY-MSGNO
                        MESSAGE_V1 = SY-MSGV1
                        MESSAGE_V2 = SY-MSGV2
                        MESSAGE_V3 = SY-MSGV3
                        MESSAGE_V4 = SY-MSGV4 )
                  TO ME->AT_RETORNO_ESTORNO.
        E_RETORNO = ME->AT_RETORNO_ESTORNO[].
        EXIT.
      ENDIF.

      IF WA_J_1BNFE_ACTIVE-NFNUM9 IS NOT INITIAL.
        CASE WA_J_1BNFE_ACTIVE-MODEL.
          WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE OR ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.
            TRY .
                DATA(LC_NFE) = ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = WA_J_1BNFE_ACTIVE-DOCNUM
                  )->SET_REGISTRO( EXPORTING I_DOCNUM = WA_J_1BNFE_ACTIVE-DOCNUM
                  )->SET_CANCELAR( EXPORTING I_MOTIVO = '10'
                  )->SET_LIBERAR_REGISTRO(
                  )->SET_CLEAR_LOG_ERRO(
                  ).
                CLEAR: LC_NFE.
              CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_ELETRONICO).
                APPEND VALUE #( TYPE = EX_DOC_ELETRONICO->MSGTY
                        ID     = EX_DOC_ELETRONICO->MSGID
                        NUMBER = EX_DOC_ELETRONICO->MSGNO
                        MESSAGE_V1 = EX_DOC_ELETRONICO->MSGV1
                        MESSAGE_V2 = EX_DOC_ELETRONICO->MSGV2
                        MESSAGE_V3 = EX_DOC_ELETRONICO->MSGV3
                        MESSAGE_V4 = EX_DOC_ELETRONICO->MSGV4 )
                  TO ME->AT_RETORNO_ESTORNO.
                E_RETORNO = ME->AT_RETORNO_ESTORNO[].
                LC_NFE->SET_LIBERAR_REGISTRO( ).
                CLEAR: LC_NFE.
                EXIT.
            ENDTRY.
          WHEN OTHERS.

            CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
              EXPORTING
                DOC_NUMBER               = I_DOC_NUMBER
                REF_TYPE                 = I_REF_TYPE
                REF_KEY                  = I_REF_KEY
                CAN_DAT                  = I_CAN_DAT
                CRE_NAM                  = I_CRE_NAM
                DOC_DAT                  = I_DOC_DAT
                PST_DAT                  = I_PST_DAT
                BEL_NR                   = I_BEL_NR
                G_JAHR                   = I_G_JAHR
              IMPORTING
                DOC_NUMBER               = E_DOCNUM
              EXCEPTIONS
                DOCUMENT_NOT_FOUND       = 1
                CANCEL_NOT_POSSIBLE      = 2
                NF_CANCEL_TYPE_NOT_FOUND = 3
                DATABASE_PROBLEM         = 4
                DOCUM_LOCK               = 5
                NFE_CANCEL_SIMULATION    = 6
                OTHERS                   = 7.

            IF SY-SUBRC IS NOT INITIAL.
              APPEND VALUE #( TYPE = SY-MSGTY
                              ID   = SY-MSGID
                              NUMBER = SY-MSGNO
                              MESSAGE_V1 = SY-MSGV1
                              MESSAGE_V2 = SY-MSGV2
                              MESSAGE_V3 = SY-MSGV3
                              MESSAGE_V4 = SY-MSGV4 )
                  TO ME->AT_RETORNO_ESTORNO.
            ENDIF.

            IF ( E_DOCNUM IS NOT INITIAL ) .
              ME->AT_DOCNUM_ESTORNO = E_DOCNUM.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  WAIT = I_BAPI_WAIT.
              R_GEROU = ABAP_TRUE.
            ELSE.
              R_GEROU = ABAP_FALSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            ENDIF.

            E_RETORNO = ME->AT_RETORNO_ESTORNO[].

        ENDCASE.
      ENDIF.

    ELSE.

      CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
        EXPORTING
          DOC_NUMBER               = I_DOC_NUMBER
          REF_TYPE                 = I_REF_TYPE
          REF_KEY                  = I_REF_KEY
          CAN_DAT                  = I_CAN_DAT
          CRE_NAM                  = I_CRE_NAM
          DOC_DAT                  = I_DOC_DAT
          PST_DAT                  = I_PST_DAT
          BEL_NR                   = I_BEL_NR
          G_JAHR                   = I_G_JAHR
        IMPORTING
          DOC_NUMBER               = E_DOCNUM
        EXCEPTIONS
          DOCUMENT_NOT_FOUND       = 1
          CANCEL_NOT_POSSIBLE      = 2
          NF_CANCEL_TYPE_NOT_FOUND = 3
          DATABASE_PROBLEM         = 4
          DOCUM_LOCK               = 5
          NFE_CANCEL_SIMULATION    = 6
          OTHERS                   = 7.

      IF SY-SUBRC IS NOT INITIAL.
        APPEND VALUE #( TYPE = SY-MSGTY
                        ID   = SY-MSGID
                        NUMBER = SY-MSGNO
                        MESSAGE_V1 = SY-MSGV1
                        MESSAGE_V2 = SY-MSGV2
                        MESSAGE_V3 = SY-MSGV3
                        MESSAGE_V4 = SY-MSGV4 )
            TO ME->AT_RETORNO_ESTORNO.
      ENDIF.

      IF ( E_DOCNUM IS NOT INITIAL ) .
        ME->AT_DOCNUM_ESTORNO = E_DOCNUM.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = I_BAPI_WAIT.
        R_GEROU = ABAP_TRUE.
      ELSE.
        R_GEROU = ABAP_FALSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      E_RETORNO = ME->AT_RETORNO_ESTORNO[].

    ENDIF.

  ENDMETHOD.


  METHOD GET_DOCUMENTO_CHAVE.

    CLEAR: E_J_1BNFE_ACTIVE.
    E_J_1BNFE_ACTIVE-REGIO     = I_CHAVE(2).
    E_J_1BNFE_ACTIVE-NFYEAR    = I_CHAVE+2(2).
    E_J_1BNFE_ACTIVE-NFMONTH   = I_CHAVE+4(2).
    E_J_1BNFE_ACTIVE-STCD1     = I_CHAVE+6(14).
    E_J_1BNFE_ACTIVE-MODEL     = I_CHAVE+20(2).
    E_J_1BNFE_ACTIVE-SERIE     = I_CHAVE+22(3).
    E_J_1BNFE_ACTIVE-NFNUM9    = I_CHAVE+25(9).
    E_J_1BNFE_ACTIVE-DOCNUM9   = I_CHAVE+34(9).
    E_J_1BNFE_ACTIVE-CDV       = I_CHAVE+43(1).

    CASE I_PROPRIA.
      WHEN ABAP_TRUE.
        SELECT SINGLE * INTO E_J_1BNFE_ACTIVE
          FROM J_1BNFE_ACTIVE AS A
         WHERE REGIO    EQ E_J_1BNFE_ACTIVE-REGIO
           AND NFYEAR   EQ E_J_1BNFE_ACTIVE-NFYEAR
           AND NFMONTH  EQ E_J_1BNFE_ACTIVE-NFMONTH
           AND STCD1    EQ E_J_1BNFE_ACTIVE-STCD1
           AND MODEL    EQ E_J_1BNFE_ACTIVE-MODEL
           AND SERIE    EQ E_J_1BNFE_ACTIVE-SERIE
           AND NFNUM9   EQ E_J_1BNFE_ACTIVE-NFNUM9
           "AND DOCNUM9  EQ E_J_1BNFE_ACTIVE-DOCNUM9
           "AND CDV      EQ E_J_1BNFE_ACTIVE-CDV
           AND FORM     NE SPACE
           AND CANCEL   NE ABAP_TRUE
           AND NOT EXISTS ( SELECT * FROM J_1BNFDOC AS D WHERE D~DOCNUM EQ A~DOCNUM AND D~CANCEL EQ ABAP_TRUE ).
      WHEN ABAP_FALSE.
        SELECT SINGLE * INTO E_J_1BNFE_ACTIVE
          FROM J_1BNFE_ACTIVE AS A
         WHERE REGIO    EQ E_J_1BNFE_ACTIVE-REGIO
           AND NFYEAR   EQ E_J_1BNFE_ACTIVE-NFYEAR
           AND NFMONTH  EQ E_J_1BNFE_ACTIVE-NFMONTH
           AND STCD1    EQ E_J_1BNFE_ACTIVE-STCD1
           AND MODEL    EQ E_J_1BNFE_ACTIVE-MODEL
           AND SERIE    EQ E_J_1BNFE_ACTIVE-SERIE
           AND NFNUM9   EQ E_J_1BNFE_ACTIVE-NFNUM9
           "AND DOCNUM9  EQ E_J_1BNFE_ACTIVE-DOCNUM9
           "AND CDV      EQ E_J_1BNFE_ACTIVE-CDV
           AND FORM     EQ SPACE
           AND CANCEL   NE ABAP_TRUE
           AND NOT EXISTS ( SELECT * FROM J_1BNFDOC AS D WHERE D~DOCNUM EQ A~DOCNUM AND D~CANCEL EQ ABAP_TRUE ).
    ENDCASE.

    IF SY-SUBRC IS INITIAL.
      R_DOCNUM = E_J_1BNFE_ACTIVE-DOCNUM.
      MESSAGE S003 WITH R_DOCNUM.
    ELSE.
      CLEAR: E_J_1BNFE_ACTIVE, R_DOCNUM.
      RAISE ERRO.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TIPO_IMPOSTO.

    DATA: IT_J_1BAJ TYPE TABLE OF J_1BAJ,
          WA_J_1BAJ TYPE J_1BAJ,
          IT_T683S  TYPE TABLE OF T683S,
          WA_T683S  TYPE T683S.

    SELECT * INTO TABLE IT_J_1BAJ FROM J_1BAJ WHERE TAXGRP EQ I_TAXGRP.

    CHECK SY-SUBRC IS INITIAL.

    SELECT * INTO TABLE IT_T683S
      FROM T683S
       FOR ALL ENTRIES IN IT_J_1BAJ
     WHERE KVEWE EQ 'A'
       AND KAPPL EQ 'TX'
       AND KALSM EQ 'TAXBRA'
       AND KSCHL EQ IT_J_1BAJ-TAXTYP
       AND KSTAT EQ 'X'.

    CHECK SY-SUBRC IS INITIAL.

    READ TABLE IT_T683S INDEX 1 INTO WA_T683S.
    E_KSCHL = WA_T683S-KSCHL.


  ENDMETHOD.


  METHOD LIMPAR.

  ENDMETHOD.


  METHOD VERIFICAR_CRIAR.

    DATA: I_PROPRIA TYPE CHAR01.

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
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING DATA_FI_MM_NAO.
    ENDIF.

    IF I_CHAVE IS NOT INITIAL.

      SELECT SINGLE * INTO E_ZIB_NFE_FORN
        FROM ZIB_NFE_FORN
       WHERE NU_CHAVE EQ I_CHAVE.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE E001 RAISING NAO_CTE_FORN.
      ENDIF.

      IF I_FORM IS NOT INITIAL.
        I_PROPRIA = ABAP_TRUE.
      ENDIF.

      CALL METHOD ZCL_FISCAL=>GET_DOCUMENTO_CHAVE
        EXPORTING
          I_CHAVE   = I_CHAVE
          I_PROPRIA = I_PROPRIA
        RECEIVING
          R_DOCNUM  = DATA(LC_DOCNUM)
        EXCEPTIONS
          ERRO      = 1
          OTHERS    = 2.

      IF SY-SUBRC IS NOT INITIAL.
        MESSAGE E002 WITH LC_DOCNUM RAISING DOCUMENTO_EXISTE.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
