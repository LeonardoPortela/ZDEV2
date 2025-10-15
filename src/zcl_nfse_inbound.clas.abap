CLASS zcl_nfse_inbound DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_cadastro .
    INTERFACES zif_pesquisa .

    TYPES:
* INICIO - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
      BEGIN OF ty_ekpo,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        meins TYPE ekpo-meins,
* INICIO - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
        vrtkz TYPE ekpo-vrtkz,
* FIM - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
      END OF ty_ekpo .
    TYPES:
      BEGIN OF ty_esll,
        packno     TYPE esll-packno,
        sub_packno TYPE esll-sub_packno,
      END OF ty_esll .
    TYPES:
      BEGIN OF ty_esll_sub,
        packno TYPE esll-packno,
        extrow TYPE esll-extrow,
        menge  TYPE esll-menge,
      END OF ty_esll_sub,

      BEGIN OF ty_t006,
        msehi   TYPE t006-msehi,
        isocode TYPE t006-isocode,
      END OF ty_t006.

    TYPES:
      ty_t_ekpo TYPE TABLE OF ty_ekpo .
    TYPES:
      ty_t_esll TYPE TABLE OF ty_esll .
    TYPES:
      ty_t_esll_sub TYPE TABLE OF ty_esll_sub .

    TYPES:
     ty_t_t006 TYPE TABLE OF ty_t006 .
* FIM - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO

    DATA mt_mess TYPE bapiret2_t .
    DATA ms_nfse_001 TYPE zibt_nfse_001 .
    CONSTANTS st_documento_00 TYPE zde_st_nfe_documento VALUE '00' ##NO_TEXT.
    CONSTANTS st_documento_99 TYPE zde_st_nfe_documento VALUE '99' ##NO_TEXT.
    CONSTANTS st_documento_01 TYPE zde_st_nfe_documento VALUE '01' ##NO_TEXT.
    CONSTANTS st_fiscal_pendente TYPE zde_st_nfe_fiscal VALUE '00' ##NO_TEXT.
    CONSTANTS st_fiscal_departamento TYPE zde_st_nfe_fiscal VALUE '01' ##NO_TEXT.
    CONSTANTS st_fiscal_depart_aprovado TYPE zde_st_nfe_fiscal VALUE '02' ##NO_TEXT.
    CONSTANTS st_fiscal_nao_aceito_fiscal TYPE zde_st_nfe_fiscal VALUE '97' ##NO_TEXT.
    CONSTANTS st_fiscal_sem_aceite_fiscal TYPE zde_st_nfe_fiscal VALUE '98' ##NO_TEXT.
    CONSTANTS st_fiscal_com_aceite_fiscal TYPE zde_st_nfe_fiscal VALUE '99' ##NO_TEXT.
    CONSTANTS st_fisico_ TYPE zde_st_nfe_fisico VALUE ' ' ##NO_TEXT.
    CONSTANTS st_fisico_00 TYPE zde_st_nfe_fisico VALUE '00' ##NO_TEXT.
    CONSTANTS st_fisico_01 TYPE zde_st_nfe_fisico VALUE '01' ##NO_TEXT.
    CONSTANTS st_fisico_aviso_gerado TYPE zde_st_nfe_fisico VALUE '02' ##NO_TEXT.
    CONSTANTS st_fisico_migo_gerada TYPE zde_st_nfe_fisico VALUE '03' ##NO_TEXT.
    CONSTANTS st_fisico_miro_gerada TYPE zde_st_nfe_fisico VALUE '04' ##NO_TEXT.
    CONSTANTS st_fisico_fiscal_gerado TYPE zde_st_nfe_fisico VALUE '05' ##NO_TEXT.
    CONSTANTS st_fisico_98 TYPE zde_st_nfe_fisico VALUE '98' ##NO_TEXT.
    CONSTANTS st_fisico_99 TYPE zde_st_nfe_fisico VALUE '99' ##NO_TEXT.
    CONSTANTS st_armazenagem_00 TYPE zde_st_nfe_armazem VALUE '00' ##NO_TEXT.
    CONSTANTS st_armazenagem_01 TYPE zde_st_nfe_armazem VALUE '01' ##NO_TEXT.
    CONSTANTS st_armazenagem_02 TYPE zde_st_nfe_armazem VALUE '02' ##NO_TEXT.
    CONSTANTS st_armazenagem_03 TYPE zde_st_nfe_armazem VALUE '03' ##NO_TEXT.
    CONSTANTS st_armazenagem_04 TYPE zde_st_nfe_armazem VALUE '04' ##NO_TEXT.
    CONSTANTS st_armazenagem_05 TYPE zde_st_nfe_armazem VALUE '05' ##NO_TEXT.
    CONSTANTS st_armazenagem_97 TYPE zde_st_nfe_armazem VALUE '97' ##NO_TEXT.
    CONSTANTS st_armazenagem_98 TYPE zde_st_nfe_armazem VALUE '98' ##NO_TEXT.
    CONSTANTS st_armazenagem_99 TYPE zde_st_nfe_armazem VALUE '99' ##NO_TEXT.
    DATA mt_ekpo_lebre TYPE ty_t_ekpo .

    METHODS nf_document_read
      IMPORTING
        !iv_docnum   TYPE j_1bdocnum
      EXPORTING
        !es_header   TYPE j_1bnfdoc
        !et_item     TYPE j_1b_tt_nflin
        !et_item_tax TYPE j_1b_tt_nfstx
        !et_partner  TYPE j_1b_tt_nfnad .
    METHODS nf_document_update
      IMPORTING
        !is_header   TYPE j_1bnfdoc
        !it_item     TYPE j_1b_tt_nflin
        !it_item_tax TYPE j_1b_tt_nfstx
        !it_partner  TYPE j_1b_tt_nfnad .
    METHODS cancelar .
    METHODS check_authorization
      IMPORTING
        !iv_object  TYPE char20
      RETURNING
        VALUE(r_ok) TYPE flag .
    METHODS constructor
      IMPORTING
        !i_guid TYPE /tcsr/e_guid_header .
    METHODS estorno .
    METHODS force_update .
    METHODS free .
    METHODS get_folha_info
      IMPORTING
        !iv_entrysheet TYPE lblni
      EXPORTING
        !ev_packno     TYPE packno
        !ev_mjahr      TYPE mjahr
        !ev_mblnr      TYPE mblnr .
    METHODS get_last_message
      RETURNING
        VALUE(r_return) TYPE bapiret2 .
    METHODS get_nfse_001
      RETURNING
        VALUE(r_ret) TYPE zibt_nfse_001 .
    METHODS get_obs_text
      RETURNING
        VALUE(r_ret) TYPE REF TO cl_gui_textedit .
    METHODS get_pdf
      RETURNING
        VALUE(ev_xpdf) TYPE /tcsr/e_xmlstring .
    METHODS get_simulate
      RETURNING
        VALUE(r_ret_simulate_tab) TYPE wsii_incinv_create_withtax .
    METHODS lzmm_nfseu05
      IMPORTING
        !i_issqn  TYPE j_1btaxval
        !i_inss   TYPE j_1btaxval
        !i_pis    TYPE j_1btaxval
        !i_cofins TYPE j_1btaxval
        !i_csll   TYPE j_1btaxval
        !i_irrf   TYPE j_1btaxval .
    METHODS message_ok
      RETURNING
        VALUE(rv_ret) TYPE flag .
    METHODS miro_frete
      IMPORTING
        !iv_retem_iss   TYPE flag OPTIONAL
      EXPORTING
        !e_belnr        TYPE belnr_d
        !e_gjahr        TYPE gjahr
      RETURNING
        VALUE(ev_subrc) TYPE sysubrc
      RAISING
        zcx_miro_exception .
    METHODS miro_frete_estorno
      EXPORTING
        !e_belnr        TYPE belnr_d
        !e_gjahr        TYPE gjahr
      RETURNING
        VALUE(ev_subrc) TYPE sysubrc
      RAISING
        zcx_miro_exception .
    METHODS nfse_inbound_cancela_fatura .
    METHODS nfse_inbound_doc_material
      EXPORTING
        !ev_mblnr TYPE mblnr
        !ev_mjahr TYPE mjahr .
    METHODS nfse_inbound_estorno_doc_mat .
    METHODS nfse_inbound_estorno_folha
      RETURNING
        VALUE(r_erro) TYPE flag .
    METHODS nfse_inbound_fatura
      RETURNING
        VALUE(r_gerou) TYPE flag .
    METHODS nfse_inbound_folha
      EXPORTING
        VALUE(ev_mjahr)      TYPE mjahr
        VALUE(ev_mblnr)      TYPE mblnr
        VALUE(ev_entrysheet) TYPE bapiessr-sheet_no
        VALUE(ev_acceptance) TYPE kzabn
        VALUE(ev_packno)     TYPE packno .
    METHODS nfse_sol_miro_softexpert
      IMPORTING
        !i_estornar  TYPE char01 DEFAULT space
        !i_desativar TYPE char01 OPTIONAL .
    METHODS only_mat_po
      RETURNING
        VALUE(ret) TYPE flag .
    METHODS only_serv_po
      RETURNING
        VALUE(ret) TYPE flag .
    METHODS set_aceitar_faturar
      IMPORTING
        !i_ck_somente_validar    TYPE char01 OPTIONAL
        !i_ck_retorno_sem_ajuste TYPE char01 OPTIONAL
        !i_us_miro               TYPE uname OPTIONAL
      RAISING
        zcx_nfe_inbound_exception
        zcx_cadastro
        zcx_pedido_compra_exception .
    METHODS set_impostos
      IMPORTING
        !i_issqn           TYPE j_1btaxval OPTIONAL
        !i_inss            TYPE j_1btaxval OPTIONAL
        !i_pis             TYPE j_1btaxval OPTIONAL
        !i_cof             TYPE j_1btaxval OPTIONAL
        !i_csll            TYPE j_1btaxval OPTIONAL
        !i_irrf            TYPE j_1btaxval OPTIONAL
        !i_piscofcsll      TYPE j_1btaxval OPTIONAL
        !i_data_vencimento TYPE datum OPTIONAL
        !i_base_issqn      TYPE j_1btaxval OPTIONAL
        !i_base_inss       TYPE j_1btaxval OPTIONAL
        !i_base_irrf       TYPE j_1btaxval OPTIONAL .
    METHODS set_nfse_em_revisao
      IMPORTING
        !i_motivo_revisao      TYPE string
        !i_usuario_solicitante TYPE syuname .
    METHODS set_nr_documento_fatura
      IMPORTING
        !i_belnr TYPE re_belnr
        !i_gjahr TYPE gjahr .
    METHODS show_message .
    METHODS nfe_atualiza_cfop_no_fiscal .
    METHODS set_value_iss
      IMPORTING
        !i_ind_iss                TYPE char01
      RETURNING
        VALUE(r_zcl_nfse_inbound) TYPE REF TO zcl_nfse_inbound .
    METHODS append_message_ov
      IMPORTING
        !i_msgty                  TYPE msgty DEFAULT 'E'
        !i_msgid                  TYPE msgid
        !i_msgno                  TYPE symsgno
        !i_msgv1                  TYPE any OPTIONAL
        !i_msgv2                  TYPE any OPTIONAL
        !i_msgv3                  TYPE any OPTIONAL
        !i_msgv4                  TYPE any OPTIONAL
      RETURNING
        VALUE(r_zcl_nfse_inbound) TYPE REF TO zcl_nfse_inbound .
    METHODS nfse_inbound_aceite_folha
      RETURNING
        VALUE(r_erro) TYPE flag .
protected section.
private section.

  data MV_GUID type /TCSR/E_GUID_HEADER .
  data MV_SERV_MAT type CHAR1 .
  data MT_NFSE_002 type ZIBC_NFSE_002 .
  data MT_NFSE_003 type ZIBC_NFSE_003 .
  data MT_NFSE_004 type ZIBC_NFSE_004 .
  data MT_NFSE_005 type ZIBC_NFSE_005 .
  data MT_NFSE_0034 type ZLESC0034 .
  data MS_TOTAL_0034 type ZLEST0034 .
  data GV_TOTAL_PO type BWERT .
  data GV_LIQUI_PO type BWERT .
  data GV_TOTAL_IMP type BWERT .
  data GO_UTIL type ref to /TCSR/C_UTIL_MONITOR .
  data MT_IMPOSTOS type /TCSR/Y_COND_LIST .
  data MV_ISSQN type J_1BTAXVAL .
  data MV_INSS type J_1BTAXVAL .
  data MV_PIS type J_1BTAXVAL .
  data MV_COF type J_1BTAXVAL .
  data MV_CSLL type J_1BTAXVAL .
  data MV_IRRF type J_1BTAXVAL .
  data MV_PISCOFCSLL type J_1BTAXVAL .
  data MV_VENCIMENTO type DATUM .
  data MV_BASE_ISSQN type J_1BTAXVAL .
  data MV_BASE_INSS type J_1BTAXVAL .
  data MV_BASE_IRRF type J_1BTAXVAL .
  data AT_GV_ISS type CHAR01 .
  data MT_ESLL type TY_T_ESLL .
  data MT_ESLL_SUB type TY_T_ESLL_SUB .
  data MT_T006 type TY_T_T006 .

  methods GET_LAST_DIGITS
    importing
      !IV_VAR type /TCSR/E_NFSE_NUMERO
      !IV_DIGIT type INT4 default 6
    returning
      value(RV_VAR) type /TCSR/E_NFSE_NUMERO .
  methods APPEND_GLOBAL_MESSAGE
    importing
      !I_MESS_TAB type BAPIRET2_T .
  methods APPEND_MESSAGE
    importing
      !I_MESSAGE type STRING
      !I_MSGTY type MSGTY default 'E' .
  methods APPEND_MESSAGE2
    importing
      !I_MSGTY type MSGTY default 'S'
      !I_MSGID type MSGID
      !I_MSGNO type SYMSGNO
      !I_MSGV1 type ANY optional
      !I_MSGV2 type ANY optional
      !I_MSGV3 type ANY optional
      !I_MSGV4 type ANY optional .
  methods BUSCA_CAT_DESTINO
    importing
      !IV_LIFNR type LIFNR
      !IV_WERKS type WERKS_D
    returning
      value(RV_DSTCAT) type J_1BDSTCAT .
  methods CALL_SHDB_ML81N_REVOKE
    returning
      value(RT_SHDB_MESSAGES) type TAB_BDCMSGCOLL .
  methods CLEAR_ALL .
  methods CLEAR_MESSAGE .
  methods CRIAR_MIRO
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
  methods DANFE
    exporting
      !E_URL type STRING
    raising
      ZCX_NFE_INBOUND_EXCEPTION .
  methods DEQUEUE_GUID .
  methods ENQUEUE_GUID
    raising
      /TCSR/CX_EXCEPTION .
  methods EXECUTE_ML81N_DELETE .
  methods FILL_0034
    importing
      !I_FORCE type FLAG optional .
  methods FILL_ACCOUNT
    importing
      !IT_ITENS type BAPI_INCINV_CREATE_ITEM_T
    returning
      value(R_ACCOUNT_TAB) type TAB_BAPI_INCINV_CREATE_ACCOUNT .
  methods FILL_ALL .
  methods FILL_GLACCOUNT
    importing
      !IT_ACCOUNT type TAB_BAPI_INCINV_CREATE_ACCOUNT
    returning
      value(R_RET) type TB_BAPI_INCINV_CRT_GL_ACCOUNT .
  methods FILL_MIRO_CAB
    returning
      value(R_MIRO_CAB) type BAPI_INCINV_CREATE_HEADER .
  methods FILL_MIRO_ITENS
    returning
      value(R_ITENS) type BAPI_INCINV_CREATE_ITEM_T .
  methods FILL_TABLE .
  methods FILL_WITHTAX
    returning
      value(R_RET) type TB_BAPI_INCINV_CREATE_WITHTAX .
  methods GERAR_ESCRIT_ENTRADA
    importing
      !IV_BELNR type RE_BELNR
      !IV_GJAHR type GJAHR
    returning
      value(EV_DOCNUM) type J_1BDOCNUM .
  methods GET_LINHA_IMPOSTO_0034
    importing
      !IV_TAXGRP type J_1BTAXGRP
      !IV_TAXLAW type J_1BTAXLW4 optional
    returning
      value(RS_IMPOSTO) type BAPI_J_1BNFSTX .
  methods LOG_TO_TAB .
  methods NF_SERIE
    returning
      value(R_RET) type XBLNR .
  methods SAVE
    importing
      !I_TAB_NUMBER type CHAR2 optional .
  methods SET_CK_FISICO
    importing
      !I_CHECK type ZDE_CK_FISICO .
  methods SET_ST_ARMAZEM
    importing
      !I_ST_ARMAZEM type ZDE_ST_NFE_ARMAZEM .
  methods SET_ST_FISCAL
    importing
      !I_ST_FISCAL type ZDE_ST_NFE_FISCAL .
  methods SET_ST_FISICO
    importing
      !I_ST_FISICO type ZDE_ST_NFE_FISICO .
  methods SET_VT_DOCS
    importing
      !I_BELNR type BELNR_D optional
      !I_GJAHR type GJAHR optional .
  methods SE_ADD_TAX_VALUES
    importing
      !IO_OBJECT type ref to ZCL_SOFT_EXPERT_WS_INJECT
    returning
      value(R_ERRO) type FLAG .
  methods CALL_SHDB_ML81N_ACCEPT
    returning
      value(RT_SHDB_MESSAGES) type TAB_BDCMSGCOLL .
  methods FILL_FOLHA_DE_SERVICO .
  methods FILL_ITEM_FOLHA_SERVICO
    importing
      !I_ST_NFSE_002 type ZIBT_NFSE_002
    changing
      !C_ST_ITEM type BAPI_INCINV_CREATE_ITEM .
ENDCLASS.



CLASS ZCL_NFSE_INBOUND IMPLEMENTATION.


  METHOD append_global_message.

    LOOP AT i_mess_tab ASSIGNING FIELD-SYMBOL(<fs_mess>).

      CALL METHOD me->append_message2
        EXPORTING
          i_msgty = <fs_mess>-type
          i_msgid = <fs_mess>-id
          i_msgno = <fs_mess>-number
          i_msgv1 = <fs_mess>-message_v1
          i_msgv2 = <fs_mess>-message_v2
          i_msgv3 = <fs_mess>-message_v3
          i_msgv4 = <fs_mess>-message_v4.

    ENDLOOP.


  ENDMETHOD.


  METHOD append_message.

    DATA(lv_msgty) = i_msgty.

    CHECK i_message IS NOT INITIAL.

    APPEND INITIAL LINE TO me->mt_mess ASSIGNING FIELD-SYMBOL(<fs_message>).

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    DATA lv_msg1 TYPE sy-msgv1.
    DATA lv_msg2 TYPE sy-msgv2.
    DATA lv_msg3 TYPE sy-msgv3.
    DATA lv_msg4 TYPE sy-msgv4.

    lv_texto = i_message.

    TRY .

        CALL FUNCTION 'TR_SPLIT_TEXT'
          EXPORTING
            iv_text  = lv_texto
            iv_len   = 40
          IMPORTING
            et_lines = lt_trtexts.

    ENDTRY.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      CASE sy-tabix.
        WHEN 1.
          <fs_message>-message_v1 = <fs_line>.
        WHEN 2.
          <fs_message>-message_v2 = <fs_line>.
        WHEN 3.
          <fs_message>-message_v3 = <fs_line>.
        WHEN 4.
          <fs_message>-message_v4 = <fs_line>.
      ENDCASE.

    ENDLOOP.

    <fs_message>-id = 'DS'.
    <fs_message>-type = lv_msgty.
    <fs_message>-number = '016'.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD append_message2.

    APPEND INITIAL LINE TO me->mt_mess ASSIGNING FIELD-SYMBOL(<fs_message>).

    <fs_message>-id = i_msgid.
    <fs_message>-type = i_msgty.
    <fs_message>-number = i_msgno.

    <fs_message>-message_v1 = i_msgv1.

    <fs_message>-message_v2 = i_msgv2.

    <fs_message>-message_v3 = i_msgv3.

    <fs_message>-message_v4 = i_msgv4.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD append_message_ov.

    r_zcl_nfse_inbound = me.

    APPEND INITIAL LINE TO me->mt_mess ASSIGNING FIELD-SYMBOL(<fs_message>).

    <fs_message>-id = i_msgid.
    <fs_message>-type = i_msgty.
    <fs_message>-number = i_msgno.

    <fs_message>-message_v1 = i_msgv1.

    <fs_message>-message_v2 = i_msgv2.

    <fs_message>-message_v3 = i_msgv3.

    <fs_message>-message_v4 = i_msgv4.

    MESSAGE ID <fs_message>-id TYPE <fs_message>-type NUMBER <fs_message>-number
      WITH <fs_message>-message_v1 <fs_message>-message_v2
           <fs_message>-message_v3 <fs_message>-message_v4 INTO <fs_message>-message.

  ENDMETHOD.


  METHOD busca_cat_destino.

    SELECT SINGLE txjcd INTO @DATA(wk_txjcd)
      FROM t001w
     WHERE werks EQ @iv_werks.

    SELECT SINGLE txjcd INTO @DATA(lf_txjcd)
      FROM lfa1
     WHERE lifnr EQ @iv_lifnr.

    IF wk_txjcd(2) EQ lf_txjcd(2).
      rv_dstcat = '0'.
    ELSE.
      rv_dstcat = '1'.
    ENDIF.

  ENDMETHOD.


  METHOD call_shdb_ml81n_revoke.

    DATA:
      lt_bdc TYPE TABLE OF bdcdata.
    DATA:
      lw_bdc        TYPE bdcdata,
      lw_ctu_params TYPE ctu_params.
*------------------------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SELP'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0340'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ENTE'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'RM11R-LBLNI'.
    lw_bdc-fval     = ms_nfse_001-lblni.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=AKCH'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ACCR'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SAVE'.
    APPEND lw_bdc TO lt_bdc.
**-------------------------------
    "Popup confirm
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLSPO1'.
    lw_bdc-dynpro   = '0300'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=YES'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0110'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ENTE'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------

    lw_ctu_params-dismode = 'N'.
    lw_ctu_params-updmode = 'S'.

    CALL TRANSACTION 'ML81N'
               USING lt_bdc
             OPTIONS FROM lw_ctu_params
            MESSAGES INTO rt_shdb_messages.

    COMMIT WORK AND WAIT.

    "WAIT UP TO 3 SECONDS.

  ENDMETHOD.


  METHOD cancelar.

    DATA lv_exibir TYPE c.
    DATA lv_mess TYPE string.
    DATA lv_canc TYPE c.

    fill_table( ).

    fill_all( ).

*    IF check_authorization( 'ZFIS_CANCE' ) NE 'X'.
*
*      lv_mess = `Sem autorização para cancelar documento`.
*
*      append_message( lv_mess ).
*
*      EXIT.
*
*    ENDIF.

    IF ms_nfse_001-cancel = abap_true.

      lv_mess = `Já está cancelada`.

      lv_exibir = 'X'.

      "append_message( lv_mess ).

      "EXIT.

    ENDIF.

    IF ms_nfse_001-belnr IS NOT INITIAL.

      lv_mess = `Impossivel cancelar com fatura gerada: ` && ms_nfse_001-belnr.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    IF ms_nfse_001-ebeln IS NOT INITIAL.

      lv_mess = `Impossivel cancelar com pedido associado`.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    IF ms_nfse_001-lblni IS NOT INITIAL.

      lv_mess = `Impossivel cancelar com serviço criado`.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    IF ms_nfse_001-mblnr IS NOT INITIAL.

      lv_mess = `Impossivel cancelar com doc.material existe`.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    CALL FUNCTION 'ZMM_NFSE_POPUP_CANCELAMENTO'
      EXPORTING
        iv_exibir = lv_exibir
      CHANGING
        ev_canc   = lv_canc
        ev_motivo = ms_nfse_001-cod_motivo
        ev_descr  = ms_nfse_001-motivo.

    IF lv_canc = 'X' AND lv_exibir IS INITIAL.

      CLEAR ms_nfse_001-cod_motivo.
      CLEAR ms_nfse_001-motivo.

    ELSE.

      ms_nfse_001-cancel = 'X'.

      UPDATE /tcsr/t_act  SET last_stepstatus = '89'
          WHERE guid_header = ms_nfse_001-guid_header.

      save( '1' ).

    ENDIF.

  ENDMETHOD.


  METHOD check_authorization.

    AUTHORITY-CHECK OBJECT iv_object
           ID iv_object FIELD 'DUMMY'.

    IF sy-subrc <> 0.
      r_ok = ''.
      EXIT.
    ENDIF.

    r_ok = 'X'.

  ENDMETHOD.


  METHOD clear_all.

    CLEAR: mt_mess, mv_guid, ms_nfse_001, mt_nfse_002,
           mt_nfse_003, mt_nfse_004, mt_nfse_005, go_util.

  ENDMETHOD.


  METHOD clear_message.

    CLEAR mt_mess.

  ENDMETHOD.


  METHOD constructor.

    mv_guid = i_guid.

  ENDMETHOD.


  METHOD criar_miro.

    DATA: wa_headerdata        TYPE bapi_incinv_create_header,
          it_itemdata          TYPE TABLE OF bapi_incinv_create_item,
          wa_itemdata          TYPE bapi_incinv_create_item,
          it_accountingdata    TYPE TABLE OF bapi_incinv_create_account,
          wa_accountingdata    TYPE bapi_incinv_create_account,
          v_aufpl              TYPE ekkn-aufpl,
          it_contas            TYPE TABLE OF bapi_incinv_create_gl_account,
          it_withtaxdata       TYPE TABLE OF bapi_incinv_create_withtax,
          wa_withtaxdata       TYPE bapi_incinv_create_withtax,
          invoicedocnumber     TYPE re_belnr,
          it_materialdata      TYPE TABLE OF bapi_incinv_create_material,
          wa_materialdata      TYPE bapi_incinv_create_material,
          it_return            TYPE TABLE OF  bapiret2,
          fiscalyear           TYPE gjahr,
          wa_header            TYPE thead,
          p_valor              TYPE netwr_fp,
          p_desconto           TYPE netwr_fp,
          p_forma_pagamento    TYPE dzlsch,
          p_princ_bnc_emp      TYPE hbkid,
          lt_lfbw              TYPE TABLE OF lfbw,
          t_grupo              TYPE STANDARD TABLE OF  rgsb4,
          lwa_zib_nfe_dist_ter TYPE zib_nfe_dist_ter,
          v_count              TYPE i,
          vg_vlt_total         TYPE ekpo-menge,
          vg_calc              TYPE ekpo-menge,
          vg_vlt_item          TYPE ekpo-menge,
          vg_item              TYPE i.

    "me->limpar( ).

    IF i_cabecalho-doc_date_cal IS NOT INITIAL.
      zcl_miro=>verificar_vencimento_fatura( i_data_vencimento = i_cabecalho-doc_date_cal i_pymt_meth = i_cabecalho-pymt_meth ).
    ELSE.
      zcl_miro=>verificar_vencimento_fatura( i_data_vencimento = i_cabecalho-doc_date_ven i_pymt_meth = i_cabecalho-pymt_meth ).
    ENDIF.

    wa_headerdata-pstng_date = zcl_miro=>verificar_criar( i_data = i_cabecalho-doc_date_mov  i_bukrs = i_cabecalho-comp_code ).

    MOVE i_cabecalho-gross_amount TO p_valor.
    MOVE i_cabecalho-dsct_amount  TO p_desconto.

    IF i_cabecalho-pymt_meth IS INITIAL AND i_cabecalho-housebankid IS NOT INITIAL.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs           = i_cabecalho-comp_code
          p_lifnr           = i_cabecalho-lifnr
          p_valor           = p_valor
          p_bvtyp           = i_cabecalho-partner_bk
        IMPORTING
          p_forma_pagamento = wa_headerdata-pymt_meth
        EXCEPTIONS
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          OTHERS            = 6.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      ENDIF.

      IF wa_headerdata-pymt_meth IS INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_forma_pagamento-msgid
                              msgno = zcx_miro_exception=>zcx_forma_pagamento-msgno )
            msgid  = zcx_miro_exception=>zcx_forma_pagamento-msgid
            msgno  = zcx_miro_exception=>zcx_forma_pagamento-msgno
            msgty  = 'E'.
      ELSE.
        wa_headerdata-pymt_meth   = wa_headerdata-pymt_meth.
        wa_headerdata-housebankid = i_cabecalho-housebankid.
      ENDIF.

    ELSEIF i_cabecalho-pymt_meth IS NOT INITIAL AND i_cabecalho-housebankid IS INITIAL.

      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs          = i_cabecalho-comp_code
          p_lifnr          = i_cabecalho-lifnr
          p_zlsch          = i_cabecalho-pymt_meth
          p_valor          = p_valor
          p_bvtyp          = i_cabecalho-partner_bk
        IMPORTING
          p_princ_bnc_emp  = wa_headerdata-housebankid
        EXCEPTIONS
          nao_fornecedor   = 1
          fornecedor_conta = 2
          fornecedor_banco = 3
          faixa_valor      = 4
          banco_empresa    = 5
          OTHERS           = 6.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      ENDIF.

      IF wa_headerdata-housebankid IS INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_banco_empresa-msgid
                              msgno = zcx_miro_exception=>zcx_banco_empresa-msgno )
            msgid  = zcx_miro_exception=>zcx_banco_empresa-msgid
            msgno  = zcx_miro_exception=>zcx_banco_empresa-msgno
            msgty  = 'E'.
      ELSE.
        wa_headerdata-pymt_meth   = i_cabecalho-pymt_meth.
        wa_headerdata-housebankid = wa_headerdata-housebankid.
      ENDIF.

    ELSEIF i_cabecalho-pymt_meth IS NOT INITIAL AND i_cabecalho-housebankid IS NOT INITIAL.
      wa_headerdata-pymt_meth   = i_cabecalho-pymt_meth.
      wa_headerdata-housebankid = i_cabecalho-housebankid.
    ELSE.
      CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
        EXPORTING
          p_bukrs           = i_cabecalho-comp_code
          p_lifnr           = i_cabecalho-lifnr
          p_zlsch           = i_cabecalho-pymt_meth
          p_valor           = p_valor
          p_bvtyp           = i_cabecalho-partner_bk
        IMPORTING
          p_forma_pagamento = wa_headerdata-pymt_meth
          p_princ_bnc_emp   = wa_headerdata-housebankid
        EXCEPTIONS
          nao_fornecedor    = 1
          fornecedor_conta  = 2
          fornecedor_banco  = 3
          faixa_valor       = 4
          banco_empresa     = 5
          OTHERS            = 6.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4
            msgty  = 'E'.
      ENDIF.
    ENDIF.

    IF i_cabecalho-pymt_meth EQ zcl_miro=>st_forma_pagamento_boleto AND i_cabecalho-boleto IS INITIAL.
      RAISE EXCEPTION TYPE zcx_miro_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto-msgid
                            msgno = zcx_miro_exception=>zcx_cod_barra_boleto-msgno )
          msgid  = zcx_miro_exception=>zcx_cod_barra_boleto-msgid
          msgno  = zcx_miro_exception=>zcx_cod_barra_boleto-msgno
          msgty  = 'E'.
    ELSEIF i_cabecalho-pymt_meth EQ zcl_miro=>st_forma_pagamento_boleto.

      IF i_cabecalho-doc_date_cal IS NOT INITIAL.
        zcl_miro=>verificar_cod_barra(
          EXPORTING
            i_boleto        = i_cabecalho-boleto
            i_valor         = CONV #( i_cabecalho-gross_amount ) " Montante em moeda interna
            i_dt_vencimento = i_cabecalho-doc_date_cal    " Data Vencimento.
        ).
      ELSE.
        zcl_miro=>verificar_cod_barra(
          EXPORTING
            i_boleto        = i_cabecalho-boleto
            i_valor         = CONV #( i_cabecalho-gross_amount ) " Montante em moeda interna
            i_dt_vencimento = i_cabecalho-doc_date_ven    " Data Vencimento.
        ).
      ENDIF.

      DATA: lc_barcode TYPE c LENGTH 47,
            lc_dmbtr   TYPE dmbtr.

      CALL FUNCTION 'CONVERSION_EXIT_ZBOLE_INPUT'
        EXPORTING
          input  = i_cabecalho-boleto
        IMPORTING
          output = lc_barcode.

      lc_dmbtr = i_cabecalho-gross_amount.

      CALL FUNCTION 'CONVERT_BARCODE'
        EXPORTING
          barcode   = lc_barcode
          dmbtr     = lc_dmbtr
        IMPORTING
          esrre     = wa_headerdata-po_ref_no
          esrnr     = wa_headerdata-po_sub_no
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
                              msgno = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno )
            msgid  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgid
            msgno  = zcx_miro_exception=>zcx_cod_barra_boleto_errado-msgno
            msgty  = 'E'.
      ENDIF.

    ENDIF.
    "CS2020000586
    IF i_cabecalho-chave_nfe IS NOT INITIAL.

      CLEAR lwa_zib_nfe_dist_ter.

      SELECT SINGLE * FROM zib_nfe_dist_ter INTO lwa_zib_nfe_dist_ter
        WHERE chave_nfe EQ  i_cabecalho-chave_nfe.

    ENDIF.
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
    IF lwa_zib_nfe_dist_ter-se_recordid IS NOT INITIAL.
      "CS2020000586
      wa_headerdata-item_text      = lwa_zib_nfe_dist_ter-se_recordid.
    ELSE.
      wa_headerdata-item_text      = i_cabecalho-item_text.
    ENDIF.

    "Chave p/ Bloqueio de Pagamento
    wa_headerdata-pmnt_block     = i_cabecalho-pmnt_block.

    IF i_cabecalho-pmnttrms IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_t052)
        FROM t052
       WHERE zterm EQ @i_cabecalho-pmnttrms.

      "Chave de Condição de Pagamento
      wa_headerdata-pmnttrms   = i_cabecalho-pmnttrms.
      wa_headerdata-dsct_days1 = i_cabecalho-doc_date_cal - i_cabecalho-doc_date_ven.

      IF wa_t052-ztag1 = wa_headerdata-dsct_days1.
        CLEAR: wa_headerdata-dsct_days1.
      ELSE.
        CLEAR: wa_headerdata-pmnttrms.
      ENDIF.

    ELSE.
      "Dias de desconto 1
      wa_headerdata-dsct_days1 = i_cabecalho-doc_date_cal - i_cabecalho-doc_date_ven.
    ENDIF.

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
    IF i_cabecalho-pymt_meth NE zcl_miro=>st_forma_pagamento_boleto.
      wa_headerdata-partner_bk   = i_cabecalho-partner_bk.
    ENDIF.

    "Tipo de Documento
    wa_headerdata-j_1bnftype     = i_cabecalho-j_1bnftype.

*    zcl_miro=>verificar_valor_nfe( i_chave_nfe = i_cabecalho-chave_nfe
*                                   i_wrbtr     = p_valor
*                                   "I_DESCONTO  = P_DESCONTO
*                                   i_waers     = i_cabecalho-currency
*                                   i_kursf     = i_cabecalho-exch_rate ).

    SELECT SINGLE * INTO @DATA(wa_j_1baa)
      FROM j_1baa
     WHERE nftype EQ @i_cabecalho-j_1bnftype.

    wa_headerdata-ref_doc_no = zcl_miro=>get_chave_referencia(
        i_nf_number  = i_cabecalho-nf_number
        i_series     = i_cabecalho-series
        i_subseries  = i_cabecalho-subseries
        i_nf_number9 = i_cabecalho-nf_number9 ).

    "Nota Fiscal Eletrônica - NF-e
    IF i_cabecalho-nf_number9 IS NOT INITIAL.
      IF wa_j_1baa-form IS NOT INITIAL OR wa_j_1baa-model NE '55' AND wa_j_1baa-doctyp NE '1'  AND wa_j_1baa-direct NE '1' AND wa_j_1baa-nfe  NE abap_true.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
                              msgno = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
                              attr1 = CONV #( i_cabecalho-j_1bnftype ) )
            msgid  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
            msgno  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
            msgv1  = CONV #( i_cabecalho-j_1bnftype )
            msgty  = 'E'.
      ENDIF.
    ELSE.
      IF wa_j_1baa-form IS NOT INITIAL OR wa_j_1baa-model NE '01' AND wa_j_1baa-doctyp NE '1'  AND wa_j_1baa-direct NE '1' AND wa_j_1baa-nfe  NE abap_false.
        RAISE EXCEPTION TYPE zcx_miro_exception
          EXPORTING
            textid = VALUE #( msgid = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
                              msgno = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
                              attr1 = CONV #( i_cabecalho-j_1bnftype ) )
            msgid  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgid
            msgno  = zcx_miro_exception=>zcx_cat_fiscal_ne_nfe-msgno
            msgv1  = CONV #( i_cabecalho-j_1bnftype )
            msgty  = 'E'.
      ENDIF.
    ENDIF.

*    CALL METHOD zcl_miro=>verificar_forn_doc_fiscal
*      EXPORTING
*        i_lifnr  = i_cabecalho-lifnr
*        i_nftype = i_cabecalho-j_1bnftype
*        i_xblnr  = wa_headerdata-ref_doc_no
*        i_data   = wa_headerdata-doc_date
*        i_werks  = wa_headerdata-bus_area.
    break rblima.
    LOOP AT i_itens	INTO DATA(wa_itens).

      v_count = sy-tabix.

      IF wa_itens-po_number IS NOT INITIAL.
        SELECT SINGLE bsart
          FROM ekko
          INTO @DATA(v1_bsart)
         WHERE ebeln EQ @wa_itens-po_number.

        zcl_miro=>verificar_tipo_pedido( i_bsart = v1_bsart i_budat = i_cabecalho-doc_date_mov ).
        zcl_miro=>verificar_tipo_documento( i_bsart = v1_bsart i_blart = i_cabecalho-doc_type ).
        zcl_miro=>verificar_chave_bloqueio( i_bsart = v1_bsart i_zlspr = i_cabecalho-pmnt_block ).
        "
        IF wa_headerdata-paymt_ref IS INITIAL.
          SELECT SINGLE *
            INTO @DATA(_w0035)
            FROM zmmt0035
            WHERE ebeln = @wa_itens-po_number.
          IF sy-subrc = 0.
            wa_headerdata-paymt_ref = _w0035-nro_sol_cp.
          ENDIF.
        ENDIF.

      ENDIF.


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


      IF wa_itemdata-po_number IS NOT INITIAL AND wa_itemdata-po_item IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_ekpo)
          FROM ekpo
         WHERE ebeln EQ @wa_itemdata-po_number
           AND ebelp EQ @wa_itemdata-po_item.

        IF sy-subrc IS INITIAL AND
           wa_ekpo-pstyp <> '1' AND
           wa_ekpo-pstyp <> '9'.
          wa_itemdata-po_pr_uom = wa_ekpo-bprme.
        ENDIF.

        IF  wa_ekpo-pstyp = '9'.
          wa_itemdata-sheet_no = ms_nfse_001-lblni.
        ENDIF.

        IF v1_bsart EQ 'ZDBP' AND sy-tcode = 'ZMM0110'.

          CLEAR: vg_vlt_total, vg_vlt_item, vg_calc.

          wa_itemdata-item_amount      = '0'.
          wa_itemdata-quantity         = '1'.

          SELECT  *  FROM ekpo INTO TABLE @DATA(it_ekpo)
            WHERE ebeln EQ @wa_ekpo-bednr
            AND   knttp EQ ' '.

          SELECT * FROM mara  INTO TABLE @DATA(it_mara)
            FOR ALL ENTRIES IN @it_ekpo
              WHERE matnr EQ @it_ekpo-matnr.

          LOOP AT it_ekpo INTO DATA(wl_ekpo).
            vg_calc = ( wl_ekpo-menge * wl_ekpo-netpr ).
            vg_vlt_total = vg_vlt_total + vg_calc.
          ENDLOOP.

          vg_item = 0.

          LOOP AT it_ekpo INTO wl_ekpo.
            CLEAR: vg_calc, vg_vlt_item.


            vg_calc = ( wl_ekpo-menge * wl_ekpo-netpr ).
            vg_vlt_item = vg_calc / vg_vlt_total.


            READ TABLE it_mara INTO DATA(wl_mara) WITH KEY matnr = wl_ekpo-matnr.

            wa_materialdata-invoice_doc_item = vg_item + 1.

* ---> S4 Migração - 21/06/2023 - FC - Inicio
      "wa_materialdata-material = wl_ekpo-matnr.

      DATA(v_len) = strlen( wl_ekpo-matnr ).

      IF v_len > 18.
        wa_materialdata-material_long = wl_ekpo-matnr.
      ELSE.
        wa_materialdata-material = wl_ekpo-matnr.
      ENDIF.
* <--- S4 Migração - 21/06/2023 - FC - Fim

            wa_materialdata-val_area         = wl_ekpo-werks.
            wa_materialdata-valuation_type   = wl_ekpo-bwtar.
            wa_materialdata-db_cr_ind        = 'S'.
            wa_materialdata-item_amount      = wa_ekpo-netwr * vg_vlt_item.
            wa_materialdata-quantity         = 1.
            wa_materialdata-base_uom         = wl_mara-meins.
            wa_materialdata-tax_code         = wa_itens-tax_code.
            wa_materialdata-taxjurcode       = wa_itens-taxjurcode.

            APPEND wa_materialdata TO it_materialdata.
            CLEAR wa_materialdata.

            CLEAR wl_ekpo.
          ENDLOOP.

        ELSE.
          wa_itemdata-item_amount      = wa_itens-item_amount.
          wa_itemdata-quantity         = wa_itens-quantity.
        ENDIF.
      ENDIF.

      APPEND wa_itemdata TO it_itemdata.

**** BUG 49216 (Inicio) - Erro classe ZCL_MIRO->CRIAR, não esta passando para a bapi a tabela ACCOUNTINGDATA
      SELECT  sakto kostl vbeln vbelp anln1 anln2 dabrz
                        fistl geber grant_nbr gsber imkey kokrs kstrg paobjnr
                        prctr ps_psp_pnr aufnr menge zekkn aufpl
            FROM ekkn
            INTO (wa_accountingdata-gl_account, wa_accountingdata-costcenter,
                  wa_accountingdata-sd_doc, wa_accountingdata-sdoc_item,
                  wa_accountingdata-asset_no, wa_accountingdata-sub_number,
                  wa_accountingdata-ref_date, wa_accountingdata-funds_ctr,
                  wa_accountingdata-fund, wa_accountingdata-grant_nbr,
                  wa_accountingdata-bus_area, wa_accountingdata-rl_est_key,
                  wa_accountingdata-co_area, wa_accountingdata-costobject,
                  wa_accountingdata-profit_segm_no, wa_accountingdata-profit_ctr,
                  wa_accountingdata-wbs_elem, wa_accountingdata-orderid,
                  wa_accountingdata-quantity, wa_accountingdata-serial_no, v_aufpl)
                WHERE ebeln EQ wa_itemdata-po_number
                AND   ebelp EQ wa_itemdata-po_item

                AND   EXISTS ( SELECT * FROM ekpo WHERE ekpo~ebeln = ekkn~ebeln AND ekpo~ebelp = ekkn~ebelp AND ekpo~vrtkz NE ' ').

        wa_accountingdata-invoice_doc_item = wa_itemdata-invoice_doc_item.
        wa_accountingdata-po_unit          = wa_itemdata-po_unit.
        wa_accountingdata-tax_code         = wa_itemdata-tax_code.
        IF v_aufpl IS NOT INITIAL.
          SELECT SINGLE vornr
            INTO wa_accountingdata-activity
            FROM afvc
            WHERE aufpl = v_aufpl.
        ENDIF.
        IF wa_accountingdata-quantity GT 0.
          wa_accountingdata-item_amount      = wa_itemdata-item_amount * (  wa_accountingdata-quantity / wa_itens-quantity ) .
          APPEND wa_accountingdata TO it_accountingdata.
        ENDIF.
        CLEAR: wa_accountingdata, v_aufpl.
      ENDSELECT.
**** BUG 49216 (Fim) - Erro classe ZCL_MIRO->CRIAR, não esta passando para a bapi a tabela ACCOUNTINGDATA

      LOOP AT i_contas INTO DATA(wa_conta) WHERE invoice_doc_item EQ wa_itens-invoice_doc_item.
        APPEND wa_conta TO it_contas.
      ENDLOOP.

    ENDLOOP.

    DELETE it_accountingdata WHERE gl_account = ' ' AND  costcenter = ' ' AND   asset_no = ' '.  "IR091638  lportelA

    p_valor = wa_headerdata-gross_amount.

*    "Validar Forma de Pagamento e Banco Empresa
*    CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
*      EXPORTING
*        p_bukrs           = i_cabecalho-comp_code
*        p_lifnr           = i_cabecalho-lifnr
*        p_zlsch           = wa_headerdata-pymt_meth
*        p_valor           = p_valor
*        p_bvtyp           = i_cabecalho-partner_bk
*      IMPORTING
*        p_forma_pagamento = p_forma_pagamento "WA_HEADERDATA-PYMT_METH
*        p_princ_bnc_emp   = p_princ_bnc_emp  "WA_HEADERDATA-HOUSEBANKID
*      EXCEPTIONS
*        nao_fornecedor    = 1
*        fornecedor_conta  = 2
*        fornecedor_banco  = 3
*        faixa_valor       = 4
*        banco_empresa     = 5
*        OTHERS            = 6.
*
*    IF sy-subrc IS NOT INITIAL.
*      RAISE EXCEPTION TYPE zcx_miro_exception
*        EXPORTING
*          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
*          msgid  = sy-msgid
*          msgno  = sy-msgno
*          msgv1  = sy-msgv1
*          msgv2  = sy-msgv2
*          msgv3  = sy-msgv3
*          msgv4  = sy-msgv4
*          msgty  = 'E'.
*    ENDIF.

*    IF p_forma_pagamento NE wa_headerdata-pymt_meth OR p_princ_bnc_emp NE wa_headerdata-housebankid.
*      RAISE EXCEPTION TYPE zcx_miro_exception
*        EXPORTING
*          textid = VALUE #( msgid = zcx_miro_exception=>zcx_forma_paga_banco-msgid
*                            msgno = zcx_miro_exception=>zcx_forma_paga_banco-msgno
*                            attr1 = CONV #( wa_headerdata-pymt_meth )
*                            attr2 = CONV #( wa_headerdata-housebankid ) )
*          msgid  = zcx_miro_exception=>zcx_forma_paga_banco-msgid
*          msgno  = zcx_miro_exception=>zcx_forma_paga_banco-msgno
*          msgty  = 'E'
*          msgv1  = CONV #( wa_headerdata-pymt_meth )
*          msgv2  = CONV #( wa_headerdata-housebankid ).
*    ENDIF.

    REFRESH it_withtaxdata.
    IF 'ZDEF_ZFTE_ZSEM' CS v1_bsart AND sy-tcode = 'ZMM0110'.
      "impostos retidos
      CALL FUNCTION 'FI_WT_READ_LFBW'
        EXPORTING
          i_lifnr   = i_cabecalho-lifnr
          i_bukrs   = i_cabecalho-comp_code
        TABLES
          t_lfbw    = lt_lfbw
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc EQ 0.
        IF lt_lfbw[] IS NOT INITIAL.
          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              class         = '0000'
              setnr         = 'MAGGI_ZMM0110_SEM'
            TABLES
              set_values    = t_grupo
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
          SORT t_grupo BY from.
          LOOP AT i_itens  INTO DATA(wa_itens2).
            IF wa_itens-po_number IS NOT INITIAL.
              SELECT SINGLE mara~matkl
                FROM ekpo
                INNER JOIN mara
                ON mara~matnr = ekpo~matnr
                INTO @DATA(v1_matkl)
               WHERE ebeln EQ @wa_itens2-po_number
               AND   ebelp EQ @wa_itens2-po_item.
            ENDIF.
            READ TABLE t_grupo INTO DATA(w_grupo) WITH KEY from = v1_matkl.
            IF sy-subrc = 0.
              DELETE lt_lfbw WHERE witht = 'IF'.
              DELETE lt_lfbw WHERE witht = 'FR'.
              EXIT.
            ENDIF.

          ENDLOOP.

          LOOP AT lt_lfbw INTO DATA(w_lfbw).
            wa_withtaxdata-split_key   = '000001'.
            wa_withtaxdata-wi_tax_type = w_lfbw-witht.
            wa_withtaxdata-wi_tax_code = w_lfbw-wt_withcd.
            APPEND wa_withtaxdata TO it_withtaxdata.

          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

*    FREE: it_accountingdata. "Correção do BUG 49216.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        headerdata       = wa_headerdata
      IMPORTING
        invoicedocnumber = invoicedocnumber
        fiscalyear       = fiscalyear
      TABLES
        itemdata         = it_itemdata
        accountingdata   = it_accountingdata " BUG 49216
        glaccountdata    = it_contas
        materialdata     = it_materialdata
        withtaxdata      = it_withtaxdata
        return           = it_return.        "TAXDATA = IT_IMPOSTOS


    append_global_message( it_return ).

    IF ( invoicedocnumber IS NOT INITIAL ) AND ( fiscalyear IS NOT INITIAL ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = i_bapi_wait.

      e_invoicedocnumber = invoicedocnumber.
      e_fiscalyear       = fiscalyear.

      r_gerou = abap_true.

      IF i_lines_miro_texto IS NOT INITIAL.
        wa_header-tdobject = 'RBKP'.
        CONCATENATE invoicedocnumber fiscalyear INTO wa_header-tdname.
        wa_header-tdid    = '0001'.
        wa_header-tdspras = sy-langu.

        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            header          = wa_header
            savemode_direct = abap_true
          TABLES
            lines           = i_lines_miro_texto[]
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.
      ENDIF.

      "Resgistro fiscal gerado pela MIRO
      IF wa_headerdata-j_1bnftype IS NOT INITIAL.

        SELECT SINGLE * INTO e_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE belnr EQ e_invoicedocnumber
           AND gjahr EQ e_fiscalyear.

        IF sy-subrc IS NOT INITIAL.

          WAIT UP TO 5 SECONDS.

          SELECT SINGLE * INTO e_j_1bnfdoc
            FROM j_1bnfdoc
           WHERE belnr EQ e_invoicedocnumber
             AND gjahr EQ e_fiscalyear.

          DATA: lc_ciclos	TYPE zde_qtd_ciclos,
                lc_tempo  TYPE zde_qtd_segundos_ciclo.

          lc_ciclos = 60.
          lc_tempo  = 5.

          WHILE lc_ciclos IS NOT INITIAL AND e_j_1bnfdoc IS INITIAL.

            "Tempo de Cadas Ciclo
            WAIT UP TO lc_tempo SECONDS.

            SELECT SINGLE * INTO e_j_1bnfdoc
              FROM j_1bnfdoc
             WHERE belnr EQ e_invoicedocnumber
               AND gjahr EQ e_fiscalyear.

            SUBTRACT 1 FROM lc_ciclos.

          ENDWHILE.

        ENDIF.
      ENDIF.

      CALL METHOD me->append_message2
        EXPORTING
          i_msgid = 'ZOB_MIRO'
          i_msgno = '002'
          i_msgv1 = e_invoicedocnumber
          i_msgv2 = e_fiscalyear.

    ELSE.
      r_gerou = abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDMETHOD.


  METHOD danfe.

    DATA: it_urllist TYPE tihttpurls2.

    CALL FUNCTION 'HTTP_GET_URL2'
      EXPORTING
        handlerclass     = 'ZCL_FMCALL_DOC_FISCAL'
      IMPORTING
        urllist          = it_urllist
      EXCEPTIONS
        http_not_enabled = 1
        OTHERS           = 2.

    CHECK sy-subrc IS INITIAL.

    READ TABLE it_urllist INDEX 1 INTO DATA(wa_urllist).

    DATA(wa_dominio) = wa_urllist-protocol && '://' && wa_urllist-host && ':' && wa_urllist-port && wa_urllist-url.

    e_url = wa_dominio && '/getnfepdf?' && 'sap-client=' && sy-mandt && '&i_guid=' && ms_nfse_001-guid_header.


  ENDMETHOD.


  METHOD DEQUEUE_GUID.

    CALL FUNCTION 'DEQUEUE_/TCSR/ENQ_GUID'
      EXPORTING
        mode_/tcsr/s_enq_guid = 'E'
        mandt                 = sy-mandt
        guid_header           = me->mv_guid
        _scope                = '3'.

  ENDMETHOD.


  METHOD ENQUEUE_GUID.

    DATA:
      lv_attr1 TYPE scx_attrname,
      lv_attr2 TYPE scx_attrname.
*-----------------------------------------------------------------------

    CALL FUNCTION 'ENQUEUE_/TCSR/ENQ_GUID'
      EXPORTING
        mode_/tcsr/s_enq_guid = 'E'
        mandt                 = sy-mandt
        guid_header           = me->mv_guid
        _scope                = '3'
      EXCEPTIONS
        foreign_lock          = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc NE 0.

*      lv_attr1 = me->mv_guid_header.
*      lv_attr2 = sy-msgv1.
*      "Set Message Key
*      mo_util_xml->set_msgkey( iv_msgno    = '001'  "GUID &1 in processing by &2
*                               in_attr1    = lv_attr1
*                               in_attr2    = lv_attr2 ).
*      RAISE EXCEPTION TYPE /tcsr/cx_exception
*        EXPORTING
*          textid = mo_util_xml->mw_msgkey.

    ENDIF.

  ENDMETHOD.


  METHOD estorno.

    DATA lv_mess TYPE string.

    fill_all( ).

    clear_message( ).

    IF ms_nfse_001-belnr IS NOT INITIAL.

      lv_mess = `A fatura: ` && ms_nfse_001-belnr && ` só pode ser estornada no SE`.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    IF ms_nfse_001-se_recordid IS NOT INITIAL AND ms_nfse_001-ck_revisao = abap_false.
      lv_mess = `A: ` && ms_nfse_001-se_recordid && ` está em processamento no SE`.
      append_message( lv_mess ).
      EXIT.
    ENDIF.

    IF check_authorization( 'ZFIS_EST' ) NE 'X'.

      lv_mess = `Sem autorização para estornar documento`.

      append_message( lv_mess ).

      EXIT.

    ENDIF.

    IF ms_nfse_001-lblni IS INITIAL.
      LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002_>).
        IF <fs_nfse_002_>-lblni IS NOT INITIAL OR <fs_nfse_002_>-mblnr IS NOT INITIAL.
          ms_nfse_001-lblni = <fs_nfse_002_>-lblni.
          ms_nfse_001-mblnr = <fs_nfse_002_>-mblnr.
          ms_nfse_001-mjahr = <fs_nfse_002_>-mjahr.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF ms_nfse_001-lblni IS NOT INITIAL.
      LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>) WHERE pstyp = '9'.
        IF <fs_nfse_002>-lblni IS NOT INITIAL.
          ms_nfse_001-lblni = <fs_nfse_002>-lblni.
        ENDIF.
        IF ms_nfse_001-lblni IS INITIAL.
          CONTINUE.
        ENDIF.

        IF me->nfse_inbound_estorno_folha( ) IS INITIAL.

          CLEAR ms_nfse_001-lblni.
          CLEAR ms_nfse_001-packno.
          CLEAR ms_nfse_001-mblnr.
          CLEAR ms_nfse_001-mjahr.

          CLEAR <fs_nfse_002>-lblni.
          CLEAR <fs_nfse_002>-packno.
          CLEAR <fs_nfse_002>-mblnr.
          CLEAR <fs_nfse_002>-mjahr.


*        CLEAR ms_nfse_001-se_status.
*        CLEAR ms_nfse_001-se_code.
*        CLEAR ms_nfse_001-se_detail.
*        CLEAR ms_nfse_001-se_recordkey.
*        CLEAR ms_nfse_001-se_recordid.


          save( '1' ).
          save( '2' ).

        ENDIF.
      ENDLOOP.
      UPDATE /tcsr/t_act  SET last_stepstatus = '88'
          WHERE guid_header = ms_nfse_001-guid_header.
      COMMIT WORK.


      EXIT.

    ENDIF.

    " Estornar MIGO
    IF ms_nfse_001-mblnr IS NOT INITIAL.
      LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002_2>) WHERE pstyp = '0'.
        IF <fs_nfse_002_2>-mblnr IS NOT INITIAL.
          ms_nfse_001-mblnr = <fs_nfse_002_2>-mblnr.
          ms_nfse_001-mjahr = <fs_nfse_002_2>-mjahr.
        ENDIF.
        IF ms_nfse_001-mblnr IS INITIAL.
          CONTINUE.
        ENDIF.

        me->nfse_inbound_estorno_doc_mat( ).

        CLEAR ms_nfse_001-lblni.
        CLEAR ms_nfse_001-packno.
        CLEAR ms_nfse_001-mblnr.
        CLEAR ms_nfse_001-mjahr.

        CLEAR <fs_nfse_002_2>-lblni.
        CLEAR <fs_nfse_002_2>-packno.
        CLEAR <fs_nfse_002_2>-mblnr.
        CLEAR <fs_nfse_002_2>-mjahr.

*      CLEAR ms_nfse_001-se_status.
*      CLEAR ms_nfse_001-se_code.
*      CLEAR ms_nfse_001-se_detail.
*      CLEAR ms_nfse_001-se_recordkey.
*      CLEAR ms_nfse_001-se_recordid.

        save( '1' ).
        save( '2' ).

      ENDLOOP.

      EXIT.

    ENDIF.


    lv_mess = `Já foi estornado todos os documentos`.

    append_message( lv_mess ).

  ENDMETHOD.


  METHOD execute_ml81n_delete.


    TYPES: BEGIN OF ty_essr,
             lblni TYPE essr-lblni,
             kzabn TYPE essr-kzabn,
           END OF ty_essr.
    DATA:
      lt_shdb_messages TYPE TABLE OF bdcmsgcoll,
      lt_return        TYPE TABLE OF bapiret2,
      lt_doc_sheet     TYPE TABLE OF /tcsr/t_ref,
      lt_essr          TYPE TABLE OF ty_essr.
    DATA:
      lw_shdb_messages TYPE bdcmsgcoll,
      lw_return        TYPE bapiret2,
      lw_log_handle    TYPE balloghndl.
    "lw_essr          TYPE ty_essr.
    DATA:
      lv_kzabn    TYPE essr-kzabn.
    DATA:
      lo_bal_log TYPE REF TO /tcsr/c_bal_log.

    FIELD-SYMBOLS: <fs_po_upd> TYPE /tcsr/t_po.
*------------------------------------------------

    DO 20 TIMES.

      "Check if Serv.EntrySheet is released
      SELECT SINGLE lblni,kzabn
        INTO @DATA(lw_essr)
        FROM essr
        WHERE lblni = @ms_nfse_001-lblni.

      IF lw_essr-kzabn EQ 'X'.
        WAIT UP TO 2 SECONDS.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

    SELECT lblni kzabn
      INTO TABLE lt_essr
      FROM essr
      WHERE lblni = ms_nfse_001-lblni.

    LOOP AT lt_essr INTO lw_essr.

      REFRESH lt_shdb_messages.

      "Check if Serv.EntrySheet is RELEASED
      IF  lw_essr-kzabn EQ 'X'.

        CALL METHOD me->append_message2
          EXPORTING
            i_msgty = 'E'
            i_msgid = 'SE'
            i_msgno = '106'.

        CALL METHOD me->append_message2
          EXPORTING
            i_msgty = 'E'
            i_msgid = '/TCSR/MSG'
            i_msgno = '306'
            i_msgv1 = lw_essr-lblni.

        log_to_tab( ).

        save( '1' ).

        EXIT.

      ENDIF.

      "----------------------------------------
      " DELETE SERV.ENTRYSHEET
      "----------------------------------------
      CALL FUNCTION 'BAPI_ENTRYSHEET_DELETE'
        EXPORTING
          entrysheet = lw_essr-lblni
        TABLES
          return     = lt_return.

      APPEND LINES OF lt_return TO mt_mess.

      log_to_tab( ).

      IF line_exists( lt_return[ type = 'E' ] ).

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        CALL METHOD me->append_message2
          EXPORTING
            i_msgty = 'E'
            i_msgid = '/TCSR/MSG'
            i_msgno = '306'
            i_msgv1 = lw_essr-lblni.

*        ms_nfse_001-lblni = ''.

        save( '1' ).

      ELSE.

        CALL METHOD me->append_message2
          EXPORTING
            i_msgty = 'S'
            i_msgid = '/TCSR/MSG'
            i_msgno = '307'
            i_msgv1 = lw_essr-lblni.

        ms_nfse_001-lblni = ''.
        ms_nfse_001-packno = ''.

        save( '1' ).
        wait UP TO 5 SECONDS.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_0034.

    READ TABLE mt_nfse_0034 TRANSPORTING NO FIELDS
      WITH KEY guid_header = mv_guid.

    IF i_force = 'X' OR sy-subrc NE 0.

      CLEAR: mt_nfse_0034, ms_total_0034.

      SELECT * FROM zlest0034
         INTO TABLE mt_nfse_0034
          WHERE guid_header = mv_guid.

    ENDIF.

    CHECK ms_total_0034 IS INITIAL.

    LOOP AT mt_nfse_0034 ASSIGNING FIELD-SYMBOL(<fs_0034>).

      ADD <fs_0034>-dmbtr TO ms_total_0034-dmbtr.
      ADD <fs_0034>-valor_cofins TO ms_total_0034-valor_cofins.
      ADD <fs_0034>-valor_pis TO ms_total_0034-valor_pis.
      ADD <fs_0034>-valor_icms TO ms_total_0034-valor_icms.
      "ADD <fs_0034>-valor_ipi TO ms_total_0034-valor_ipi.

      ms_total_0034-rate_cofins = <fs_0034>-rate_cofins.
      ms_total_0034-rate_pis = <fs_0034>-rate_pis.
      "ms_total_0034-rate_ipi = <fs_0034>-rate_ipi.

      IF ms_total_0034-valor_pis IS NOT INITIAL.

        IF ms_total_0034-rate_pis IS INITIAL.
          ms_total_0034-rate_pis = '1.65'.
        ENDIF.

      ENDIF.

      IF ms_total_0034-valor_cofins IS NOT INITIAL.

        IF ms_total_0034-rate_cofins IS INITIAL.
          ms_total_0034-rate_cofins = '7.6'.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_account.
    DATA:
      wa_ekpo                      TYPE ekpo,
      wa_ekko                      TYPE ekko,
      it_entrysheet_account_assign TYPE STANDARD TABLE OF bapieskn,
      it_entrysheet_srv_accass_val TYPE STANDARD TABLE OF bapieskl,
      it_return	                   TYPE STANDARD TABLE OF bapireturn1,
      wa_accountingdata            TYPE bapi_incinv_create_account,
      v_aufpl                      TYPE ekkn-aufpl,
      v_serial_no                  TYPE dzekkn,
      v_menge                      TYPE ekpo-menge,
      v_aplzl                      TYPE ekkn-aplzl,
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Início de Alteração
      v_netwr                      TYPE ekkn-netwr,
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Fim de Alteração
*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Início de Alteração
      vg_zekkn                      TYPE ekkn-zekkn.
*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Fim de Alteração

    LOOP AT it_itens ASSIGNING FIELD-SYMBOL(<fs_item>).

      LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>)
          WHERE ebeln = <fs_item>-po_number
            AND ebelp = <fs_item>-po_item.

        IF <fs_nfse_002>-pstyp = '9'.
          SELECT SINGLE *
            INTO wa_ekpo
            FROM ekpo
          WHERE ebeln EQ <fs_item>-po_number
          AND   ebelp EQ <fs_item>-po_item
          AND   twrkz NE ' '. "somente com rateio
          IF sy-subrc = 0.
            v_serial_no = 1.
            SELECT  ekkn~sakto ekkn~kostl vbeln vbelp anln1 anln2 dabrz
                    ekkn~fistl ekkn~geber ekkn~grant_nbr gsber imkey kokrs kstrg paobjnr
                    prctr ekkn~ps_psp_pnr aufnr ekkn~menge zekkn  aufpl aplzl ekpo~menge
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Início de Alteração
                    ekkn~netwr
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Fim de Alteração
              FROM ekkn
              INNER JOIN ekpo
              ON  ekpo~ebeln = ekkn~ebeln
              AND ekpo~ebelp = ekkn~ebelp
              INTO (wa_accountingdata-gl_account, wa_accountingdata-costcenter,
                    wa_accountingdata-sd_doc, wa_accountingdata-sdoc_item,
                    wa_accountingdata-asset_no, wa_accountingdata-sub_number,
                    wa_accountingdata-ref_date, wa_accountingdata-funds_ctr,
                    wa_accountingdata-fund, wa_accountingdata-grant_nbr,
                    wa_accountingdata-bus_area, wa_accountingdata-rl_est_key,
                    wa_accountingdata-co_area, wa_accountingdata-costobject,
                    wa_accountingdata-profit_segm_no, wa_accountingdata-profit_ctr,
                    wa_accountingdata-wbs_elem, wa_accountingdata-orderid,
                    wa_accountingdata-quantity, wa_accountingdata-serial_no, v_aufpl , v_aplzl,  v_menge,
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Início de Alteração
                    v_netwr )
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Fim de Alteração
             WHERE ekkn~ebeln EQ <fs_item>-po_number
                AND   ekkn~ebelp EQ <fs_item>-po_item
                AND   ekpo~twrkz NE ' ' "somente com rateio
              ORDER BY zekkn.

*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Início de Alteração
              vg_zekkn = wa_accountingdata-serial_no.
*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Fim de Alteração

              wa_accountingdata-serial_no        = v_serial_no.
              wa_accountingdata-invoice_doc_item = <fs_item>-invoice_doc_item.
              wa_accountingdata-po_unit          = <fs_item>-po_unit.
              wa_accountingdata-tax_code         = <fs_item>-tax_code.
              IF v_aufpl IS NOT INITIAL.
                SELECT SINGLE vornr
                  INTO wa_accountingdata-activity
                  FROM afvc
                  WHERE aufpl = v_aufpl
                  AND   aplzl = v_aplzl.
              ENDIF.
              IF wa_accountingdata-quantity GT 0.
                wa_accountingdata-item_amount      = ( <fs_nfse_002>-dmbtr_iv - <fs_nfse_002>-netwr_imp ) * ( wa_accountingdata-quantity / v_menge ).
* INICIO - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
*                wa_accountingdata-quantity = 0. "no serviço zera a quantidade

                READ TABLE mt_ekpo_lebre INTO DATA(ls_ekpo_lebre) WITH KEY ebeln = <fs_item>-po_number
                                                                         ebelp = <fs_item>-po_item BINARY SEARCH.

                IF sy-subrc IS INITIAL AND ls_ekpo_lebre-vrtkz IS NOT INITIAL. " Se tiver rateio...

*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Início de Alteração

                  SELECT SINGLE aufnr,
                                kostl
                    FROM eskn
                    INTO @DATA(ls_eskn)
                    WHERE packno EQ @<fs_item>-ref_doc
                    AND   bekkn  EQ @vg_zekkn.

                    IF sy-subrc IS INITIAL.

                      wa_accountingdata-costcenter = ls_eskn-kostl.
                      wa_accountingdata-orderid = ls_eskn-aufnr.

                    ENDIF.

*** Stefanini - IR255180 - 08/09/2025 - RBRIBEIRO - Fim de Alteração

*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Início de Alteração
*                   wa_accountingdata-quantity = wa_accountingdata-item_amount.
                  wa_accountingdata-quantity = v_netwr.
*** Stefanini - IR236300 - 07/05/2025 - LAZAROSR - Fim de Alteração

                ELSE.
                  wa_accountingdata-quantity = 0.
                ENDIF.
* FIM - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
                APPEND wa_accountingdata TO r_account_tab.
              ENDIF.
              CLEAR: wa_accountingdata, v_aufpl.
              ADD 1 TO v_serial_no.
            ENDSELECT.

*            CALL FUNCTION 'BAPI_ENTRYSHEET_GETDETAIL'
*              EXPORTING
*                entrysheet                    = <fs_nfse_002>-lblni
*              TABLES
*                entrysheet_account_assignment = it_entrysheet_account_assign
*                entrysheet_srv_accass_values  = it_entrysheet_srv_accass_val
*                return                        = it_return.
*            SELECT SINGLE *
*                 FROM ekko
*                 INTO wa_ekko
*                 WHERE ebeln = <fs_item>-po_number.
*
*            LOOP AT it_entrysheet_account_assign INTO DATA(w_entrysheet_account_assign).
*              APPEND INITIAL LINE TO r_account_tab ASSIGNING FIELD-SYMBOL(<fs_account>).
*
*              <fs_account>-invoice_doc_item = <fs_item>-invoice_doc_item..
*              <fs_account>-serial_no   = w_entrysheet_account_assign-serial_no.
*              <fs_account>-tax_code    = w_entrysheet_account_assign-tax_code.
*              <fs_account>-taxjurcode  = w_entrysheet_account_assign-taxjurcode.
*              <fs_account>-item_amount = w_entrysheet_account_assign-accass_val.
*              IF wa_ekko-bsart+0(1) = 'Y' AND <fs_nfse_002>-netwr_imp GT 0.
*                <fs_account>-item_amount = <fs_account>-item_amount - ( <fs_nfse_002>-netwr_imp  * ( w_entrysheet_account_assign-percentage / 100 ) ).
*              ENDIF.
*              <fs_account>-co_area     = w_entrysheet_account_assign-co_area.
*              <fs_account>-profit_ctr  = w_entrysheet_account_assign-profit_ctr.
*              <fs_account>-gl_account  = w_entrysheet_account_assign-gl_account.
*              <fs_account>-costcenter  = w_entrysheet_account_assign-costcenter.
*              <fs_account>-bus_area    = w_entrysheet_account_assign-bus_area.
*              <fs_account>-quantity    = 0.
*            ENDLOOP.
          ENDIF.
        ELSE.

          SELECT  ekkn~sakto ekkn~kostl vbeln vbelp anln1 anln2 dabrz
                     ekkn~fistl ekkn~geber ekkn~grant_nbr gsber imkey kokrs kstrg paobjnr
                     prctr ekkn~ps_psp_pnr aufnr ekkn~menge zekkn  aufpl aplzl ekpo~menge
           FROM ekkn
           INNER JOIN ekpo
           ON  ekpo~ebeln = ekkn~ebeln
           AND ekpo~ebelp = ekkn~ebelp
           INTO (wa_accountingdata-gl_account, wa_accountingdata-costcenter,
                 wa_accountingdata-sd_doc, wa_accountingdata-sdoc_item,
                 wa_accountingdata-asset_no, wa_accountingdata-sub_number,
                 wa_accountingdata-ref_date, wa_accountingdata-funds_ctr,
                 wa_accountingdata-fund, wa_accountingdata-grant_nbr,
                 wa_accountingdata-bus_area, wa_accountingdata-rl_est_key,
                 wa_accountingdata-co_area, wa_accountingdata-costobject,
                 wa_accountingdata-profit_segm_no, wa_accountingdata-profit_ctr,
                 wa_accountingdata-wbs_elem, wa_accountingdata-orderid,
                 wa_accountingdata-quantity, wa_accountingdata-serial_no, v_aufpl , v_aplzl,  v_menge)
          WHERE ekkn~ebeln EQ <fs_item>-po_number
             AND   ekkn~ebelp EQ <fs_item>-po_item
             AND   ekpo~twrkz NE ' '. "somente com rateio


            wa_accountingdata-invoice_doc_item = <fs_item>-invoice_doc_item.
            wa_accountingdata-po_unit          = <fs_item>-po_unit.
            wa_accountingdata-tax_code         = <fs_item>-tax_code.
            IF v_aufpl IS NOT INITIAL.
              SELECT SINGLE vornr
                INTO wa_accountingdata-activity
                FROM afvc
                WHERE aufpl = v_aufpl.
            ENDIF.
            IF wa_accountingdata-quantity GT 0.
              wa_accountingdata-item_amount      = ( <fs_nfse_002>-dmbtr_iv - <fs_nfse_002>-netwr_imp ) * ( wa_accountingdata-quantity / v_menge ).
              APPEND wa_accountingdata TO r_account_tab.
            ENDIF.
            CLEAR: wa_accountingdata, v_aufpl.
          ENDSELECT.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_all.

    CHECK ms_nfse_001-guid_header NE mv_guid.

    CLEAR mt_impostos[].

    SELECT SINGLE * FROM zibt_nfse_001
      INTO ms_nfse_001
        WHERE guid_header = mv_guid.

    SELECT * FROM zibt_nfse_002
      INTO TABLE mt_nfse_002
       WHERE guid_header = mv_guid.

    SELECT * FROM zibt_nfse_003
      INTO TABLE mt_nfse_003
       WHERE guid_header = mv_guid.

    SELECT * FROM zibt_nfse_004
      INTO TABLE mt_nfse_004
       WHERE guid_header = mv_guid.

    SELECT * FROM zibt_nfse_005
      INTO TABLE mt_nfse_005
       WHERE guid_header = mv_guid.

    fill_0034( ).
* INICIO - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    fill_folha_de_servico( ).
* FIM - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    CLEAR: gv_liqui_po, gv_total_po,gv_total_imp.

    LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>).

      IF <fs_nfse_002>-pstyp = '9'.
        gv_total_po = gv_total_po + ( <fs_nfse_002>-dmbtr_iv ).
      ELSE.
        gv_total_po = gv_total_po + ( <fs_nfse_002>-netwr * <fs_nfse_002>-menge_iv ).
      ENDIF.
      ADD <fs_nfse_002>-netwr_imp TO gv_total_imp.

      ms_nfse_001-ebeln = <fs_nfse_002>-ebeln.

    ENDLOOP.

    "ALRS
    gv_total_po = ms_nfse_001-nfse_value.
    gv_liqui_po = ms_nfse_001-nfse_value.

    IF line_exists( mt_nfse_002[ pstyp = '9' ] ) AND lines( mt_nfse_002 ) > 1.
      mv_serv_mat = '2'. "< - tem serviço e material normal
    ELSE.
      mv_serv_mat = '1'. "<- só tem serviço
    ENDIF.

    IF mt_impostos IS INITIAL.

      CREATE OBJECT go_util
        EXPORTING
          iv_guid_header = ms_nfse_001-guid_header.

      TRY .
          go_util->get_xml( ).

        CATCH /tcsr/cx_exception INTO DATA(lo_exception).
          EXIT.
          "Display Error message
*            MESSAGE ID lo_exception->if_t100_message~t100key-msgid
*               TYPE 'E'
*             NUMBER lo_exception->if_t100_message~t100key-msgno
*               WITH lo_exception->if_t100_message~t100key-attr1
*                    lo_exception->if_t100_message~t100key-attr2
*                    lo_exception->if_t100_message~t100key-attr3
*                    lo_exception->if_t100_message~t100key-attr4.
      ENDTRY.

      DATA lt_po_list TYPE /tcsr/y_po_list.

      go_util->get_scr_cond_list(
        EXPORTING
          iv_tolerance_tax  = '0.50'
        IMPORTING
          et_scr_cond_list  = mt_impostos
        CHANGING
          it_po_list        = lt_po_list[]
          ).

    ENDIF.

    " 19.09.2022 - RAMON LIMA - Adição parametros -->
    IF line_exists( mt_impostos[ txcode = 'ISS' ] ).
      DATA(lw_tax) = mt_impostos[ txcode = 'ISS' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

    IF line_exists( mt_impostos[ txcode = 'INSS' ] ).
      lw_tax = mt_impostos[ txcode = 'INSS' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

    IF line_exists( mt_impostos[ txcode = 'PISCOFCSLL' ] ).
      lw_tax  = mt_impostos[ txcode = 'PISCOFCSLL' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

    " --------- PIS
    IF line_exists( mt_impostos[ txcode = 'PIS' ] ).
      lw_tax  = mt_impostos[ txcode = 'PIS' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

    " --------- COFINS
    IF line_exists( mt_impostos[ txcode = 'COFI' ] ).
      lw_tax  = mt_impostos[ txcode = 'COFI' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

    " --------- CSLL
    IF line_exists( mt_impostos[ txcode = 'CSLL' ] ).
      lw_tax  = mt_impostos[ txcode = 'CSLL' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.


    IF line_exists( mt_impostos[ txcode = 'IR' ] ).
      lw_tax  = mt_impostos[ txcode = 'IR' ] .
      SUBTRACT lw_tax-xml_value FROM gv_liqui_po.
    ENDIF.

  ENDMETHOD.


  METHOD fill_glaccount.

    LOOP AT it_account ASSIGNING FIELD-SYMBOL(<fs_account>).

      APPEND INITIAL LINE TO r_ret ASSIGNING FIELD-SYMBOL(<fs_ret>).

      <fs_ret>-invoice_doc_item = <fs_account>-invoice_doc_item.
      <fs_ret>-gl_account = <fs_account>-gl_account.
      <fs_ret>-item_amount = 0.
      <fs_ret>-db_cr_ind = 'S'.
      <fs_ret>-comp_code = ms_nfse_001-bukrs.
      <fs_ret>-tax_code = <fs_account>-tax_code.
      <fs_ret>-costcenter = <fs_account>-costcenter.
      <fs_ret>-bus_area = <fs_account>-bus_area.

    ENDLOOP.


  ENDMETHOD.


  METHOD fill_miro_cab.
    r_miro_cab-invoice_ind = abap_true.
    r_miro_cab-doc_type = 'RE'.
    r_miro_cab-doc_date = ms_nfse_001-dtemissao.
    r_miro_cab-pstng_date = sy-datum.
    r_miro_cab-ref_doc_no = nf_serie( ).
    r_miro_cab-comp_code = ms_nfse_001-bukrs.
    r_miro_cab-diff_inv = ms_nfse_001-lifnr.
    r_miro_cab-currency = 'BRL'. "ms_nfse_001-waers.
    r_miro_cab-gross_amount = gv_total_po.
    r_miro_cab-calc_tax_ind = abap_true.
    r_miro_cab-pmnttrms = '0001'.
    IF mv_vencimento IS NOT INITIAL.
      r_miro_cab-bline_date =  mv_vencimento.
    ELSE.
      r_miro_cab-bline_date =  ms_nfse_001-dt_vencimento.
    ENDIF.
    mv_vencimento = r_miro_cab-bline_date.
    r_miro_cab-partner_bk = ms_nfse_001-zbvtyp.
    r_miro_cab-housebankid = ms_nfse_001-housebankid.

    r_miro_cab-pymt_meth =  ms_nfse_001-pymt_meth.
    r_miro_cab-pmnt_block = ms_nfse_001-zlspr.
    r_miro_cab-business_place = ms_nfse_001-branch.
    r_miro_cab-alloc_nmbr = ms_nfse_001-ebeln.
    r_miro_cab-item_text  = ms_nfse_001-se_recordid.
    r_miro_cab-header_txt  = ms_nfse_001-obs_financeira.
    r_miro_cab-bus_area   = ms_nfse_001-branch.

    r_miro_cab-j_1bnftype = 'NS'.

    IF ms_nfse_001-pymt_meth EQ zcl_miro=>st_forma_pagamento_boleto.
*      zcl_miro=>verificar_cod_barra(
*           EXPORTING
*             i_boleto        = ms_nfse_001-boleto
*             i_valor         = CONV #(  r_miro_cab-gross_amount ) " Montante em moeda interna
*             i_dt_vencimento = mv_vencimento ).    " Data Vencimento.

      DATA: lc_barcode TYPE c LENGTH 47,
            lc_dmbtr   TYPE dmbtr.

      CALL FUNCTION 'CONVERSION_EXIT_ZBOLE_INPUT'
        EXPORTING
          input  = ms_nfse_001-boleto
        IMPORTING
          output = lc_barcode.

*      lc_dmbtr = r_miro_cab-gross_amount - ( mv_issqn + mv_inss + mv_pis + mv_cof + mv_csll + mv_irrf + mv_piscofcsll +  gv_total_imp ).
      lc_dmbtr = r_miro_cab-gross_amount.

      CALL FUNCTION 'CONVERT_BARCODE'
        EXPORTING
          barcode   = lc_barcode
          dmbtr     = lc_dmbtr
        IMPORTING
          esrre     = r_miro_cab-po_ref_no
          esrnr     = r_miro_cab-po_sub_no
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.

      IF sy-subrc IS NOT INITIAL.
        lc_dmbtr = gv_liqui_po.

        CALL FUNCTION 'CONVERT_BARCODE'
          EXPORTING
            barcode   = lc_barcode
            dmbtr     = lc_dmbtr
          IMPORTING
            esrre     = r_miro_cab-po_ref_no
            esrnr     = r_miro_cab-po_sub_no
          EXCEPTIONS
            not_valid = 1
            OTHERS    = 2.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD fill_miro_itens.

    DATA lv_doc_item TYPE rblgp.
    DATA lw_kna1 TYPE kna1.

    LOOP AT mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>).

      ADD 1 TO lv_doc_item.

      APPEND INITIAL LINE TO r_itens ASSIGNING FIELD-SYMBOL(<fs_item>).

      <fs_item>-invoice_doc_item = lv_doc_item.
      <fs_item>-po_number = <fs_nfse_002>-ebeln.
      <fs_item>-po_item = <fs_nfse_002>-ebelp.

      IF <fs_nfse_002>-pstyp = '9'.

        IF <fs_nfse_002>-lblni IS NOT INITIAL.
          <fs_item>-ref_doc = <fs_nfse_002>-lblni.
          <fs_item>-sheet_no = <fs_nfse_002>-lblni.
        ELSE.
          <fs_item>-ref_doc = ms_nfse_001-lblni.
          <fs_item>-sheet_no = ms_nfse_001-lblni.
        ENDIF.
        <fs_item>-item_amount = <fs_nfse_002>-dmbtr_iv - <fs_nfse_002>-netwr_imp.
        SELECT SINGLE mwskz
             INTO <fs_item>-tax_code
             FROM ekpo
             WHERE ebeln = <fs_nfse_002>-ebeln
             AND   ebelp = <fs_nfse_002>-ebelp.
      ELSE.

        IF <fs_nfse_002>-mblnr IS NOT INITIAL.
          <fs_item>-ref_doc = <fs_nfse_002>-mblnr.
        ELSE.
          <fs_item>-ref_doc = ms_nfse_001-mblnr.
        ENDIF.
        <fs_item>-ref_doc_it = '0001'.
        <fs_item>-quantity = <fs_nfse_002>-menge_iv.
        <fs_item>-po_pr_qnt = <fs_nfse_002>-menge_iv.
        <fs_item>-item_amount = ( <fs_nfse_002>-netwr * <fs_nfse_002>-menge_iv ) - <fs_nfse_002>-netwr_imp.
        SELECT SINGLE mwskz, meins,bprme, txjcd
             INTO ( @<fs_item>-tax_code, @<fs_item>-po_unit, @<fs_item>-po_pr_uom, @<fs_nfse_002>-taxjurcode )
             FROM ekpo
             WHERE ebeln = @<fs_nfse_002>-ebeln
             AND   ebelp = @<fs_nfse_002>-ebelp.
      ENDIF.

      <fs_item>-ref_doc_year = sy-datum(4).

* INICIO - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
      fill_item_folha_servico(
      EXPORTING
        i_st_nfse_002 = <fs_nfse_002>
      CHANGING
        c_st_item = <fs_item> ).
* FIM - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_table.

    CHECK ms_nfse_001-guid_header NE mv_guid.

    SELECT SINGLE * FROM zibt_nfse_001
      INTO ms_nfse_001
        WHERE guid_header = mv_guid.

    CHECK sy-subrc NE 0.

    SELECT SINGLE /tcsr/t_act~bukrs, /tcsr/t_act~branch, /tcsr/t_act~cancel,
       /tcsr/t_act~last_stepstatus,/tcsr/t_hd~nfse_numero,/tcsr/t_hd~nfse_serie,
       /tcsr/t_hd~nfse_value,/tcsr/t_hd~dtemissao,/tcsr/t_act~lifnr,
       /tcsr/t_hd~nfse_year,/tcsr/t_hd~nfse_month,/tcsr/t_act~guid_header,
       /tcsr/t_hd~p_cpf,/tcsr/t_hd~p_cnpj,/tcsr/t_hd~t_cnpj
        INTO @DATA(lw_dados)
        FROM /tcsr/t_act
        INNER JOIN /tcsr/t_hd
        ON /tcsr/t_hd~guid_header EQ /tcsr/t_act~guid_header
        WHERE /tcsr/t_act~guid_header = @mv_guid.

    CHECK sy-subrc EQ 0 .

    MOVE-CORRESPONDING lw_dados TO ms_nfse_001.

*      SELECT SINGLE * FROM lfa1
*        INTO @DATA(lw_lfa1)
*          WHERE lifnr = @lw_dados-lifnr.
*
*      IF sy-subrc EQ 0.
*
*        ms_nfse_001-name1 = lw_lfa1-name1.
*        ms_nfse_001-stcd1 = lw_lfa1-stcd1.
*        ms_nfse_001-stcd2 = lw_lfa1-stcd2.
*        ms_nfse_001-stcd3 = lw_lfa1-stcd3.
*
*      ENDIF.

  ENDMETHOD.


  METHOD fill_withtax.

    DATA lt_lfbw TYPE TABLE OF lfbw.
    DATA wa_lfbw TYPE lfbw.
    DATA lw_withtax TYPE bapi_incinv_create_withtax.
    DATA lv_apag_ct TYPE c.

    CLEAR r_ret.

    CALL FUNCTION 'FI_WT_READ_LFBW'
      EXPORTING
        i_lifnr   = ms_nfse_001-lifnr
        i_bukrs   = ms_nfse_001-bukrs
*       I_TYPE    =
      TABLES
        t_lfbw    = lt_lfbw
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    " ------- PIS + COFINS + CSLL
    IF mv_piscofcsll  IS NOT INITIAL.
      CLEAR lw_withtax.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'CT'.
      lw_withtax-wi_tax_code = 'CT'.
      lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      lw_withtax-wi_tax_amt = mv_piscofcsll.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ----------- PIS
    IF mv_pis IS NOT INITIAL.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'PI'.
      lw_withtax-wi_tax_code = 'PI'.
      lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      lw_withtax-wi_tax_amt = mv_pis.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ----------- COFINS
    IF mv_cof IS NOT INITIAL.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'CF'.
      lw_withtax-wi_tax_code = 'C0'.
      lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      lw_withtax-wi_tax_amt  = mv_cof.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ----------- CSLL
    IF mv_csll IS NOT INITIAL.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'CS'.
      lw_withtax-wi_tax_code = 'CS'.
      lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      lw_withtax-wi_tax_amt = mv_csll.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ------- IRRF
    IF mv_irrf IS NOT INITIAL.
      READ TABLE lt_lfbw INTO wa_lfbw WITH KEY witht = 'IR'.
      IF sy-subrc NE 0.
        wa_lfbw-wt_withcd = 'R1'.
*        wa_lfbw-wt_withcd = '00'.
      ENDIF.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'IR'.
      lw_withtax-wi_tax_code = wa_lfbw-wt_withcd.
      IF mv_base_irrf IS NOT INITIAL.
        lw_withtax-wi_tax_base = mv_base_irrf.
      ELSE.
        lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      ENDIF.
      lw_withtax-wi_tax_amt = mv_irrf.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ------- ISS
    IF mv_issqn IS NOT INITIAL.
      READ TABLE lt_lfbw INTO wa_lfbw WITH KEY witht = 'IS'.
      IF sy-subrc NE 0.
        wa_lfbw-wt_withcd = 'R1'.
*        wa_lfbw-wt_withcd = '00'.
      ENDIF.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'IS'.
      lw_withtax-wi_tax_code = wa_lfbw-wt_withcd.
      IF mv_base_issqn IS NOT INITIAL.
        lw_withtax-wi_tax_base = mv_base_issqn.
      ELSE.
        lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      ENDIF.
      lw_withtax-wi_tax_amt = mv_issqn.
      APPEND lw_withtax TO r_ret.
    ENDIF.

    " ------- INSS
    IF mv_inss IS NOT INITIAL.
      READ TABLE lt_lfbw INTO wa_lfbw WITH KEY witht = 'IN'.
      IF sy-subrc NE 0.
        wa_lfbw-wt_withcd = 'N1'.
*        wa_lfbw-wt_withcd = '00'.
      ENDIF.
      lw_withtax-split_key = '000001'.
      lw_withtax-wi_tax_type = 'IN'.
      lw_withtax-wi_tax_code = wa_lfbw-wt_withcd.
      IF mv_base_inss IS NOT INITIAL.
        lw_withtax-wi_tax_base = mv_base_inss.
      ELSE.
        lw_withtax-wi_tax_base = ms_nfse_001-nfse_value.
      ENDIF.
      lw_withtax-wi_tax_amt = mv_inss.
      APPEND lw_withtax TO r_ret.
    ENDIF.



  ENDMETHOD.


  METHOD force_update.

    CLEAR ms_nfse_001-guid_header.

  ENDMETHOD.


  METHOD free.

    me->dequeue_guid( ).

    me->clear_all( ).

  ENDMETHOD.


  METHOD gerar_escrit_entrada.

    DATA ls_header TYPE bapi_j_1bnfdoc.
    DATA ls_header_add TYPE bapi_j_1bnfdoc_add.
    DATA ls_nfcheck TYPE bapi_j_1bnfcheck.
    DATA lt_item TYPE TABLE OF bapi_j_1bnflin.
    DATA lt_impo TYPE TABLE OF bapi_j_1bnfstx.
    DATA lt_ret TYPE TABLE OF bapiret2.
    DATA lt_partner TYPE TABLE OF bapi_j_1bnfnad.
    DATA ls_mara TYPE mara.
    DATA ls_makt TYPE makt.
    DATA lv_netpr TYPE /tcsr/e_nfse_value.

    ls_header-nftype = 'NT'.
    ls_header-nfesrv = abap_true.
    ls_header-manual = abap_true.
    ls_nfcheck-chekcon = abap_true.
    ls_header-direct = '1'.

    "ls_header-

    ls_header-docstat =  '1'.
    ls_header-parvw = 'SP'. " 15.12.2022
    ls_header-parid = ms_nfse_001-lifnr.
    ls_header-docdat = ms_nfse_001-dtemissao.

    ls_header-nfnum  = get_last_digits( iv_var = ms_nfse_001-nfse_numero ).

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = sy-datum " 09.02.2023 - 103626
        p_bukrs        = ms_nfse_001-bukrs
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      IMPORTING
        p_data_val     = ls_header-pstdat
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.

    IF ms_nfse_001-nfse_serie IS INITIAL.
      ms_nfse_001-nfse_serie = '1'.
    ENDIF.

    "ls_header-pstdat  = p_data_val.
    ls_header-bukrs   = ms_nfse_001-bukrs.
    ls_header-branch  = ms_nfse_001-branch. " = wa_dados-werks.
    ls_header-waerk   = 'BRL'.
    ls_header-series  = ms_nfse_001-nfse_serie.

    "Item da parte fiscal
    APPEND INITIAL LINE TO lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
    <fs_item>-itmnum    = 10.
    <fs_item>-bwkey     = ms_nfse_001-branch.
    <fs_item>-werks     = ms_nfse_001-branch.
    <fs_item>-reftyp    = 'LI'.
    <fs_item>-refkey    = iv_belnr && iv_gjahr.

    SELECT SINGLE matnr FROM zlest0037 "#EC CI_FLDEXT_OK[2215424]
      INTO <fs_item>-matnr
        WHERE bukrs      EQ ms_nfse_001-bukrs
          AND cd_modal   EQ '01'
          AND lifnr      EQ ms_nfse_001-lifnr
          AND ck_servico EQ abap_true
          AND matkl      EQ space.

    IF sy-subrc NE 0.

      SELECT SINGLE matnr FROM zlest0037 "#EC CI_FLDEXT_OK[2215424]
        INTO <fs_item>-matnr
           WHERE bukrs      EQ ms_nfse_001-bukrs
             AND cd_modal   EQ '01'
             AND lifnr      EQ space
             AND ck_servico EQ abap_true
             AND matkl      EQ space.

    ENDIF.

    "Logística: Revisão de Faturas
    <fs_item>-reftyp    = 'LI'.
    "st_obj_item-matnr     = wa_dados-matnr.
    <fs_item>-itmtyp    = 'ZH'.
    <fs_item>-menge     = '1'.
    "st_obj_item-netoth    =
    <fs_item>-netpr     = ms_nfse_001-nfse_value.
    <fs_item>-netwr     = ms_nfse_001-nfse_value.
    "ls_item-maktx   = wa_material_text_record-maktx

    CALL FUNCTION 'J_1B_MATERIAL_READ' "#EC CI_FLDEXT_OK[2215424]
      EXPORTING
        matnr                = <fs_item>-matnr
        val_area             = ms_nfse_001-branch
        val_type             = space
        language             = sy-langu
        i_werks              = ms_nfse_001-branch
      IMPORTING
        nbm                  = <fs_item>-nbm
        matuse               = <fs_item>-matuse
        matorg               = <fs_item>-matorg
        material_record      = ls_mara
        material_text_record = ls_makt
        e_matkl              = <fs_item>-matkl
      EXCEPTIONS
        material_not_found   = 1
        valuation_not_found  = 2
        OTHERS               = 3.

    IF ls_makt IS NOT INITIAL.
      <fs_item>-maktx = ls_makt-maktx.
    ENDIF.

* Leis Fiscais para entrada de frete terceiro
    SELECT SINGLE * FROM zlest0040 INTO @DATA(sl_zlest0040) WHERE  iva   = 'S1'
                                                       AND fatura EQ 'T'.
    <fs_item>-taxlw1 = sl_zlest0040-icms.
    <fs_item>-taxlw2 = sl_zlest0040-ipi.
    <fs_item>-taxlw4 = sl_zlest0040-cofins.
    <fs_item>-taxlw5 = sl_zlest0040-pis.

    " Busca CFOP
    DATA(vl_dstcat) = busca_cat_destino( iv_lifnr = ms_nfse_001-lifnr iv_werks = ms_nfse_001-branch ).

    SELECT SINGLE industry FROM j_1bbranch
      INTO @DATA(vl_industry)
        WHERE bukrs  EQ @ms_nfse_001-bukrs
          AND branch EQ @ms_nfse_001-branch.

    SELECT SINGLE cfop FROM zlest0030
      INTO @DATA(vl_cfop)
       WHERE direct     EQ '1'
         AND dstcat     EQ @vl_dstcat
         AND industry   EQ @vl_industry
         AND tpparceiro EQ '1'
         AND vkaus      EQ 'V'
         AND tdlnr      EQ @ms_nfse_001-lifnr
         AND bukrs      EQ @ms_nfse_001-bukrs.

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE cfop INTO vl_cfop
        FROM zlest0030
       WHERE direct     EQ '1'
         AND dstcat     EQ vl_dstcat
         AND industry   EQ vl_industry
         AND tpparceiro EQ '1'
         AND vkaus      EQ 'V'
         AND tdlnr      EQ ms_nfse_001-lifnr
         AND bukrs      EQ space.

      IF sy-subrc IS NOT INITIAL.

        SELECT SINGLE cfop INTO vl_cfop
          FROM zlest0030
         WHERE direct     EQ '1'
           AND dstcat     EQ vl_dstcat
           AND industry   EQ vl_industry
           AND tpparceiro EQ '1'
           AND vkaus      EQ 'V'
           AND tdlnr      EQ space
           AND bukrs      EQ ms_nfse_001-bukrs.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE cfop INTO vl_cfop
            FROM zlest0030
           WHERE direct     EQ '1'
             AND dstcat     EQ vl_dstcat
             AND industry   EQ vl_industry
             AND tpparceiro EQ '1'
             AND vkaus      EQ 'V'
             AND tdlnr      EQ space
             AND bukrs      EQ space.

        ENDIF.
      ENDIF.
    ENDIF.

    <fs_item>-cfop_10 = vl_cfop.

    " 19.12.2022 - RAMON -->
    <fs_item>-meins         = ls_mara-meins.
    <fs_item>-meins_trib  =  ls_mara-meins.

    <fs_item>-netpr = ms_total_0034-dmbtr - ( ms_total_0034-valor_pis + ms_total_0034-valor_cofins ).
    "<fs_item>-netwr = ms_nfse_001-nfse_value.
    <fs_item>-netwr = ms_total_0034-dmbtr - ( ms_total_0034-valor_pis + ms_total_0034-valor_cofins ).
    "<fs_item>-netoth = ms_nfse_001-nfse_value. " st_obj_item-add-nfnett, não achei entao coloquei a toth

    "ls_header-FORM = 'NF55'.
    ls_header-docstat = space.
    "ls_header-cod_sit = '00'.
    "ls_header-ind_emit = '0'.
    ls_header-model = '03'. " 09.02.2023 - 103626


    " 19.12.2022 - RAMON -->

    " Alimentar a estrutura do parceiros da nota (somente o parceiro LF é necessário)
    APPEND INITIAL LINE TO lt_partner ASSIGNING FIELD-SYMBOL(<fs_partner>).
    <fs_partner>-mandt  = sy-mandt.
    <fs_partner>-parvw  = 'LF'.
    <fs_partner>-parid  = ms_nfse_001-lifnr.
    <fs_partner>-partyp = 'V'.

    " Calcula impostos para registro fiscal

    DATA(ls_stx) = get_linha_imposto_0034( 'ICMS' ).
    APPEND ls_stx TO lt_impo.

    ls_stx = get_linha_imposto_0034( 'IPI' ).
    APPEND ls_stx TO lt_impo.

    ls_stx = get_linha_imposto_0034( 'COFI' ).
    APPEND ls_stx TO lt_impo.

    ls_stx = get_linha_imposto_0034( 'PIS' ).
    APPEND ls_stx TO lt_impo.

    CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        obj_header     = ls_header
        obj_header_add = ls_header_add
        nfcheck        = ls_nfcheck
      IMPORTING
        e_docnum       = ev_docnum
      TABLES
        obj_partner    = lt_partner
        obj_item       = lt_item
        obj_item_tax   = lt_impo
        "obj_ot_partner = ti_obj_ot_partner
        return         = lt_ret.

    APPEND LINES OF lt_ret TO mt_mess.

    CHECK ev_docnum IS NOT INITIAL.

    break rblima.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CALL METHOD me->nf_document_read
      EXPORTING
        iv_docnum   = ev_docnum
      IMPORTING
        es_header   = DATA(ls_header_doc)
        et_item     = DATA(lt_item_doc)
        et_item_tax = DATA(lt_tax_doc)
        et_partner  = DATA(lt_partner_doc).

    ls_header_doc-cod_sit = '00'.
    ls_header_doc-ind_emit = '1'.

    LOOP AT lt_tax_doc ASSIGNING FIELD-SYMBOL(<fs_tax>).

      IF <fs_tax>-taxgrp = 'IPI'.

        <fs_tax>-base      = 0.
        <fs_tax>-rate      = 0.
        <fs_tax>-taxval     = 0.
        <fs_tax>-excbas   = 0.
        <fs_tax>-othbas = ms_nfse_001-nfse_value.

      ENDIF.

    ENDLOOP.

    CALL METHOD me->nf_document_update
      EXPORTING
        is_header   = ls_header_doc
        it_item     = lt_item_doc
        it_item_tax = lt_tax_doc
        it_partner  = lt_partner_doc.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.


    "Seleciona informação ZLEST0032
    SELECT * FROM zlest0032
      INTO TABLE @DATA(lt_0032)
        FOR  ALL ENTRIES IN @me->mt_nfse_0034
          WHERE tknum = @me->mt_nfse_0034-tknum.


    "Setar informação do docnum na tabela ZLEST0034.
    LOOP AT me->mt_nfse_0034 ASSIGNING FIELD-SYMBOL(<ws_mt_nfse_0034>).
      <ws_mt_nfse_0034>-en_docnum = ev_docnum.

      READ TABLE lt_0032 ASSIGNING FIELD-SYMBOL(<fs_0032>)
       WITH KEY tknum = <ws_mt_nfse_0034>-tknum.

      IF sy-subrc EQ 0.
        <fs_0032>-docnum = <ws_mt_nfse_0034>-en_docnum.
      ENDIF.

    ENDLOOP.

    IF lt_0032 IS NOT INITIAL.
      MODIFY zlest0032 FROM TABLE lt_0032.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.



  ENDMETHOD.


  METHOD get_folha_info.

    CHECK iv_entrysheet IS NOT INITIAL.

    DO 5 TIMES.

      "Select Serv.EntrySheet Created
      SELECT SINGLE packno
        INTO ms_nfse_001-packno
        FROM essr
        WHERE lblni = iv_entrysheet.

      IF sy-subrc EQ 0.
        EXIT.
      ELSE.
        WAIT UP TO 1 SECONDS.
      ENDIF.

    ENDDO.

    SELECT SINGLE gjahr belnr FROM ekbe
      INTO  (ev_mjahr,ev_mblnr)
        WHERE ebeln = ms_nfse_001-ebeln
          AND bewtp = 'E'
          AND bwart = '101'.

  ENDMETHOD.


  METHOD get_last_digits.

    DATA(lv_length) = strlen( iv_var ).

    DATA(lv_dif) = lv_length - iv_digit.

    IF lv_dif > 0.
      rv_var = iv_var+lv_dif(iv_digit).
    ELSE.
      rv_var = iv_var.
    ENDIF.

  ENDMETHOD.


  METHOD get_last_message.

    DATA(lv_lines) = lines( mt_mess ).

    CHECK lv_lines > 0.

    READ TABLE mt_mess INTO r_return INDEX lv_lines.

  ENDMETHOD.


  METHOD get_linha_imposto_0034.

    CLEAR rs_imposto.

    CASE iv_taxgrp.
      WHEN 'ICMS'.

        rs_imposto-taxtyp = 'ICM0'.
        rs_imposto-itmnum = 10.
        rs_imposto-base = 0.
        rs_imposto-rate = 0.
        rs_imposto-taxval = 0.
        rs_imposto-excbas = ms_nfse_001-nfse_value.
        rs_imposto-othbas  = 0.

      WHEN 'IPI'.

        rs_imposto-taxtyp = 'IPI0'.
        rs_imposto-itmnum = 10.
        rs_imposto-base = 0.
        rs_imposto-rate = 0.
        rs_imposto-taxval = 0.
        rs_imposto-excbas = ms_nfse_001-nfse_value.
        rs_imposto-othbas  = 0.

      WHEN 'COFI'.

        rs_imposto-taxtyp = 'ICOF'.
        rs_imposto-itmnum = 10.
        rs_imposto-base      = ms_nfse_001-nfse_value.
        rs_imposto-rate       = ms_total_0034-rate_cofins.
        rs_imposto-taxval    = ms_total_0034-valor_cofins.
        rs_imposto-excbas  = 0.
        rs_imposto-othbas  = 0.

      WHEN 'PIS'.

        rs_imposto-taxtyp = 'IPIS'.
        rs_imposto-itmnum = 10.
        rs_imposto-base =  ms_nfse_001-nfse_value.
        rs_imposto-rate =   ms_total_0034-rate_pis.
        rs_imposto-taxval = ms_total_0034-valor_pis.
        rs_imposto-excbas  = 0.
        rs_imposto-othbas  = 0.

    ENDCASE.




  ENDMETHOD.


  METHOD get_nfse_001.

    fill_all( ).

    r_ret = ms_nfse_001.

  ENDMETHOD.


  method GET_OBS_TEXT.

*CALL METHOD ret->get_text_as_r3table
**  EXPORTING
**    only_when_modified     = FALSE
**  IMPORTING
**    table                  =
**    is_modified            =
**  EXCEPTIONS
**    error_dp               = 1
**    error_cntl_call_method = 2
**    error_dp_create        = 3
**    potential_data_loss    = 4
**    others                 = 5
*        .
*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.




*    CREATE OBJECT ret
*      EXPORTING
**        max_number_chars       =
**        style                  = 0
**        wordwrap_mode          = WORDWRAP_AT_WINDOWBORDER
**        wordwrap_position      = -1
**        wordwrap_to_linebreak_mode = FALSE
**        filedrop_mode          = DROPFILE_EVENT_OFF
*        parent                 =
**        lifetime               =
**        name                   =
**      EXCEPTIONS
**        error_cntl_create      = 1
**        error_cntl_init        = 2
**        error_cntl_link        = 3
**        error_dp_create        = 4
**        gui_type_not_supported = 5
**        others                 = 6
*        .
*    IF sy-subrc <> 0.
**     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*
*
*    value( R_RET )
*
*CREATE OBJECT lr_text_edit
*      EXPORTING
*        wordwrap_mode     = 2 ” 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
*        wordwrap_position = 254   ” pos of wordwrap, only makes sense with wordwrap_mode=2
*        parent   = lr_custom_cont.     ” Parent Container
*
*
  endmethod.


  METHOD get_pdf.

    DATA lv_guid TYPE string.

    fill_all( ).

    CHECK ms_nfse_001-guid_header IS NOT INITIAL.

    lv_guid = ms_nfse_001-guid_header.

    CALL FUNCTION 'Z_GRC_ARQUIVO_DOC'
      EXPORTING
        i_guid = lv_guid
        i_tipo = 'PDF'
      IMPORTING
        out    = ev_xpdf.

  ENDMETHOD.


  method get_simulate.

    data lt_with type table of bapi_incinv_create_withtax.
    data lw_values type zde_zsexpt00004_values.

    fill_all( ).

    data(lw_cab) = fill_miro_cab( ).
    data(lt_itens) = fill_miro_itens( ).
    data(lt_account) = fill_account( lt_itens ).
    data(lt_glaccount) = fill_glaccount( lt_account ).

    clear mt_mess.

    if lt_itens is initial.

      append_message( exporting
                      i_message = 'Preços de simulação necessita de um pedido associado'
                      i_msgty = 'W' ).
      exit.

    endif.

    " foi colocado fixo para passar na simulação
*    lw_cab-partner_bk = '0001'.
    lw_cab-ref_doc_no = '59999-999'. " <- somente para simulação

    call function 'ZMM_NFSE_GET_WITHTAX'
      exporting
        i_header          = lw_cab
        i_guid_header     = ms_nfse_001-guid_header
      tables
        it_itemdata       = lt_itens
        it_accountingdata = lt_account
        it_glaccountdata  = lt_glaccount
        et_withtaxdata    = lt_with
        et_bapiret2       = mt_mess.

    r_ret_simulate_tab = lt_with.

  endmethod.


  METHOD log_to_tab.

    LOOP AT mt_mess ASSIGNING FIELD-SYMBOL(<fs_mess>).

      APPEND INITIAL LINE TO mt_nfse_005 ASSIGNING FIELD-SYMBOL(<fs_005>).

      <fs_005>-guid_header = mv_guid.
      <fs_005>-dt_registro = sy-datum.
      <fs_005>-hr_registro = sy-uzeit.
      <fs_005>-msgty = <fs_mess>-type.
      <fs_005>-msgid = <fs_mess>-id.
      <fs_005>-msgno = <fs_mess>-number.
      <fs_005>-message = <fs_mess>-message.
      <fs_005>-msgv1 = <fs_mess>-message_v1.
      <fs_005>-msgv2 = <fs_mess>-message_v2.
      <fs_005>-msgv3 = <fs_mess>-message_v3.
      <fs_005>-msgv4 = <fs_mess>-message_v4.
      <fs_005>-us_registro = sy-uname.

    ENDLOOP.

  ENDMETHOD.


  method LZMM_NFSEU05.
  endmethod.


  METHOD message_ok.

    CLEAR rv_ret.

    READ TABLE mt_mess TRANSPORTING NO FIELDS
      WITH KEY type = 'S'.

    CHECK sy-subrc EQ 0.

    rv_ret = 'X'.

  ENDMETHOD.


  METHOD miro_frete.

    DATA lv_erro TYPE c.
    DATA lv_mess TYPE string.

    DATA lv_iss TYPE j_1btaxval.

    ev_subrc = 4.

    fill_all( ).

    fill_0034( 'X' ).

    IF mt_nfse_0034 IS INITIAL.

      append_message( 'Nenhum documento associado a essa nfse' ).
      EXIT.

    ENDIF.

    " 31.03.2023 - 107971 - RBL -->
    " logica invertida, pq em prd o default é reter....
    IF iv_retem_iss = 'X'.
      ms_nfse_001-nao_retem_iss = space.
    ELSE.
      ms_nfse_001-nao_retem_iss = 'X'.
    ENDIF.
    " 31.03.2023 - 107971 - RBL --<

    LOOP AT mt_nfse_0034 ASSIGNING FIELD-SYMBOL(<fs_0034>) WHERE re_belnr IS NOT INITIAL.

      DATA(lv_belnr) = <fs_0034>-re_belnr.
      DATA(lv_gjahr) = <fs_0034>-gjahr.
      lv_erro = 'X'.
      EXIT.

    ENDLOOP.

    IF  lv_erro = 'X'.

      lv_mess = `Já existe o documento ` && lv_belnr && ` ` && lv_gjahr.
      append_message( lv_mess ).
      EXIT.

    ENDIF.

    IF ms_nfse_001-dt_vencimento < sy-datum.

***     Inicio - ALX

      lv_mess = 'Data do Vencimento tem que ser 72 horas maior que Data de Lançamento'.
      append_message( lv_mess ).

*      append_message2(
*          i_msgty = 'E'
*          i_msgid = 'ZZFI'
*          i_msgno = '018'  ).

***     Fim - ALX

      EXIT.

    ENDIF.

    IF ms_nfse_001-pymt_meth IS INITIAL.

      lv_mess = 'Para criar MIRO FRETE é necessário informar forma de pagamento'.
      append_message( lv_mess ).
      EXIT.

    ENDIF.


    clear_message( ).

    " 31.03.2023 - 107971 - RBL --------->
*    IF sy-tcode EQ 'ZFIS63'.
*      IF me->at_gv_iss IS NOT INITIAL.
*        mv_issqn = VALUE #( mt_impostos[ txcode = 'ISS' ]-xml_value DEFAULT '0.00' ).
*      ENDIF.
*    ELSE.
*      mv_issqn = VALUE #( mt_impostos[ txcode = 'ISS' ]-xml_value DEFAULT '0.00' ).
*    ENDIF.

    IF ms_nfse_001-nao_retem_iss = space.
      lv_iss = VALUE #( mt_impostos[ txcode = 'ISS' ]-xml_value DEFAULT '0.00' ).
    ELSE.
      lv_iss = 0.
    ENDIF.

    me->set_impostos( i_issqn = lv_iss ).

    " 31.03.2023 - 107971 - RBL ---------<

    DATA(lt_withtax) = fill_withtax( ).

    CALL FUNCTION 'ZMM_NFSE_MIRO_FRETE'
      EXPORTING
        is_nfse_001   = ms_nfse_001
        i_grossamount = ms_nfse_001-nfse_value
        i_commit      = 'X'
      IMPORTING
        e_belnr       = ms_nfse_001-belnr_fret
        e_ghjar       = ms_nfse_001-gjahr_fret
      TABLES
        it_withtax    = lt_withtax
        et_return     = mt_mess
        ct_0034       = mt_nfse_0034.

    IF ms_nfse_001-belnr_fret IS NOT INITIAL.

      set_vt_docs( i_belnr = ms_nfse_001-belnr_fret i_gjahr = ms_nfse_001-gjahr_fret ).

      append_message2(
          i_msgty = 'S'
          i_msgid = 'M8'
          i_msgno = '060'
          i_msgv1 = ms_nfse_001-belnr_fret ).

    ENDIF.

    e_belnr = ms_nfse_001-belnr_fret.
    e_gjahr = ms_nfse_001-gjahr_fret.

    IF e_belnr IS NOT INITIAL.

      ms_nfse_001-docnum = gerar_escrit_entrada( EXPORTING iv_belnr = e_belnr iv_gjahr = e_gjahr ).

    ENDIF.

    IF e_belnr IS INITIAL.
      ev_subrc = 4.
    ELSE.
      ev_subrc = 0.
    ENDIF.

    log_to_tab( ).

    save( '1' ).

    save( '5' ).

    save( '34' ).

  ENDMETHOD.


  METHOD miro_frete_estorno.

    ev_subrc = 4.

    fill_all( ).

    fill_0034( 'X' ).

    IF mt_nfse_0034 IS INITIAL.

      append_message( 'Nenhum documento associado a essa nfse' ).
      EXIT.

    ENDIF.

    clear_message( ).

    DATA(lv_belnr) = mt_nfse_0034[ 1 ]-re_belnr.
    DATA(lv_gjahr) = mt_nfse_0034[ 1 ]-re_gjahr.
    DATA lv_doc_canc TYPE j_1bdocnum.

    CALL FUNCTION 'ZMM_NFSE_MIRO_FRETE_ESTORNO'
      EXPORTING
        i_bukrs       = ms_nfse_001-bukrs
        i_lifnr       = ms_nfse_001-lifnr
        i_docnum      = ms_nfse_001-docnum
        i_belnr       = lv_belnr
        i_gjahr       = lv_gjahr
        i_commit      = 'X'
      IMPORTING
        e_belnr_out   = e_belnr
        e_gjahr_out   = e_gjahr
        e_docnum_canc = lv_doc_canc
      TABLES
        et_return     = mt_mess.

    log_to_tab( ).

    IF e_belnr IS NOT INITIAL.

      set_vt_docs( ).

      ms_nfse_001-belnr_fret = space.
      ms_nfse_001-gjahr_fret = space.
      ms_nfse_001-docnum = space.

      save( '1' ).
      save( '34' ).

      ev_subrc = 0.

    ENDIF.

    save('5').

  ENDMETHOD.


  METHOD nfe_atualiza_cfop_no_fiscal.
    DATA: obj_header        TYPE j_1bnfdoc,
          obj_partner       TYPE TABLE OF j_1bnfnad,
          obj_item          TYPE TABLE OF j_1bnflin,
          obj_item_tax      TYPE TABLE OF j_1bnfstx,
          obj_header_msg    TYPE TABLE OF j_1bnfftx,
          obj_refer_msg     TYPE TABLE OF j_1bnfref,
          obj_ot_partner    TYPE TABLE OF j_1bnfcpd,
          obj_import_di     TYPE TABLE OF j_1bnfimport_di,
          obj_import_adi    TYPE TABLE OF j_1bnfimport_adi,
          obj_cte_res       TYPE TABLE OF j_1bcte_d_res,
          obj_cte_docref    TYPE TABLE OF j_1bcte_d_docref,
          obj_trans_volumes TYPE TABLE OF j_1bnftransvol,
          obj_trailer_info  TYPE TABLE OF j_1bnftrailer,
          obj_trade_notes   TYPE TABLE OF j_1bnftradenotes,
          obj_add_info      TYPE TABLE OF j_1bnfadd_info,
          obj_ref_proc      TYPE TABLE OF j_1bnfrefproc,
          obj_sugar_suppl   TYPE TABLE OF j_1bnfsugarsuppl,
          obj_sugar_deduc   TYPE TABLE OF j_1bnfsugardeduc,
          obj_vehicle       TYPE TABLE OF j_1bnfvehicle,
          obj_pharmaceut    TYPE TABLE OF j_1bnfpharmaceut,
          obj_fuel          TYPE TABLE OF j_1bnffuel,
          obj_export        TYPE TABLE OF j_1bnfe_export,
          obj_nve           TYPE TABLE OF j_1bnfnve.

    DATA: lc_refkey TYPE j_1bnflin-refkey,
          lc_nfobjn TYPE j_1binterf-nfobjn.

    CHECK me->ms_nfse_001-belnr IS NOT INITIAL.

    CONCATENATE me->ms_nfse_001-belnr me->ms_nfse_001-gjahr INTO lc_refkey.

    SELECT SINGLE * INTO @DATA(wa_j_1bnflin)
      FROM j_1bnflin
     WHERE refkey EQ @lc_refkey
       AND reftyp EQ 'LI'.

    CHECK sy-subrc IS INITIAL.


    SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
      FROM j_1bnfdoc
     WHERE docnum EQ @wa_j_1bnflin-docnum.

    CHECK sy-subrc IS INITIAL.


    CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
      EXPORTING
        doc_number         = wa_j_1bnfdoc-docnum
      IMPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    CALL FUNCTION 'J_1B_NF_OBJECT_READ'
      EXPORTING
        obj_number        = lc_nfobjn
      IMPORTING
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE regio
      INTO @DATA(_regioe)
      FROM t001w
      WHERE werks = @me->ms_nfse_001-branch.

    SELECT SINGLE regio
          INTO @DATA(_regiof)
          FROM lfa1
          WHERE lifnr = @me->ms_nfse_001-lifnr.


    LOOP AT obj_item ASSIGNING FIELD-SYMBOL(<wa_item_nota>).

*Inicio Altearção - Leandro Valentim Ferreira - 21.06.23 - #114554
      SELECT SINGLE steuc
       INTO @DATA(vl_steuc)
       FROM marc
       WHERE matnr EQ @<wa_item_nota>-matnr
         AND werks EQ @<wa_item_nota>-werks.

      IF <wa_item_nota>-nbm ne vl_steuc.
        <wa_item_nota>-nbm = vl_steuc.
      ENDIF.
*Fim Altearção - Leandro Valentim Ferreira - 21.06.23 - #114554

      CHECK <wa_item_nota>-cfop IS INITIAL.
      IF _regioe EQ _regiof.
        <wa_item_nota>-cfop = '1933AA'.
      ELSE.
        <wa_item_nota>-cfop = '2933AA'.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'J_1B_NF_OBJECT_UPDATE'
      EXPORTING
        obj_number        = lc_nfobjn
        obj_header        = obj_header
      TABLES
        obj_partner       = obj_partner
        obj_item          = obj_item
        obj_item_tax      = obj_item_tax
        obj_header_msg    = obj_header_msg
        obj_refer_msg     = obj_refer_msg
        obj_ot_partner    = obj_ot_partner
        obj_import_di     = obj_import_di
        obj_import_adi    = obj_import_adi
        obj_cte_res       = obj_cte_res
        obj_cte_docref    = obj_cte_docref
        obj_trans_volumes = obj_trans_volumes
        obj_trailer_info  = obj_trailer_info
        obj_trade_notes   = obj_trade_notes
        obj_add_info      = obj_add_info
        obj_ref_proc      = obj_ref_proc
        obj_sugar_suppl   = obj_sugar_suppl
        obj_sugar_deduc   = obj_sugar_deduc
        obj_vehicle       = obj_vehicle
        obj_pharmaceut    = obj_pharmaceut
        obj_fuel          = obj_fuel
        obj_export        = obj_export
        obj_nve           = obj_nve
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'J_1B_NF_DOC_UPDATE_FROM_OBJECT'
      EXPORTING
        obj_number         = lc_nfobjn
      EXCEPTIONS
        object_not_found   = 1
        document_not_found = 2
        update_problem     = 3
        docum_lock         = 4
        OTHERS             = 5.

    CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
      EXPORTING
        obj_number       = lc_nfobjn
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

  ENDMETHOD.


  METHOD nfse_inbound_cancela_fatura.

    DATA: e_invoicedocnumber_estorno TYPE re_belnr,
          e_fiscalyear_estorno       TYPE gjahr,
          e_retorno                  TYPE bapiret2_t,
          i_invoicedocnumber         TYPE re_belnr,
          i_fiscalyear               TYPE gjahr.

    DATA: wa_cabecalho TYPE zde_miro_cabecalho,
          handle       TYPE REF TO zcl_memory_nfe_inbound,
          root         TYPE REF TO zcl_memory_nfe_inbound_handle,
          oref         TYPE REF TO zcl_memory_nfe_inbound_handle.



    IF me->ms_nfse_001-belnr IS NOT INITIAL.
      "Estornar Aviso de Recebimento
      DATA: lc_miro TYPE REF TO zcl_miro.

      CREATE OBJECT lc_miro.

      CALL METHOD lc_miro->estornar
        EXPORTING
          i_bapi_wait                = space
        IMPORTING
          e_invoicedocnumber_estorno = e_invoicedocnumber_estorno
          e_fiscalyear_estorno       = e_fiscalyear_estorno
          e_retorno                  = e_retorno
        CHANGING
          i_invoicedocnumber         = me->ms_nfse_001-belnr
          i_fiscalyear               = me->ms_nfse_001-gjahr
          i_postingdate              = sy-datum
        RECEIVING
          r_gerou                    = DATA(r_estornou_miro).

      CLEAR: lc_miro.

      mt_mess = e_retorno.

      IF r_estornou_miro EQ abap_true.

        TRY.

*            handle = zcl_memory_nfe_inbound=>attach_for_read( inst_name = CONV #( wa_cabecalho-lifnr && lc_referencia ) ).
*
*            oref ?= handle->root.
*
*            CLEAR oref.
*
*            handle->detach( ).
*
*            handle->free_area( ).
*
*            handle->free_instance( ).

          CATCH cx_shm_inconsistent.  "
          CATCH cx_shm_no_active_version.  "
          CATCH cx_shm_read_lock_active.  "
          CATCH cx_shm_exclusive_lock_active.  "
          CATCH cx_shm_parameter_error.  "
          CATCH cx_shm_change_lock_active.  "
          CATCH cx_shm_attach_error.
        ENDTRY.

        WAIT UP TO 2 SECONDS.


        UPDATE /tcsr/t_act  SET last_stepstatus = '88'
          WHERE guid_header = me->ms_nfse_001-guid_header.
        COMMIT WORK.

        me->ms_nfse_001-se_detail  = 'Fatura estornada pelo SE'.
        me->set_ck_fisico( i_check     = abap_false ).

        me->set_st_fisico( i_st_fisico = me->st_fisico_00 ).

        "me->set_nr_fiscal( i_docnum = 0 ).

        me->set_nr_documento_fatura( i_belnr = space i_gjahr = 0 ).

        "me->set_st_documento( i_st_documento = me->st_fisico_00 ).

        "CLEAR me->ms_nfse_001-belnr.
        "CLEAR me->ms_nfse_001-gjahr.

        append_message2( i_msgty = sy-msgty
                         i_msgid = sy-msgid
                         i_msgno = sy-msgno
                         i_msgv1 = sy-msgv1
                         i_msgv2 = sy-msgv2
                         i_msgv3 = sy-msgv3
                         i_msgv4 = sy-msgv4 ).

      ELSE.



      ENDIF.

      log_to_tab(  ).

      me->save( '1' ).
      me->save( '5' ).

    ENDIF.

  ENDMETHOD.


  method nfse_inbound_doc_material.

    types: begin of ty_ekko,
             ebeln type ekko-ebeln,
             lifnr type ekko-lifnr,
           end of ty_ekko.
    types: begin of ty_ekpo,
             ebeln type ekpo-ebeln,
             ebelp type ekpo-ebelp,
             matnr type ekpo-matnr,
             werks type ekpo-werks,
             lgort type ekpo-lgort,
             menge type ekpo-menge,
             meins type ekpo-meins,
           end of ty_ekpo.
    types: begin of ty_mseg,
             mblnr type mseg-mblnr,
             mjahr type mseg-mjahr,
             zeile type mseg-zeile,
             ebeln type mseg-ebeln,
             ebelp type mseg-ebelp,
           end of ty_mseg.
    data:
      lt_ekpo          type table of ty_ekpo,
      lt_mseg          type table of ty_mseg,
      lt_goodsmvt_item type table of bapi2017_gm_item_create,
      lt_return        type table of bapiret2,
      lt_po_list_assig type /tcsr/y_po_list.
    data:
      lw_goodsmvt_header type bapi2017_gm_head_01,
      lw_goodsmvt_code   type bapi2017_gm_code,
      lw_head_ret        type bapi2017_gm_head_ret,
      lw_log_handle      type balloghndl.
    data:
      lv_mess     type string,
      lv_lifnr    type ekko-lifnr,
      lv_mat_doc  type bapi2017_gm_head_ret-mat_doc,
      lv_doc_year type bapi2017_gm_head_ret-doc_year,
      lv_line     type bapi2017_gm_item_create-line_id,
      lv_lines    type i.
    field-symbols:
      <fs_po_list>       type /tcsr/s_po_list,
      <fs_ekpo>          type ty_ekpo,
      <fs_mseg>          type ty_mseg,
      <fs_goodsmvt_item> type bapi2017_gm_item_create,
      <fs_return>        type bapiret2,
      <fs_po_upd>        type /tcsr/t_po.
    data:
      lo_bal_log type ref to /tcsr/c_bal_log.
*------------------------------------------------

    fill_all( ).
    clear_message( ).

    if ms_nfse_001-mblnr is not initial.

      lv_mess = `Já existe doc.mat: ` && ms_nfse_001-mblnr.

      append_message( lv_mess ).

      exit.

    endif.

    read table mt_nfse_002 assigning field-symbol(<fs_nfse_002_>) with key pstyp = '0'.

    if sy-subrc ne 0.

      append_message( 'Não há item para gerar doc.mat' ).
      exit.

    endif.

    loop at  mt_nfse_002 assigning field-symbol(<fs_nfse_002>) where pstyp = '0'.
      refresh: lt_ekpo,
               lt_goodsmvt_item.
      "Select Vendor from Purchase Order
      select single lifnr
        into lv_lifnr
        from ekko
       where ebeln = <fs_nfse_002>-ebeln.
      check: sy-subrc eq 0.

      "Select Purchase Items
      select ebeln ebelp matnr werks lgort menge meins
        from ekpo
        into table lt_ekpo
        where ebeln = <fs_nfse_002>-ebeln
          and ebelp = <fs_nfse_002>-ebelp.

      "--------------------------------------------
      " FILL BAPI STRUCTURES
      "--------------------------------------------

* Header
      lw_goodsmvt_header-doc_date         = ms_nfse_001-dtemissao.
      lw_goodsmvt_header-pstng_date       = sy-datlo.

      lw_goodsmvt_header-pr_uname         = sy-uname.
      lw_goodsmvt_code-gm_code            = '01'.                   "Goods movement code

      lw_goodsmvt_header-ref_doc_no = nf_serie( ).
      "lw_goodsmvt_header-header_txt = nf_serie( ).

      clear: lv_line.

      "LOOP AT me->mt_po_list_assig ASSIGNING <fs_po_list>.

      read table lt_ekpo assigning <fs_ekpo>
                         with key ebeln = <fs_nfse_002>-ebeln
                                  ebelp = <fs_nfse_002>-ebelp.
      check: sy-subrc eq 0.

      append initial line to lt_goodsmvt_item assigning <fs_goodsmvt_item>.
* ---> S4 Migração - 21/06/2023 - FC - Inicio
      "<fs_goodsmvt_item>-material       = <fs_ekpo>-matnr.

      data(v_len) = strlen( <fs_ekpo>-matnr ).

      if v_len > 18.
        <fs_goodsmvt_item>-material_long = <fs_ekpo>-matnr.
      else.
        <fs_goodsmvt_item>-material = <fs_ekpo>-matnr.
      endif.
* <--- S4 Migração - 21/06/2023 - FC - Fim
      <fs_goodsmvt_item>-plant          = <fs_ekpo>-werks.
      <fs_goodsmvt_item>-stge_loc       = <fs_ekpo>-lgort.
      <fs_goodsmvt_item>-move_type      = '101'.                  "Set Movement ID
      <fs_goodsmvt_item>-vendor         = lv_lifnr.               "Set Vendor
      <fs_goodsmvt_item>-entry_qnt      = <fs_nfse_002>-menge_iv.  "Qtd Filled
      <fs_goodsmvt_item>-entry_uom      = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-entry_uom_iso  = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-po_pr_qnt      = <fs_nfse_002>-menge_iv.  "Qtd Filled
      <fs_goodsmvt_item>-orderpr_un     = <fs_ekpo>-meins.
      <fs_goodsmvt_item>-orderpr_un_iso = <fs_ekpo>-meins.

      "Set PO/ITEM
      <fs_goodsmvt_item>-po_number      = <fs_ekpo>-ebeln.
      <fs_goodsmvt_item>-po_item        = <fs_ekpo>-ebelp.
      <fs_goodsmvt_item>-mvt_ind        = 'B'.                    "Movimento de mercadoria por pedido
      <fs_goodsmvt_item>-line_id        = lv_line = lv_line + 1.

      "ENDLOOP.
      "US162279
      data(lc_user_job) = zcl_job=>get_user_job( ).
      if lc_user_job is not initial.
        sy-uname =  lc_user_job.
      endif.
      "US162279

      call function 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        exporting
          goodsmvt_header  = lw_goodsmvt_header
          goodsmvt_code    = lw_goodsmvt_code
        importing
          goodsmvt_headret = lw_head_ret
          materialdocument = lv_mat_doc
          matdocumentyear  = lv_doc_year
        tables
          goodsmvt_item    = lt_goodsmvt_item
          return           = mt_mess.

      "save( '5' ).

      if lv_mat_doc is not initial.

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = 'X'.

        do 5 times.

          "Select Detail of Invoice document Created
          select mblnr
                 mjahr
                 zeile
                 ebeln
                 ebelp
            from mseg
            into table lt_mseg
           where mblnr = lv_mat_doc
             and mjahr = lv_doc_year.
          if sy-subrc eq 0.
            exit.
          endif.
          wait up to 1 seconds.

        enddo.

        if sy-subrc eq 0.

          ev_mblnr = lv_mat_doc.
          ev_mjahr = lv_doc_year.

          ms_nfse_001-mblnr = lv_mat_doc.
          ms_nfse_001-mjahr = lv_doc_year.

          <fs_nfse_002>-mblnr = lv_mat_doc.
          <fs_nfse_002>-mjahr = lv_doc_year.


          save( '1' ). "< -- apenas dados de cabeçalho
          save( '2' ).

          append_message2(
              i_msgty = 'S'
              i_msgid = '/TCSR/MSG'
              i_msgno = '360'
              i_msgv1 = lv_mat_doc
              i_msgv2 = lv_doc_year ).

          log_to_tab( ).

          save( '5' ).

        endif.

      else.

        call function 'BAPI_TRANSACTION_ROLLBACK'.

        append_message2(
            i_msgty = 'S'
            i_msgid = '/TCSR/MSG'
            i_msgno = '361' ).

        log_to_tab( ).

        save( '5' ).

      endif.
    endloop.
    commit work.

  endmethod.


  METHOD nfse_inbound_estorno_doc_mat.

    DATA lw_header_ret TYPE bapi2017_gm_head_ret.

    DATA lt_mess TYPE TABLE OF bapiret2.

    fill_all( ).

    CHECK ms_nfse_001-mblnr IS NOT INITIAL.

    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
      EXPORTING
        materialdocument = ms_nfse_001-mblnr
        matdocumentyear  = ms_nfse_001-mjahr
      IMPORTING
        goodsmvt_headret = lw_header_ret
      TABLES
        return           = lt_mess.

    IF lt_mess IS NOT INITIAL.

      APPEND LINES OF lt_mess TO mt_mess.

    ELSE.

      CALL METHOD me->append_message2
        EXPORTING
          i_msgid = 'ZMIGO'
          i_msgno = '002'
          i_msgv1 = ms_nfse_001-mblnr
          i_msgv2 = ms_nfse_001-mjahr
          i_msgv3 = lw_header_ret-mat_doc
          i_msgv4 = lw_header_ret-doc_year.

    ENDIF.

    log_to_tab( ).

    IF lw_header_ret-mat_doc IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CLEAR ms_nfse_001-mblnr.
      CLEAR ms_nfse_001-mjahr.

      save( '1' ).

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    ENDIF.

    save( '5' ).


  ENDMETHOD.


  METHOD nfse_inbound_estorno_folha.

    fill_all( ).

    DATA(lt_bdc_mess) = call_shdb_ml81n_revoke( ).

    LOOP AT lt_bdc_mess ASSIGNING FIELD-SYMBOL(<fs_mess>).

      IF <fs_mess>-msgtyp = 'E'.
        r_erro = 'X'.
      ENDIF.


      CALL METHOD me->append_message2
        EXPORTING
          i_msgid = <fs_mess>-msgid
          i_msgno = CONV symsgno( <fs_mess>-msgnr )
          i_msgty = <fs_mess>-msgtyp
          i_msgv1 = <fs_mess>-msgv1
          i_msgv2 = <fs_mess>-msgv2
          i_msgv3 = <fs_mess>-msgv3
          i_msgv4 = <fs_mess>-msgv4.

    ENDLOOP.

    IF r_erro IS INITIAL.
      wait UP TO 5 SECONDS.
      execute_ml81n_delete( ).
    ENDIF.

    save( '5' ).

  ENDMETHOD.


method nfse_inbound_fatura.

  data lv_mess type string.
  data : MSG_ID       type  T100-ARBGB,
         MSG_NO       type  T100-MSGNR,
         MSG_VAR1     type  BALM-MSGV1,
         MSG_VAR2     type  BALM-MSGV2,
         MSG_VAR3     type  BALM-MSGV3,
         MSG_VAR4     type  BALM-MSGV4.
  data cx_miro_exception type ref to zcx_miro_exception.
  data: vg_vlt_total    type ekpo-menge,
        vg_vlt_item     type ekpo-menge,
        vg_calc         type ekpo-menge,
        it_materialdata type table of bapi_incinv_create_material,
        wa_materialdata type bapi_incinv_create_material,
        vg_item         type i.

  r_gerou = abap_false.

  fill_all( ).

  clear_message( ).

  if ms_nfse_001-belnr is not initial.

    lv_mess = `Já existe fatura: ` && ms_nfse_001-belnr.

    append_message( lv_mess ).

    exit.

  endif.
*"BUG SOLTO 193572*
  try.
      zcl_miro=>verificar_vencimento_fatura( i_data_vencimento = ms_nfse_001-dt_vencimento i_pymt_meth = ms_nfse_001-pymt_meth ).
    catch zcx_miro_exception into  cx_miro_exception.
      write cx_miro_exception->if_t100_message~t100key-msgid to msg_id.
      write cx_miro_exception->if_t100_message~t100key-msgno to msg_no.
      write cx_miro_exception->if_t100_message~T100KEY-ATTR1 to msg_var1.
      write cx_miro_exception->if_t100_message~T100KEY-ATTR2 to msg_var2.
      write cx_miro_exception->if_t100_message~T100KEY-ATTR3 to msg_var3.
      write cx_miro_exception->if_t100_message~T100KEY-ATTR4 to msg_var4.
       call function 'MESSAGE_PREPARE'
      exporting
        msg_id                 = msg_id
        msg_no                 = msg_no
        msg_var1               = msg_var1
        msg_var2               = msg_var2
        msg_var3               = msg_var3
        msg_var4               = msg_var4
      importing
        msg_text               = lv_mess
      exceptions
        function_not_completed = 1
        message_not_found      = 2
        others                 = 3.

      append_message( lv_mess ).
      exit.
  endtry.

*BUG SOLTO 193572*


  data(lw_miro_cab) = fill_miro_cab( ).

  " foi colocado fixo para passar na simulação
  lw_miro_cab-partner_bk = '0001'.
  lw_miro_cab-partner_bk =  ms_nfse_001-zbvtyp.

  data(lt_itens) = fill_miro_itens( ).

  data(lt_account) = fill_account( lt_itens ).
  data(lt_glaccount) = fill_glaccount( lt_account ).
  data(lt_withtax) = fill_withtax( ).

  refresh lt_glaccount. "ALRS
  "
  " DEBITO POSTERIOR
  loop at lt_itens assigning field-symbol(<fs_itemdata>).

    select single * into @data(wa_ekpo_nfse) " pedido debito posterior
      from ekpo
     where ebeln eq @<fs_itemdata>-po_number
       and ebelp eq @<fs_itemdata>-po_item.

    select single bsart into @data(v1_bsart) from ekko where ebeln = @<fs_itemdata>-po_number.

    if v1_bsart eq 'YDBP' or v1_bsart eq 'ZDBP'.

      clear: vg_vlt_total, vg_vlt_item, vg_calc.

      select  *  from ekpo into table @data(it_ekpo)  " pedido de ESTOQUE a que se refere o debito posterior
        where ebeln eq @wa_ekpo_nfse-bednr
        and   knttp eq ' '.

      if sy-subrc = 0.
        <fs_itemdata>-item_amount      = '0'.
        <fs_itemdata>-quantity         = '1'.
      endif.

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

        wa_materialdata-invoice_doc_item = <fs_itemdata>-invoice_doc_item. "vg_item + 1.

* ---> S4 Migração - 21/06/2023 - FC - Inicio
        "wa_materialdata-material         = wl_ekpo-matnr.

        data(v_len) = strlen( wl_ekpo-matnr ).

        if v_len > 18.
          wa_materialdata-material_long = wl_ekpo-matnr.
        else.
          wa_materialdata-material = wl_ekpo-matnr.
        endif.
* <--- S4 Migração - 21/06/2023 - FC - Fim

        wa_materialdata-val_area         = wl_ekpo-werks.
        wa_materialdata-valuation_type   = wl_ekpo-bwtar.
        wa_materialdata-db_cr_ind        = 'S'.
        wa_materialdata-item_amount      = wa_ekpo_nfse-netwr * vg_vlt_item.
        wa_materialdata-quantity         = 1.
        wa_materialdata-base_uom         = wl_mara-meins.
        wa_materialdata-tax_code         = <fs_itemdata>-tax_code.
        wa_materialdata-taxjurcode       = wl_ekpo-txjcd. "<fs_itemdata>-taxjurcode.

        append wa_materialdata to it_materialdata.
        clear wa_materialdata.

        clear wl_ekpo.
      endloop.

    endif.

  endloop.
  " DEBITO POSTERIOR
  if it_materialdata[] is not initial.
    refresh: lt_account, lt_glaccount.
  endif.

  call function 'BAPI_INCOMINGINVOICE_CREATE' "#EC CI_USAGE_OK[2438131]
    exporting
      headerdata       = lw_miro_cab
    importing
      invoicedocnumber = ms_nfse_001-belnr
      fiscalyear       = ms_nfse_001-gjahr
    tables
      itemdata         = lt_itens
      accountingdata   = lt_account
      glaccountdata    = lt_glaccount
      materialdata     = it_materialdata " para debito posterior de estoque
      withtaxdata      = lt_withtax
      return           = mt_mess.

  if ms_nfse_001-belnr is initial.

    r_gerou = abap_false.

    call function 'BAPI_TRANSACTION_ROLLBACK'.

  else.

    r_gerou = abap_true.

    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 #114554
    data: tl_return type table of bapirettab.

***    CALL FUNCTION 'ZNFSE_MIRO_ATTACHMENT'
***      EXPORTING
***        im_docno  = ms_nfse_001-guid_header
***        im_miro   = ms_nfse_001-belnr
***        im_gjahr  = ms_nfse_001-gjahr
***      TABLES
***        et_return = tl_return.
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 #114554

    update /tcsr/t_act  set last_stepstatus = '101'
          where guid_header = ms_nfse_001-guid_header.

    wait up to 3 seconds.
    "CFOP de serviço vem em branco
    me->nfe_atualiza_cfop_no_fiscal( ).
    ms_nfse_001-se_detail  = 'Fatura gerada pelo SE'.


    save( '1' ). "< -- apenas dados de cabeçalho

    append_message2(
        i_msgty = 'S'
        i_msgid = 'M8'
        i_msgno = '060'
        i_msgv1 = ms_nfse_001-belnr ).

  endif.


  log_to_tab( ).

  save( '5' ).

endmethod.


  method nfse_inbound_folha.

    data:

      lt_po_items    type table of bapiekpo,
      lt_po_services type table of bapiesll,
      lt_ekkn        type table of bapiekkn,
      lt_po_acc      type table of bapieskl,
      lt_po_return   type table of bapireturn,
      "BAPI
      lt_bapi_eskn   type table of bapiesknc,
      lt_bapi_esll   type table of bapiesllc,
      lt_bapi_eskl   type table of bapiesklc.
    "lt_bapi_return TYPE TABLE OF bapiret2.
    data:
      lw_po_header   type bapiekkol,
      lw_po_items    type bapiekpo,
      lw_po_services type bapiesll,
      lw_po_return   type bapireturn,
      "BAPI
      lw_bapi_header type bapiessrc,
      lw_bapi_esll   type bapiesllc,
      lw_bapi_return type bapiret2,
      lw_out_return  type bapireturn1,
      "BalLog
      lw_log_handle  type balloghndl.
    data:
      lv_mess           type string,
      lv_strlen         type i,
      lv_line_no        type bapiesllc-line_no,
      lv_pckg_no        type bapiesll-pckg_no,
      lv_subpckg_no     type bapiesll-subpckg_no,
      lv_gross_val_unit type bapiesll-gross_val,
      lv_ref_doc_no     type bapiessrc-ref_doc_no,
      lv_acceptance     type bapiessr-acceptance.
    "lv_entrysheet     TYPE bapiessr-sheet_no,
    "lv_packno         TYPE esll-packno.
    field-symbols:
      <fs_po_list> type /tcsr/s_po_list.
    data:
      lo_bal_log type ref to /tcsr/c_bal_log,
      lo_param   type ref to /tcsr/c_param.

    fill_all( ).

    select single * from zibt_nfse_001
      into ms_nfse_001
        where guid_header = ms_nfse_001-guid_header.

    if ms_nfse_001-lblni is not initial.

      lv_mess = `Já existe a folha de serviço: ` && ms_nfse_001-lblni.

      call method get_folha_info
        exporting
          iv_entrysheet = ms_nfse_001-lblni
        importing
          ev_packno     = ms_nfse_001-packno
          ev_mjahr      = ev_mjahr
          ev_mblnr      = ev_mblnr.

      ev_entrysheet = ms_nfse_001-lblni.
      ev_packno = ms_nfse_001-packno.

      if ms_nfse_001-mjahr is initial.

        ms_nfse_001-mblnr = ev_mblnr.
        ms_nfse_001-mjahr = ev_mjahr.

      endif.

      save( '1' ).

      append_message( lv_mess ).

      exit.

    endif.


    read table mt_nfse_002 assigning field-symbol(<fs_nfse_002_>) with key pstyp = '9'.

    if sy-subrc ne 0.

      append_message( 'Não há item de serviço associado' ).
      exit.

    endif.

    clear_message( ).

    create object lo_param.

    loop at mt_nfse_002 assigning field-symbol(<fs_nfse_002>) where pstyp = '9'.
      refresh: lt_bapi_eskn,
                lt_bapi_esll,
                lt_bapi_eskl,
                lt_po_items,
                lt_ekkn,
                lt_po_services,
                lt_po_acc,
                lt_po_return.

      select essr~lblni
            into  table @data(it_lblni)
            from ekbe
            inner join essr on essr~lblni = ekbe~belnr
            inner join esll as s1 on s1~packno = essr~packno
            inner join esll as s2 on s2~packno = s1~sub_packno
             where ekbe~ebeln = @<fs_nfse_002>-ebeln
             and ekbe~ebelp   = @<fs_nfse_002>-ebelp
             and ekbe~vgabe   = '9'
             and ekbe~dmbtr   > 0.

      loop at it_lblni into data(wa_lblni).
        "Se folha não tem MIGO então preparada para aceite
        select count(*)
         from ekbe as migo
         where migo~lfbnr = wa_lblni-lblni
         and   migo~bwart = '101'.
        if sy-subrc ne 0.
          ms_nfse_001-lblni = wa_lblni-lblni.
          exit.
        endif.
        "se folha tem migo estornada então preparada para aceite
        select count(*)
          from ekbe as migo
          where migo~lfbnr = wa_lblni-lblni
          and migo~bwart = '102'.
        if sy-subrc = 0.
          ms_nfse_001-lblni = wa_lblni-lblni.
          exit.
        endif.
      endloop.

      if  ms_nfse_001-lblni is not initial.
        select single *
          from zintegrcoupa01
          into @data(wa_zintegrcoupa01)
          where ident_proc eq 'FS'
          and   fields     eq @ms_nfse_001-lblni. "COUPA FAZ o ACEITE
        if sy-subrc = 0.
          if me->nfse_inbound_aceite_folha( ) is initial. "COUPA
            select single gjahr belnr from ekbe
             into  (ev_mjahr,ev_mblnr)
               where ebeln = <fs_nfse_002>-ebeln
                 and bewtp = 'E'
                 and bwart = '101'
                 and lfbnr = ms_nfse_001-lblni
                 and xwsbr = space.

            if sy-subrc eq 0.

              ms_nfse_001-mblnr = ev_mblnr.
              ms_nfse_001-mjahr = ev_mjahr.

              <fs_nfse_002>-mblnr = ev_mblnr.
              <fs_nfse_002>-mjahr = ev_mjahr.
            endif.

            call function 'MS_RESET_STORAGE_LIMITS'.

            save( '1' ). "< -- apenas dados de cabeçalho
            save( '2' ). "
            continue.
          endif.
        endif.
      endif.

      call function 'BAPI_PO_GETDETAIL' "#EC CI_USAGE_OK[2438131]
        exporting                       "#EC CI_USAGE_OK[1803189]
          purchaseorder              = <fs_nfse_002>-ebeln
          items                      = 'X'
          account_assignment         = 'X'
          services                   = 'X'
        importing
          po_header                  = lw_po_header
        tables
          po_items                   = lt_po_items
          po_item_account_assignment = lt_ekkn
          po_item_services           = lt_po_services
          po_item_srv_accass_values  = lt_po_acc
          return                     = lt_po_return.

      read table lt_po_items into lw_po_items with key po_item = <fs_nfse_002>-ebelp.
      read table lt_po_services into lw_po_services with key pckg_no = lw_po_items-pckg_no.
      delete lt_po_services where pckg_no ne lw_po_services-pckg_no and pckg_no ne lw_po_services-subpckg_no .
      delete lt_po_acc      where pckg_no ne lw_po_services-pckg_no and pckg_no ne lw_po_services-subpckg_no .
      "
      lw_bapi_header-po_number    = lw_po_items-po_number.
      lw_bapi_header-po_item      = lw_po_items-po_item.


      lw_bapi_header-acceptance   = 'X'.

      lw_bapi_header-doc_date     = ms_nfse_001-dtemissao.
      lw_bapi_header-post_date    = sy-datum.

      if lw_po_items-acctasscat = 'U'.
        lw_bapi_header-accasscat  = 'K'.
      else.
        lw_bapi_header-accasscat  = lw_po_items-acctasscat.
      endif.

      lw_bapi_header-pckg_no      = 1."lw_po_items-pckg_no.

      lw_bapi_header-ref_doc_no   = nf_serie( ).
      lw_bapi_header-ext_number   = nf_serie( ).

      " novo
      data(tabix) = sy-tabix.
      sort lt_po_services by line_no.
      loop at lt_po_services into lw_po_services.
        tabix = sy-tabix.

        move-corresponding lw_po_services to lw_bapi_esll.

        read table mt_nfse_003 assigning field-symbol(<fs_nfse_003>)
          with key extrow = lw_po_services-ext_line
                   ebeln  = <fs_nfse_002>-ebeln
                   ebelp  = <fs_nfse_002>-ebelp.

        if sy-subrc eq 0.
          lw_bapi_esll-quantity   = <fs_nfse_003>-menge.
        endif.

        if lw_bapi_header-short_text is initial.
          lw_bapi_header-short_text = lw_po_services-short_text.
        endif.

        " 20.10.2022 - zerar nums pckg -->
        if tabix = 1.
          lw_bapi_esll-pckg_no = tabix.
        else.
          lw_bapi_esll-pckg_no = 2.
        endif.
        lw_bapi_esll-pln_line = 0.
        lw_bapi_esll-pln_pckg = 0.

        if lw_bapi_esll-pckg_no = 1.
          lw_bapi_esll-subpckg_no = 2.
        endif.

        " 20.10.2022 - zerar nums pckg --<

        append lw_bapi_esll to lt_bapi_esll.

      endloop.

"US164103
*      lt_bapi_eskl = corresponding #( lt_po_acc ).


*      if lt_bapi_eskl is not initial.
*
**        lt_bapi_eskl[ 1 ]-pckg_no = 2.
*        loop at lt_bapi_eskl assigning field-symbol(<fs_eskl>).
*          <fs_eskl>-pckg_no = 2.
*        endloop.
*
*        loop at lt_po_acc assigning field-symbol(<fs_acc_po>).
*
*          append initial line to lt_bapi_eskn assigning field-symbol(<fs_eskn>).
*
*          <fs_eskn>-pckg_no = 2.
*
*          <fs_eskn>-serial_no = <fs_acc_po>-serial_no.
*
*          read table lt_ekkn assigning field-symbol(<fs_ekkn>) with key po_item = <fs_nfse_002>-ebelp
*                                                                        serial_no = <fs_acc_po>-serial_no.
*
*          check sy-subrc eq 0.
*
*          <fs_eskn>-gl_account = <fs_ekkn>-g_l_acct.
*          <fs_eskn>-bus_area = <fs_ekkn>-bus_area.
*          <fs_eskn>-costcenter = <fs_ekkn>-cost_ctr.
*          <fs_eskn>-co_area = <fs_ekkn>-co_area.
*          <fs_eskn>-profit_ctr  = <fs_ekkn>-profit_ctr.
*          <fs_eskn>-order            =  <fs_ekkn>-order_no.
*          <fs_eskn>-routing_no       =  <fs_ekkn>-routing_no.
*
*          select single *
*                 into @data(wl_ekkn)
*                 from ekkn
*                 where ebeln eq @<fs_nfse_002>-ebeln
*                   and ebelp eq @<fs_nfse_002>-ebelp
*                   and zekkn eq @<fs_ekkn>-serial_no.
*
*          if wl_ekkn-aufpl is not initial and wl_ekkn-aplzl is not initial.
*            select single vornr
*                   into @data(lv_vornr)
*                   from afvc
*                   where aufpl eq @wl_ekkn-aufpl
*                     and aplzl eq @wl_ekkn-aplzl.
*            if lv_vornr is not initial.
*              <fs_eskn>-activity  =  lv_vornr.
*              clear lv_vornr.
*            endif.
*          endif.
*        endloop.
*
*      endif.

      loop at lt_ekkn assigning field-symbol(<fs_ekkn>) where po_item = <fs_nfse_002>-ebelp.

        append initial line to lt_bapi_eskn assigning field-symbol(<fs_eskn>).

        <fs_eskn>-pckg_no = 2.

        <fs_eskn>-serial_no   = <fs_ekkn>-serial_no.
        <fs_eskn>-gl_account  = <fs_ekkn>-g_l_acct.
        <fs_eskn>-bus_area    = <fs_ekkn>-bus_area.
        <fs_eskn>-costcenter  = <fs_ekkn>-cost_ctr.
        <fs_eskn>-co_area     = <fs_ekkn>-co_area.
        <fs_eskn>-profit_ctr  = <fs_ekkn>-profit_ctr.
        <fs_eskn>-order       =  <fs_ekkn>-order_no.
        <fs_eskn>-routing_no  =  <fs_ekkn>-routing_no.

        select single *
               into @data(wl_ekkn)
               from ekkn
               where ebeln eq @<fs_nfse_002>-ebeln
                 and ebelp eq @<fs_nfse_002>-ebelp
                 and zekkn eq @<fs_ekkn>-serial_no.

        if wl_ekkn-aufpl is not initial and wl_ekkn-aplzl is not initial.
          select single vornr
                 into @data(lv_vornr)
                 from afvc
                 where aufpl eq @wl_ekkn-aufpl
                   and aplzl eq @wl_ekkn-aplzl.
          if lv_vornr is not initial.
            <fs_eskn>-activity  =  lv_vornr.
            clear lv_vornr.
          endif.
        endif.
      endloop.
      "US164103 FIM
                                                         "US162279
      data(lc_user_job) = zcl_job=>get_user_job( ).
      if lc_user_job is not initial.
        sy-uname =  lc_user_job.
      endif.
                                                            "US162279
      call function 'BAPI_ENTRYSHEET_CREATE'
        exporting
          entrysheetheader            = lw_bapi_header
          testrun                     = ''
        importing
          entrysheet                  = ms_nfse_001-lblni
        tables
          entrysheetaccountassignment = lt_bapi_eskn
          entrysheetservices          = lt_bapi_esll
          entrysheetsrvaccassvalues   = lt_bapi_eskl
          return                      = mt_mess.

      read table mt_mess transporting no fields with key type = 'E'.
      .
      if sy-subrc ne 0 and ms_nfse_001-lblni is not initial.

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = abap_true.

        do 5 times.

          "Select Serv.EntrySheet Created
          select single packno
            into ms_nfse_001-packno
            from essr
            where lblni = ms_nfse_001-lblni.

          if sy-subrc eq 0.
            exit.
          else.
            wait up to 1 seconds.
          endif.

        enddo.
        <fs_nfse_002>-lblni  = ms_nfse_001-lblni.
        <fs_nfse_002>-packno = ms_nfse_001-packno.

        " existe serviço E é só um
*        IF line_exists( mt_nfse_002[ pstyp = '9' ] ) AND lines( mt_nfse_002 ) = 1.
        if <fs_nfse_002>-pstyp = '9'.
          select single gjahr belnr from ekbe
            into  (ev_mjahr,ev_mblnr)
              where ebeln = <fs_nfse_002>-ebeln
                and bewtp = 'E'
                and bwart = '101'
                and lfbnr = ms_nfse_001-lblni
                and xwsbr = space.

          if sy-subrc eq 0.

            ms_nfse_001-mblnr = ev_mblnr.
            ms_nfse_001-mjahr = ev_mjahr.

            <fs_nfse_002>-mblnr = ev_mblnr.
            <fs_nfse_002>-mjahr = ev_mjahr.
          endif.

        endif.

        call function 'MS_RESET_STORAGE_LIMITS'.

        save( '1' ). "< -- apenas dados de cabeçalho
        save( '2' ). "

        ev_entrysheet = ms_nfse_001-lblni.
        ev_acceptance = lw_bapi_header-acceptance.
        ev_packno = ms_nfse_001-packno.

        log_to_tab( ).

        save( '5' ).

      else.

        clear ms_nfse_001-lblni.

        call function 'BAPI_TRANSACTION_ROLLBACK'.

        log_to_tab( ).

        save( '5' ).

      endif.

    endloop.

  endmethod.


  method nfse_sol_miro_softexpert.

    data: objeto       type ref to zcl_soft_expert_ws_inject,
          wa_values    type zde_zsexpt00004_values,
          wa_anexos    type zde_zsexpt00007,
          lc_vl_total  type c length 20,
          lc_text1     type string,
          lc_text2     type string,
          lc_info      type c length 4000,
          lv_mess      type string,
          lv_guid      type string,
          v1_zterm     type ekko-zterm, "USER STORY 163047 / AOENNING
          lv_date_venc type zibt_nfse_001-dtemissao, "USER STORY 163047 / AOENNING
          lv_dia_cod   type p, "USER STORY 163047 / AOENNING
          lv_dias      type ekko-zbd1t.

    fill_all( ).

    clear_message( ).

    if ms_nfse_001-ck_revisao = abap_false and ms_nfse_001-se_recordid is not initial.

      lv_mess = `Já existe SM em processamento: ` && ms_nfse_001-se_recordid.

      append_message( lv_mess ).

      exit.

    endif.

    ">>>>>>>Inicio USER STORY 163047 / AOENNING
    if sy-tcode eq 'ZFIS63'.

      "Seleciona dados cabecalho pedido.
      select single * from ekko into @data(wa_pedido)
        where ebeln eq @ms_nfse_001-ebeln.
      if wa_pedido is not initial.
        "Verifica stvarv ZFIS63_VALIDA_TIP_PEDIDO.
        select single * from tvarvc into @data(wa_tipo_pedido)
          where name eq 'ZFIS63_VALIDA_TIP_PEDIDO'
            and low  eq @wa_pedido-bsart.
        if sy-subrc eq 0.
*          v1_zterm = wa_pedido-zterm+1(3).
          clear: lv_dias.
          lv_dias = wa_pedido-zbd1t.
          if lv_dias is not initial and ms_nfse_001-dtemissao is not initial.
            "Calcular date.
            lv_date_venc = ( ms_nfse_001-dtemissao + lv_dias ).
            if ms_nfse_001-dt_vencimento < lv_date_venc.
*            Somente permitido prorrogar  '
*                                    'a data de vencimento Pedido. Procurar '
*                                    'departamento de Suprimentos!'.
              lv_mess = 'Data de vencimento é menor que a data da nota/condição pagamento do pedido. Procurar departamento de Suprimentos!'.

              append_message( lv_mess ).
              exit.
            endif.
          endif.
        endif.
      endif.
    endif.
    ">>>>>>>Fim USER STORY 163047 / AOENNING


    if ms_nfse_001-belnr is not initial.

      lv_mess = `Já existe Fatura gerada: ` && ms_nfse_001-belnr.

      append_message( lv_mess ).

      exit.

    endif.

    if ms_nfse_001-ebeln is initial.

      lv_mess = `É necessário associar um pedido`.

      append_message( lv_mess ).

      exit.

    endif.

    read table mt_nfse_002 assigning field-symbol(<fs_nfse_002>) with key pstyp = '9'.
    if sy-subrc = 0.
      if ms_nfse_001-lblni is initial.
        lv_mess = `É necessário criar serviço primeiro`.
        append_message( lv_mess ).
        exit.

      endif.
    endif.

    if ms_nfse_001-mblnr is initial.

      lv_mess = `É necessário criar doc.material primeiro`.

      append_message( lv_mess ).

      exit.

    endif.

    if ms_nfse_001-dt_vencimento is initial.

      lv_mess = `Informar data de vencimento`.

      append_message( lv_mess ).

      exit.

    endif.

    if ms_nfse_001-pymt_meth is initial.

      lv_mess = `Informar forma de pagamento`.

      append_message( lv_mess ).

      exit.

    endif.


    case i_estornar.
      when abap_false.
        try .
            "Objeto Generico
            objeto = cast #( zcl_soft_expert_ws_inject=>zif_soft_expert_ws_inject~get_instance(
                               i_workflow_sap = zcl_soft_expert_workflow=>zif_soft_expert_workflow~st_process_solicitacao_miro
                            )->clear(
                            ) ).

            wa_values-value = ''.

            wa_values-value = ms_nfse_001-ebeln.

            wa_values-key         = ms_nfse_001-guid_header.
            wa_values-entityid    = 'SOLMIRO'.
            wa_values-attributeid = 'PEDCOMPRA'.
            wa_values-tabname     = 'ITENS'.
            wa_values-fieldname   = 'EBELN'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'CHAVE'.
            wa_values-value       = ms_nfse_001-guid_header.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'DTVENC'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_data.
            wa_values-value       = ms_nfse_001-dt_vencimento.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'TEMBOLETO'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            wa_values-value       = cond string( when ms_nfse_001-pymt_meth ne zcl_miro=>st_forma_pagamento_boleto then '0' else '1' ).
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'DTDOCUMENTO'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_data.
            wa_values-value       = ms_nfse_001-dtemissao.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'FILIALCOD'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            wa_values-value       = ms_nfse_001-branch.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'FORNECEDORCNPJ'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.

            if ms_nfse_001-p_cnpj is not initial.
              wa_values-value       = ms_nfse_001-p_cnpj.
            else.
              wa_values-value       = ms_nfse_001-p_cpf.
            endif.

            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'FORNECEDORNOME'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            wa_values-value       = ms_nfse_001-forne_razao.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'CDFORNECEDOR'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            wa_values-value       = ms_nfse_001-lifnr.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'REFNF'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.

            "CONCATENATE ms_nfse_001-nfse_numero '-' ms_nfse_001-nfse_serie INTO wa_values-value.

            wa_values-value = nf_serie( ).

            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'VALOR'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_moeda.
            lc_vl_total = ms_nfse_001-nfse_value.
            condense lc_vl_total no-gaps.
            wa_values-value = lc_vl_total.
            objeto->add_entity_value( i_entity_value = wa_values ).

            "Enviar valor liquido
            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'VALORLIQUIDO'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_moeda.
            lc_vl_total = gv_liqui_po.
            condense lc_vl_total no-gaps.
            wa_values-value = lc_vl_total.
            objeto->add_entity_value( i_entity_value = wa_values ).

            if ms_nfse_001-mblnr is not initial.
              wa_values-tabname     = 'NOTA'.
              wa_values-attributeid = 'NRMIGO'.
              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
              concatenate ms_nfse_001-mblnr '/' ms_nfse_001-mjahr into wa_values-value.
              objeto->add_entity_value( i_entity_value = wa_values ).
            endif.


            wa_values-attributeid = 'IMOBILIZADO'.
            wa_values-value       = '0'.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'MERCADORIA'.
            wa_values-value       = '0'.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'SERVICO'.
            wa_values-value       = '1'.
            objeto->add_entity_value( i_entity_value = wa_values ).

            if ms_nfse_001-zlspr is not initial.
              wa_values-attributeid = 'LANCBLOQUEADO'. "lacnbloqueado BUG 56662 IR058236

              wa_values-value       = '1'.
              objeto->add_entity_value( i_entity_value = wa_values ).
            endif.

            wa_values-attributeid = 'CARTORIO'.
            wa_values-value       = '0'.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'ARQUIVARNOTA'.
            wa_values-value       = '10'.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'IVA'.
            wa_values-value       = ms_nfse_001-mwskz.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'TIPODEDOCUMENTO'.
            wa_values-value       = '1'.
            objeto->add_entity_value( i_entity_value = wa_values ).

* ------->  US#170323 - MMSILVA - 06.05.2025 - Inicio
            wa_values-attributeid = 'chkforapolitica'.
            wa_values-value       = cond #( when ms_nfse_001-ck_fpol = abap_true then '1'
                                                                                 else '0' ).
            objeto->add_entity_value( i_entity_value = wa_values ).
* ------->  US#170323 - MMSILVA - 06.05.2025 - Fim


            select single * from lfbk
              into @data(lw_lfbk)

             where lifnr eq @ms_nfse_001-lifnr
               and bvtyp eq @ms_nfse_001-zbvtyp.

            select single * from bnka
              into @data(lw_bnka)
             where banks eq @lw_lfbk-banks
               and bankl eq @lw_lfbk-bankl.

            if sy-subrc is initial.
              wa_values-attributeid = 'BANCO'.
              wa_values-value       = lw_bnka-bankl(3).
              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'AGENCIA'.
              if not lw_lfbk-bkont is initial.
                concatenate lw_lfbk-bankl+4(11) '-' lw_lfbk-bkont into wa_values-value.
              else.
                wa_values-value = lw_lfbk-bankl+4(11).
              endif.
              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'CONTA'.
              wa_values-value       = lw_lfbk-bankn.
              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
              objeto->add_entity_value( i_entity_value = wa_values ).
            endif.

            if  ms_nfse_001-pymt_meth eq zcl_miro=>st_forma_pagamento_boleto or ms_nfse_001-obs_financeira is not initial.
              wa_values-attributeid = 'INFO'.
              clear wa_values-value.
              if  ms_nfse_001-pymt_meth eq zcl_miro=>st_forma_pagamento_boleto.
                wa_values-value       = ms_nfse_001-boleto.
              endif.
              if ms_nfse_001-obs_financeira is not initial.
                concatenate ms_nfse_001-obs_financeira wa_values-value into  wa_values-value separated by space.
              endif.
              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
              objeto->add_entity_value( i_entity_value = wa_values ).
            endif.
*            IF ms_nfse_001-obs_financeira IS NOT INITIAL.
*              wa_values-attributeid = 'INFO'.
*              wa_values-value       = ms_nfse_001-obs_financeira.
*              wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
*              objeto->add_entity_value( i_entity_value = wa_values ).
*            ENDIF.


            "Add Anexo """"""""""""""""""""""""""""""""
            me->danfe( importing e_url = data(e_url) ).

            wa_anexos-activityid = 'atvsolicitarmiro'.

            lv_guid = ms_nfse_001-guid_header.

            concatenate lv_guid '.pdf' into wa_anexos-filename.

            wa_anexos-file_url = e_url.

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
            select single *
               from /tcsr/t_pdf
               into @data(lw_sit_pdf)
               where guid_header = @lv_guid.

            if lw_sit_pdf-pdfurl is not initial.
              concatenate lv_guid '.url' into wa_anexos-filename.
              wa_anexos-file_url = lw_sit_pdf-pdfurl.
            endif.
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554

            objeto->add_attachments( i_attachment = wa_anexos ).


*            " 19.09.2022 - RAMON LIMA - Adição parametros -->
            if line_exists( mt_impostos[ txcode = 'ISS' ] ).

              data(lw_tax) = mt_impostos[ txcode = 'ISS' ] .

              wa_values-attributeid = 'ISSQN1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).


              wa_values-attributeid = 'ISSQN3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).
            else.
              wa_values-attributeid = 'ISSQN1'.
              wa_values-value       = 0.
              objeto->add_entity_value( i_entity_value = wa_values ).


              wa_values-attributeid = 'ISSQN3'.
              wa_values-value       = 0.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

            if line_exists( mt_impostos[ txcode = 'INSS' ] ).

              lw_tax = mt_impostos[ txcode = 'INSS' ] .

              wa_values-attributeid = 'INSS1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'INSS3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

            if line_exists( mt_impostos[ txcode = 'PISCOFCSLL' ] ).

              lw_tax  = mt_impostos[ txcode = 'PISCOFCSLL' ] .

              wa_values-attributeid = 'PISCOFCSLL1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'PISCOFCSLL3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

            " --------- PIS
            if line_exists( mt_impostos[ txcode = 'PIS' ] ).

              lw_tax  = mt_impostos[ txcode = 'PIS' ] .

              wa_values-attributeid = 'PIS1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'PIS3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

            " --------- COFINS
            if line_exists( mt_impostos[ txcode = 'COFI' ] ).

              lw_tax  = mt_impostos[ txcode = 'COFI' ] .

              wa_values-attributeid = 'COFINS1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'COFINS3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

            " --------- CSLL
            if line_exists( mt_impostos[ txcode = 'CSLL' ] ).

              lw_tax  = mt_impostos[ txcode = 'CSLL' ] .

              wa_values-attributeid = 'CSLL1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'CSLL3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.


            if line_exists( mt_impostos[ txcode = 'IR' ] ).

              lw_tax  = mt_impostos[ txcode = 'IR' ] .

              wa_values-attributeid = 'IRRF1'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

              wa_values-attributeid = 'IRRF3'.
              wa_values-value       = lw_tax-xml_value.
              objeto->add_entity_value( i_entity_value = wa_values ).

            endif.

*            " 19.09.2022 - RAMON LIMA - Adição parametros --<

            if se_add_tax_values( objeto ) ne space.

              lv_mess = `Não foi possivel calcular impostos`.

              append_message( lv_mess ).

              exit.

            endif.

            "Criar WorkFlowSE

            data(lo_se_workflow) = zcl_soft_expert_workflow=>zif_soft_expert_workflow~get_instance( ).

            lo_se_workflow->set_process_workflow_inject( i_inject = objeto ).

            lo_se_workflow->set_attachment( i_attachments = objeto->attachments ).

            lo_se_workflow->create_new_workflow( exporting i_desativar    = i_desativar
                                                           i_new_workflow = value #( status    = ms_nfse_001-se_status
                                                                                     code      = ms_nfse_001-se_code
                                                                                     detail    = ms_nfse_001-se_detail
                                                                                     recordkey = ms_nfse_001-se_recordkey
                                                                                     recordid  = ms_nfse_001-se_recordid )
                                                 importing e_new_workflow = data(e_new_workflow)
                                                          ).

            ms_nfse_001-se_code      = e_new_workflow-code.
            ms_nfse_001-se_detail    = e_new_workflow-detail.
            ms_nfse_001-se_recordid  = e_new_workflow-recordid.
            ms_nfse_001-se_recordkey = e_new_workflow-recordkey.
            ms_nfse_001-se_status    = e_new_workflow-status.

            clear ms_nfse_001-ck_revisao. "Se voltou como revisão, agora limpa novamente

            ms_nfse_001-se_code      = e_new_workflow-code.
            ms_nfse_001-se_detail    = e_new_workflow-detail.
            ms_nfse_001-se_recordid  = e_new_workflow-recordid.
            ms_nfse_001-se_recordkey = e_new_workflow-recordkey.
            ms_nfse_001-se_status    = e_new_workflow-status.

            save( '1' ).

            if ms_nfse_001-se_recordid is not initial.

              append_message2(
                i_msgty = 'S'
                i_msgid = 'ZSEWORKFLOW'
                i_msgno = '000'
                i_msgv1 = 'Processo no SE criado:'
                i_msgv2 = ms_nfse_001-se_recordid ).

              log_to_tab( ).

              save( '5' ).

            endif.

            sy-msgty = 'S'.

          catch zcx_soft_expert_workflow into data(ex_soft_expert_workflow).    " .
            ms_nfse_001-se_code      = e_new_workflow-code.
            ms_nfse_001-se_detail    = e_new_workflow-detail.
            ms_nfse_001-se_recordid  = e_new_workflow-recordid.
            ms_nfse_001-se_recordkey = e_new_workflow-recordkey.
            ms_nfse_001-se_status    = e_new_workflow-status.

            save( '1' ).
            ex_soft_expert_workflow->published_erro( exporting i_msgty = 'S' i_msgty_display = 'S' ).

            message id sy-msgid type sy-msgty number sy-msgno into data(mtext) with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

            append_message2(
              i_msgty = 'E'
              i_msgid = sy-msgid
              i_msgno = sy-msgno
              i_msgv1 = sy-msgv1
              i_msgv2 = sy-msgv2
              i_msgv3 = sy-msgv3
              i_msgv4 = sy-msgv4 ).

            log_to_tab( ).

            save( '5' ).

            "zcl_nfe_inbound=>gera_erro_geral( EXPORTING i_texto = mtext ).
        endtry.
      when abap_true.

        "Não vai estornar mais a SM
        check 1 = 2.


      when 'E'.

        check ms_nfse_001-se_recordid is not initial.

        try .

            objeto = cast #( zcl_soft_expert_ws_inject=>zif_soft_expert_ws_inject~get_instance(
                               i_workflow_sap = zcl_soft_expert_workflow=>zif_soft_expert_workflow~st_process_solicitacao_miro
                            )->clear(
                            ) ).

            wa_values-value = ' '.

            wa_values-key         = ms_nfse_001-guid_header.
            wa_values-entityid    = 'SOLMIRO'.
            wa_values-attributeid = 'PEDCOMPRA'.
            wa_values-tabname     = 'ITENS'.
            wa_values-fieldname   = 'EBELN'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-tabname     = 'NOTA'.
            wa_values-attributeid = 'DTVENC'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            objeto->add_entity_value( i_entity_value = wa_values ).

            wa_values-attributeid = 'INFO'.
            wa_values-type        = zif_soft_expert_workflow=>st_type_field_value_texto.
            objeto->add_entity_value( i_entity_value = wa_values ).


            "Cancelar WorkFlowSE
            zcl_soft_expert_workflow=>zif_soft_expert_workflow~get_instance(
              )->set_process_workflow_inject( i_inject = objeto
              )->set_attachment( i_attachments = objeto->attachments
              )->cancel_workflow_novo(
                   exporting i_new_workflow = value #( status    = ms_nfse_001-se_status
                                                       code      = ms_nfse_001-se_code
                                                       detail    = ms_nfse_001-se_detail
                                                       recordkey = ms_nfse_001-se_recordkey
                                                       recordid  = ms_nfse_001-se_recordid )
                   importing e_new_workflow = e_new_workflow
            ).


            data(lc_se_recordid) = ms_nfse_001-se_recordid.

            clear: ms_nfse_001-se_code,
                   ms_nfse_001-se_detail,
                   ms_nfse_001-se_recordid,
                   ms_nfse_001-se_recordkey,
                   ms_nfse_001-se_status.

            sy-msgty = 'S'.

          catch zcx_soft_expert_workflow into ex_soft_expert_workflow.
            ex_soft_expert_workflow->published_erro( exporting i_msgty = 'S' i_msgty_display = 'S' ).
            message id sy-msgid type sy-msgty number sy-msgno into mtext with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            zcl_nfe_inbound=>gera_erro_geral( exporting i_texto = mtext ).
        endtry.

    endcase.

  endmethod.


  METHOD nf_document_read.

    DATA lt_header_msg TYPE TABLE OF j_1bnfftx.
    DATA lt_refer_msg TYPE TABLE OF j_1bnfref.

    CHECK iv_docnum IS NOT INITIAL.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
      EXPORTING
        doc_number         = iv_docnum
      IMPORTING
        doc_header         = es_header
      TABLES
        doc_partner        = et_partner
        doc_item           = et_item
        doc_item_tax       = et_item_tax
        doc_header_msg     = lt_header_msg
        doc_refer_msg      = lt_refer_msg
*       DOC_OT_PARTNER     =
*       DOC_IMPORT_DI      =
*       DOC_IMPORT_ADI     =
*       DOC_CTE_RES        =
*       DOC_CTE_DOCREF     =
*       DOC_TRANS_VOLUMES  =
*       DOC_TRAILER_INFO   =
*       DOC_TRADE_NOTES    =
*       DOC_ADD_INFO       =
*       DOC_REF_PROC       =
*       DOC_SUGAR_SUPPL    =
*       DOC_SUGAR_DEDUC    =
*       DOC_VEHICLE        =
*       DOC_PHARMACEUT     =
*       DOC_FUEL           =
*       DOC_EXPORT         =
*       DOC_NVE            =
*       DOC_TRACEABILITY   =
*       DOC_PHARMA         =
*       DOC_PAYMENT        =
*       DOC_TEC_RESP       =
      EXCEPTIONS
        document_not_found = 1
        docum_lock         = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD nf_document_update.

    DATA lt_header_msg TYPE TABLE OF j_1bnfftx.
    DATA lt_refer_msg TYPE TABLE OF j_1bnfref.

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UPDATE'
      EXPORTING
        doc_number            = is_header-docnum
        doc_header            = is_header
      TABLES
        doc_partner           = it_partner
        doc_item              = it_item
        doc_item_tax          = it_item_tax
        doc_header_msg        = lt_header_msg
        doc_refer_msg         = lt_refer_msg
*       DOC_OT_PARTNER        =
*       DOC_IMPORT_DI         =
*       DOC_IMPORT_ADI        =
*       DOC_CTE_DOCREF        =
*       DOC_CTE_RES           =
*       DOC_TRANS_VOLUMES     =
*       DOC_TRAILER_INFO      =
*       DOC_TRADE_NOTES       =
*       DOC_ADD_INFO          =
*       DOC_REF_PROC          =
*       DOC_SUGAR_SUPPL       =
*       DOC_SUGAR_DEDUC       =
*       DOC_VEHICLE           =
*       DOC_PHARMACEUT        =
*       DOC_FUEL              =
*       DOC_EXPORT            =
*       DOC_NVE               =
*       DOC_TRACEABILITY      =
*       DOC_PHARMA            =
*       DOC_PAYMENT           =
*       DOC_TEC_RESP          =
      EXCEPTIONS
        document_not_found    = 1
        update_problem        = 2
        doc_number_is_initial = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD nf_serie.

    DATA lv_num TYPE c LENGTH 40.

    IF ms_nfse_001-nfse_numero CA '-'.
      SPLIT ms_nfse_001-nfse_numero AT '-' INTO ms_nfse_001-nfse_numero ms_nfse_001-nfse_serie.
    ENDIF.

    IF ms_nfse_001-nfse_serie IS INITIAL.
      ms_nfse_001-nfse_serie = '1'.
    ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
    ms_nfse_001-nfse_serie = ms_nfse_001-dtemissao+2(2).
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554

    lv_num = ms_nfse_001-nfse_numero.

    SHIFT lv_num LEFT DELETING LEADING '0'.

    CONDENSE lv_num NO-GAPS.

    CHECK lv_num IS NOT INITIAL.

    DATA(_tam) = strlen( lv_num ).
    IF _tam GT 9.
      _tam = _tam - 9.
      lv_num = lv_num+_tam(9) .
      SHIFT lv_num LEFT DELETING LEADING '0'.
    ELSE.
      lv_num = lv_num .
    ENDIF.

    CONCATENATE lv_num '-' ms_nfse_001-nfse_serie INTO lv_num.

    r_ret = lv_num.

  ENDMETHOD.


  METHOD only_mat_po.

    fill_all( ).

    ret = 'X'.

    LOOP AT  mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>)
      WHERE pstyp = '9'.
      ret = space.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD only_serv_po.

    fill_all( ).

    READ TABLE mt_nfse_002 ASSIGNING FIELD-SYMBOL(<fs_nfse_002>) WITH KEY pstyp = '9'.

    IF sy-subrc EQ 0.

      ret = 'X'.

    ELSE.

      ret = ''.

    ENDIF.

  ENDMETHOD.


  METHOD save.

    CHECK ms_nfse_001-guid_header is not INITIAL.

    IF i_tab_number = '1' OR i_tab_number = space.

      MODIFY zibt_nfse_001 FROM ms_nfse_001.

    ENDIF.

    IF i_tab_number = '2' OR i_tab_number = space.

      MODIFY zibt_nfse_002 FROM TABLE mt_nfse_002.

    ENDIF.

    IF i_tab_number = '3' OR i_tab_number = space.

      MODIFY zibt_nfse_003 FROM TABLE mt_nfse_003.

    ENDIF.

    IF i_tab_number = '4' OR i_tab_number = space.

      MODIFY zibt_nfse_004 FROM TABLE mt_nfse_004.

    ENDIF.

    IF i_tab_number = '5' OR i_tab_number = space.

      MODIFY zibt_nfse_005 FROM TABLE mt_nfse_005.

    ENDIF.

    IF i_tab_number = '34' OR i_tab_number = space.

      MODIFY zlest0034 FROM TABLE mt_nfse_0034.

    ENDIF.

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD set_aceitar_faturar.


    IF me->ms_nfse_001-tp_compra_futura IS INITIAL OR me->ms_nfse_001-tp_compra_futura EQ zcl_nfe_inbound=>tp_compra_futura_fatura.
      me->ms_nfse_001-ck_alterou                = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD set_ck_fisico.

    IF me->ms_nfse_001-ck_fisico NE i_check.

      me->ms_nfse_001-ck_alterou = abap_true.

    ENDIF.

    me->ms_nfse_001-ck_fisico = i_check.

  ENDMETHOD.


  METHOD set_impostos.

    IF i_issqn IS NOT INITIAL.
      mv_issqn = i_issqn.
    ENDIF.

    IF i_inss IS NOT INITIAL.
      mv_inss  = i_inss .
    ENDIF.

    IF i_pis IS NOT INITIAL.
      mv_pis   = i_pis.
    ENDIF.

    IF i_cof IS NOT INITIAL.
      mv_cof   = i_cof.
    ENDIF.

    IF i_csll IS NOT INITIAL.
      mv_csll  = i_csll.
    ENDIF.

    IF i_irrf IS NOT INITIAL.
      mv_irrf  = i_irrf.
    ENDIF.

    IF i_piscofcsll IS NOT INITIAL.
      mv_piscofcsll = i_piscofcsll.
    ENDIF.

    IF i_base_issqn IS NOT INITIAL.
      mv_base_issqn = i_base_issqn.
    ENDIF.

    IF i_base_inss IS NOT INITIAL.
      mv_base_inss = i_base_inss.
    ENDIF.

    IF i_base_irrf IS NOT INITIAL.
      mv_base_irrf = i_base_irrf.
    ENDIF.

    IF i_data_vencimento IS NOT INITIAL.
      mv_vencimento = i_data_vencimento.
    ENDIF.

  ENDMETHOD.


  METHOD set_nfse_em_revisao.

    DATA: wa_revisao TYPE zibt_nfse_004.

    CHECK me->mv_guid IS NOT INITIAL.

    wa_revisao-guid_header   = me->mv_guid.
    wa_revisao-dt_registro = sy-datum.
    wa_revisao-hr_registro = sy-uzeit.
    wa_revisao-ds_motivo   = i_motivo_revisao.
    wa_revisao-ck_ultimo   = abap_true.
    wa_revisao-us_revisao  = i_usuario_solicitante.

    UPDATE zibt_nfse_004
       SET ck_ultimo = abap_false
     WHERE guid_header EQ me->mv_guid
       AND ck_ultimo EQ abap_true.

    MODIFY zibt_nfse_004 FROM wa_revisao.


    me->ms_nfse_001-ck_revisao = abap_true.
    me->ms_nfse_001-se_detail  = i_motivo_revisao.

    CLEAR:  ms_nfse_001-se_code,
            ms_nfse_001-se_detail,
            ms_nfse_001-se_recordid,
            ms_nfse_001-se_recordkey.

    me->save( '1' ).

  ENDMETHOD.


  METHOD set_nr_documento_fatura.

    IF ( me->ms_nfse_001-belnr NE i_belnr ) OR ( me->ms_nfse_001-gjahr NE i_gjahr ).
      me->ms_nfse_001-ck_alterou = abap_true.
    ENDIF.

    me->ms_nfse_001-belnr = i_belnr.
    me->ms_nfse_001-gjahr = i_gjahr.


  ENDMETHOD.


  METHOD set_st_armazem.

    IF me->ms_nfse_001-st_armazem NE i_st_armazem.
      me->ms_nfse_001-ck_alterou = abap_true.
    ENDIF.

    me->ms_nfse_001-st_armazem = i_st_armazem.

  ENDMETHOD.


  METHOD set_st_fiscal.

    IF me->ms_nfse_001-st_fiscal NE i_st_fiscal.
      me->ms_nfse_001-ck_alterou = abap_true.
    ENDIF.

    me->ms_nfse_001-st_fiscal = i_st_fiscal.

  ENDMETHOD.


  METHOD set_st_fisico.

    IF me->ms_nfse_001-st_fisico NE i_st_fisico.
      me->ms_nfse_001-ck_alterou = abap_true.
    ENDIF.

    me->ms_nfse_001-st_fisico = i_st_fisico.

  ENDMETHOD.


  METHOD set_value_iss.

    r_zcl_nfse_inbound = me.

    IF i_ind_iss IS NOT INITIAL.
      me->at_gv_iss = i_ind_iss.
    ENDIF.
  ENDMETHOD.


  METHOD set_vt_docs.

    CHECK mt_nfse_0034 IS NOT INITIAL.

    SELECT * FROM zlest0032
      INTO TABLE @DATA(lt_0032)
        FOR  ALL ENTRIES IN @mt_nfse_0034
          WHERE tknum = @mt_nfse_0034-tknum.

    LOOP AT mt_nfse_0034 ASSIGNING FIELD-SYMBOL(<fs_0034>).

      <fs_0034>-re_belnr  = i_belnr.
      <fs_0034>-re_gjahr  = i_gjahr.
      <fs_0034>-en_docnum = space.

      READ TABLE lt_0032 ASSIGNING FIELD-SYMBOL(<fs_0032>)
        WITH KEY tknum = <fs_0034>-tknum.

      IF sy-subrc EQ 0.

        <fs_0032>-belnr = i_belnr.
        <fs_0032>-gjahr = i_gjahr.

        IF <fs_0032>-belnr IS INITIAL.
          <fs_0032>-docnum = ''.
        ELSE.
          <fs_0032>-docnum = <fs_0034>-en_docnum.
        ENDIF.

      ENDIF.

    ENDLOOP.

    "IF i_del_ind IS INITIAL.

    MODIFY zlest0032 FROM TABLE lt_0032.

    UPDATE /tcsr/t_act
      SET belnr = i_belnr
          gjahr = i_gjahr
          WHERE guid_header = ms_nfse_001-guid_header.

    "ENDIF.


  ENDMETHOD.


  method se_add_tax_values.

    data lt_with type table of bapi_incinv_create_withtax.
    data lw_values type zde_zsexpt00004_values.

    data(lw_cab) = fill_miro_cab( ).
    data(lt_itens) = fill_miro_itens( ).
    data(lt_account) = fill_account( lt_itens ).
    data(lt_glaccount) = fill_glaccount( lt_account ).

    clear r_erro.

    clear mt_mess.

    " foi colocado fixo para passar na simulação
*    lw_cab-partner_bk = '0001'.


    call function 'ZMM_NFSE_GET_WITHTAX'
      exporting
        i_header          = lw_cab
        i_guid_header     = ms_nfse_001-guid_header
      tables
        it_itemdata       = lt_itens
        it_accountingdata = lt_account
        it_glaccountdata  = lt_glaccount
        et_withtaxdata    = lt_with
        et_bapiret2       = mt_mess.

    if line_exists( mt_mess[ type = 'E' ] ).

      log_to_tab( ).

      save( '5' ).

      r_erro = 'X'.

      exit.

    endif.

    loop at lt_with assigning field-symbol(<fs_tax>).

      clear lw_values.

      lw_values-key         = ms_nfse_001-guid_header.
      lw_values-entityid    = 'SOLMIRO'.
      lw_values-tabname     = 'NOTA'.
      lw_values-fieldname   = 'EBELN'.

      if <fs_tax>-wi_tax_type = 'CT'." AND <fs_tax>-wi_tax_code = 'CT'.

        lw_values-attributeid = 'PISCOFCSLL2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      if <fs_tax>-wi_tax_type = 'IR'."  AND <fs_tax>-wi_tax_code = 'R1'.

        lw_values-attributeid = 'IRRF2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      if <fs_tax>-wi_tax_type = 'IS'."  AND <fs_tax>-wi_tax_code = 'R5'.

        lw_values-attributeid = 'ISSQN2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      if <fs_tax>-wi_tax_type = 'IN'.

        lw_values-attributeid = 'INSS2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      " -----------------CSLL
      if <fs_tax>-wi_tax_type = 'CS'.

        lw_values-attributeid = 'CSLL2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      " -----------------COFINS
      if <fs_tax>-wi_tax_type = 'CF'.

        lw_values-attributeid = 'COFINS2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.

      " -----------------PIS
      if <fs_tax>-wi_tax_type = 'PI'.

        lw_values-attributeid = 'PIS2'.

        lw_values-value       = <fs_tax>-wi_tax_amt.

        io_object->add_entity_value( i_entity_value = lw_values ).

      endif.



    endloop.

  endmethod.


  METHOD show_message.

*    DESCRIBE TABLE mt_mess LINES DATA(lv_lines).
*
*    IF lv_lines <= 1 OR sy-batch = 'X'.
*
*      LOOP AT mt_mess ASSIGNING FIELD-SYMBOL(<fs_ret2>).
*
*        MESSAGE ID <fs_ret2>-id
*              TYPE 'S'
*            NUMBER <fs_ret2>-number
*              WITH <fs_ret2>-message_v1
*                   <fs_ret2>-message_v2
*                   <fs_ret2>-message_v3
*                   <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.
*
*      ENDLOOP.
*
*    ELSE.

      CALL FUNCTION 'MESSAGES_INITIALIZE'.

      LOOP AT mt_mess ASSIGNING FIELD-SYMBOL(<fs_ret2>).

        IF <fs_ret2>-id IS INITIAL.

          <fs_ret2>-id = 'DS'. "<-classe padrao abap
          <fs_ret2>-number = '016'.
          <fs_ret2>-message_v1 = <fs_ret2>-message.

        ENDIF.

        CALL FUNCTION 'MESSAGE_STORE'
          EXPORTING
            arbgb                  = <fs_ret2>-id
            "EXCEPTION_IF_NOT_ACTIVE  = 'X'
            msgty                  = <fs_ret2>-type
            msgv1                  = <fs_ret2>-message_v1
            msgv2                  = <fs_ret2>-message_v2
            msgv3                  = <fs_ret2>-message_v3
            msgv4                  = <fs_ret2>-message_v4
            txtnr                  = <fs_ret2>-number
            "ZEILE                    = ' '
            "IMPORTING
            "ACT_SEVERITY             =
            "MAX_SEVERITY             =
          EXCEPTIONS
            message_type_not_valid = 1
            not_active             = 2
            OTHERS                 = 3.     "#EC CI_SUBRC

      ENDLOOP.

      CALL FUNCTION 'MESSAGES_STOP'
        EXCEPTIONS
          a_message = 1
          e_message = 2
          i_message = 3
          w_message = 4
          OTHERS    = 5.     "#EC CI_SUBRC

      CALL FUNCTION 'MESSAGES_SHOW'
        EXPORTING
          "CORRECTIONS_OPTION          = ' '
          "CORRECTIONS_FUNC_TEXT       = ' '
          "LINE_FROM                   = ' '
          "LINE_TO                     = ' '
          "OBJECT                      = ' '
          "SEND_IF_ONE                 = ' '
          batch_list_type     = 'B'
          show_linno          = ' '
          show_linno_text     = 'X'
          show_linno_text_len = '3'
          i_use_grid          = ' '
          i_amodal_window     = ' '
          "MSG_SELECT_FUNC             = ' '
          "MSG_SELECT_FUNC_TEXT        = ' '
          "IMPORTING
          "CORRECTIONS_WANTED          =
          "E_EXIT_COMMAND              =
          "MSG_SELECTED                =
        EXCEPTIONS
          inconsistent_range  = 1
          no_messages         = 2
          OTHERS              = 3.     "#EC CI_SUBRC

*    ENDIF.

  ENDMETHOD.


  METHOD zif_cadastro~gravar_registro.

    DATA: i_fator         TYPE zde_nfe_fator,
          cx_erro         TYPE REF TO zcx_nfe_inbound_exception,
          cx_erro_pedi    TYPE REF TO zcx_pedido_compra_exception,
          cx_erro_migo    TYPE REF TO zcx_migo_exception,
          cx_erro_charg   TYPE REF TO zcx_charg_exception,
          cx_erro_miro    TYPE REF TO zcx_miro_exception,
          cx_erro_aviso   TYPE REF TO zcx_delivery,
          lc_cd_lote_item TYPE zde_cd_lote_item,
          lc_validou      TYPE char1.

    i_gravou = abap_false.

    IF me->ms_nfse_001-ck_alterou EQ abap_true.

      IF me->ms_nfse_001-ck_retorno_sem_ajuste = abap_false.
        lc_validou = me->zif_cadastro~validar_registro( ).
      ELSE.
        lc_validou = abap_true.
      ENDIF.

      IF lc_validou EQ abap_true.

        TRY.
            i_gravou = me->nfse_inbound_fatura( ).

            IF i_gravou EQ abap_true.

              me->set_st_fisico( i_st_fisico = me->st_fisico_99 ).
              me->set_ck_fisico( i_check = abap_true ).

              TRY .


                CATCH zcx_nfe_inbound_exception INTO cx_erro.

              ENDTRY.

            ELSE.

            ENDIF.

            EXIT.

          CATCH zcx_miro_exception INTO cx_erro_miro.

        ENDTRY.
      ENDIF.

      IF me->ms_nfse_001-ck_aceite_faturar EQ abap_false AND
         me->ms_nfse_001-ck_aceite_fiscal  EQ abap_false AND
         me->ms_nfse_001-ck_aceite_fisico  EQ abap_false .

      ENDIF.

    ENDIF.

    me->ms_nfse_001-ck_aceite_fiscal          = abap_false.
    me->ms_nfse_001-ck_aceite_fisico          = abap_false.
    me->ms_nfse_001-ck_aceite_faturar         = abap_false.

  ENDMETHOD.


  METHOD zif_cadastro~set_registro.

    DATA: wa_nota TYPE zib_nfe_dist_ter.

    "me->clear_all( ).

    me->fill_all( ).

    IF me->ms_nfse_001 IS NOT INITIAL.

      TRY.
          CALL METHOD me->enqueue_guid.
        CATCH /tcsr/cx_exception .
      ENDTRY.

      "me->nota       = wa_nota.

      "me->refresh( ).

      "me->ck_alterou = abap_false.

    ELSE.

*      SELECT SINGLE * INTO @DATA(wa_zib_nfe_erro)
*        FROM zib_dfe_erro
*       WHERE chave EQ @i_id_registro.
*
*      IF sy-subrc IS INITIAL.
*        MESSAGE s398(00)
*        WITH 'XML recebido com erro!' wa_zib_nfe_erro-ds_erro+000(050) wa_zib_nfe_erro-ds_erro+050(050) wa_zib_nfe_erro-ds_erro+100(150).
*      ELSE.
*        "MESSAGE s103 WITH i_id_registro.
*      ENDIF.
*
*      RAISE EXCEPTION TYPE zcx_cadastro
*        EXPORTING
*          textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) )
*          msgid  = sy-msgid
*          msgno  = sy-msgno
*          msgty  = 'E'
*          msgv1  = sy-msgv1.

    ENDIF.


  ENDMETHOD.


  METHOD zif_cadastro~validar_registro.

    e_validou = 'X'.

  ENDMETHOD.


  METHOD call_shdb_ml81n_accept.
    DATA:
      lt_bdc    TYPE TABLE OF bdcdata,
      xdata(10).
    CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO xdata SEPARATED BY '.'.
    DATA:
      lw_bdc        TYPE bdcdata,
      lw_ctu_params TYPE ctu_params.
*------------------------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SELP'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0340'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ENTE'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'RM11R-LBLNI'.
    lw_bdc-fval     = ms_nfse_001-lblni.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=AKCH'.
    APPEND lw_bdc TO lt_bdc.
*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=ACCP'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'ESSR-XBLNR'.
    lw_bdc-fval     = nf_serie( ).
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'ESSR-BLDAT'.
    lw_bdc-fval     = xdata.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'ESSR-BUDAT'.
    lw_bdc-fval     = xdata.
    APPEND lw_bdc TO lt_bdc.

*-------------------------------
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLMLSR'.
    lw_bdc-dynpro   = '0400'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=SAVE'.
    APPEND lw_bdc TO lt_bdc.
**-------------------------------
    "Popup confirm
    CLEAR: lw_bdc.
    lw_bdc-dynbegin = 'X'.
    lw_bdc-program  = 'SAPLSPO1'.
    lw_bdc-dynpro   = '0300'.
    APPEND lw_bdc TO lt_bdc.

    CLEAR: lw_bdc.
    lw_bdc-fnam     = 'BDC_OKCODE'.
    lw_bdc-fval     = '=YES'.
    APPEND lw_bdc TO lt_bdc.
**-------------------------------
*    CLEAR: lw_bdc.
*    lw_bdc-dynbegin = 'X'.
*    lw_bdc-program  = 'SAPLMLSR'.
*    lw_bdc-dynpro   = '0110'.
*    APPEND lw_bdc TO lt_bdc.
*
*    CLEAR: lw_bdc.
*    lw_bdc-fnam     = 'BDC_OKCODE'.
*    lw_bdc-fval     = '=ENTE'.
*    APPEND lw_bdc TO lt_bdc.
**-------------------------------

    lw_ctu_params-dismode = 'N'.
    lw_ctu_params-updmode = 'S'.

    CALL TRANSACTION 'ML81N'
               USING lt_bdc
             OPTIONS FROM lw_ctu_params
            MESSAGES INTO rt_shdb_messages.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD fill_folha_de_servico.

* INICIO - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    IF mt_nfse_002 IS NOT INITIAL.

      SELECT ebeln
             ebelp
             meins
* INICIO - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
             vrtkz
* FIM - IR225249 - 24/03/2025 - STEFANINI - RBRIBEIRO
        FROM ekpo
        INTO TABLE mt_ekpo_lebre
        FOR ALL ENTRIES IN mt_nfse_002
        WHERE ebeln EQ mt_nfse_002-ebeln
        AND   ebelp EQ mt_nfse_002-ebelp
        AND   lebre EQ 'X'.

      IF sy-subrc IS INITIAL.

        SORT mt_ekpo_lebre BY ebeln ebelp.

        SELECT msehi
               isocode
          FROM t006
          INTO TABLE mt_t006
          FOR ALL ENTRIES IN mt_ekpo_lebre
          WHERE msehi EQ mt_ekpo_lebre-meins.

          IF sy-subrc IS INITIAL.
            SORT mt_t006 BY msehi.
          ENDIF.

          SELECT packno
                 sub_packno
            FROM esll
            INTO TABLE mt_esll
            FOR ALL ENTRIES IN mt_nfse_002
            WHERE packno EQ mt_nfse_002-packno.

          IF mt_esll IS NOT INITIAL.

            SORT mt_esll BY packno.

            SELECT packno
                   extrow
                   menge
              FROM esll
              INTO TABLE mt_esll_sub
              FOR ALL ENTRIES IN mt_esll
              WHERE packno EQ mt_esll-sub_packno.

            IF mt_esll_sub IS NOT INITIAL.
              SORT mt_esll_sub BY packno.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
* FIM - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    ENDMETHOD.


  METHOD fill_item_folha_servico.
* INICIO - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
    READ TABLE mt_ekpo_lebre INTO DATA(ls_ekpo_lebre)
    WITH KEY ebeln = i_st_nfse_002-ebeln
             ebelp = i_st_nfse_002-ebelp
             BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      READ TABLE mt_esll INTO DATA(ls_esll)
      WITH KEY packno = i_st_nfse_002-packno
               BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        READ TABLE mt_esll_sub INTO DATA(ls_esll_sub)
        WITH KEY packno = ls_esll-sub_packno
                 BINARY SEARCH.

        IF sy-subrc IS INITIAL.

          c_st_item-sheet_item = ls_esll_sub-extrow.
          c_st_item-quantity   = ls_esll_sub-menge.

          READ TABLE mt_t006 INTO DATA(ls_t006)
            WITH KEY msehi = ls_ekpo_lebre-meins.
          IF sy-subrc IS INITIAL.
            c_st_item-po_unit = ls_t006-msehi.
            c_st_item-po_unit_iso = ls_t006-isocode.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

* FIM - IR220086 - 17/02/2025 - STEFANINI - RBRIBEIRO
  ENDMETHOD.


  METHOD nfse_inbound_aceite_folha.

    DATA(lt_bdc_mess) = call_shdb_ml81n_accept( ).

    LOOP AT lt_bdc_mess ASSIGNING FIELD-SYMBOL(<fs_mess>).

      IF <fs_mess>-msgtyp = 'E'.
        r_erro = 'X'.
      ENDIF.


      CALL METHOD me->append_message2
        EXPORTING
          i_msgid = <fs_mess>-msgid
          i_msgno = CONV symsgno( <fs_mess>-msgnr )
          i_msgty = <fs_mess>-msgtyp
          i_msgv1 = <fs_mess>-msgv1
          i_msgv2 = <fs_mess>-msgv2
          i_msgv3 = <fs_mess>-msgv3
          i_msgv4 = <fs_mess>-msgv4.

    ENDLOOP.

    IF r_erro = 'X'.
      WAIT UP TO 1 SECONDS.
      log_to_tab( ).
      save( '5' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
