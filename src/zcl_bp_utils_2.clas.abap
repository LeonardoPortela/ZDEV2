class ZCL_BP_UTILS_2 definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IM_TEST type CHAR1 default ABAP_TRUE
      !IM_TCODE type TCODE optional
      !IT_BDCDATA type BDCDATA_TAB optional .
  methods MT_BP_DISPLAY
    importing
      !IM_PARTNER type BU_PARTNER optional
      !IM_LIFNR type LIFNR optional
      !IM_KUNNR type KUNNR
      !IM_VENDOR_VIEW type CHAR1
      !IM_CUSTOMER_VIEW type CHAR1
    exporting
      !EM_ERRO type ABAP_BOOL .
  methods MT_BP_PROCESS_DATA
    changing
      !CT_BDCMSG type TAB_BDCMSGCOLL .
  methods MT_BP_PROCESS_OLD_SHDB
    changing
      value(CT_BDCMSG) type TAB_BDCMSGCOLL optional .
  methods MT_SET_DATA_BY_SHDB
    importing
      !IT_BDCDATA type BDCDATA_TAB .
  methods MT_SET_DATA_DIRECTLY
    importing
      !IS_ADDR1_DATA type ADDR1_DATA optional
      !IS_ADSMTP type ADSMTP optional
      !IS_KNA1 type KNA1 optional
      !IS_KNB1 type KNB1 optional
      !IS_KNBW type KNBW optional
      !IS_KNVI type KNVI optional
      !IS_KNVK type KNVK optional
      !IS_KNVV type KNVV optional
      !IS_LFA1 type LFA1 optional
      !IT_LFB1 type CVIS_LFB1_T optional
      !IT_LFBW type CVIS_LFBW_T optional
      !IS_RF02D type RF02D optional
      !IS_RF02K type RF02K optional
      !IS_SZA1_D0100 type SZA1_D0100 optional
      !IT_ADSMTP type BBPT_ER_ADSMTP optional
      !IT_KNB1 type CVIS_KNB1_T optional
      !IT_KNBW type CVIS_KNBW_T optional
      !IT_KNVI type CVIS_KNVI_T optional
      !IT_KNVV type CVIS_KNVV_T optional
      !IT_LFBK type LFBK_T optional
      !IT_LFM1 type CTE_T_LFM1 optional .
  methods MT_BP_CREATE_CUSTOMER
    importing
      !IM_TEST type ABAP_BOOL default ABAP_TRUE
      !IS_KNA1 type KNA1
      !IS_KNVK type KNVK optional
      !IT_KNB1 type CVIS_KNB1_T optional
      !IT_KNBW type CVIS_KNBW_T optional
      !IT_KNVI type CVIS_KNVI_T
      !IT_KNVV type CVIS_KNVV_T
      !IT_ADSMTP type BBPT_ER_ADSMTP
      !IS_ADDR1_DATA type ADDR1_DATA optional
      !IS_ADDR2_DATA type ADDR2_DATA optional
      !IS_SZA1_D0100 type SZA1_D0100 optional
      !IT_KNBK type KNBK_T optional
      !IS_BUS_JOEL_MAIN type BUS_JOEL_MAIN optional
      !IS_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
    changing
      !ET_RETURN type BAPIRET2_T
      !EM_PARTNER type BU_PARTNER .
  methods MT_BP_CREATE_SUPPLIER
    importing
      !IM_TEST type ABAP_BOOL default ABAP_TRUE
      !IS_LFA1 type LFA1
      !IT_LFBK type LFBK_T optional
      !IT_LFB1 type CVIS_LFB1_T optional
      !IT_LFBW type CVIS_LFBW_T optional
      !IS_RF02K type RF02K optional
      !IS_ADDR1_DATA type ADDR1_DATA optional
      !IS_SZA1_D0100 type SZA1_D0100 optional
      !IT_ADSMTP type BBPT_ER_ADSMTP optional
      !IT_LFM1 type CVIS_LFM1_T optional
      !IS_BUS_JOEL_MAIN type BUS_JOEL_MAIN optional
      !IS_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
      !IS_BP_CENTRAL_DATA type BUS_EI_STRUC_CENTRAL optional
    changing
      !ET_RETURN type BAPIRET2_T
      !EM_PARTNER type BU_PARTNER .
  methods MT_BP_MAINTAIN
    importing
      !IM_TEST type ABAP_BOOL default ABAP_TRUE
      !IT_DATA type CVIS_EI_EXTERN_T
    exporting
      !ET_RETURN type BAPIRET2_T
      !EM_PARTNER type BU_PARTNER .
  methods MT_BP_UPDATE_CUSTOMER
    importing
      !IM_TEST type ABAP_BOOL default ABAP_TRUE
      !IM_BP_TYPE type BU_TYPE optional
      !IS_KNA1 type KNA1
      !IS_KNVK type KNVK optional
      !IT_KNB1 type CVIS_KNB1_T optional
      !IT_KNBW type CVIS_KNBW_T optional
      !IT_KNVI type CVIS_KNVI_T
      !IT_KNVV type CVIS_KNVV_T
      !IT_ADSMTP type BBPT_ER_ADSMTP
      !IS_ADDR1_DATA type ADDR1_DATA optional
      !IS_ADDR2_DATA type ADDR2_DATA optional
      !IS_SZA1_D0100 type SZA1_D0100 optional
      !IT_KNBK type KNBK_T optional
      !IS_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
      !IS_ALTERACAO type ZDE_BP_ALTERACAO optional
    changing
      !ET_RETURN type BAPIRET2_T
      !EM_PARTNER type BU_PARTNER .
  methods MT_BP_UPDATE_SUPPLIER
    importing
      !IM_TEST type ABAP_BOOL default ABAP_TRUE
      !IM_BP_TYPE type BU_TYPE optional
      !IS_LFA1 type LFA1
      !IT_LFBK type LFBK_T optional
      !IT_LFB1 type CVIS_LFB1_T optional
      !IT_LFBW type CVIS_LFBW_T optional
      !IS_RF02K type RF02K optional
      !IS_ADDR1_DATA type ADDR1_DATA optional
      !IS_SZA1_D0100 type SZA1_D0100 optional
      !IT_ADSMTP type BBPT_ER_ADSMTP optional
      !IT_LFM1 type CVIS_LFM1_T optional
      !IM_BU_GROUP type BU_GROUP optional
      !IS_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
      !IS_BP_CENTRAL_DATA type BUS_EI_STRUC_CENTRAL optional
      !IS_ALTERACAO type ZDE_BP_ALTERACAO optional
    exporting
      !ET_RETURN type BAPIRET2_T
      !EM_PARTNER type BU_PARTNER .
  methods MT_FILL_PART_CENT_ADDRESS_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_ADDR1_DATA type ADDR1_DATA
      !IS_SZA1_D0100 type SZA1_D0100
      !IT_ADSMTP type BBPT_ER_ADSMTP
      !IS_ALTERACAO type ZDE_BP_ALTERACAO optional
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_ADDRESS_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_ADDR1_DATA type ADDR1_DATA
      !IM_PARTNER type BU_PARTNER
      !IS_SZA1_D0100 type SZA1_D0100
      !IT_ADSMTP type BBPT_ER_ADSMTP
      !IS_ALTERACAO type ZDE_BP_ALTERACAO optional
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_COMMON_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_BU_TYPE type BU_TYPE
      !IS_KNA1 type KNA1
      !IS_ADDR1_DATA type ADDR1_DATA
      !IS_BUS_JOEL_MAIN type BUS_JOEL_MAIN optional
      !IM_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_COMMON_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_BU_TYPE type BU_TYPE
      !IS_LFA1 type LFA1
      !IS_ADDR1_DATA type ADDR1_DATA
      !IM_BU_GROUP type BU_GROUP optional
      !IS_BUS_JOEL_MAIN type BUS_JOEL_MAIN optional
      !IS_PERSONAL_DATA type BUS_EI_STRUC_CENTRAL_PERSON optional
      !IS_PERSONAL_DATA2 type FSBP_EI_STRUC_PERSON optional
      !IS_BP_CENTRAL_DATA type BUS_EI_STRUC_CENTRAL optional
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_ROLES_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_ROLE type BU_ROLE
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_TAX_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_KNA1 type KNA1
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_TAX_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_LFA1 type LFA1
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_ADDRESS
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_KNA1 type KNA1
      !IS_ADDR1_DATA type ADDR1_DATA optional
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_COMPANY
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IT_KNB1 type CVIS_KNB1_T
      !IT_KNBW type CVIS_KNBW_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_CONTACT
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_KNA1 type KNA1
      !IS_SZA1_D0100 type SZA1_D0100
      !IS_KNVK type KNVK
      !IT_ADSMTP type BBPT_ER_ADSMTP
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_DATA
    importing
      !IS_KNA1 type KNA1
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_HEADER
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_CREATE type ABAP_BOOL
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_SALES
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IT_KNVV type CVIS_KNVV_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_TAX
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IT_KNVI type CVIS_KNVI_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_HEADER_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_PARTNER type BU_PARTNER
      !IM_GUID type BU_PARTNER_GUID_BAPI
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_SUP_ADDRESS
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_LFA1 type LFA1
      !IS_ADDR1_DATA type ADDR1_DATA
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_SUP_BANKDETAIL
    importing
      !IM_OBJECT_TASK type CVI_EI_BANK_TASK
      !IT_LFBK type LFBK_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_SUP_CONTACT
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_LFA1 type LFA1
      !IS_SZA1_D0100 type SZA1_D0100
      !IT_ADSMTP type BBPT_ER_ADSMTP
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_SUP_DATA
    importing
      !IS_RF02K type RF02K
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_SUP_HEADER
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_CREATE type ABAP_BOOL
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_GET_BP_FROM_CUSTOMER
    importing
      !IM_KUNNR type KUNNR
    changing
      !CM_PARTNER type BU_PARTNER .
  methods MT_GET_BP_FROM_VENDOR
    importing
      !IM_LIFNR type LIFNR
    changing
      !CM_PARTNER type BU_PARTNER .
  methods MT_GET_BP_GUID
    importing
      !IM_PARTNER type BU_PARTNER
    changing
      !CM_PARTNER_GUID type BU_PARTNER_GUID .
  methods MT_MOVE_DATA
    importing
      !IS_FROM type ANY
    changing
      !CS_DATA type ANY
      !CS_DATAX type ANY .
  methods MT_RETURN_MESSAGE_GENERIC
    importing
      !IM_TYPE type ANY
      !IM_MSG type ANY
    changing
      !CT_RETURN type BAPIRET2_T .
  methods MT_SHDB_CLEAR_BDC
    changing
      !ET_RESULT_TAB type BDCDATA_TAB .
  methods MT_SHDB_CONVERT_RETURN_BDCMSG
    importing
      !IM_TCODE type SYST_TCODE
      !IT_RETURN type BAPIRET2_T
    changing
      !CT_BDCMSG type TAB_BDCMSGCOLL .
  methods MT_SHDB_MOVE_VALUE
    importing
      !IM_FROM type STRING
    changing
      !CM_TO type ANY .
  methods MT_SHDB_PARSE_BDC
    importing
      !IT_BDC type BDCDATA_TAB
    changing
      !CS_ADDR1_DATA type ADDR1_DATA
      !CS_ADSMTP type ADSMTP
      !CS_KNA1 type KNA1
      !CS_LFA1 type LFA1
      !CT_LFBK type LFBK_T
      !CT_LFB1 type CVIS_LFB1_T
      !CT_LFBW type CVIS_LFBW_T
      !CS_RF02K type RF02K
      !CS_KNB1 type KNB1
      !CS_KNBW type KNBW
      !CS_KNVI type KNVI
      !CS_KNVK type KNVK
      !CS_KNVV type KNVV
      !CS_RF02D type RF02D
      !CS_SZA1_D0100 type SZA1_D0100
      !CT_ADSMTP type BBPT_ER_ADSMTP
      !CT_KNB1 type CVIS_KNB1_T
      !CT_KNBW type CVIS_KNBW_T
      !CT_KNVI type CVIS_KNVI_T
      !CT_KNVV type CVIS_KNVV_T
      !CT_KNBK type KNBK_T
      !CT_LFM1 type CVIS_LFM1_T
      !CS_BUS_JOEL_MAIN type BUS_JOEL_MAIN optional .
  methods MT_FILL_PART_SUP_COMPANY
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IT_LFB1 type CVIS_LFB1_T
      !IT_LFBW type CVIS_LFBW_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CUST_BANKDETAIL
    importing
      !IM_OBJECT_TASK type CVI_EI_BANK_TASK
      !IT_KNBK type KNBK_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_DATAX
    importing
      value(IS_DATA) type ANY optional
    changing
      value(CS_DATAX) type ANY optional .
  methods MT_FILL_PART_SUP_PURCH_ORG
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IT_LFM1 type CVIS_LFM1_T
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
protected section.
private section.

  types:
************************************************************************
*   TIPOS          (Y_...)                                             *
************************************************************************
    BEGIN OF y_depara,
      tab_de   TYPE char50,
      tab_para TYPE char50,
    END OF y_depara .
  types:
    BEGIN OF y_header,
      num_devs  TYPE numc5,
      num_batch TYPE numc5,
      num_bapi  TYPE numc5,
      num_tab   TYPE numc5,
      num_trans TYPE numc5,
    END OF y_header .
  types:
    BEGIN OF y_data,
      mandt    TYPE sy-mandt,
      pgmid    TYPE tadir-pgmid,
      object   TYPE tadir-object,
      progname TYPE reposrc-progname,
      text     TYPE tstct-ttext,
    END OF y_data .
  types:
    BEGIN OF y_object,
      mandt     TYPE sy-mandt,
      progname  TYPE reposrc-progname,
      object    TYPE trobj_name,
      txt_tpobj TYPE ddtext,
      mensagem  TYPE bapi_msg,
    END OF y_object .
  types:
    BEGIN OF y_transact,
      tcode TYPE tstct-tcode,
      ttext TYPE tstct-ttext,
    END OF y_transact .
  types:
    BEGIN OF y_lfa1,
      lifnr TYPE lfa1-lifnr,
      ktokk TYPE lfa1-ktokk,
      stcd1 TYPE lfa1-stcd1,
      stcd2 TYPE lfa1-stcd2,
    END OF y_lfa1 .
  types:
    BEGIN OF y_t077k,
      ktokk TYPE t077k-ktokk,
      numkr TYPE t077k-numkr,
      ktokd TYPE t077k-ktokd,
    END OF y_t077k .
  types:
    BEGIN OF y_t077y,
      ktokk TYPE t077y-ktokk,
      txt30 TYPE t077y-txt30,
    END OF y_t077y .
  types:
    BEGIN OF y_nriv,
      object     TYPE nriv-object,
      subobject  TYPE nriv-subobject,
      nrrangenr  TYPE nriv-nrrangenr,
      fromnumber TYPE nriv-fromnumber,
      tonumber   TYPE nriv-tonumber,
      externind  TYPE nriv-externind,
    END OF y_nriv .
  types:
    BEGIN OF y_kna1,
      kunnr TYPE kna1-kunnr,
      ktokd TYPE kna1-ktokd,
      stcd1 TYPE kna1-stcd1,
      stcd2 TYPE kna1-stcd2,
      stkzn TYPE kna1-stkzn,
    END OF y_kna1 .
  types:
    BEGIN OF y_t077d,
      ktokd TYPE t077d-ktokd,
      numkr TYPE t077d-numkr,
    END OF y_t077d .
  types:
    BEGIN OF y_t077x,
      ktokd TYPE t077x-ktokd,
      txt30 TYPE t077x-txt30,
    END OF y_t077x .
  types:
    BEGIN OF y_cabecalho,
      fildname TYPE char20,
    END OF y_cabecalho .
  types:
    BEGIN OF y_sapversion,
      component  TYPE dlvunit,
      newrelease TYPE saprelease,
    END OF y_sapversion .
  types:
    BEGIN OF y_function,
      funcname TYPE tfdir-funcname,
    END OF y_function .
  types:
    BEGIN OF ty_customer_object,
      object   TYPE tadir-object,
      obj_name TYPE tadir-obj_name,
      devclass TYPE tadir-devclass,
      genflag  TYPE tadir-genflag,
      dlvunit  TYPE tdevc-dlvunit,
    END OF ty_customer_object .

  " Variaveis
  data GV_BP_TYPE type BU_TYPE .
  data GV_PARTNER type BU_PARTNER .
  data GV_TEST type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data GV_TCODE type TCODE .
  " Tabelas
  data GT_ADSMTP type BBPT_ER_ADSMTP .
  data GT_BDC type BDCDATA_TAB .
  data GT_KNB1 type CVIS_KNB1_T .
  data GT_KNBW type CVIS_KNBW_T .
  data GT_KNVI type CVIS_KNVI_T .
  data GT_KNBK type KNBK_T .
  data GT_KNVV type CVIS_KNVV_T .
  data GT_LFBK type LFBK_T .
  data GT_RETURN type BAPIRET2_T .
  data GT_BDCDATA type BDCDATA_TAB .
  data GT_BDCMSG type TAB_BDCMSGCOLL .
  " Estruturas
  data GS_ADDR1_DATA type ADDR1_DATA .
  data GS_ADSMTP type ADSMTP .
  data GS_BUT000 type BUT000 .
  data GS_KNA1 type KNA1 .
  data GS_KNB1 type KNB1 .
  data GS_KNBW type KNBW .
  data GS_KNVI type KNVI .
  data GS_KNVK type KNVK .
  data GS_KNVV type KNVV .
  data GS_LFA1 type LFA1 .
  data GT_LFB1 type CVIS_LFB1_T .
  data GT_LFBW type CVIS_LFBW_T .
  data GS_RF02D type RF02D .
  data GS_RF02K type RF02K .
  data GS_SZA1_D0100 type SZA1_D0100 .
  data GS_ADDR2_DATA type ADDR2_DATA .
  data GT_LFM1 type CVIS_LFM1_T .
  data GS_BUS_JOEL_MAIN type BUS_JOEL_MAIN .
  data GV_BU_GROUP type BU_GROUP .
ENDCLASS.



CLASS ZCL_BP_UTILS_2 IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    gv_tcode = im_tcode.
    gv_test  = im_test.

    IF it_bdcdata IS NOT INITIAL.
      gt_bdcdata = it_bdcdata.
    ENDIF.

  ENDMETHOD.


  METHOD mt_bp_create_customer.

    CONSTANTS c_customer_role_0 TYPE bu_role VALUE 'FLCU00'.
    CONSTANTS c_customer_role_1 TYPE bu_role VALUE 'FLCU01'.
    CONSTANTS c_vendor_role_0   TYPE bu_role VALUE 'FLVN00'.
    CONSTANTS c_vendor_role_1   TYPE bu_role VALUE 'FLVN01'.

    CONSTANTS c_valid_to        TYPE datum   VALUE '99991231'.

    DATA: lv_msg         TYPE bapi_msg,
          lv_bu_type     TYPE bu_type,
          lv_object_task TYPE bus_ei_object_task. "I = Include (New); U = Update (Change)

    DATA: lt_bp_data TYPE cvis_ei_extern_t,
          lt_rules   TYPE TABLE OF bapibus1006_bproles.

*----------------------------------------------------------------------*
*  VALIDAÇÕES INICIAIS
*----------------------------------------------------------------------*
    "Campo obrigatório: STCD1
    IF is_kna1-stcd1 IS INITIAL AND is_kna1-stcd2 IS INITIAL.
      lv_msg = |Campo obrigatório: CPF ou CNPJ|.
      me->mt_return_message_generic(
        EXPORTING
          im_type   = 'E'
          im_msg    = lv_msg
        CHANGING
          ct_return = et_return                  " Tabela de retorno
      ).
      RETURN.
    ENDIF.

*** Inicio - Rubenilson Pereira - 06.12.24 - US159528
    IF is_kna1-ktokd EQ 'ZPRF' OR
       is_kna1-ktokd EQ 'ZCPF' OR
       is_kna1-ktokd EQ 'ZPRJ' OR
       is_kna1-ktokd EQ 'ZCPJ'.

      "Verifica se o CPF/CNPJ informado já existe.
      SELECT SINGLE kunnr FROM kna1
        INTO @DATA(lv_kunnr)
        WHERE stcd1 = @is_kna1-stcd1
          AND stcd2 = @is_kna1-stcd2
          AND stcd3 = @is_kna1-stcd3.

*** Inicio - Rubenilson Pereira - 21.01.25 - US163841
         "Verifica se o CPF/CNPJ informado já existe.
      SELECT SINGLE lifnr FROM lfa1
        INTO @DATA(lv_lifnr)
        WHERE stcd1 = @is_kna1-stcd1
          AND stcd2 = @is_kna1-stcd2
          AND stcd3 = @is_kna1-stcd3.
*** Fim - Rubenilson Pereira - 21.01.25 - US163841

    ELSE.

      "Verifica se o CPF/CNPJ informado já existe.
      SELECT SINGLE kunnr FROM kna1
        INTO lv_kunnr
        WHERE stcd1 = is_kna1-stcd1
          AND stcd2 = is_kna1-stcd2.

    ENDIF.
*** Fim - Rubenilson Pereira - 06.12.24 - US159528

    IF sy-subrc = 0.

*** Inicio - Rubenilson Pereira - 21.01.25 - US163841
*      lv_msg = COND #( WHEN is_kna1-stcd1 IS NOT INITIAL THEN is_kna1-stcd1 ELSE is_kna1-stcd2 ).
*      lv_msg = |Documento { lv_msg } já pertencente ao cliente { lv_kunnr }|.
      lv_msg = |Esse cadastro já existe' { lv_kunnr && '/' && lv_lifnr } 'para fazer atualizações em um cadastro existente, abrir uma CCF e selecionar alteração'|.
*** Fim - Rubenilson Pereira - 21.01.25 - US163841

      me->mt_return_message_generic(
        EXPORTING
          im_type   = 'E'
          im_msg    = lv_msg
        CHANGING
          ct_return = et_return                  " Tabela de retorno
      ).
      RETURN.

    ENDIF.

*----------------------------------------------------------------------*
*  MAPEIA DADOS PARA CLASSE DE CRIAÇÃO DE BP
*----------------------------------------------------------------------*
    APPEND INITIAL LINE TO lt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>).

    "Define se é uma Organização (2) ou uma Pessoa (1)
    lv_bu_type = COND #( WHEN is_kna1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).
    lv_object_task = 'I'.

    "PARTNER
    "--------------------------------------------------------------------
    "PARTNER - Header
    me->mt_fill_part_header_sup(
      EXPORTING
        im_object_task = lv_object_task       " Interface externa: código de modificação objeto
        im_partner     = space                " Nº parceiro de negócios
        im_guid        = space                " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Common
    me->mt_fill_part_cent_common_cust(
      EXPORTING
        im_object_task = lv_object_task
        im_bu_type     = lv_bu_type
        is_kna1        = is_kna1
        is_addr1_data  = is_addr1_data
        is_BUS_JOEL_MAIN  = is_BUS_JOEL_MAIN  " ALRS estrutura JOEL
        im_personal_data  = is_personal_data
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    "PARTNER - Central - Address
    me->mt_fill_part_cent_address_cust(
      EXPORTING
        im_object_task = lv_object_task       " Objects data
        is_addr1_data  = is_addr1_data        " Estrutura de transferência para endereços
        is_sza1_d0100  = is_sza1_d0100        " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp            " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
    ).

    "PARTNER - Central - Tax
    me->mt_fill_part_cent_tax_cust(
      EXPORTING
        im_object_task = lv_object_task
        is_kna1        = is_kna1
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    IF is_kna1-ktokd = 'ZFNF' OR
       is_kna1-ktokd = 'ZFUN' OR
       is_kna1-ktokd = 'ZFEX' OR
       is_kna1-ktokd = 'ZIMP' OR
       is_kna1-ktokd = 'ZPEN' OR
       is_kna1-ktokd = 'ZFNJ' OR
       is_kna1-ktokd = 'ZPRF' OR
       is_kna1-ktokd = 'ZPRJ' OR
       is_kna1-ktokd = 'ZFFJ' OR
       is_kna1-ktokd = 'ZFFF'.

      " PARTNER - Central - Roles
      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_vendor_role_0    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).

      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_vendor_role_1    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).
    ELSE.

      " PARTNER - Central - Roles
      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_customer_role_0    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).

      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_customer_role_1    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
).
*** Inicio - Rubenilson Pereira - 06.12.24 - US159528
      IF is_kna1-ktokd = 'ZCPF' OR
         is_kna1-ktokd = 'ZCPJ'.

        " PARTNER - Central - Roles
        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_vendor_role_0    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_vendor_role_1    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

      ENDIF.
    ENDIF.
*** Fim - Rubenilson Pereira - 06.12.24 - US159528

    "SUPPLIER
    "--------------------------------------------------------------------
    "Customer - Header
    me->mt_fill_part_cust_header(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        im_create      = abap_true
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Data
    me->mt_fill_part_cust_data(
      EXPORTING
        is_kna1   = is_kna1               " Atual.dados mestre fornecedor: campos de tela e operativos
      CHANGING
        cs_bp_data = <fs_bp_data>           " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Address
    me->mt_fill_part_cust_address(
      EXPORTING
        im_object_task =  lv_object_task                " Interface externa: código de modificação objeto
        is_kna1        =  is_kna1                " Mestre de fornecedores (parte geral)
        is_addr1_data  =  is_addr1_data                " Estrutura de transferência para endereços
      CHANGING
        cs_bp_data     =  <fs_bp_data>                " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Contact
    me->mt_fill_part_cust_contact(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        is_kna1        = is_kna1
        is_sza1_d0100  = is_sza1_d0100      " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_knvk        = is_knvk
        it_adsmtp      = it_adsmtp          " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_tax(
      EXPORTING
        im_object_task = lv_object_task
        it_knvi        = it_knvi
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_company(
      EXPORTING
        im_object_task = lv_object_task
        it_knb1        = it_knb1
        it_knbw        = it_knbw
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_sales(
      EXPORTING
        im_object_task = lv_object_task
        it_knvv        = it_knvv
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    me->mt_fill_part_cust_bankdetail(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação dados bancários
        it_knbk        = it_knbk            " Interface externa: dados detalhes bancários
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    UNASSIGN <fs_bp_data>.

*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
    me->mt_bp_maintain(
      EXPORTING
        im_test    = im_test
        it_data    = lt_bp_data     " Entrada da integração de clientes/fornecedores
      IMPORTING
        et_return  = et_return      " Tabela de retorno
        em_partner = em_partner     " Nº parceiro de negócios
    ).

  ENDMETHOD.


  METHOD mt_bp_create_supplier.

    CONSTANTS c_customer_role_0 TYPE bu_role VALUE 'FLCU00'.
    CONSTANTS c_customer_role_1 TYPE bu_role VALUE 'FLCU01'.
    CONSTANTS c_vendor_role_0   TYPE bu_role VALUE 'FLVN00'.
    CONSTANTS c_vendor_role_1   TYPE bu_role VALUE 'FLVN01'.

    CONSTANTS c_valid_to        TYPE datum   VALUE '99991231'.

    DATA: lv_msg         TYPE bapi_msg,
          lv_bu_type     TYPE bu_type,
          lv_object_task TYPE bus_ei_object_task. "I = Include (New); U = Update (Change)

    DATA: lt_bp_data TYPE cvis_ei_extern_t,
          lt_rules   TYPE TABLE OF bapibus1006_bproles.

*----------------------------------------------------------------------*
*  VALIDAÇÕES INICIAIS
*----------------------------------------------------------------------*
    "Campo obrigatório: STCD1
    IF is_lfa1-stcd1 IS INITIAL AND is_lfa1-stcd2 IS INITIAL.
      lv_msg = |Campo obrigatório: CPF ou CNPJ|.
      me->mt_return_message_generic(
        EXPORTING
          im_type   = 'E'
          im_msg    = lv_msg
        CHANGING
          ct_return = et_return                  " Tabela de retorno
      ).
      RETURN.
    ENDIF.

*** Inicio - Rubenilson Pereira - 06.12.24 - US159528
    IF is_lfa1-ktokk EQ 'ZPRF' OR
       is_lfa1-ktokk EQ 'ZCPF' OR
       is_lfa1-ktokk EQ 'ZPRJ' OR
       is_lfa1-ktokk EQ 'ZCPJ'.

      SELECT SINGLE lifnr
        FROM lfa1
        INTO @DATA(lv_lifnr)
        WHERE stcd1 = @is_lfa1-stcd1
          AND stcd2 = @is_lfa1-stcd2
          AND stcd3 = @is_lfa1-stcd3.

*** Inicio - Rubenilson Pereira - 21.01.25 - US163841
      SELECT SINGLE kunnr
        FROM kna1
        INTO @DATA(lv_kunnr)
        WHERE stcd1 = @is_lfa1-stcd1
          AND stcd2 = @is_lfa1-stcd2
          AND stcd3 = @is_lfa1-stcd3.
*** Fim - Rubenilson Pereira - 21.01.25 - US163841

    ELSE.
*** Fim - Rubenilson Pereira - 06.12.24 - US159528

      "Verifica se o CPF/CNPJ informado já existe.
      SELECT SINGLE lifnr
        FROM lfa1
        INTO lv_lifnr
        WHERE stcd1 = is_lfa1-stcd1
          AND stcd2 = is_lfa1-stcd2.

    ENDIF.

    IF sy-subrc = 0.

      IF is_lfa1-ktokk EQ 'ZFUN'.

        SELECT *
          FROM lfa1
          INTO TABLE @DATA(lt_lfa1)
          WHERE stcd1 = @is_lfa1-stcd1
            AND stcd2 = @is_lfa1-stcd2.
        IF sy-subrc IS INITIAL.

          DELETE lt_lfa1 WHERE ktokk = 'ZMOT'.

          IF lt_lfa1 IS NOT INITIAL.
            lv_msg = COND #( WHEN is_lfa1-stcd1 IS NOT INITIAL THEN is_lfa1-stcd1 ELSE is_lfa1-stcd2 ).
            lv_msg = |Documento { lv_msg } já pertencente ao cliente { lv_lifnr }|.

*** Inicio - Rubenilson Pereira - 21.01.25 - US163841
*      lv_msg = COND #( WHEN is_lfa1-stcd1 IS NOT INITIAL THEN is_lfa1-stcd1 ELSE is_lfa1-stcd2 ).
*      lv_msg = |Documento { lv_msg } já pertencente ao cliente { lv_lifnr }|.
            lv_msg = |Esse cadastro já existe' { lv_kunnr && '/' && lv_lifnr } 'para fazer atualizações em um cadastro existente, abrir uma CCF e selecionar alteração'|.
*** Fim - Rubenilson Pereira - 21.01.25 - US163841
            me->mt_return_message_generic(
              EXPORTING
                im_type   = 'E'
                im_msg    = lv_msg
              CHANGING
                ct_return = et_return                  " Tabela de retorno
            ).
            RETURN.
          ENDIF.
        ENDIF.

      ELSE.

        lv_msg = COND #( WHEN is_lfa1-stcd1 IS NOT INITIAL THEN is_lfa1-stcd1 ELSE is_lfa1-stcd2 ).
        lv_msg = |Documento { lv_msg } já pertencente ao cliente { lv_lifnr }|.
        me->mt_return_message_generic(
          EXPORTING
            im_type   = 'E'
            im_msg    = lv_msg
          CHANGING
            ct_return = et_return                  " Tabela de retorno
        ).
        RETURN.


      ENDIF.
    ENDIF.

*----------------------------------------------------------------------*
*  MAPEIA DADOS PARA CLASSE DE CRIAÇÃO DE BP
*----------------------------------------------------------------------*
    APPEND INITIAL LINE TO lt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>).

    "Define se é uma Organização (2) ou uma Pessoa (1)
    lv_bu_type = COND #( WHEN is_lfa1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).

    lv_object_task = 'I'.

    "PARTNER
    "--------------------------------------------------------------------
    "PARTNER - Header
    me->mt_fill_part_header_sup(
      EXPORTING
        im_object_task = lv_object_task       " Interface externa: código de modificação objeto
        im_partner     = space                " Nº parceiro de negócios
        im_guid        = space                " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Common
    me->mt_fill_part_cent_common_sup(
      EXPORTING
        im_object_task = lv_object_task       " Interface externa: código de modificação objeto
        im_bu_type     = lv_bu_type           " Categoria do parceiro de negócios
        is_lfa1        = is_lfa1              " Mestre de fornecedores (parte geral)
        is_addr1_data  = is_addr1_data        " Estrutura de transferência para endereços
        is_BUS_JOEL_MAIN  = is_BUS_JOEL_MAIN  " ALRS estrutura JOEL
        is_personal_data  = is_personal_data
        is_bp_central_data = is_bp_central_data " Rubenilson - 01.10.24 #153421
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Address
    me->mt_fill_part_cent_address_sup(
      EXPORTING
        im_object_task = lv_object_task       " Objects data
        is_addr1_data  = is_addr1_data        " Estrutura de transferência para endereços
        im_partner     = space                " Nº parceiro de negócios
        is_sza1_d0100  = is_sza1_d0100        " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp            " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
    ).

    "PARTNER - Central - Tax
    me->mt_fill_part_cent_tax_sup(
      EXPORTING
        im_object_task = lv_object_task       " Table Name
        is_lfa1        = is_lfa1              " Join RFC
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    IF is_lfa1-ktokk = 'ZFNF' OR
       is_lfa1-ktokk = 'ZFUN' OR
       is_lfa1-ktokk = 'ZFEX' OR
       is_lfa1-ktokk = 'ZIMP' OR
       is_lfa1-ktokk = 'ZPEN' OR
       is_lfa1-ktokk = 'ZFNJ' OR
       is_lfa1-ktokk = 'ZPRF' OR
       is_lfa1-ktokk = 'ZPRJ' OR
       is_lfa1-ktokk = 'ZFFJ' OR
       is_lfa1-ktokk = 'ZFFF'..


      " PARTNER - Central - Roles
      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_vendor_role_0    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).

      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_vendor_role_1    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).

*** Inicio - Rubenilson Pereira - 06.12.24 - US159528
      IF is_lfa1-ktokk = 'ZPRF' OR
       is_lfa1-ktokk = 'ZPRJ'.

        " PARTNER - Central - Roles
        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_customer_role_0    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_customer_role_1    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
  ).
      ENDIF.
*** Fim - Rubenilson Pereira - 06.12.24 - US159528

    ELSE.

      " PARTNER - Central - Roles
      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_customer_role_0    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
      ).

      me->mt_fill_part_cent_roles_sup(
        EXPORTING
          im_object_task = lv_object_task       " Interface externa: código de modificação objeto
          im_role        = c_customer_role_1    " Função de parceiro de negócios
        CHANGING
          cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
).

*** Inicio - Rubenilson Pereira - 06.12.24 - US159528
      IF is_lfa1-ktokk = 'ZCPF' OR
         is_lfa1-ktokk = 'ZCPJ'.

        " PARTNER - Central - Roles
        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_vendor_role_0    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task       " Interface externa: código de modificação objeto
            im_role        = c_vendor_role_1    " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).
      ENDIF.
*** Fim - Rubenilson Pereira - 06.12.24 - US159528

    ENDIF.

    "SUPPLIER
    "--------------------------------------------------------------------
    "Supplier - Header
    me->mt_fill_part_sup_header(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        im_create      = abap_true
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Data
    me->mt_fill_part_sup_data(
      EXPORTING
        is_rf02k   = is_rf02k               " Atual.dados mestre fornecedor: campos de tela e operativos
      CHANGING
        cs_bp_data = <fs_bp_data>           " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Address
    me->mt_fill_part_sup_address(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        is_lfa1        = is_lfa1            " Mestre de fornecedores (parte geral)
        is_addr1_data  = is_addr1_data      " Estrutura de transferência para endereços
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Contact
    me->mt_fill_part_sup_contact(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        is_lfa1        = is_lfa1            " Mestre de fornecedores (parte geral)
        is_sza1_d0100  = is_sza1_d0100      " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp          " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_bankdetail(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação dados bancários
        it_lfbk        = it_lfbk            " Interface externa: dados detalhes bancários
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_company(
      EXPORTING
        im_object_task = lv_object_task
        it_lfb1        = it_lfb1
        it_lfbw        = it_lfbw
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_purch_org(
      EXPORTING
        im_object_task = lv_object_task
        it_lfm1        = it_lfm1
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    UNASSIGN <fs_bp_data>.

*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
    me->mt_bp_maintain(
      EXPORTING
        im_test    = im_test
        it_data    = lt_bp_data     " Entrada da integração de clientes/fornecedores
      IMPORTING
        et_return  = et_return      " Tabela de retorno
        em_partner = em_partner     " Nº parceiro de negócios
    ).

  ENDMETHOD.


  METHOD MT_BP_DISPLAY.

    DATA abap_true       TYPE char1 VALUE 'X'.
    DATA abap_false      TYPE char1 VALUE ' '.
    DATA ls_role         TYPE bus_roles.
    DATA lo_error        TYPE REF TO cx_root.
    DATA lv_partner      TYPE bu_partner.

    DATA(lo_request) = NEW cl_bupa_navigation_request( ).
    DATA(lo_options) = NEW cl_bupa_dialog_joel_options( ).

    IF im_partner IS NOT INITIAL.
      lv_partner = im_partner.
    ELSE.
      IF im_lifnr IS NOT INITIAL.
        me->mt_get_bp_from_vendor(
          EXPORTING
            im_lifnr   = im_lifnr       " Nº conta do fornecedor
          CHANGING
            cm_partner = lv_partner     " Nº parceiro de negócios
        ).

      ELSEIF im_kunnr IS NOT INITIAL.
        me->mt_get_bp_from_customer(
          EXPORTING
            im_kunnr   = im_kunnr       " Nº conta do fornecedor
          CHANGING
            cm_partner = lv_partner     " Nº parceiro de negócios
        ).
      ENDIF.
    ENDIF.

    IF lv_partner IS INITIAL.
      em_erro = abap_true.
    ENDIF.

    "Seta o nr do parceiro a ser exibido
    lo_request->set_partner_number( lv_partner ).

    "Apenas exibição
    lo_request->set_bupa_activity( lo_request->gc_activity_display ). " 01 - Create, 02 - Change, 03 - Display

    "Desabilita navegação para outros BPs
    lo_options->set_navigation_disabled( abap_true ).

    "Desabilita criação de outro BP
    lo_options->set_bupr_create_not_allowed( abap_true ).

    "Desabilita navegação para outra atividade
    lo_options->set_activity_switching_off( abap_true ).

    "Se quiser levar a alguma visão específica
    "--------------------------------------------------------------------
    IF im_vendor_view   = abap_true AND
       im_customer_view = abap_false.

      ls_role-role = 'FLVN00'.                        "--> Varia de cliente para cliente!
      lo_request->set_bupa_partner_role( ls_role ).
      lo_request->set_bupa_sub_header_id( 'FLVN01' ). "--> Varia de cliente para cliente!

      "Desabilita navegação para outra atividade
      lo_options->set_activity_switching_off( abap_true ).

    ELSEIF im_vendor_view   = abap_false AND
           im_customer_view = abap_true.

      ls_role-role = 'FLCU00'.                        "--> Varia de cliente para cliente!
      lo_request->set_bupa_partner_role( ls_role ).
      lo_request->set_bupa_sub_header_id( 'FLCU01' ). "--> Varia de cliente para cliente!

      "Desabilita navegação para outra atividade
      lo_options->set_activity_switching_off( abap_true ).

    ENDIF.

    "Chama a tela do BP
    "--------------------------------------------------------------------
    TRY.
        cl_bupa_dialog_joel=>start_with_navigation( iv_request = lo_request
                                                    iv_options = lo_options ).
      CATCH cx_root INTO lo_error.
        em_erro = abap_true.
    ENDTRY.
  ENDMETHOD.


  METHOD MT_BP_MAINTAIN.

    DATA lt_return_map TYPE mdg_bs_bp_msgmap_t.
    DATA lt_return     TYPE bapiretm.

    CLEAR et_return.

    "Valida antes de efetivar alteração.
    DATA(ls_data) = it_data[ 1 ].
    cl_md_bp_maintain=>validate_single(
      EXPORTING
        i_data        = ls_data
      IMPORTING
        et_return_map = lt_return_map ).

    IF lt_return_map[] IS NOT INITIAL.
      LOOP AT lt_return_map ASSIGNING FIELD-SYMBOL(<fs_l_return_map>).
        APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<fs_return>).
        MOVE-CORRESPONDING <fs_l_return_map> TO <fs_return>.
        REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
        UNASSIGN <fs_return>.
      ENDLOOP.
      RETURN.
    ENDIF.

    "IMPORTANTE!
    "Se por algum motivo algum dado não está sendo preenchido mesmo sendo passado
    "para a classe, e nenhum erro está retornando, ver transação MDS_PPO2.
    "Foi nessa transação que eu vi que os dados da KNB1 não estavam sendo gravados
    "porque o campo KNA1-CFOPC não estava sendo informado.
    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data     = it_data
        i_test_run = im_test
      IMPORTING
        e_return   = lt_return.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_l_return>).
      LOOP AT <fs_l_return>-object_msg ASSIGNING FIELD-SYMBOL(<fs_object_msg>).
        APPEND INITIAL LINE TO et_return ASSIGNING <fs_return>.
        MOVE-CORRESPONDING <fs_object_msg> TO <fs_return>.
        REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
        UNASSIGN <fs_return>.
      ENDLOOP.
    ENDLOOP.

    IF im_test = abap_false.
      IF line_exists( et_return[ type = 'E' ] ).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*        WAIT UP TO 2 SECONDS.

        SELECT SINGLE partner
          FROM but000
          INTO @DATA(lv_partner)
            WHERE partner_guid = @ls_data-partner-header-object_instance-bpartnerguid.

        IF sy-subrc = 0.
          em_partner = lv_partner.
          CLEAR et_return.
          DATA(lv_msg) = |Parceiro { lv_partner }: Cliente criado com sucesso.|.
          me->mt_return_message_generic(
            EXPORTING
              im_type   = 'S'
              im_msg    = lv_msg
            CHANGING
              ct_return = et_return                 " Tabela de retorno
          ).
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD MT_BP_PROCESS_DATA.

    DATA:
      lv_kunnr TYPE kna1-kunnr .

    IF gs_lfa1 IS NOT INITIAL.

      IF gs_lfa1-lifnr IS NOT INITIAL. "UPDATE SUPPLIER

        SELECT SINGLE  *
          FROM IBPSUPPLIER INTO @DATA(LWA_IBPSUPPLIER)
         WHERE SUPPLIER = @GS_LFA1-LIFNR.

        IF SY-SUBRC NE 0  .
          MESSAGE | BP para fornecedor { GS_LFA1-LIFNR } não encontrada !| TYPE 'E'.
        ENDIF.

        CALL FUNCTION 'BUPA_NUMBERS_GET'
          EXPORTING
            iv_partner = lwa_ibpsupplier-businesspartner  " Business Partner Number
          IMPORTING
            es_but000  = gs_but000.     " Business Partner Data

        IF gs_but000 IS NOT INITIAL.
          gv_bp_type = gs_but000-type.
          GV_BU_GROUP = gs_but000-BU_GROUP.
        ENDIF.

        me->mt_bp_update_supplier(
          EXPORTING
            im_test       = gv_test
            im_bp_type    = gv_bp_type      " Categoria do parceiro de negócios
            im_bu_group   = gv_bu_group     " ALRS
            is_lfa1       = gs_lfa1         " Mestre de fornecedores (parte geral)
            it_lfbk       = gt_lfbk         " Mestre de fornecedores (coordenadas do banco)
            it_lfb1       = gt_lfb1         " Mestre de fornecedores (empresa)
            it_lfbw       = gt_lfbw         " Mestre de fornecedores (empresa)
            is_rf02k      = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
            is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
            is_sza1_d0100 = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
            it_lfm1       = gt_lfm1
          IMPORTING
            et_return     = gt_return       " Tabela de retorno
            em_partner    = gv_partner      " Nº parceiro de negócios
        ).

      ELSE. " CREATE SUPPLIER

        "Cria BP
        "------------------------------------------------------------------
        me->mt_bp_create_supplier(
          EXPORTING
            im_test       = gv_test
            is_lfa1       = gs_lfa1         " Mestre de fornecedores (parte geral)
            it_lfbk       = gt_lfbk         " Interface externa: dados detalhes bancários
            it_lfb1       = gt_lfb1         " Mestre de fornecedores (empresa)
            it_lfbw       = gt_lfbw         " Mestre de fornecedores (empresa)
            is_rf02k      = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
            is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
            is_sza1_d0100 = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
            is_bus_joel_main = gs_bus_joel_main "ALRS
            it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
          CHANGING
            et_return     = gt_return       " Tabela de retorno
            em_partner    = gv_partner      " Nº parceiro de negócios
        ).
      ENDIF.

    ENDIF.

    me->mt_shdb_convert_return_bdcmsg(
  EXPORTING
    im_tcode  = gv_tcode                  " Campo do sistema ABAP: código de transação atual
    it_return = gt_return                  " Tabela de retorno
  CHANGING
    ct_bdcmsg = ct_bdcmsg                  " Tabela de mensagens na transação
).


    IF gv_partner IS NOT INITIAL.
      LOOP AT ct_bdcmsg ASSIGNING FIELD-SYMBOL(<fs_bdcmsg>)
           WHERE msgtyp = 'S'.
        <fs_bdcmsg>-msgv2 = gv_partner.
      ENDLOOP.

    ELSE.

      IF gs_kna1 IS NOT INITIAL. " Update Customer

        "Retoques finais!
        "------------------------------------------------------------------
        IF gs_kna1-name1 IS INITIAL.
          gs_kna1-name1 = gs_addr1_data-name1.
        ENDIF.
        IF gs_kna1-name3 IS INITIAL.
          gs_kna1-name3 = gs_addr1_data-name1.
        ENDIF.

        IF gs_rf02d-ktokd IS NOT INITIAL.
          gs_kna1-ktokd = gs_rf02d-ktokd.
        ENDIF.

        IF gt_knb1 IS INITIAL AND gs_knb1 IS NOT INITIAL.
          APPEND gs_knb1 TO gt_knb1.
        ENDIF.
        LOOP AT gt_knb1 ASSIGNING FIELD-SYMBOL(<fs_knb1>).
          <fs_knb1>-bukrs = gs_rf02d-bukrs.
        ENDLOOP.

        IF gt_knbw IS INITIAL AND gs_knbw IS NOT INITIAL.
          APPEND gs_knbw TO gt_knbw.
        ENDIF.
        LOOP AT gt_knbw ASSIGNING FIELD-SYMBOL(<fs_knbw>).
          <fs_knbw>-bukrs = gs_rf02d-bukrs.
        ENDLOOP.

        IF gt_knvi IS INITIAL AND gs_knvi IS NOT INITIAL.
          APPEND gs_knvi TO gt_knvi.
        ENDIF.
        LOOP AT gt_knvi ASSIGNING FIELD-SYMBOL(<fs_knvi>).
          <fs_knvi>-aland = COND #( WHEN <fs_knvi>-aland IS INITIAL THEN 'BR'   ELSE <fs_knvi>-aland ).
          <fs_knvi>-tatyp = COND #( WHEN <fs_knvi>-tatyp IS INITIAL THEN 'IBRX' ELSE <fs_knvi>-tatyp ).
        ENDLOOP.

        IF gt_knvv IS INITIAL AND gs_knvv IS NOT INITIAL.
          APPEND gs_knvv TO gt_knvv.
        ENDIF.
        LOOP AT gt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>).
          <fs_knvv>-vkorg = COND #( WHEN <fs_knvv>-vkorg IS INITIAL THEN gs_rf02d-vkorg ELSE <fs_knvv>-vkorg ).
          <fs_knvv>-vtweg = COND #( WHEN <fs_knvv>-vtweg IS INITIAL THEN gs_rf02d-vtweg ELSE <fs_knvv>-vtweg ).
          <fs_knvv>-spart = COND #( WHEN <fs_knvv>-spart IS INITIAL THEN gs_rf02d-spart ELSE <fs_knvv>-spart ).
        ENDLOOP.

        IF gt_adsmtp IS INITIAL AND gs_adsmtp IS NOT INITIAL.
          APPEND gs_adsmtp TO gt_adsmtp.
        ENDIF.

        IF ( gs_kna1-stcd1 IS NOT INITIAL OR gs_kna1-stcd2 IS NOT INITIAL ) AND
           gs_kna1-kunnr IS INITIAL.

          SELECT SINGLE kunnr FROM kna1
            INTO @lv_kunnr
            WHERE stcd1 = @gs_kna1-stcd1
              AND stcd2 = @gs_kna1-stcd2.

        ELSE.

          lv_kunnr = gs_kna1-kunnr.

        ENDIF.

        IF lv_kunnr is NOT INITIAL.

          SELECT SINGLE  *
            FROM IBUPACUSTOMER INTO @DATA(LWA_IBUPACUSTOMER)
           WHERE CUSTOMER = @lv_kunnr.

          IF SY-SUBRC NE 0  .
            MESSAGE | BP para cliente { lv_kunnr } não encontrada !| TYPE 'E'.
          ENDIF.

          CALL FUNCTION 'BUPA_NUMBERS_GET'
            EXPORTING
              iv_partner = LWA_IBUPACUSTOMER-businesspartner     " Business Partner Number
            IMPORTING
              es_but000  = gs_but000.   " Business Partner Data

          gv_bp_type = gs_but000-type.

          me->mt_bp_update_customer(
                                     EXPORTING
                                     im_test       = gv_test
                                     im_bp_type    = gv_bp_type
                                     is_kna1       = gs_kna1         " Mestre de fornecedores (parte geral)
                                     is_knvk       = gs_knvk         " Interface externa: dados detalhes bancários
                                     it_knb1       = gt_knb1         " Mestre de fornecedores (empresa)
                                     it_knbw       = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
                                     it_knvi       = gt_knvi         " Mestre de clientes - indicadores de impostos
                                     it_knvv       = gt_knvv         " Mestre de clientes (área de vendas)
                                     it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
                                     is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
                                     is_addr2_data = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
                                     is_sza1_d0100 = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
                                     CHANGING
                                     et_return     = gt_return       " Tabela de retorno
                                     em_partner    = gv_partner      " Nº parceiro de negócios
                                   ).

        ELSE. " Create Customer

          "Cria BP
          "------------------------------------------------------------------
          me->mt_bp_create_customer(
            EXPORTING
              im_test       = gv_test
              is_kna1       = gs_kna1         " Mestre de fornecedores (parte geral)
              is_knvk       = gs_knvk         " Interface externa: dados detalhes bancários
              it_knb1       = gt_knb1         " Mestre de fornecedores (empresa)
              it_knbw       = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
              it_knvi       = gt_knvi         " Mestre de clientes - indicadores de impostos
              it_knvv       = gt_knvv         " Mestre de clientes (área de vendas)
              it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
              is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
              is_addr2_data = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
              is_sza1_d0100 = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
            CHANGING
              et_return     = gt_return       " Tabela de retorno
              em_partner    = gv_partner      " Nº parceiro de negócios
          ).

        ENDIF.

        me->mt_shdb_convert_return_bdcmsg(
                                            EXPORTING
                                             im_tcode  = gv_tcode                  " Campo do sistema ABAP: código de transação atual
                                             it_return = gt_return                  " Tabela de retorno
                                            CHANGING
                                             ct_bdcmsg = ct_bdcmsg                  " Tabela de mensagens na transação
                                         ).

        IF gv_partner IS NOT INITIAL.
          LOOP AT ct_bdcmsg ASSIGNING FIELD-SYMBOL(<fs_bdcmsg_cust>)
               WHERE msgtyp = 'S'.
            <fs_bdcmsg_cust>-msgv2 = gv_partner.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method MT_BP_PROCESS_OLD_SHDB.
    me->mt_set_data_by_shdb( it_bdcdata = gt_bdcdata ).
  endmethod.


  METHOD MT_BP_UPDATE_CUSTOMER.

    DATA: lv_msg         TYPE bapi_msg,
          lv_bu_type     TYPE bu_type,
          lv_bu_partner  TYPE bu_partner,
          lv_object_task TYPE bus_ei_object_task. "I = Include (New); U = Update (Change)

    DATA: lt_bp_data TYPE cvis_ei_extern_t,
          lt_rules   TYPE TABLE OF bapibus1006_bproles.

**----------------------------------------------------------------------*
**  VALIDAÇÕES INICIAIS
**----------------------------------------------------------------------*
*    "Campo obrigatório: STCD1
*    IF is_kna1-stcd1 IS INITIAL AND is_kna1-stcd2 IS INITIAL.
*      lv_msg = |Campo obrigatório: CPF ou CNPJ|.
*      me->mt_return_message_generic(
*        EXPORTING
*          im_type   = 'E'
*          im_msg    = lv_msg
*        CHANGING
*          ct_return = et_return                  " Tabela de retorno
*      ).
*      RETURN.
*    ENDIF.
*
*    "Verifica se o CPF/CNPJ informado já existe.
*    SELECT SINGLE kunnr FROM kna1
*      INTO @DATA(lv_kunnr)
*      WHERE stcd1 = @is_kna1-stcd1
*        AND stcd2 = @is_kna1-stcd2.
*    IF sy-subrc = 0.
*      lv_msg = COND #( WHEN is_kna1-stcd1 IS NOT INITIAL THEN is_kna1-stcd1 ELSE is_kna1-stcd2 ).
*      lv_msg = |Documento { lv_msg } já pertencente ao cliente { lv_kunnr }|.
*      me->mt_return_message_generic(
*        EXPORTING
*          im_type   = 'E'
*          im_msg    = lv_msg
*        CHANGING
*          ct_return = et_return                  " Tabela de retorno
*      ).
*      RETURN.
*    ENDIF.

*----------------------------------------------------------------------*
*  MAPEIA DADOS PARA CLASSE DE CRIAÇÃO DE BP
*----------------------------------------------------------------------*
    APPEND INITIAL LINE TO lt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>).

    "Define o tipo do BP (1- Pessoa 2- Organização 3- Grupo)
    IF im_bp_type IS NOT INITIAL.
      lv_bu_type = im_bp_type.
    ELSE.
      lv_bu_type = COND #( WHEN is_kna1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).
    ENDIF.

    lv_object_task = 'U'.

    SELECT SINGLE  *
      FROM IBUPACUSTOMER INTO @DATA(LWA_IBUPACUSTOMER)
     WHERE customer = @is_kna1-kunnr.

    IF SY-SUBRC NE 0  .
      MESSAGE | BP para cliente { is_kna1-kunnr } não encontrada !| TYPE 'E'.
    ENDIF.

    lv_bu_partner = lwa_ibupacustomer-businesspartner.

    "Customer - Header
    me->mt_fill_part_cust_header(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        im_create      = abap_true
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER
    "--------------------------------------------------------------------
    "PARTNER - Header
    me->mt_fill_part_header_sup(
      EXPORTING
        im_object_task = lv_object_task       " Interface externa: código de modificação objeto
        im_partner     = lv_bu_partner        " Nº cliente
        im_guid        = space                " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Common
    me->mt_fill_part_cent_common_cust(
      EXPORTING
        im_object_task = lv_object_task
        im_bu_type     = lv_bu_type
        is_kna1        = is_kna1
        is_addr1_data  = is_addr1_data
        im_personal_data = is_personal_data
        is_personal_data2 = is_personal_data2
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    "PARTNER - Central - Address
    me->mt_fill_part_cent_address_cust(
      EXPORTING
        im_object_task = lv_object_task       " Objects data
        is_addr1_data  = is_addr1_data        " Estrutura de transferência para endereços
        is_sza1_d0100  = is_sza1_d0100        " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp            " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_alteracao   = is_alteracao         " US #184108 - MMSILVA - 03.06.2025
      CHANGING
        cs_bp_data     = <fs_bp_data>         " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
    ).

    "PARTNER - Central - Tax
    me->mt_fill_part_cent_tax_cust(
      EXPORTING
        im_object_task = lv_object_task
        is_kna1        = is_kna1
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
      EXPORTING
        businesspartner      = lv_bu_partner   " Business Partner Number
      TABLES
        businesspartnerroles = lt_rules.         " Business Partner Roles

    IF lt_rules IS NOT INITIAL.

      LOOP AT lt_rules ASSIGNING FIELD-SYMBOL(<fs_rules>).

        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task                  " Interface externa: código de modificação objeto
            im_role        = <fs_rules>-partnerrolecategory                  " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>                 " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

      ENDLOOP.
    ENDIF.

    "SUPPLIER
    "--------------------------------------------------------------------
*    "Customer - Header
*    me->mt_fill_part_cust_header(
*      EXPORTING
*        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
*        im_create      = abap_true
*      CHANGING
*        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
*    ).

    "Supplier - Data
    me->mt_fill_part_cust_data(
      EXPORTING
        is_kna1   = is_kna1               " Atual.dados mestre fornecedor: campos de tela e operativos
      CHANGING
        cs_bp_data = <fs_bp_data>           " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Address
    me->mt_fill_part_cust_address(
      EXPORTING
        im_object_task =  lv_object_task                " Interface externa: código de modificação objeto
        is_kna1        =  is_kna1                " Mestre de fornecedores (parte geral)
        is_addr1_data  =  is_addr1_data                " Estrutura de transferência para endereços
      CHANGING
        cs_bp_data     =  <fs_bp_data>                " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Contact
    me->mt_fill_part_cust_contact(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação objeto
        is_kna1        = is_kna1
        is_sza1_d0100  = is_sza1_d0100      " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        is_knvk        = is_knvk
        it_adsmtp      = it_adsmtp          " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_tax(
      EXPORTING
        im_object_task = lv_object_task
        it_knvi        = it_knvi
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_company(
      EXPORTING
        im_object_task = lv_object_task
        it_knb1        = it_knb1
        it_knbw        = it_knbw
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_cust_sales(
      EXPORTING
        im_object_task = lv_object_task
        it_knvv        = it_knvv
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    me->mt_fill_part_cust_bankdetail(
      EXPORTING
        im_object_task = lv_object_task     " Interface externa: código de modificação dados bancários
        it_knbk        = it_knbk            " Interface externa: dados detalhes bancários
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).


    UNASSIGN <fs_bp_data>.

*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
    me->mt_bp_maintain(
      EXPORTING
        im_test    = im_test
        it_data    = lt_bp_data     " Entrada da integração de clientes/fornecedores
      IMPORTING
        et_return  = et_return      " Tabela de retorno
        em_partner = em_partner     " Nº parceiro de negócios
    ).

  ENDMETHOD.


  METHOD MT_BP_UPDATE_SUPPLIER.

    DATA: lv_msg         TYPE bapi_msg,
          lv_bu_type     TYPE bu_type,
          lv_BU_GROUP     TYPE bu_group,
          lv_object_task TYPE bus_ei_object_task. "I = Include (New); U = Update (Change)

    DATA: lt_bp_data TYPE cvis_ei_extern_t,
          lt_rules   TYPE TABLE OF bapibus1006_bproles.



*----------------------------------------------------------------------*
*  MAPEIA DADOS PARA CLASSE DE CRIAÇÃO DE BP
*----------------------------------------------------------------------*
    APPEND INITIAL LINE TO lt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>).

    "Define o tipo do BP (1- Pessoa 2- Organização 3- Grupo)
    IF im_bp_type IS NOT INITIAL.
      lv_bu_type = im_bp_type.
      lv_bu_group = im_bu_group. "ALRS
    ELSE.
      lv_bu_type = COND #( WHEN is_lfa1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).
      CLEAR lv_bu_group. "ALRS
    ENDIF.

    lv_object_task = 'U'.

    SELECT SINGLE  *
      FROM IBPSUPPLIER INTO @DATA(LWA_IBPSUPPLIER)
     WHERE SUPPLIER = @is_lfa1-lifnr.

    IF SY-SUBRC NE 0  .
      MESSAGE | BP para fornecedor { is_lfa1-lifnr } não encontrada !| TYPE 'E'.
    ENDIF.



    "PARTNER
    "--------------------------------------------------------------------
    "PARTNER - Header
    me->mt_fill_part_header_sup(
      EXPORTING
        im_object_task = lv_object_task                 " Interface externa: código de modificação objeto
        im_partner     = conv #( lwa_ibpsupplier-businesspartner )         " Nº parceiro de negócios
        im_guid        = space                          " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
      CHANGING
        cs_bp_data     = <fs_bp_data>                   " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Common
    me->mt_fill_part_cent_common_sup(
      EXPORTING
        im_object_task = lv_object_task          " Interface externa: código de modificação objeto
        im_bu_type     = lv_bu_type              " Categoria do parceiro de negócios
        IM_BU_GROUP    = lv_BU_GROUP             " ALRS
        is_lfa1        = is_lfa1                 " Mestre de fornecedores (parte geral)
        is_addr1_data  = is_addr1_data           " Estrutura de transferência para endereços
        is_personal_Data = is_personal_data
        is_personal_data2 = is_personal_data2
        is_bp_central_data = is_bp_central_data " Rubenilson - 01.10.24 #153421
      CHANGING
        cs_bp_data     = <fs_bp_data>            " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "PARTNER - Central - Address
    me->mt_fill_part_cent_address_sup(
      EXPORTING
        im_object_task = lv_object_task            " Objects data
        is_addr1_data  = is_addr1_data             " Estrutura de transferência para endereços
        im_partner     = conv #( lwa_ibpsupplier-businesspartner )             " Nº parceiro de negócios
        is_sza1_d0100  = is_sza1_d0100             " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp                 " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        is_alteracao   = is_alteracao              "US #184108 - MMSILVA - 03.06.2025
      CHANGING
        cs_bp_data     = <fs_bp_data>              " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
    ).

    "PARTNER - Central - Tax
    me->mt_fill_part_cent_tax_sup(
      EXPORTING
        im_object_task = lv_object_task           " Table Name
        is_lfa1        = is_lfa1                  " Join RFC
      CHANGING
        cs_bp_data     = <fs_bp_data>             " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
      EXPORTING
        businesspartner      = lwa_ibpsupplier-businesspartner    " Business Partner Number
      TABLES
        businesspartnerroles = lt_rules.         " Business Partner Roles

    IF lt_rules IS NOT INITIAL.

      LOOP AT lt_rules ASSIGNING FIELD-SYMBOL(<fs_rules>).

        me->mt_fill_part_cent_roles_sup(
          EXPORTING
            im_object_task = lv_object_task                  " Interface externa: código de modificação objeto
            im_role        = <fs_rules>-partnerrolecategory                  " Função de parceiro de negócios
          CHANGING
            cs_bp_data     = <fs_bp_data>                 " Interface complexa do parceiro negócios na integ.clnt.forn.
        ).

      ENDLOOP.
    ENDIF.


    "SUPPLIER
    "--------------------------------------------------------------------
    "Supplier - Header
    me->mt_fill_part_sup_header(
      EXPORTING
        im_object_task = lv_object_task                  " Interface externa: código de modificação objeto
        im_create      = abap_false
      CHANGING
        cs_bp_data     = <fs_bp_data>                  " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Data
    me->mt_fill_part_sup_data(
      EXPORTING
        is_rf02k   = is_rf02k                  " Atual.dados mestre fornecedor: campos de tela e operativos
      CHANGING
        cs_bp_data = <fs_bp_data>                  " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    "Supplier - Address
    me->mt_fill_part_sup_address(
      EXPORTING
        im_object_task = lv_object_task          " Interface externa: código de modificação objeto
        is_lfa1        = is_lfa1                 " Mestre de fornecedores (parte geral)
        is_addr1_data  = is_addr1_data           " Estrutura de transferência para endereços
      CHANGING
        cs_bp_data     = <fs_bp_data>            " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).


    "Supplier - Contact
    me->mt_fill_part_sup_contact(
      EXPORTING
        im_object_task = lv_object_task                  " Interface externa: código de modificação objeto
        is_lfa1        = is_lfa1                  " Mestre de fornecedores (parte geral)
        is_sza1_d0100  = is_sza1_d0100                  " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
        it_adsmtp      = it_adsmtp                  " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      CHANGING
        cs_bp_data     = <fs_bp_data>                  " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_bankdetail(
      EXPORTING
        im_object_task = lv_object_task                  " Interface externa: código de modificação dados bancários
        it_lfbk        = it_lfbk                  " Interface externa: dados detalhes bancários
      CHANGING
        cs_bp_data     = <fs_bp_data>                  " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_company(
      EXPORTING
        im_object_task = lv_object_task
        it_lfb1        = it_lfb1
        it_lfbw        = it_lfbw
      CHANGING
        cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
    ).

    me->mt_fill_part_sup_purch_org(
      EXPORTING
        im_object_task = lv_object_task
        it_lfm1        = it_lfm1
      CHANGING
        cs_bp_data     = <fs_bp_data>
    ).

    UNASSIGN <fs_bp_data>.

*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
    me->mt_bp_maintain(
      EXPORTING
        im_test    = im_test
        it_data    = lt_bp_data     " Entrada da integração de clientes/fornecedores
      IMPORTING
        et_return  = et_return      " Tabela de retorno
        em_partner = em_partner     " Nº parceiro de negócios
    ).

  ENDMETHOD.


  METHOD MT_FILL_DATAX.

    DATA:
      lo_struct  TYPE REF TO cl_abap_structdescr,
      lo_structx TYPE REF TO cl_abap_structdescr.

    lo_struct  ?= cl_abap_typedescr=>describe_by_data( is_data ).
    lo_structx ?= cl_abap_typedescr=>describe_by_data( cs_datax ).

    DATA(lt_details)  = lo_struct->components[].
    DATA(lt_detailsx) = lo_struct->components[].

    LOOP AT lt_details ASSIGNING FIELD-SYMBOL(<fs_details>).

      READ TABLE lt_detailsx ASSIGNING FIELD-SYMBOL(<fs_detailsx>) WITH KEY name = <fs_details>-name.

      IF sy-subrc EQ 0.

        ASSIGN COMPONENT <fs_details>-name OF STRUCTURE is_data
          TO FIELD-SYMBOL(<fs_field>).

        ASSIGN COMPONENT <fs_detailsx>-name OF STRUCTURE cs_datax
          TO FIELD-SYMBOL(<fs_fieldx>).

        IF <fs_field>  IS ASSIGNED AND
           <fs_fieldx> IS ASSIGNED.

          <fs_fieldx> = COND #( WHEN <fs_field> IS INITIAL THEN abap_false ELSE abap_true ).

        ENDIF.

      ENDIF.

      UNASSIGN:
        <fs_field> ,
        <fs_fieldx>.

    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CENT_ADDRESS_CUST.

    DATA:
      lt_addresses_all TYPE STANDARD TABLE OF bapibus1006_addresses,
      lt_return	       TYPE STANDARD TABLE OF bapiret2,
      l_insr           TYPE c,
      lt_smtp          TYPE TABLE OF bapiadsmtp, "US #184108 - MMSILVA - 30.05.2025
      lt_tel           TYPE TABLE OF bapiadtel. "US #184108 - MMSILVA - 30.05.2025

    IF is_addr1_data IS NOT INITIAL.

      l_insr = 'X'.

      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_address>).
      <fs_address>-data_key-guid = ''.
*      <fs_address>-task          = im_object_task.

      IF im_object_task EQ 'U'.

        <fs_address>-task          = 'M'.

        IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

          CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
            EXPORTING
              businesspartner = cs_bp_data-partner-header-object_instance-bpartner
            TABLES
              addresses_all   = lt_addresses_all
              return          = lt_return.

          READ TABLE lt_addresses_all ASSIGNING FIELD-SYMBOL(<fs_addr>)
            WITH KEY standardaddress = 'X'.

          IF sy-subrc EQ 0.

            <fs_address>-data_key-guid = <fs_addr>-addressguid.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
            CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
                addressguid     = <fs_addr>-addressguid
              TABLES
                bapiadsmtp      = lt_smtp
                bapiadtel       = lt_tel
                return          = lt_return.

*            READ TABLE lt_smtp ASSIGNING FIELD-SYMBOL(<fs_smtp_exist>) INDEX 1.
*            READ TABLE lt_tel ASSIGNING FIELD-SYMBOL(<fs_tel_exist>) INDEX 1.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

          ENDIF.

          CLEAR: cs_bp_data-ensure_create-create_customer.

        ENDIF.

      ELSE.

        <fs_address>-task          = im_object_task.

      ENDIF.

*--> Precisa de ajuste para o form funcionar aqui.
*    perform ZF_MOVE_DATA
*      using iS_ADDR1_DATA
*      changing <FS_ADDRESS>-DATA-POSTAL-DATA
*               <FS_ADDRESS>-DATA-POSTAL-DATAX.
      MOVE-CORRESPONDING is_addr1_data TO <fs_address>-data-postal-data.
      IF NOT is_addr1_data-street IS INITIAL.
        <fs_address>-data-postal-datax-street = 'X'.
      ENDIF.
      IF NOT is_addr1_data-country IS INITIAL.
        <fs_address>-data-postal-datax-country = 'X'.
      ENDIF.
      IF NOT is_addr1_data-region IS INITIAL.
        <fs_address>-data-postal-datax-region = 'X'.
      ENDIF.
      IF NOT is_addr1_data-transpzone IS INITIAL.
        <fs_address>-data-postal-datax-transpzone = 'X'.
      ENDIF.
      IF NOT is_addr1_data-taxjurcode IS INITIAL.
        <fs_address>-data-postal-datax-taxjurcode = 'X'.
      ENDIF.
*<-- Precisa de ajuste para o form funcionar aqui.

      <fs_address>-task                         = im_object_task.
      <fs_address>-data-postal-data-city        = is_addr1_data-city1.
      <fs_address>-data-postal-datax-city       = 'X'.
      <fs_address>-data-postal-data-district    = is_addr1_data-city2.
      <fs_address>-data-postal-datax-district   = 'X'.
      <fs_address>-data-postal-data-langu       = is_addr1_data-langu.
      <fs_address>-data-postal-datax-langu      = 'X'.
      <fs_address>-data-postal-data-house_no    = is_addr1_data-house_num1.
      <fs_address>-data-postal-datax-house_no   = 'X'.
      <fs_address>-data-postal-data-house_no2   = is_addr1_data-house_num2.
      <fs_address>-data-postal-datax-house_no2  = 'X'.
      <fs_address>-data-postal-data-postl_cod1  = is_addr1_data-post_code1.
      <fs_address>-data-postal-datax-postl_cod1 = 'X'.
      <fs_address>-data-postal-data-postl_cod2  = is_addr1_data-post_code2.
      <fs_address>-data-postal-datax-postl_cod2 = 'X'.
      <fs_address>-data-postal-data-langu       = sy-langu.
      <fs_address>-data-postal-datax-langu      = 'X'.

      APPEND INITIAL LINE TO <fs_address>-data-addr_usage-addr_usages ASSIGNING FIELD-SYMBOL(<fs_usage>).
      <fs_usage>-currently_valid = abap_true.
      <fs_usage>-task = im_object_task.
      <fs_usage>-data_key-addresstype = 'XXDEFAULT'.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
      LOOP AT lt_tel ASSIGNING FIELD-SYMBOL(<fs_tel_exist>).

        IF is_sza1_d0100-tel_number IS INITIAL OR <fs_tel_exist>-consnumber = '000'.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone_tel>).
        CLEAR <fs_phone_tel>.

        "Número enviado encontrado
        IF <fs_tel_exist>-telephone = is_sza1_d0100-tel_number.
          IF is_alteracao-telefone_subst IS INITIAL.
            "Caso seja enviado um número existente e não seja uma alteração:
            <fs_phone_tel>-contact-task            = 'D'.
            <fs_phone_tel>-contact-data-consnumber = <fs_tel_exist>-consnumber.
            <fs_phone_tel>-contact-data-tel_no     = <fs_tel_exist>-tel_no.
            <fs_phone_tel>-contact-data-telephone  = <fs_tel_exist>-telephone.
            "Marca alteração
            <fs_phone_tel>-contact-datax-tel_no     = 'X'.
            <fs_phone_tel>-contact-datax-telephone  = 'X'.
          ELSE.
            "Caso seja enviado um número existente e seja uma alteração:
            <fs_phone_tel>-contact-task            = 'U'.
            <fs_phone_tel>-contact-data-consnumber = <fs_tel_exist>-consnumber.
            <fs_phone_tel>-contact-data-tel_no     = is_alteracao-telefone_subst.
            <fs_phone_tel>-contact-data-telephone  = is_alteracao-telefone_subst.
            "Marca alteração
            <fs_phone_tel>-contact-datax-tel_no     = 'X'.
            <fs_phone_tel>-contact-datax-telephone  = 'X'.
          ENDIF.

        ELSE.
          "Caso o telefone atual não é o número enviado e existe no cadastro, mantém
          <fs_phone_tel>-contact-task            = 'U'.
          <fs_phone_tel>-contact-data-consnumber = <fs_tel_exist>-consnumber.
          <fs_phone_tel>-contact-data-tel_no     = <fs_tel_exist>-tel_no.
          <fs_phone_tel>-contact-data-telephone  = <fs_tel_exist>-telephone.
          "Marca alteração
          <fs_phone_tel>-contact-datax-tel_no     = 'X'.
          <fs_phone_tel>-contact-datax-telephone  = 'X'.
        ENDIF.

      ENDLOOP.

      "Caso não exista telefone algum, incluir o telefone enviado
      READ TABLE lt_tel WITH KEY telephone = is_sza1_d0100-tel_number TRANSPORTING NO FIELDS.
      IF ( lines( lt_tel ) = 0 OR sy-subrc <> 0 ) AND is_sza1_d0100-tel_number IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>).
        <fs_phone>-contact-task           = 'I'.
        <fs_phone>-contact-data-tel_no    = is_sza1_d0100-tel_number.
        <fs_phone>-contact-data-telephone = is_sza1_d0100-tel_number.
        " Marcar alteração
        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_phone>-contact-data
          CHANGING
            cs_datax = <fs_phone>-contact-datax
        ).
      ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

      IF NOT is_sza1_d0100-mob_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING <fs_phone>." Comunicação
        <fs_phone>-contact-task            = im_object_task.
        <fs_phone>-contact-data-tel_no     = is_sza1_d0100-mob_number.
        <fs_phone>-contact-datax-tel_no    = 'X'.
        <fs_phone>-contact-data-telephone  = is_sza1_d0100-mob_number.
        <fs_phone>-contact-datax-telephone = 'X'.
        <fs_phone>-contact-data-r_3_user   = '2'.
      ENDIF.
      IF NOT is_sza1_d0100-fax_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_cfax>)." Comunicação
        <fs_cfax>-contact-task         = im_object_task.
        <fs_cfax>-contact-data-fax_no  = is_sza1_d0100-fax_number.
        <fs_cfax>-contact-datax-fax_no = 'X'.
        <fs_cfax>-contact-data-fax     = is_sza1_d0100-fax_number.
        <fs_cfax>-contact-datax-fax    = 'X'.
      ENDIF.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
      LOOP AT lt_smtp ASSIGNING FIELD-SYMBOL(<fs_smtp_exist>).

        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail_smtp>).
        CLEAR <fs_cemail_smtp>.

        IF is_sza1_d0100-smtp_addr IS INITIAL.
          CONTINUE.
        ENDIF.

        "E-mail enviado encontrado
        IF <fs_smtp_exist>-e_mail = is_sza1_d0100-smtp_addr.
          IF is_alteracao-email_subst IS INITIAL AND <fs_smtp_exist>-consnumber <> '000'.
            "Caso seja enviado um e-mail existente e não seja uma alteração:
            <fs_cemail_smtp>-contact-task            = 'D'.
            <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
            <fs_cemail_smtp>-contact-data-e_mail     = <fs_smtp_exist>-e_mail.
            "Marca alteração
            <fs_cemail_smtp>-contact-datax-e_mail    = 'X'.
          ELSEIF is_alteracao-email_subst IS NOT INITIAL AND <fs_smtp_exist>-consnumber <> '000'.
            "Caso seja enviado um e-mail existente e seja uma alteração:
            <fs_cemail_smtp>-contact-task            = 'M'.
            <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
            <fs_cemail_smtp>-contact-data-e_mail     = is_alteracao-email_subst.
            "Marca alteração
            <fs_cemail_smtp>-contact-datax-e_mail    = 'X'.
          ENDIF.

        ELSE.
          "Caso o e-mail atual não é o e-mail enviado, mantém (sem alteração)
          <fs_cemail_smtp>-contact-task            = ''.
          <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
          <fs_cemail_smtp>-contact-data-e_mail     = <fs_smtp_exist>-e_mail.
          "Não marca alteração
          READ TABLE lt_smtp WITH KEY e_mail = is_sza1_d0100-smtp_addr TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            "Caso o email atual é o número enviado e não existe no cadastro, insere
            <fs_cemail_smtp>-contact-task            = 'I'.
            <fs_cemail_smtp>-contact-data-e_mail     = is_sza1_d0100-smtp_addr.
            "Marca alteração
            <fs_cemail_smtp>-contact-datax-e_mail    = 'X'.
          ENDIF.
        ENDIF.

      ENDLOOP.

      "Caso não exista e-mail algum, incluir o e-mail enviado
      IF lines( lt_smtp ) = 0 AND is_sza1_d0100-smtp_addr IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail>).
        <fs_cemail>-contact-task           = 'I'.
        <fs_cemail>-contact-data-e_mail    = is_sza1_d0100-smtp_addr.
        " Marcar alteração
        <fs_cemail>-contact-datax-e_mail   = 'X'.
      ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

    ENDIF.

    IF me->gs_kna1-telfx IS NOT INITIAL.

      IF l_insr IS INITIAL AND <fs_address> IS NOT ASSIGNED.

        l_insr = 'X'.

        APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING <fs_address>.
        <fs_address>-data_key-guid = ''.

        IF im_object_task EQ 'U'.

          <fs_address>-task          = 'M'.

          IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

            CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
              TABLES
                addresses_all   = lt_addresses_all
                return          = lt_return.

            READ TABLE lt_addresses_all ASSIGNING <fs_addr>
              WITH KEY standardaddress = 'X'.

            IF sy-subrc EQ 0.

              <fs_address>-data_key-guid = <fs_addr>-addressguid.

            ENDIF.

            CLEAR: cs_bp_data-ensure_create-create_customer.

          ENDIF.

        ELSE.

          <fs_address>-task          = im_object_task.

        ENDIF.

      ENDIF.

      IF <fs_address> IS ASSIGNED.

        APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_fax>).
        <fs_fax>-contact-task      = im_object_task.
        <fs_fax>-contact-data-fax  = me->gs_kna1-telfx.
        <fs_fax>-contact-datax-fax = 'X'.

      ENDIF.

    ENDIF.

    IF me->gs_adsmtp-smtp_addr IS NOT INITIAL.

      IF l_insr IS INITIAL AND <fs_address> IS NOT ASSIGNED.

        l_insr = 'X'.

        APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING <fs_address>.
        <fs_address>-data_key-guid = ''.

        IF im_object_task EQ 'U'.

          <fs_address>-task          = 'M'.

          IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

            CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
              TABLES
                addresses_all   = lt_addresses_all
                return          = lt_return.

            READ TABLE lt_addresses_all ASSIGNING <fs_addr>
              WITH KEY standardaddress = 'X'.

            IF sy-subrc EQ 0.

              <fs_address>-data_key-guid = <fs_addr>-addressguid.

            ENDIF.

            CLEAR: cs_bp_data-ensure_create-create_customer.

          ENDIF.

        ELSE.

          <fs_address>-task          = im_object_task.

        ENDIF.

      ENDIF.

      IF <fs_address> IS ASSIGNED.

        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
        <fs_smtp>-contact-task         = im_object_task.
        <fs_smtp>-contact-data-e_mail  = me->gs_adsmtp-smtp_addr.
        <fs_smtp>-contact-datax-e_mail = 'X'.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD MT_FILL_PART_CENT_ADDRESS_SUP.

    DATA: lv_address_guid TYPE bu_address_guid,
          lt_return	      TYPE STANDARD TABLE OF bapiret2, "US #184108 - MMSILVA - 30.05.2025
          lt_smtp         TYPE TABLE OF bapiadsmtp, "US #184108 - MMSILVA - 30.05.2025
          lt_tel          TYPE TABLE OF bapiadtel. "US #184108 - MMSILVA - 30.05.2025.

    IF cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key = '0002'.
      IF is_sza1_d0100-title_medi = 'SENHORA'.
        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key = '0001'.
      ENDIF.
    ENDIF.

    IF is_addr1_data IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_address>).
      CASE im_object_task.
        WHEN 'I'. " CREATE
          <fs_address>-data_key-guid = ''.
        WHEN 'U' OR 'M'. " UPDATE
          CALL FUNCTION 'BUPA_ADDRESSES_READ'
            EXPORTING
              iv_partner            = im_partner                   " Business Partner Number
            IMPORTING
*             ev_standard_addrnumber   =                          " Address Number
              ev_standard_addrguid  = lv_address_guid  " UUID in Character Format
            EXCEPTIONS
              no_partner_specified  = 1
              no_valid_record_found = 2
              not_found             = 3
              blocked_partner       = 4
              OTHERS                = 5.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF lv_address_guid IS NOT INITIAL.
            <fs_address>-data_key-guid = CONV #( lv_address_guid ).

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
            CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
                addressguid     = <fs_address>-data_key-guid
              TABLES
                bapiadsmtp      = lt_smtp
                bapiadtel       = lt_tel
                return          = lt_return.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      <fs_address>-task          = im_object_task.

*--> Precisa de ajuste para o form funcionar aqui.
*    perform ZF_MOVE_DATA
*      using IS_ADDR1_DATA
*      changing <FS_ADDRESS>-DATA-POSTAL-DATA
*               <FS_ADDRESS>-DATA-POSTAL-DATAX.
      MOVE-CORRESPONDING is_addr1_data TO <fs_address>-data-postal-data.
      IF NOT is_addr1_data-street IS INITIAL.
        <fs_address>-data-postal-datax-street = 'X'.
      ENDIF.
      IF NOT is_addr1_data-country IS INITIAL.
        <fs_address>-data-postal-datax-country = 'X'.
      ENDIF.
      IF NOT is_addr1_data-region IS INITIAL.
        <fs_address>-data-postal-datax-region = 'X'.
      ENDIF.
      IF NOT is_addr1_data-transpzone IS INITIAL.
        <fs_address>-data-postal-datax-transpzone = 'X'.
      ENDIF.
      IF NOT is_addr1_data-taxjurcode IS INITIAL.
        <fs_address>-data-postal-datax-taxjurcode = 'X'.
      ENDIF.
*<-- Precisa de ajuste para o form funcionar aqui.

      <fs_address>-task                         = im_object_task.
      <fs_address>-data-postal-data-city        = is_addr1_data-city1.
      <fs_address>-data-postal-data-district    = is_addr1_data-city2.
      <fs_address>-data-postal-data-langu       = is_addr1_data-langu.
      <fs_address>-data-postal-data-house_no    = is_addr1_data-house_num1.
      <fs_address>-data-postal-data-house_no2   = is_addr1_data-house_num2.
      <fs_address>-data-postal-data-postl_cod1  = is_addr1_data-post_code1.
      <fs_address>-data-postal-data-postl_cod2  = is_addr1_data-post_code2.
      <fs_address>-data-postal-data-langu       = sy-langu.

*      <fs_address>-data-postal-datax-city       = 'X'.
*      <fs_address>-data-postal-datax-district   = 'X'.
*      <fs_address>-data-postal-datax-langu      = 'X'.
*      <fs_address>-data-postal-datax-house_no   = 'X'.
*      <fs_address>-data-postal-datax-house_no2  = 'X'.
*      <fs_address>-data-postal-datax-postl_cod1 = 'X'.
*      <fs_address>-data-postal-datax-postl_cod2 = 'X'.
*      <fs_address>-data-postal-datax-langu      = 'X'.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_address>-data-postal-data
        CHANGING
          cs_datax = <fs_address>-data-postal-datax
      ).

      APPEND INITIAL LINE TO <fs_address>-data-addr_usage-addr_usages ASSIGNING FIELD-SYMBOL(<fs_usage>).
      <fs_usage>-currently_valid = abap_true.
      <fs_usage>-task = im_object_task.
      <fs_usage>-data_key-addresstype = 'XXDEFAULT'.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
      LOOP AT lt_tel ASSIGNING FIELD-SYMBOL(<fs_tel_exist>).

        IF is_sza1_d0100-tel_number IS INITIAL OR <fs_tel_exist>-consnumber = '000'.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone_tel>).
        CLEAR <fs_phone_tel>.

        "Número enviado encontrado
        IF <fs_tel_exist>-telephone = is_sza1_d0100-tel_number.
          IF is_alteracao-telefone_subst IS INITIAL.
            "Caso seja enviado um número existente e não seja uma alteração:
            <fs_phone_tel>-contact-task            = 'D'.
            <fs_phone_tel>-contact-data-tel_no     = <fs_tel_exist>-tel_no.
            <fs_phone_tel>-contact-data-telephone  = <fs_tel_exist>-telephone.
            "Marca alteração
            me->mt_fill_datax(
              EXPORTING
                is_data  = <fs_phone_tel>-contact-data
              CHANGING
                cs_datax = <fs_phone_tel>-contact-datax
            ).
          ELSE.
            "Caso seja enviado um número existente e seja uma alteração:
            <fs_phone_tel>-contact-task            = 'U'.
            <fs_phone_tel>-contact-data-consnumber = <fs_tel_exist>-consnumber.
            <fs_phone_tel>-contact-data-tel_no     = is_alteracao-telefone_subst.
            <fs_phone_tel>-contact-data-telephone  = is_alteracao-telefone_subst.
            "Marca alteração
            me->mt_fill_datax(
              EXPORTING
                is_data  = <fs_phone_tel>-contact-data
              CHANGING
                cs_datax = <fs_phone_tel>-contact-datax
            ).
          ENDIF.
        ENDIF.

      ENDLOOP.

      "Caso não exista telefone algum, incluir o telefone enviado
      READ TABLE lt_tel WITH KEY telephone = is_sza1_d0100-tel_number TRANSPORTING NO FIELDS.
      IF ( lines( lt_tel ) = 0 OR sy-subrc <> 0 ) AND is_sza1_d0100-tel_number IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>).
        <fs_phone>-contact-task           = 'I'.
        <fs_phone>-contact-data-tel_no    = is_sza1_d0100-tel_number.
        <fs_phone>-contact-data-telephone = is_sza1_d0100-tel_number.
        " Marcar alteração
        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_phone>-contact-data
          CHANGING
            cs_datax = <fs_phone>-contact-datax
        ).
      ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
*      IF is_sza1_d0100-tel_number IS NOT INITIAL.
*        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>)." Comunicação
*
*        IF <fs_tel_exist> IS NOT ASSIGNED.
*          <fs_phone>-contact-task          = 'I'.
*        ELSEIF <fs_tel_exist> IS ASSIGNED AND <fs_tel_exist>-telephone = is_sza1_d0100-tel_number.
*          <fs_phone>-contact-task          = 'D'.
*        ELSE.
*          <fs_phone>-contact-task          = im_object_task.
*        ENDIF.
**        <fs_phone>-contact-data-tel_no     = is_sza1_d0100-tel_number.
**        <fs_phone>-contact-data-telephone  = is_sza1_d0100-tel_number.
*
*        <fs_phone>-contact-datax-tel_no    = 'X'.
*        <fs_phone>-contact-datax-telephone = 'X'.
*
*        me->mt_fill_datax(
*          EXPORTING
*            is_data  = <fs_phone>-contact-data
*          CHANGING
*            cs_datax = <fs_phone>-contact-datax
*        ).
*
*    ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

      IF NOT is_sza1_d0100-mob_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING <fs_phone>." Comunicação
        <fs_phone>-contact-task            = im_object_task.
        <fs_phone>-contact-data-tel_no     = is_sza1_d0100-mob_number.
        <fs_phone>-contact-data-telephone  = is_sza1_d0100-mob_number.
        <fs_phone>-contact-data-r_3_user   = '2'.

*        <fs_phone>-contact-datax-tel_no    = 'X'.
*        <fs_phone>-contact-datax-telephone = 'X'.

        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_phone>-contact-data
          CHANGING
            cs_datax = <fs_phone>-contact-datax
        ).

      ENDIF.
      IF NOT is_sza1_d0100-fax_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_cfax>)." Comunicação
        <fs_cfax>-contact-task         = im_object_task.
        <fs_cfax>-contact-data-fax_no  = is_sza1_d0100-fax_number.
        <fs_cfax>-contact-data-fax     = is_sza1_d0100-fax_number.

*        <fs_cfax>-contact-datax-fax_no = 'X'.
*        <fs_cfax>-contact-datax-fax    = 'X'.

        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_cfax>-contact-data
          CHANGING
            cs_datax = <fs_cfax>-contact-datax
        ).

      ENDIF.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
      LOOP AT lt_smtp ASSIGNING FIELD-SYMBOL(<fs_smtp_exist>).

        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail_smtp>).
        CLEAR <fs_cemail_smtp>.

        IF is_sza1_d0100-smtp_addr IS INITIAL.
          CONTINUE.
        ENDIF.

        "E-mail enviado encontrado
        IF <fs_smtp_exist>-e_mail = is_sza1_d0100-smtp_addr.
          IF is_alteracao-email_subst IS INITIAL AND <fs_smtp_exist>-consnumber <> '000'.
            "Caso seja enviado um e-mail existente e não seja uma alteração:
            <fs_cemail_smtp>-contact-task            = 'D'.
            <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
            <fs_cemail_smtp>-contact-data-e_mail     = <fs_smtp_exist>-e_mail.
            "Marca alteração
            me->mt_fill_datax(
              EXPORTING
                is_data  = <fs_cemail_smtp>-contact-data
              CHANGING
                cs_datax = <fs_cemail_smtp>-contact-datax
            ).
          ELSEIF is_alteracao-email_subst IS NOT INITIAL AND <fs_smtp_exist>-consnumber <> '000'.
            "Caso seja enviado um e-mail existente e seja uma alteração:
            <fs_cemail_smtp>-contact-task            = 'U'.
            <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
            <fs_cemail_smtp>-contact-data-e_mail     = is_alteracao-email_subst.
            "Marca alteração
            me->mt_fill_datax(
              EXPORTING
                is_data  = <fs_cemail_smtp>-contact-data
              CHANGING
                cs_datax = <fs_cemail_smtp>-contact-datax
            ).

          ELSE.
            "Caso o e-mail atual não é o e-mail enviado, mantém (sem alteração)
            <fs_cemail_smtp>-contact-task            = ''.
            <fs_cemail_smtp>-contact-data-consnumber = <fs_smtp_exist>-consnumber.
            <fs_cemail_smtp>-contact-data-e_mail     = <fs_smtp_exist>-e_mail.
            "Não marca alteração
            READ TABLE lt_smtp WITH KEY e_mail = is_sza1_d0100-smtp_addr TRANSPORTING NO FIELDS.
            IF sy-subrc <> 0.
              "Caso o email atual é o número enviado e não existe no cadastro, insere
              <fs_cemail_smtp>-contact-task            = 'I'.
              <fs_cemail_smtp>-contact-data-e_mail     = is_sza1_d0100-smtp_addr.
              "Marca alteração
              me->mt_fill_datax(
                EXPORTING
                  is_data  = <fs_cemail_smtp>-contact-data
                CHANGING
                  cs_datax = <fs_cemail_smtp>-contact-datax
              ).
              APPEND INITIAL LINE TO lt_smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
              <fs_smtp>-e_mail = is_sza1_d0100-smtp_addr.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

      "Caso não exista e-mail algum, incluir o e-mail enviado
      IF lines( lt_smtp ) = 0 AND is_sza1_d0100-smtp_addr IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail>).
        <fs_cemail>-contact-task           = 'I'.
        <fs_cemail>-contact-data-e_mail    = is_sza1_d0100-smtp_addr.
        " Marcar alteração
        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_cemail>-contact-data
          CHANGING
            cs_datax = <fs_cemail>-contact-datax
        ).
      ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***


**** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
*    IF NOT is_sza1_d0100-smtp_addr IS INITIAL.
*      DATA: lv_count TYPE i.
*      ADD 1 TO lv_count.
*      APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail>)." Comunicação

*      IF <fs_smtp_exist> IS NOT ASSIGNED.
*        <fs_cemail>-contact-task          = 'I'.
*      ELSEIF <fs_smtp_exist> IS ASSIGNED AND <fs_smtp_exist>-e_mail = is_sza1_d0100-smtp_addr.
*        <fs_cemail>-contact-task          = 'D'.
*      ELSE.
*        <fs_cemail>-contact-task          = im_object_task.
*      ENDIF.

*      <fs_cemail>-contact-data-e_mail     = is_sza1_d0100-smtp_addr.
*      <fs_cemail>-contact-data-std_no     = abap_true.
*      <fs_cemail>-contact-data-consnumber = lv_count.
*
*      <fs_cemail>-contact-datax-e_mail    = 'X'.
*      <fs_cemail>-contact-datax-std_no    = 'X'.
*
*      me->mt_fill_datax(
*        EXPORTING
*          is_data  = <fs_cemail>-contact-data
*        CHANGING
*          cs_datax = <fs_cemail>-contact-datax
*      ).
*
*      LOOP AT it_adsmtp ASSIGNING FIELD-SYMBOL(<fs_adsmtp>).
*        ADD 1 TO lv_count.
*        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING <fs_cemail>.
*        <fs_cemail>-contact-data-consnumber = lv_count.
*        <fs_cemail>-contact-task            = im_object_task.
*        <fs_cemail>-contact-data-e_mail     = <fs_adsmtp>-smtp_addr.
*
**          <fs_cemail>-contact-datax-e_mail    = 'X'.
*
*        me->mt_fill_datax(
*          EXPORTING
*            is_data  = <fs_cemail>-contact-data
*          CHANGING
*            cs_datax = <fs_cemail>-contact-datax
*        ).
*
*      ENDLOOP.
*    ENDIF.
**** US #184108 - MMSILVA - 30.05.2025 - Fim ***
    ENDIF.
  ENDMETHOD.


  METHOD mt_fill_part_cent_common_cust.

    cs_bp_data-partner-central_data-common-data-bp_control-category = im_bu_type.

    cs_bp_data-partner-central_data-common-data-bp_control-grouping  = is_bus_joel_main-creation_group. "ALRS

    IF NOT is_addr1_data-sort1 IS INITIAL.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_addr1_data-sort1.
    ELSE.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_kna1-sortl.
    ENDIF.
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm1     = abap_true.
    cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm2      = is_addr1_data-sort2.
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm2     = abap_true.
    cs_bp_data-partner-central_data-common-data-bp_centraldata-partnerlanguage  = sy-langu.
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.

    CASE im_bu_type.
      WHEN '1'. "Person
        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = ''.
        cs_bp_data-partner-central_data-common-data-bp_person-firstname           = is_kna1-name1.

        IF cs_bp_data-partner-central_data-common-data-bp_person-firstname IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_person-firstname          = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_person-lastname            = is_kna1-name2.

        IF cs_bp_data-partner-central_data-common-data-bp_person-lastname IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_person-lastname           = 'X'.
        ENDIF.

*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
        IF im_personal_data-birthdate IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-birthdate           = im_personal_data-birthdate.
          cs_bp_data-partner-central_data-common-datax-bp_person-birthdate          = 'X'.
        ENDIF.

        IF im_personal_data-birthplace IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-birthplace          = im_personal_data-birthplace.
          cs_bp_data-partner-central_data-common-datax-bp_person-birthplace         = 'X'.
        ENDIF.

        IF im_personal_data-maritalstatus IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-maritalstatus       = im_personal_data-maritalstatus.
          cs_bp_data-partner-central_data-common-datax-bp_person-maritalstatus      = 'X'.
        ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***

        cs_bp_data-partner-finserv_data-common-data-fsbp_person-proprty_st        = is_personal_data2-proprty_st.

        cs_bp_data-partner-central_data-common-data-bp_person-nationality         = is_kna1-land1.
        cs_bp_data-partner-central_data-common-datax-bp_person-nationality        = 'X'.
        cs_bp_data-partner-central_data-common-data-bp_person-correspondlanguage  = sy-langu.
        cs_bp_data-partner-central_data-common-datax-bp_person-correspondlanguage = 'X'.

        IF cs_bp_data-partner-central_data-common-data-bp_person-firstname IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_person-firstname  = is_addr1_data-name1.
          cs_bp_data-partner-central_data-common-datax-bp_person-firstname = 'X'.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_person-lastname IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_person-lastname  = is_addr1_data-name2.
          cs_bp_data-partner-central_data-common-datax-bp_person-lastname = 'X'.

        ENDIF.


      WHEN '2'. "Organization
        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = '0003'.
        cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_kna1-name1.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name1 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_kna1-name2.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name2 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name3  = is_kna1-name3.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name3 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name4  = is_kna1-name4.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name4 = 'X'.
        ENDIF.


        IF cs_bp_data-partner-central_data-common-data-bp_organization-name1 IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_addr1_data-name1.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name1 = 'X'.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name2 IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_addr1_data-name2.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name2 = 'X'.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name3 IS INITIAL AND
           is_addr1_data-name3 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name3 = is_addr1_data-name3.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name3 = 'X'.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name4 IS INITIAL AND
           is_addr1_data-name4 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name4 = is_addr1_data-name4.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name4 = 'X'.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD mt_fill_part_cent_common_sup.

    cs_bp_data-partner-central_data-common-data-bp_control-category = im_bu_type.
    IF im_bu_group IS NOT INITIAL.
      cs_bp_data-partner-central_data-common-data-bp_control-grouping  = im_bu_group. "ALRS
    ELSE.
      cs_bp_data-partner-central_data-common-data-bp_control-grouping  = is_bus_joel_main-creation_group. "ALRS
    ENDIF.
    IF NOT is_addr1_data-sort1 IS INITIAL.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_addr1_data-sort1.
    ELSE.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_lfa1-sortl.
    ENDIF.

    cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm2      = is_addr1_data-sort2.
    cs_bp_data-partner-central_data-common-data-bp_centraldata-partnerlanguage  = sy-langu.
*    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm1     = abap_true.
*    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm2     = abap_true.
*    cs_bp_data-partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.

    me->mt_fill_datax(
      EXPORTING
        is_data  = cs_bp_data-partner-central_data-common-data-bp_centraldata
      CHANGING
        cs_datax = cs_bp_data-partner-central_data-common-datax-bp_centraldata
    ).

    cs_bp_data-partner-central_data-common-data-bp_centraldata-centralarchivingflag = is_bp_central_data-centralarchivingflag. " Rubenilson - 01.10.24 #153421
    cs_bp_data-partner-central_data-common-data-bp_centraldata-centralblock         = is_bp_central_data-centralblock." Rubenilson - 01.10.24 #153421
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-centralarchivingflag = abap_true." Rubenilson - 01.10.24 #153421
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-centralblock         = abap_true." Rubenilson - 01.10.24 #153421

    CASE im_bu_type.
      WHEN '1'. "Person

        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = ''.
        cs_bp_data-partner-central_data-common-data-bp_person-firstname           = is_lfa1-name1.
        cs_bp_data-partner-central_data-common-data-bp_person-lastname            = is_lfa1-name2.
        cs_bp_data-partner-central_data-common-data-bp_person-nationality         = is_lfa1-land1.
*** US #184108 - MMSILVA - 30.05.2025 - Inicio ***
        IF is_lfa1-gbdat IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-birthdate           = is_lfa1-gbdat.
          cs_bp_data-partner-central_data-common-datax-bp_person-birthdate          = 'X'.
        ENDIF.
*** US #184108 - MMSILVA - 30.05.2025 - Fim ***
        cs_bp_data-partner-central_data-common-data-bp_person-birthplace          = is_lfa1-gbort.
        cs_bp_data-partner-central_data-common-data-bp_person-correspondlanguage  = sy-langu.
        cs_bp_data-partner-central_data-common-data-bp_person-maritalstatus       = is_personal_data-maritalstatus.
        cs_bp_data-partner-finserv_data-common-data-fsbp_person-proprty_st        = is_personal_data2-proprty_st.

        IF cs_bp_data-partner-central_data-common-data-bp_person-firstname IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.
                                                            "IR164101
*          cs_bp_data-partner-central_data-common-data-bp_person-firstname  = is_addr1_data-name1.
          cs_bp_data-partner-central_data-common-data-bp_person-lastname  = is_addr1_data-name1.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_person-lastname IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.
                                                            "IR164101
*          cs_bp_data-partner-central_data-common-data-bp_person-lastname  = is_addr1_data-name2.

        ENDIF.

*        cs_bp_data-partner-central_data-common-datax-bp_person-firstname          = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_person-lastname           = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_person-nationality        = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_person-correspondlanguage = 'X'.

        me->mt_fill_datax(
          EXPORTING
            is_data  = cs_bp_data-partner-central_data-common-data-bp_person
          CHANGING
            cs_datax = cs_bp_data-partner-central_data-common-datax-bp_person
        ).
        cs_bp_data-partner-central_data-common-datax-bp_person-firstname  = 'X'.

      WHEN '2'. "Organization
        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = '0003'.
        cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_lfa1-name1.
        cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_lfa1-name2.
        cs_bp_data-partner-central_data-common-data-bp_organization-name3  = is_lfa1-name3.
        cs_bp_data-partner-central_data-common-data-bp_organization-name4  = is_lfa1-name4.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name1 IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_addr1_data-name1.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name2 IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_addr1_data-name2.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name3 IS INITIAL AND
           is_addr1_data-name3 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name3 = is_addr1_data-name3.

        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name4 IS INITIAL AND
           is_addr1_data-name4 IS NOT INITIAL.

          cs_bp_data-partner-central_data-common-data-bp_organization-name4 = is_addr1_data-name4.

        ENDIF.

*        cs_bp_data-partner-central_data-common-datax-bp_organization-name1 = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_organization-name2 = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_organization-name3 = 'X'.
*        cs_bp_data-partner-central_data-common-datax-bp_organization-name4 = 'X'.

        me->mt_fill_datax(
          EXPORTING
            is_data  = cs_bp_data-partner-central_data-common-data-bp_organization
          CHANGING
            cs_datax = cs_bp_data-partner-central_data-common-datax-bp_organization
        ).

    ENDCASE.

  ENDMETHOD.


  METHOD MT_FILL_PART_CENT_ROLES_SUP.

    CONSTANTS c_valid_to TYPE d VALUE '99991231'.

    cs_bp_data-partner-central_data-role-current_state = abap_false. "-> Se deixar TRUE, a classe apaga as outras roles existentes

    APPEND INITIAL LINE TO cs_bp_data-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<fs_role>).
    <fs_role>-task              = im_object_task.
    <fs_role>-data_key          = im_role.
    <fs_role>-currently_valid   = abap_true.
*  <FS_ROLE>-DATA-ROLECATEGORY = IM_ROLE.
    <fs_role>-data-valid_from   = sy-datum.
    <fs_role>-data-valid_to     = c_valid_to.
    <fs_role>-datax-valid_from  = abap_true.
    <fs_role>-datax-valid_to    = abap_true.
    UNASSIGN <fs_role>.

  ENDMETHOD.


  METHOD MT_FILL_PART_CENT_TAX_CUST.

    IF NOT is_kna1-stcd1 IS INITIAL
    OR NOT is_kna1-stcd2 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<fs_taxnum>).
      <fs_taxnum>-task = im_object_task.
      IF is_kna1-stcd1 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_kna1-stcd1.
        <fs_taxnum>-data_key-taxtype = 'BR1'.
      ELSEIF is_kna1-stcd2 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_kna1-stcd2.
        <fs_taxnum>-data_key-taxtype   = 'BR2'.
        cs_bp_data-partner-central_data-taxnumber-common-data-nat_person = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_kna1-stcd3 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
***   US #184108 - MMSILVA - 02.06.2025  - Inicio
      IF is_kna1-kunnr IS NOT INITIAL.
        SELECT SINGLE stcd3 FROM kna1 INTO @DATA(ls_stcd3) WHERE kunnr = @is_kna1-kunnr.
        IF sy-subrc IS INITIAL AND ls_stcd3 IS INITIAL.
          <fs_taxnum>-task = 'I'.
        ELSE.
          <fs_taxnum>-task = im_object_task.
        ENDIF.
      ELSE.
        <fs_taxnum>-task = im_object_task.
      ENDIF.
***   US #184108 - MMSILVA - 02.06.2025  - Fim
      <fs_taxnum>-data_key-taxnumber = is_kna1-stcd3.
      <fs_taxnum>-data_key-taxtype = 'BR3'.
    ENDIF.

    IF NOT is_kna1-stcd4 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
      <fs_taxnum>-task = im_object_task.
      <fs_taxnum>-data_key-taxnumber = is_kna1-stcd4.
      <fs_taxnum>-data_key-taxtype = 'BR4'.
    ENDIF.

  ENDMETHOD.


  METHOD MT_FILL_PART_CENT_TAX_SUP.

    IF NOT is_lfa1-stcd1 IS INITIAL
   OR NOT is_lfa1-stcd2 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<fs_taxnum>).
      <fs_taxnum>-task = im_object_task.
      IF is_lfa1-stcd1 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_lfa1-stcd1.
        <fs_taxnum>-data_key-taxtype = 'BR1'.
      ELSEIF is_lfa1-stcd2 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_lfa1-stcd2.
        <fs_taxnum>-data_key-taxtype   = 'BR2'.
        cs_bp_data-partner-central_data-taxnumber-common-data-nat_person = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_lfa1-stcd3 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
* --> BUG #168893 - MMSILVA - 14.05.2025 - Inicio
      IF is_lfa1-lifnr IS NOT INITIAL.
        SELECT SINGLE stcd3 FROM lfa1 INTO @DATA(ls_stcd3) WHERE lifnr = @is_lfa1-lifnr.
        IF sy-subrc IS INITIAL AND ls_stcd3 IS INITIAL.
          <fs_taxnum>-task = 'I'.
        ELSE.
          <fs_taxnum>-task = im_object_task.
        ENDIF.
      ELSE.
        <fs_taxnum>-task = im_object_task.
      ENDIF.
* --> BUG #168893 - MMSILVA - 14.05.2025 - Fim
      "<fs_taxnum>-task = im_object_task. "BUG #168893 - MMSILVA - 14.05.2025 - Comentado devido gerar erro em casos que deve ser INCLUÍDO e não ATUALIZADO.
      <fs_taxnum>-data_key-taxnumber = is_lfa1-stcd3.
      <fs_taxnum>-data_key-taxtype = 'BR3'.
    ENDIF.

    IF NOT is_lfa1-stcd4 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
      <fs_taxnum>-task = im_object_task.
      <fs_taxnum>-data_key-taxnumber = is_lfa1-stcd4.
      <fs_taxnum>-data_key-taxtype = 'BR4'.
    ENDIF.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_ADDRESS.

*  CS_BP_DATA-customer-central_data-central-data-katr1  = IS_ADDR1_DATA-extension2.
*  CS_BP_DATA-customer-central_data-central-datax-katr1 = abap_true.
*  CS_BP_DATA-customer-central_data-central-data-katr6  = IS_ADDR1_DATA-extension1.
*  CS_BP_DATA-customer-central_data-central-datax-katr6 = abap_true.

    cs_bp_data-customer-central_data-central-data-katr1  = is_addr1_data-extension2.
    cs_bp_data-customer-central_data-central-datax-katr1 = abap_true.
    cs_bp_data-customer-central_data-central-data-katr6  = is_addr1_data-extension1.
    cs_bp_data-customer-central_data-central-datax-katr6 = abap_true.

    cs_bp_data-customer-central_data-address-task = im_object_task.
    cs_bp_data-customer-central_data-address-postal-data-name = is_kna1-name1.
    cs_bp_data-customer-central_data-address-postal-datax-name = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_2 = is_kna1-name2.
    cs_bp_data-customer-central_data-address-postal-datax-name_2 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_3 = is_kna1-name3.
    cs_bp_data-customer-central_data-address-postal-datax-name_3 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_4 = is_kna1-name4.
    cs_bp_data-customer-central_data-address-postal-datax-name_4 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-country = is_kna1-land1.
    cs_bp_data-customer-central_data-address-postal-datax-country = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-location = is_kna1-locco.
    cs_bp_data-customer-central_data-address-postal-datax-location = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-city = is_addr1_data-city1.
    cs_bp_data-customer-central_data-address-postal-datax-city = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-district = is_addr1_data-city2.
    cs_bp_data-customer-central_data-address-postal-datax-district = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-po_box = is_addr1_data-po_box.
    cs_bp_data-customer-central_data-address-postal-datax-po_box = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-postl_cod1 = is_addr1_data-post_code1.
    cs_bp_data-customer-central_data-address-postal-datax-postl_cod1 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-postl_cod2 = is_addr1_data-post_code2.
    cs_bp_data-customer-central_data-address-postal-datax-postl_cod2 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-region = is_kna1-regio.
    cs_bp_data-customer-central_data-address-postal-datax-region = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-sort1 = is_kna1-sortl.
    cs_bp_data-customer-central_data-address-postal-datax-sort1 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-street = is_addr1_data-street.
    cs_bp_data-customer-central_data-address-postal-datax-street = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-taxjurcode = is_kna1-txjcd.
    cs_bp_data-customer-central_data-address-postal-datax-taxjurcode = abap_true.
    MOVE-CORRESPONDING is_addr1_data TO cs_bp_data-customer-central_data-address-postal-data.
    cs_bp_data-customer-central_data-address-postal-data-langu = sy-langu.
    cs_bp_data-customer-central_data-address-postal-datax-langu = abap_true.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_BANKDETAIL.

    LOOP AT it_knbk ASSIGNING FIELD-SYMBOL(<fs_knbk>).

      APPEND VALUE #(
        task = 'M'
        data-bank_ctry     = 'BR'
        data-bank_key      = <fs_knbk>-bankl
        data-bank_acct     = <fs_knbk>-bankn
        data-ctrl_key      = <fs_knbk>-bkont
        data-accountholder = <fs_knbk>-koinh

        datax-bank_ctry     = abap_true
        datax-bank_key      = abap_true
        datax-bank_acct     = abap_true
        datax-ctrl_key      = abap_true
        datax-accountholder = abap_true

      ) TO cs_bp_data-partner-central_data-bankdetail-bankdetails.

    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_COMPANY.

    TYPES:
      BEGIN OF ty_knb1,
        kunnr TYPE knb1-kunnr,
        bukrs TYPE knb1-bukrs,
      END OF ty_knb1.

    DATA:
      lt_knb1   TYPE STANDARD TABLE OF ty_knb1,
      lt_knb1_p TYPE cvis_knb1_t,
      l_task    TYPE cmd_ei_company_task.

    SELECT kunnr bukrs
      INTO TABLE lt_knb1
      FROM knb1
      FOR ALL ENTRIES IN it_knb1
      WHERE kunnr EQ it_knb1-kunnr
        AND bukrs EQ it_knb1-bukrs.

    IF sy-subrc EQ 0.

      SORT lt_knb1 BY kunnr bukrs ASCENDING.

    ENDIF.

    lt_knb1_p[] = it_knb1[].

    SORT lt_knb1_p BY kunnr bukrs ASCENDING.

*    LOOP AT it_knb1 ASSIGNING FIELD-SYMBOL(<fs_knb1>).
    LOOP AT lt_knb1_p ASSIGNING FIELD-SYMBOL(<fs_knb1>).

      l_task = im_object_task.

      READ TABLE lt_knb1 TRANSPORTING NO FIELDS
        WITH KEY kunnr = <fs_knb1>-kunnr
                 bukrs = <fs_knb1>-bukrs
                 BINARY SEARCH.

      IF sy-subrc NE 0.

        l_task = 'I'.

      ENDIF.

      APPEND INITIAL LINE TO cs_bp_data-customer-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company>).
      <fs_company>-task     = l_task.
      <fs_company>-data_key = <fs_knb1>-bukrs.

      me->mt_move_data(
           EXPORTING
             is_from  = <fs_knb1>
           CHANGING
             cs_data  = <fs_company>-data
             cs_datax = <fs_company>-datax
         ).

      LOOP AT it_knbw ASSIGNING FIELD-SYMBOL(<fs_knbw>)
                      WHERE bukrs = <fs_knb1>-bukrs.
        APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING FIELD-SYMBOL(<fs_wtax_type>).
        <fs_wtax_type>-data_key-witht = <fs_knbw>-witht.
        <fs_wtax_type>-task = l_task.

        me->mt_move_data(
     EXPORTING
       is_from  = <fs_knbw>
     CHANGING
       cs_data  = <fs_wtax_type>-data
       cs_datax = <fs_wtax_type>-datax
   ).

        UNASSIGN <fs_wtax_type>.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT it_knbw ASSIGNING <fs_knbw>.
          APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING <fs_wtax_type>.
          <fs_wtax_type>-data_key-witht = <fs_knbw>-witht.
          <fs_wtax_type>-task = l_task.

          me->mt_move_data(
            EXPORTING
              is_from  = <fs_knbw>
            CHANGING
              cs_data  = <fs_wtax_type>-data
              cs_datax = <fs_wtax_type>-datax
          ).

          UNASSIGN <fs_wtax_type>.
        ENDLOOP.
      ENDIF.

      UNASSIGN <fs_company>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_CONTACT.

    IF is_kna1-telf1 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone
      ASSIGNING FIELD-SYMBOL(<fs_phone>).
      <fs_phone>-contact-data-telephone   = is_kna1-telf1.
      <fs_phone>-contact-datax-telephone  = abap_true.
      <fs_phone>-contact-data-tel_no      = is_kna1-telf1.
      <fs_phone>-contact-datax-tel_no     = abap_true.
    ELSEIF is_sza1_d0100-tel_number IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone ASSIGNING <fs_phone>.
      <fs_phone>-contact-data-tel_no      = is_sza1_d0100-tel_number.
      <fs_phone>-contact-datax-tel_no     = abap_true.
      <fs_phone>-contact-data-telephone   = is_sza1_d0100-tel_number.
      <fs_phone>-contact-datax-telephone  = abap_true.


      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
      <fs_smtp>-contact-task           = im_object_task.
      <fs_smtp>-contact-data-e_mail    = is_sza1_d0100-smtp_addr.
      <fs_smtp>-contact-datax-e_mail   = abap_true.

      LOOP AT it_adsmtp INTO DATA(is_adsmtp).
        APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-smtp-smtp ASSIGNING <fs_smtp>.
        <fs_smtp>-contact-task           = im_object_task.
        <fs_smtp>-contact-data-e_mail    = is_adsmtp-smtp_addr.
        <fs_smtp>-contact-datax-e_mail   = abap_true.
      ENDLOOP.

      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-contact-contacts ASSIGNING FIELD-SYMBOL(<fs_contact>).
      <fs_contact>-address_type_1-task    = im_object_task.
      APPEND INITIAL LINE TO <fs_contact>-address_type_1-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phones>).
      <fs_phones>-contact-data-telephone  = is_sza1_d0100-tel_number.
      <fs_phones>-contact-datax-telephone = abap_true.
      <fs_phones>-contact-data-tel_no     = is_sza1_d0100-tel_number.
      <fs_phones>-contact-datax-tel_no    = abap_true.

    ENDIF.
    IF is_kna1-telf2 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone
      ASSIGNING <fs_phone>.
      <fs_phone>-contact-data-telephone   = is_kna1-telf2.
      <fs_phone>-contact-datax-telephone  = abap_true.
      <fs_phone>-contact-data-tel_no      = is_kna1-telf2.
      <fs_phone>-contact-datax-tel_no     = abap_true.
    ENDIF.
    IF is_knvk IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-contact-contacts ASSIGNING <fs_contact>.
      <fs_contact>-task = im_object_task.
      MOVE-CORRESPONDING is_knvk TO <fs_contact>-data.
    ENDIF.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_DATA.

    me->mt_move_data(
      EXPORTING
        is_from  = is_kna1
      CHANGING
        cs_data  = cs_bp_data-customer-central_data-central-data
        cs_datax = cs_bp_data-customer-central_data-central-datax
    ).

    IF cs_bp_data-customer-central_data-central-data-ktokd IS NOT INITIAL.
      "ATENÇÃO!!!
      "Existe uma regra que impede que esse dado seja definido sem ser por customização.
      "Ver classe CVI_MAPPER->MAP_BPS_TO_VENDORS e procurar por "flexible handling of account groups"
      SELECT SINGLE bu_group
        FROM tbd001
        INTO cs_bp_data-partner-central_data-common-data-bp_control-grouping
      WHERE ktokd = cs_bp_data-customer-central_data-central-data-ktokd.
    ENDIF.
  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_HEADER.

    cs_bp_data-customer-header-object_task   = im_object_task.
    cs_bp_data-ensure_create-create_customer = im_create.
  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_SALES.

    LOOP AT it_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>).
      APPEND INITIAL LINE TO cs_bp_data-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
      <fs_sales>-task = im_object_task.
      <fs_sales>-data_key-vkorg = <fs_knvv>-vkorg.
      <fs_sales>-data_key-vtweg = <fs_knvv>-vtweg.
      <fs_sales>-data_key-spart = <fs_knvv>-spart.

      me->mt_move_data(
        EXPORTING
          is_from  = <fs_knvv>
        CHANGING
          cs_data  = <fs_sales>-data
          cs_datax = <fs_sales>-datax
      ).

      UNASSIGN <fs_sales>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_TAX.

    LOOP AT it_knvi ASSIGNING FIELD-SYMBOL(<fs_knvi>).
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-tax_ind-tax_ind ASSIGNING FIELD-SYMBOL(<fs_tax>).
      <fs_tax>-task = im_object_task.
      <fs_tax>-data_key-aland = <fs_knvi>-aland.
      <fs_tax>-data_key-tatyp = <fs_knvi>-tatyp.

      me->mt_move_data(
        EXPORTING
          is_from  = <fs_knvi>
        CHANGING
          cs_data  = <fs_tax>-data
          cs_datax = <fs_tax>-datax
      ).

      UNASSIGN <fs_tax>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_HEADER_SUP.

    CASE im_object_task.
      WHEN 'I'.
        cs_bp_data-partner-header-object_task                  = im_object_task.
        cs_bp_data-partner-header-object_instance-bpartnerguid = cl_system_uuid=>create_uuid_x16_static( ).

      WHEN 'U' OR 'M'.

        DATA: lv_guid TYPE bu_partner_guid.

        me->mt_get_bp_guid(
          EXPORTING
            im_partner      = im_partner              " Nº parceiro de negócios
          CHANGING
            cm_partner_guid = lv_guid                 " GUID de um parceiro de negócios
        ).

        cs_bp_data-partner-header-object_task                  = im_object_task.
        cs_bp_data-partner-header-object_instance-bpartner     = im_partner.
        cs_bp_data-partner-header-object_instance-bpartnerguid = lv_guid.

    ENDCASE.
  ENDMETHOD.


  METHOD mt_fill_part_sup_address.

    "ALRS
    cs_bp_data-vendor-central_data-central-data-scacd = is_lfa1-scacd.
    cs_bp_data-vendor-central_data-central-data-dlgrp = is_lfa1-dlgrp.
    cs_bp_data-vendor-central_data-central-data-gbdat = is_lfa1-gbdat.
    cs_bp_data-vendor-central_data-central-data-profs = is_lfa1-profs.
    cs_bp_data-vendor-central_data-central-data-gbort = is_lfa1-gbort.

    "ALRS
    cs_bp_data-vendor-central_data-address-task = im_object_task.
    cs_bp_data-vendor-central_data-address-postal-data-name = is_lfa1-name1.
    cs_bp_data-vendor-central_data-address-postal-data-name_2 = is_lfa1-name2.
    cs_bp_data-vendor-central_data-address-postal-data-name_3 = is_lfa1-name3.
    cs_bp_data-vendor-central_data-address-postal-data-name_4 = is_lfa1-name4.
    cs_bp_data-vendor-central_data-address-postal-data-country = is_lfa1-land1.
*  CS_BP_DATA-VENDOR-central_data-address-postal-data-location = IS_LFA1-locco.
*  CS_BP_DATA-VENDOR-central_data-address-postal-datax-location = abap_true.
    cs_bp_data-vendor-central_data-address-postal-data-city = is_addr1_data-city1.
    cs_bp_data-vendor-central_data-address-postal-data-district = is_addr1_data-city2.
    cs_bp_data-vendor-central_data-address-postal-data-po_box = is_addr1_data-po_box.
    cs_bp_data-vendor-central_data-address-postal-data-postl_cod1 = is_addr1_data-post_code1.
    cs_bp_data-vendor-central_data-address-postal-data-postl_cod2 = is_addr1_data-post_code2.
    cs_bp_data-vendor-central_data-address-postal-data-region = is_lfa1-regio.
    cs_bp_data-vendor-central_data-address-postal-data-sort1 = is_lfa1-sortl.
    cs_bp_data-vendor-central_data-address-postal-data-street = is_addr1_data-street.
    cs_bp_data-vendor-central_data-address-postal-data-taxjurcode = is_lfa1-txjcd.
    MOVE-CORRESPONDING is_addr1_data TO cs_bp_data-vendor-central_data-address-postal-data.
    cs_bp_data-vendor-central_data-address-postal-data-langu = sy-langu.

*    cs_bp_data-vendor-central_data-address-postal-datax-name = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-name_2 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-name_3 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-name_4 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-country = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-city = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-district = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-po_box = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-postl_cod1 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-postl_cod2 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-region = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-sort1 = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-street = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-taxjurcode = abap_true.
*    cs_bp_data-vendor-central_data-address-postal-datax-langu = abap_true.

    me->mt_fill_datax(
      EXPORTING
        is_data  = cs_bp_data-vendor-central_data-address-postal-data
      CHANGING
        cs_datax = cs_bp_data-vendor-central_data-address-postal-datax
    ).

    cs_bp_data-vendor-central_data-central-data-kraus = is_lfa1-kraus.
    cs_bp_data-vendor-central_data-central-data-stenr = is_lfa1-stenr. "US #172720 - MMSILVA - 01.04.2025
    cs_bp_data-vendor-central_data-central-data-revdb = is_lfa1-revdb.

    cs_bp_data-vendor-central_data-central-data-bahns = is_lfa1-bahns. "ALRS

    me->mt_fill_datax(
      EXPORTING
        is_data  = cs_bp_data-vendor-central_data-central-data
      CHANGING
        cs_datax = cs_bp_data-vendor-central_data-central-datax
    ).


*** inicio - Rubenilson - 01.10.24 - #153421
    cs_bp_data-vendor-central_data-central-data-nodel = is_lfa1-nodel.
    cs_bp_data-vendor-central_data-central-data-loevm = is_lfa1-loevm.
    cs_bp_data-vendor-central_data-central-data-sperz = is_lfa1-sperz.
    cs_bp_data-vendor-central_data-central-data-sperr = is_lfa1-sperr.
    cs_bp_data-vendor-central_data-central-data-sperm = is_lfa1-sperm.

    cs_bp_data-vendor-central_data-central-datax-nodel =
    cs_bp_data-vendor-central_data-central-datax-loevm =
    cs_bp_data-vendor-central_data-central-datax-sperz =
    cs_bp_data-vendor-central_data-central-datax-sperr =
    cs_bp_data-vendor-central_data-central-datax-sperm = abap_true.
*** Fim - Rubenilson - 01.10.24 - #153421

*** inicio - Rubenilson - 14.10.24 - #155127
    cs_bp_data-vendor-central_data-central-data-j_1kftind  = is_lfa1-j_1kftind.
    cs_bp_data-vendor-central_data-central-data-j_1kfrepre = is_lfa1-j_1kfrepre .

    cs_bp_data-vendor-central_data-central-datax-j_1kftind  =
    cs_bp_data-vendor-central_data-central-datax-j_1kfrepre = abap_true.
*** Fim - Rubenilson - 14.10.24 - #155127

  ENDMETHOD.


  METHOD MT_FILL_PART_SUP_BANKDETAIL.
*** US #184108 - MMSILVA - 13.06.2025 - Ini ***
    DATA: ls_but0bk TYPE but0bk.
*** US #184108 - MMSILVA - 13.06.2025 - Fim ***

    LOOP AT it_lfbk ASSIGNING FIELD-SYMBOL(<fs_lfbk>).
*** US #184108 - MMSILVA - 13.06.2025 - Ini ***
      CLEAR: ls_but0bk.
      SELECT SINGLE * FROM but0bk INTO @ls_but0bk WHERE bkvid = @<fs_lfbk>-koinh.
*** US #184108 - MMSILVA - 13.06.2025 - Fim ***

      INSERT INITIAL LINE INTO TABLE cs_bp_data-partner-central_data-bankdetail-bankdetails
        ASSIGNING FIELD-SYMBOL(<fs_bankdetails>).

*** US #184108 - MMSILVA - 13.06.2025 - Ini ***
      "Inclusão
      IF ls_but0bk IS INITIAL.
        <fs_bankdetails>-task     = 'M'.
        <fs_bankdetails>-data_key = <fs_lfbk>-koinh.
      "Exclusão
      ELSEIF <fs_lfbk>-bankl = ls_but0bk-bankl AND <fs_lfbk>-bankn = ls_but0bk-bankn.
        <fs_bankdetails>-task     = 'D'.
        <fs_bankdetails>-data_key = <fs_lfbk>-koinh.
      "Alteração
      ELSE.
        <fs_bankdetails>-task     = 'U'.
        <fs_bankdetails>-data_key = <fs_lfbk>-koinh.
      ENDIF.
*** US #184108 - MMSILVA - 13.06.2025 - Fim ***

      <fs_bankdetails>-data-bank_ctry     = 'BR'.
      <fs_bankdetails>-data-bank_key      = <fs_lfbk>-bankl.
      <fs_bankdetails>-data-bank_acct     = <fs_lfbk>-bankn.

*** US #184108 - MMSILVA - 13.06.2025 - Ini ***
      IF <fs_lfbk>-bkont IS NOT INITIAL.
        <fs_bankdetails>-data-ctrl_key      = <fs_lfbk>-bkont.
        "Limpar campo
      ELSE.
        <fs_bankdetails>-data-ctrl_key      = ' '.
      ENDIF.
*** US #184108 - MMSILVA - 13.06.2025 - Fim ***

      <fs_bankdetails>-data-accountholder = <fs_lfbk>-koinh.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_bankdetails>-data
        CHANGING
          cs_datax = <fs_bankdetails>-datax
      ).

*      APPEND VALUE #(
*        task = 'M'
*        data-bank_ctry     = 'BR'
*        data-bank_key      = <fs_lfbk>-bankl
*        data-bank_acct     = <fs_lfbk>-bankn
*        data-ctrl_key      = <fs_lfbk>-bkont
*        data-accountholder = <fs_lfbk>-koinh
*
*        datax-bank_ctry     = abap_true
*        datax-bank_key      = abap_true
*        datax-bank_acct     = abap_true
*        datax-ctrl_key      = abap_true
*        datax-accountholder = abap_true
*
*      ) TO cs_bp_data-partner-central_data-bankdetail-bankdetails.

    ENDLOOP.

  ENDMETHOD.


  METHOD mt_fill_part_sup_company.

    TYPES:
      BEGIN OF ty_lfb1,
        lifnr TYPE lfb1-lifnr,
        bukrs TYPE lfb1-bukrs,
      END OF ty_lfb1.

    DATA:
      lt_lfb1   TYPE STANDARD TABLE OF ty_lfb1,
      lt_lfb1_p TYPE cvis_lfb1_t,
      l_task    TYPE vmd_ei_company_task,
      lt_lfbw   TYPE TABLE OF lfbw, "US #184108 - MMSILVA - 29.05.2025
      it_lfbw_p TYPE TABLE OF lfbw. "US #184108 - MMSILVA - 29.05.2025

*   US #184108 - MMSILVA - 29.05.2025 - Inicio
    it_lfbw_p = it_lfbw.

    SELECT *
      FROM lfbw
      INTO TABLE lt_lfbw
      FOR ALL ENTRIES IN it_lfb1
      WHERE lifnr EQ it_lfb1-lifnr
      AND   bukrs EQ it_lfb1-bukrs.
    IF sy-subrc EQ 0.
      LOOP AT lt_lfbw INTO DATA(ls_lfbw).
        READ TABLE it_lfb1 INTO DATA(ls_lfb1) INDEX 1.
        ls_lfbw-lifnr = ls_lfb1-lifnr.
        APPEND ls_lfbw TO it_lfbw_p.
      ENDLOOP.
    ENDIF.

    SORT it_lfbw_p BY lifnr DESCENDING.
*   US #184108 - MMSILVA - 29.05.2025 - Fim

    SELECT lifnr bukrs
      INTO TABLE lt_lfb1
      FROM lfb1
      FOR ALL ENTRIES IN it_lfb1
      WHERE lifnr EQ it_lfb1-lifnr
        AND bukrs EQ it_lfb1-bukrs.

    IF sy-subrc EQ 0.

      SORT lt_lfb1 BY lifnr bukrs ASCENDING.

    ENDIF.

    lt_lfb1_p[] = it_lfb1[].

    SORT lt_lfb1_p BY lifnr bukrs ASCENDING.

    LOOP AT lt_lfb1_p ASSIGNING FIELD-SYMBOL(<fs_lfb1>).

      l_task = im_object_task.

      READ TABLE lt_lfb1 TRANSPORTING NO FIELDS
        WITH KEY lifnr = <fs_lfb1>-lifnr
                 bukrs = <fs_lfb1>-bukrs
                 BINARY SEARCH.

      IF sy-subrc NE 0.

        l_task = 'I'.

      ENDIF.

      APPEND INITIAL LINE TO cs_bp_data-vendor-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company>).
      <fs_company>-task     = l_task.
      <fs_company>-data_key = <fs_lfb1>-bukrs.

      me->mt_move_data(
        EXPORTING
          is_from  = <fs_lfb1>
        CHANGING
          cs_data  = <fs_company>-data
          cs_datax = <fs_company>-datax
      ).

*** Inicio - Rubenilson - 01.10.24 #153421
      <fs_company>-datax-nodel =
      <fs_company>-datax-sperr =
      <fs_company>-datax-loevm = abap_true.
*** Fim - Rubenilson - 01.10.24 #153421

      LOOP AT it_lfbw_p ASSIGNING FIELD-SYMBOL(<fs_lfbw>) "US #184108 - MMSILVA - 29.05.2025 - Alterado de it_lfbw PARA it_lfbw_p
                      WHERE bukrs = <fs_lfb1>-bukrs.
        APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING FIELD-SYMBOL(<fs_wtax_type>).
        <fs_wtax_type>-data_key-witht = <fs_lfbw>-witht.
***     US #184108 - MMSILVA - 29.05.2025 - Inicio *** Alterado devido não ser possível atualizar, apenas inserir ou deletar
        IF <fs_lfbw>-lifnr IS NOT INITIAL.
          <fs_wtax_type>-task = 'D'.
        ELSE.
          <fs_wtax_type>-task = 'I'.
        ENDIF.
***     US #184108 - MMSILVA - 29.05.2025 - Fim *** Alterado devido não ser possível atualizar, apenas inserir ou deletar

        me->mt_move_data(
          EXPORTING
            is_from  = <fs_lfbw>
          CHANGING
            cs_data  = <fs_wtax_type>-data
            cs_datax = <fs_wtax_type>-datax
        ).

        UNASSIGN <fs_wtax_type>.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT it_lfbw_p ASSIGNING <fs_lfbw>. "US #184108 - MMSILVA - 29.05.2025 - Alterado de it_lfbw PARA it_lfbw_p
          APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING <fs_wtax_type>.
          <fs_wtax_type>-data_key-witht = <fs_lfbw>-witht.
***     US #184108 - MMSILVA - 29.05.2025 - Inicio *** Alterado devido não ser possível atualizar, apenas inserir ou deletar
          IF <fs_lfbw>-lifnr IS NOT INITIAL.
            <fs_wtax_type>-task = 'D'.
          ELSE.
            <fs_wtax_type>-task = 'I'.
          ENDIF.
***     US #184108 - MMSILVA - 29.05.2025 - Fim *** Alterado devido não ser possível atualizar, apenas inserir ou deletar

          me->mt_move_data(
            EXPORTING
              is_from  = <fs_lfbw>
            CHANGING
              cs_data  = <fs_wtax_type>-data
              cs_datax = <fs_wtax_type>-datax
          ).

          UNASSIGN <fs_wtax_type>.
        ENDLOOP.
      ENDIF.

      UNASSIGN <fs_company>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_SUP_CONTACT.

    IF is_lfa1-telf1 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-address-communication-phone-phone
      ASSIGNING FIELD-SYMBOL(<fs_phone>).
      <fs_phone>-contact-data-telephone = is_lfa1-telf1.
      <fs_phone>-contact-data-tel_no = is_lfa1-telf1.

*      <fs_phone>-contact-datax-telephone = abap_true.
*      <fs_phone>-contact-datax-tel_no = abap_true.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_phone>-contact-data
        CHANGING
          cs_datax = <fs_phone>-contact-datax
      ).

    ELSEIF is_sza1_d0100-tel_number IS NOT INITIAL.

      APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-address-communication-phone-phone ASSIGNING <fs_phone>.
      <fs_phone>-contact-task           = im_object_task.
      <fs_phone>-contact-data-tel_no    = is_sza1_d0100-tel_number.
      <fs_phone>-contact-data-telephone = is_sza1_d0100-tel_number.

*      <fs_phone>-contact-datax-tel_no   = abap_true.
*      <fs_phone>-contact-datax-telephone   = abap_true.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_phone>-contact-data
        CHANGING
          cs_datax = <fs_phone>-contact-datax
      ).

      APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-address-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
      <fs_smtp>-contact-task           = im_object_task.
      <fs_smtp>-contact-data-e_mail    = is_sza1_d0100-smtp_addr.

*      <fs_smtp>-contact-datax-e_mail   = abap_true.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_smtp>-contact-data
        CHANGING
          cs_datax = <fs_smtp>-contact-datax
      ).

      LOOP AT gt_adsmtp INTO DATA(gs_adsmtp).
        APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-address-communication-smtp-smtp ASSIGNING <fs_smtp>.
        <fs_smtp>-contact-task           = im_object_task.
        <fs_smtp>-contact-data-e_mail    = gs_adsmtp-smtp_addr.

*        <fs_smtp>-contact-datax-e_mail   = abap_true.

        me->mt_fill_datax(
          EXPORTING
            is_data  = <fs_smtp>-contact-data
          CHANGING
            cs_datax = <fs_smtp>-contact-datax
        ).

      ENDLOOP.

      APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-contact-contacts ASSIGNING FIELD-SYMBOL(<fs_contact>).
      <fs_contact>-address_type_1-task = im_object_task.
      APPEND INITIAL LINE TO <fs_contact>-address_type_1-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phones>).
      <fs_phones>-contact-data-telephone  = is_sza1_d0100-tel_number.
      <fs_phones>-contact-data-tel_no     = is_sza1_d0100-tel_number.

*      <fs_phones>-contact-datax-telephone = abap_true.
*      <fs_phones>-contact-datax-tel_no    = abap_true.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_phones>-contact-data
        CHANGING
          cs_datax = <fs_phones>-contact-datax
      ).

    ENDIF.
    IF is_lfa1-telf2 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-vendor-central_data-address-communication-phone-phone
      ASSIGNING <fs_phone>.
      <fs_phone>-contact-data-telephone = is_lfa1-telf2.
      <fs_phone>-contact-data-tel_no = is_lfa1-telf2.

*      <fs_phone>-contact-datax-telephone = abap_true.
*      <fs_phone>-contact-datax-tel_no = abap_true.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_phone>-contact-data
        CHANGING
          cs_datax = <fs_phone>-contact-datax
      ).

    ENDIF.
*  IF ps_knvk IS NOT INITIAL.
*    APPEND INITIAL LINE TO CS_BP_DATA-VENDOR-central_data-contact-contacts ASSIGNING <fs_contact>.
*    <fs_contact>-task = IM_OBJECT_TASK.
*    MOVE-CORRESPONDING ps_knvk TO <fs_contact>-data.
*  ENDIF.
  ENDMETHOD.


  METHOD MT_FILL_PART_SUP_DATA.

    me->mt_move_data(
      EXPORTING
        is_from  = is_rf02k
      CHANGING
        cs_data  = cs_bp_data-vendor-central_data-central-data
        cs_datax = cs_bp_data-vendor-central_data-central-datax
    ).

    IF cs_bp_data-vendor-central_data-central-data-ktokk IS NOT INITIAL.
      "ATENÇÃO!!!
      "Existe uma regra que impede que esse dado seja definido sem ser por customização.
      "Ver classe CVI_MAPPER->MAP_BPS_TO_VENDORS e procurar por "flexible handling of account groups"
      SELECT SINGLE bu_group
        FROM tbd001
        INTO cs_bp_data-partner-central_data-common-data-bp_control-grouping
      WHERE ktokd = cs_bp_data-vendor-central_data-central-data-ktokk.
    ENDIF.
  ENDMETHOD.


  METHOD MT_FILL_PART_SUP_HEADER.

    cs_bp_data-vendor-header-object_task   = im_object_task.
    cs_bp_data-ensure_create-create_vendor = im_create.
  ENDMETHOD.


  METHOD mt_fill_part_sup_purch_org.


    LOOP AT it_lfm1 ASSIGNING FIELD-SYMBOL(<fs_lfm1>).

      INSERT INITIAL LINE INTO TABLE cs_bp_data-vendor-purchasing_data-purchasing
        ASSIGNING FIELD-SYMBOL(<fs_purchasing>).

      <fs_purchasing>-task = 'M'.
      <fs_purchasing>-data_key-ekorg = <fs_lfm1>-ekorg.
      <fs_purchasing>-data-waers = <fs_lfm1>-waers. "ALRS

      MOVE-CORRESPONDING:
        <fs_lfm1> TO <fs_purchasing>-data.

      me->mt_fill_datax(
        EXPORTING
          is_data  = <fs_purchasing>-data
        CHANGING
          cs_datax = <fs_purchasing>-datax
      ).

      <fs_purchasing>-datax-sperm = abap_true."Rubenilson - 01.10.24 #153421

      UNASSIGN <fs_purchasing>.

    ENDLOOP.


  ENDMETHOD.


  METHOD MT_GET_BP_FROM_CUSTOMER.

    CLEAR cm_partner.

    SELECT SINGLE partner_guid
        FROM cvi_cust_link
        INTO @DATA(lv_partner_guid)
        WHERE customer = @im_kunnr.
    IF sy-subrc = 0.
      CALL FUNCTION 'BUPA_NUMBERS_GET'
        EXPORTING
          iv_partner_guid = lv_partner_guid
        IMPORTING
          ev_partner      = cm_partner.
    ELSE.
      SELECT SINGLE partner
        FROM bd001 INTO @cm_partner
        WHERE kunnr = @im_kunnr.
    ENDIF.

  ENDMETHOD.


  METHOD MT_GET_BP_FROM_VENDOR.

    CLEAR cm_partner.

    SELECT SINGLE partner_guid
        FROM cvi_vend_link
        INTO @DATA(lv_partner_guid)
        WHERE vendor = @im_lifnr.
    IF sy-subrc = 0.
      CALL FUNCTION 'BUPA_NUMBERS_GET'
        EXPORTING
          iv_partner_guid = lv_partner_guid
        IMPORTING
          ev_partner      = cm_partner.
    ELSE.
      SELECT SINGLE partner
          FROM bc001
          INTO @cm_partner
          WHERE lifnr = @im_lifnr.
    ENDIF.

  ENDMETHOD.


  METHOD MT_GET_BP_GUID.

    CLEAR cm_partner_guid.
    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner      = im_partner
      IMPORTING
        ev_partner_guid = cm_partner_guid.

  ENDMETHOD.


  METHOD MT_MOVE_DATA.

    DATA lo_structure TYPE REF TO cl_abap_structdescr.
    DATA lo_exception TYPE REF TO cx_root.
    DATA lt_fields    TYPE abap_component_tab.

    FIELD-SYMBOLS <fs_f_from> TYPE any.
    FIELD-SYMBOLS <fs_f_data> TYPE any.
    FIELD-SYMBOLS <fs_f_datax> TYPE any.

    lo_structure ?= cl_abap_typedescr=>describe_by_data( cs_data ).
    lt_fields = lo_structure->get_components( ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

      TRY.
          UNASSIGN <fs_f_from>.
          ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE is_from TO <fs_f_from>.
          CHECK sy-subrc = 0.
*          CHECK <fs_f_from> IS NOT INITIAL.
          IF <fs_f_from> IS NOT INITIAL OR
             ( <fs_f_from> IS INITIAL AND <fs_fields>-name EQ 'ZAHLS' ). " Considering Unlocking

            UNASSIGN <fs_f_data>.
            UNASSIGN <fs_f_datax>.
            ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE cs_data TO <fs_f_data>.
            CHECK sy-subrc = 0.
            ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE cs_datax TO <fs_f_datax>.
            CHECK sy-subrc = 0.
            <fs_f_data> = <fs_f_from>.
            <fs_f_datax> = abap_true.

          ENDIF.

        CATCH cx_root INTO lo_exception.
          CONTINUE.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.


  METHOD MT_RETURN_MESSAGE_GENERIC.

    DATA lv_msg TYPE bapi_msg.
    lv_msg = im_msg.

    APPEND INITIAL LINE TO ct_return ASSIGNING FIELD-SYMBOL(<fs_return>).
    <fs_return>-type    = im_type.
    <fs_return>-id      = 'CVI_API'.
    <fs_return>-number  = '054'.
    <fs_return>-message = lv_msg.
    <fs_return>-message_v1 = lv_msg(50).
    <fs_return>-message_v2 = lv_msg+50(50).
    <fs_return>-message_v3 = lv_msg+100(50).
    <fs_return>-message_v4 = lv_msg+150(50).
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD MT_SET_DATA_BY_SHDB.

    "Variaveis
    DATA: lv_partner TYPE bu_partner,
          lv_bp_type TYPE bu_type.

    "Estruturas
    DATA: ls_addr1_data TYPE addr1_data,
          ls_adsmtp     TYPE adsmtp,
          ls_kna1       TYPE kna1,
          ls_lfa1       TYPE lfa1,
          ls_lfb1       TYPE lfb1,
          ls_rf02k      TYPE rf02k,
          ls_knb1       TYPE knb1,
          ls_knbw       TYPE knbw,
          ls_knvi       TYPE knvi,
          ls_knvk       TYPE knvk,
          ls_knvv       TYPE knvv,
          ls_rf02d      TYPE rf02d,
          ls_sza1_d0100 TYPE sza1_d0100,
          ls_but000     TYPE but000.

    "Tabelas
    DATA: lt_bdc    TYPE bdcdata_tab,
          lt_lfbk   TYPE cvis_ei_bankdetail_t,
          lt_adsmtp TYPE bbpt_er_adsmtp,
          lt_knb1   TYPE cvis_knb1_t,
          lt_knbw   TYPE cvis_knbw_t,
          lt_knvi   TYPE cvis_knvi_t,
          lt_knvv   TYPE cvis_knvv_t,
          lt_return TYPE bapiret2_t.


    lt_bdc[] = it_bdcdata[].

    "Elimina registros desnecessários para facilitar caso precise depurar.
    me->mt_shdb_clear_bdc(
      CHANGING
        et_result_tab = lt_bdc               " Ctg.tabela p/BDCDATA
    ).

    "Mapeia SHDB nas estruturas
    me->mt_shdb_parse_bdc(
      EXPORTING
        it_bdc        = lt_bdc                  " destino lógico (indicado em chamada de função)
      CHANGING
        cs_addr1_data = gs_addr1_data                  " Estrutura de transferência para endereços
        cs_adsmtp     = gs_adsmtp                 " Estrutura transf.p/endereços SMTP (admin.endereços central)
        cs_kna1       = gs_kna1                 " Mestre de clientes (parte geral)
        cs_lfa1       = gs_lfa1                 " Mestre de fornecedores (parte geral)
        ct_lfbk       = gt_lfbk
        ct_lfb1       = gt_lfb1                 " Mestre de fornecedores (empresa)
        ct_lfbw       = gt_lfbw                 " Mestre de fornecedores (empresa)
        cs_rf02k      = gs_rf02k                 " Atual.dados mestre fornecedor: campos de tela e operativos
        cs_knb1       = gs_knb1                  " Mestre de clientes (empresa)
        cs_knbw       = gs_knbw                 " Mestre de clientes (ctgs.de impostos retidos na fonte) X
        cs_knvi       = gs_knvi                 " Mestre de clientes - indicadores de impostos
        cs_knvk       = gs_knvk
        cs_knvv       = gs_knvv
        cs_rf02d      = gs_rf02d
        cs_sza1_d0100 = gs_sza1_d0100
        cs_bus_joel_main = gs_bus_joel_main "ALRS
        ct_adsmtp     = gt_adsmtp
        ct_knb1       = gt_knb1
        ct_knbw       = gt_knbw
        ct_knvi       = gt_knvi
        ct_knvv       = gt_knvv
        ct_knbk       = gt_knbk
        ct_lfm1       = gt_lfm1
    ).

  ENDMETHOD.


  METHOD MT_SET_DATA_DIRECTLY.

    gs_addr1_data   = is_addr1_data.
    gs_adsmtp       = is_adsmtp.
    gs_kna1         = is_kna1.
    gs_knb1         = is_knb1.
    gs_knbw         = is_knbw.
    gs_knvi         = is_knvi.
    gs_knvk         = is_knvk.
    gs_knvv         = is_knvv.
    gs_lfa1         = is_lfa1.
    gt_lfb1         = it_lfb1.
    gt_lfbw         = it_lfbw.
    gs_rf02d        = is_rf02d.
    gs_rf02k        = is_rf02k.
    gs_sza1_d0100   = is_sza1_d0100.
    gt_adsmtp       = it_adsmtp.
    gt_knb1         = it_knb1.
    gt_knbw         = it_knbw.
    gt_knvi         = it_knvi.
    gt_knvv         = it_knvv.
    gt_lfbk         = it_lfbk.
    gt_lfm1         = it_lfm1.

  ENDMETHOD.


  METHOD MT_SHDB_CLEAR_BDC.

    DELETE et_result_tab WHERE program IS NOT INITIAL.
    DELETE et_result_tab WHERE fnam IS INITIAL.
    DELETE et_result_tab WHERE fnam = 'BDC_CURSOR'.
    DELETE et_result_tab WHERE fnam = 'BDC_OKCODE'.

  ENDMETHOD.


  METHOD MT_SHDB_CONVERT_RETURN_BDCMSG.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      APPEND INITIAL LINE TO ct_bdcmsg ASSIGNING FIELD-SYMBOL(<fs_bdcmsg>).
      <fs_bdcmsg>-tcode   = im_tcode.
      <fs_bdcmsg>-dyname  = ''.
      <fs_bdcmsg>-dynumb  = ''.
      <fs_bdcmsg>-msgtyp  = <fs_return>-type.
      <fs_bdcmsg>-msgspra = sy-langu.
      <fs_bdcmsg>-msgid   = <fs_return>-id.
      <fs_bdcmsg>-msgnr   = <fs_return>-number.
      <fs_bdcmsg>-msgv1   = <fs_return>-message_v1.
      <fs_bdcmsg>-msgv2   = <fs_return>-message_v2.
      <fs_bdcmsg>-msgv3   = <fs_return>-message_v3.
      <fs_bdcmsg>-msgv4   = <fs_return>-message_v4.
      <fs_bdcmsg>-env     = ''.
      <fs_bdcmsg>-fldname = <fs_return>-field.
      UNASSIGN <fs_bdcmsg>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_SHDB_MOVE_VALUE.

    DATA lo_descr_ref TYPE REF TO cl_abap_typedescr.
    DATA lv_aux TYPE string.
    DATA lv_aux_date TYPE d.

    "Determina qual o tipo da variável destino:
    lo_descr_ref = cl_abap_typedescr=>describe_by_data( cm_to ).
    CASE lo_descr_ref->type_kind.
      WHEN 'D'. "DATA
        lv_aux = im_from.
        REPLACE ALL OCCURRENCES OF '/' IN lv_aux WITH ''.
        REPLACE ALL OCCURRENCES OF '.' IN lv_aux WITH ''.
        lv_aux_date = lv_aux.
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = lv_aux_date
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          lv_aux_date = |{ lv_aux_date+4(4) }{ lv_aux_date+2(2) }{ lv_aux_date(2) }|.
          CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
            EXPORTING
              date                      = lv_aux_date
            EXCEPTIONS
              plausibility_check_failed = 1
              OTHERS                    = 2.
        ENDIF.
        IF sy-subrc = 0.
          cm_to = lv_aux_date.
        ENDIF.

      WHEN OTHERS.
        cm_to = im_from.
    ENDCASE.

  ENDMETHOD.


  METHOD MT_SHDB_PARSE_BDC.

    TYPES:
      BEGIN OF y_bdc_tab,
        table(40)  TYPE c,
        reccord(3) TYPE n,
        field(40)  TYPE c,
        value      TYPE bdcdata-fnam,
      END OF y_bdc_tab.

    DATA ls_knb1 TYPE knb1.
    DATA ls_knvi TYPE knvi.
    DATA ls_knvv TYPE knvv.
    DATA ls_knbw TYPE knbw.
    DATA ls_knbk TYPE knbk.
    DATA ls_lfbk TYPE lfbk.
    DATA ls_lfb1 TYPE lfb1.
    DATA ls_lfbw TYPE lfbw.
    DATA ls_lfm1 TYPE lfm1.

    DATA v_table_name   TYPE bdcdata-fnam.
    DATA v_struct_name  TYPE bdcdata-fnam.
    DATA v_field_name   TYPE bdcdata-fnam.
    DATA v_reccord      TYPE bdcdata-fnam.
    DATA lt_bdc_tab     TYPE TABLE OF y_bdc_tab.
    DATA lt_bdc_tab_aux TYPE TABLE OF y_bdc_tab.

    FIELD-SYMBOLS <fs_struct> TYPE any.
    FIELD-SYMBOLS <fs_field>  TYPE any.
    FIELD-SYMBOLS <fs_table>  TYPE STANDARD TABLE.

    LOOP AT it_bdc ASSIGNING FIELD-SYMBOL(<fs_bdc>).
      CLEAR v_struct_name.
      CLEAR v_field_name.
      IF NOT <fs_bdc>-fnam CA '()' AND
         NOT <fs_bdc>-fnam CS 'LFB1' AND
         NOT <fs_bdc>-fnam CS 'LFBW' AND
         NOT <fs_bdc>-fnam CS 'LFM1'.
        "ESTRUTURAS
        SPLIT <fs_bdc>-fnam AT '-' INTO v_struct_name v_field_name.
        v_struct_name = |CS_{ v_struct_name }|.
        ASSIGN (v_struct_name) TO <fs_struct>.
        IF <fs_struct> IS ASSIGNED.
          ASSIGN COMPONENT v_field_name OF STRUCTURE <fs_struct> TO <fs_field>.
          IF <fs_field> IS ASSIGNED.
            me->mt_shdb_move_value(
              EXPORTING
                im_from = CONV #( <fs_bdc>-fval )
              CHANGING
                cm_to   = <fs_field>
            ).
            UNASSIGN <fs_field>.
          ENDIF.
          UNASSIGN <fs_struct>.
        ENDIF.

      ELSE.
        "TABELAS
        SPLIT <fs_bdc>-fnam AT '-' INTO v_struct_name v_field_name.
        SPLIT v_field_name AT '(' INTO v_field_name v_reccord.
        REPLACE ALL OCCURRENCES OF ')' IN v_reccord WITH space.
        CONDENSE v_reccord NO-GAPS.
        APPEND INITIAL LINE TO lt_bdc_tab ASSIGNING FIELD-SYMBOL(<fs_bdc_tab>).
        <fs_bdc_tab>-table   = v_struct_name.
        <fs_bdc_tab>-field   = v_field_name.
        <fs_bdc_tab>-reccord = v_reccord.
        <fs_bdc_tab>-value   = <fs_bdc>-fval.
        UNASSIGN <fs_bdc_tab>.
      ENDIF.
    ENDLOOP.

    SORT lt_bdc_tab BY table reccord field.
    lt_bdc_tab_aux[] = lt_bdc_tab[].
    DELETE ADJACENT DUPLICATES FROM lt_bdc_tab_aux COMPARING table reccord.

    LOOP AT lt_bdc_tab_aux ASSIGNING FIELD-SYMBOL(<fs_bdc_tab_aux>).

      CLEAR v_table_name.
      v_table_name = |CT_{ <fs_bdc_tab_aux>-table }|.
      ASSIGN (v_table_name) TO <fs_table>.
      CHECK <fs_table> IS ASSIGNED.

      CLEAR v_struct_name.
      v_struct_name = |LS_{ <fs_bdc_tab_aux>-table }|.
      ASSIGN (v_struct_name) TO <fs_struct>.
      CHECK <fs_struct> IS ASSIGNED.

      LOOP AT lt_bdc_tab ASSIGNING <fs_bdc_tab>
                         WHERE table   = <fs_bdc_tab_aux>-table
                           AND reccord = <fs_bdc_tab_aux>-reccord.
        ASSIGN COMPONENT <fs_bdc_tab>-field OF STRUCTURE <fs_struct> TO <fs_field>.
        CHECK <fs_field> IS ASSIGNED.
        me->mt_shdb_move_value(
          EXPORTING
            im_from = CONV #( <fs_bdc_tab>-value )
          CHANGING
            cm_to   = <fs_field>
        ).
        UNASSIGN <fs_field>.
      ENDLOOP.

      APPEND <fs_struct> TO <fs_table>.

      UNASSIGN <fs_bdc_tab>.
      UNASSIGN <fs_table>.
      UNASSIGN <fs_struct>.

    ENDLOOP.
    UNASSIGN <fs_bdc_tab_aux>.

    IF cs_addr1_data IS INITIAL.
      IF cs_kna1 IS NOT INITIAL.
        cs_addr1_data-country    = cs_kna1-land1.
        cs_addr1_data-name1      = cs_kna1-name1.
        cs_addr1_data-name2      = cs_kna1-name2.
        cs_addr1_data-city1      = cs_kna1-ort01.
        cs_addr1_data-city2      = cs_kna1-ort02.
        cs_addr1_data-post_code1 = cs_kna1-pstlz.
        cs_addr1_data-region     = cs_kna1-regio.
        cs_addr1_data-street     = cs_kna1-stras.
      ELSEIF cs_lfa1 IS NOT INITIAL.
        cs_addr1_data-country    = cs_lfa1-land1.
        cs_addr1_data-name1      = cs_lfa1-name1.
        cs_addr1_data-name2      = cs_lfa1-name2.
        cs_addr1_data-city1      = cs_lfa1-ort01.
        cs_addr1_data-city2      = cs_lfa1-ort02.
        cs_addr1_data-post_code1 = cs_lfa1-pstlz.
        cs_addr1_data-region     = cs_lfa1-regio.
        cs_addr1_data-street     = cs_lfa1-stras.
      ENDIF.
    ENDIF.

    IF cs_kna1-cfopc IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = cs_kna1-cfopc
        IMPORTING
          output = cs_kna1-cfopc.
    ENDIF.

    IF ct_lfb1[] IS NOT INITIAL.

      LOOP AT ct_lfb1 ASSIGNING FIELD-SYMBOL(<fs_lfb1>).

        LOOP AT it_bdc ASSIGNING <fs_bdc> WHERE fnam CS 'BUKRS'
                                            AND fval IS NOT INITIAL.

          <fs_lfb1>-bukrs = <fs_bdc>-fval.
          EXIT.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    IF ct_lfbw[] IS NOT INITIAL.

      LOOP AT ct_lfbw ASSIGNING FIELD-SYMBOL(<fs_lfbw>).

        LOOP AT it_bdc ASSIGNING <fs_bdc> WHERE fnam CS 'BUKRS'
                                            AND fval IS NOT INITIAL.

          <fs_lfbw>-bukrs = <fs_bdc>-fval.
          EXIT.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
