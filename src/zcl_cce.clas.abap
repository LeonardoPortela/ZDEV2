class ZCL_CCE definition
  public
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .
  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  data AT_CCE type ZCARTA_CORRECAO .
  data AT_CAMPOS_CORRECAO type ZSDT0081_T .
  data LO_SERVICE_LOCATOR type ref to CL_J_1BNFE_CF_SERVICE_LOC .
  data LO_CLOUD_CTE_PROCESSOR type ref to CL_NFE_CLOUD_CTE_PROCESSOR .
  data GC_ERR_V type J_1BNFE_MS_ERROR value 'V' ##NO_TEXT.
  data GC_ERR_R type J_1BNFE_MS_ERROR value 'R' ##NO_TEXT.
  data LS_CTE_EVENT_PARAMETERS type NFE_CLOUD_CTE_MAP_EVENT_PARAMS .
  data LS_CTE_CCE_ERROR_INFO type NFE_CLOUD_CTE_CCE_ERROR_INFO .

  methods CONSTRUCTOR
    importing
      value(I_DOCNUM) type J_1BDOCNUM optional
      value(I_ID_CC) type NUM optional .
  methods SET_DOCNUM
    importing
      !I_DOCNUM type J_1BDOCNUM .
  methods SET_TEXTO_CORRECAO
    importing
      !I_TEXTO_CORRECAO type CHAR1000SF .
  methods SET_NOVO_LOC_COLETA
    importing
      !I_NOVO_LOC_COLETA type LIFNR .
  methods SET_NOVO_LOC_ENTREGA
    importing
      !I_NOVO_LOC_ENTREGA type LIFNR .
  methods SET_NOVO_TERMINAL
    importing
      !I_NOVO_TERMINAL type LIFNR .
  methods SET_NOVO_AGENTE
    importing
      !I_NOVO_AGENTE type LIFNR .
  methods ENVIAR
    returning
      value(E_ENVIADA) type CHAR01 .
  methods IMPRIMIR .
  methods CHECK_DISP_TRANSF
    returning
      value(E_DISPONIVEL) type CHAR01 .
  methods TRANSF_ESTOQUE
    importing
      !I_TP_TRANSF type CHAR01
      !I_CENTRO_ORIGEM type WERKS_D
      !I_DEPOSITO_ORIGEM type LGORT_D
      !I_CENTRO_DESTINO type WERKS_D optional
      !I_DEPOSITO_DESTINO type LGORT_D
      !I_JOB type CHAR01 optional
    returning
      value(E_GRAVOU) type CHAR01 .
  methods SET_DADOS_TRANSF_AUTO
    importing
      !I_TP_TRANSF type CHAR01
      !I_CENTRO_ORIGEM type WERKS_D
      !I_DEPOSITO_ORIGEM type LGORT_D
      !I_CENTRO_DESTINO type WERKS_D
      !I_DEPOSITO_DESTINO type LGORT_D .
  methods ENVIAR_GRC
    importing
      !I_MODE type CHAR01
    returning
      value(R_ENVIADA) type CHAR01
    exceptions
      DOCUMENT_NOT_FOUND .
  methods DEQUEUE_EVENT
    importing
      !I_DOCNUM type J_1BDOCNUM .
  methods SET_CAMPOS_CORRECAO_CTE
    importing
      !I_ZSDT0081_T type ZSDT0081_T .
  methods CCE_CTE_OUT
    importing
      !P_XNFEACTIVE type J_1BXNFEACTIVE
      !P_CALLMODE type FLAG
      !P_RFCDEST type RFCDEST
      !P_ACC_KEY44 type J_1B_NFE_ACCESS_KEY_DTEL44
      !P_EVENT_TYPE type NUM6
      !P_EVENT_SEQNUM type J_1BNFE_EVENT_SEQNO
      !P_EVENT_ISSUE_TMPL type J_1BNFE_CREA_TSTAMP
      !P_TIME_ZONE type TZNZONE
      !P_RESEND type FLAG
      !P_EVENT_DOCNUM type J_1BDOCNUM
      !P_EVENT_UUID type NFE_DOCUMENT_UUID
      !P_IS_CLOUD_SERVICE type CHAR1
      !P_VERSION type J_1BNFEXMLVERSION optional
    changing
      !P_CCEFLD type J_1BNFE_T_CCEFLD
      !P_BAPIRET2 type BAPIRETTAB
      !P_SUBRC type SY-SUBRC
      !P_EVENT_MS_ERROR type J_1BNFE_MS_ERROR .
  methods SERVICE_EVENT_CCE
    importing
      !IV_ACTIVE type J_1BNFE_ACTIVE
      !IV_ACCESS_KEY_44 type J_1B_NFE_ACCESS_KEY_DTEL44
      !IV_EVENT_TYPE type NUM6
      !IV_CONTENT type STRING
      !IS_BRANCH_SADR type SADR
      !IS_EVENT type J_1BNFE_EVENT
    changing
      !IV_ERROR type J_1BNFE_MS_ERROR
      !IV_IS_CLOUD_SERVICE type CHAR1
      !CT_BAPIRET type BAPIRETTAB .
  methods READ_DOC_DA
    importing
      !IV_DOCNUM type J_1BDOCNUM
    changing
      !CS_DOC type J_1BNFDOC .
  methods SERVICE_CCE_MAP
    importing
      !IV_ACTIVE type J_1BNFE_ACTIVE
      !IV_ACCESS_KEY_44 type J_1B_NFE_ACCESS_KEY_DTEL44
      !IV_EVENT_TYPE type NUM6
      !IV_CONTENT type STRING
      !IS_BRANCH_SADR type SADR
      !IS_EVENT type J_1BNFE_EVENT
      !LV_NFE_VERSION type J_1BNFEXMLVERSION
    changing
      !IV_REQUEST_JSON type STRING
    raising
      CX_J_1BNFE_CF .
protected section.
private section.

  methods CHECK_GRAVADA
    returning
      value(E_GRAVADA) type CHAR01 .
ENDCLASS.



CLASS ZCL_CCE IMPLEMENTATION.


  METHOD cce_cte_out.

    CONSTANTS: gc_err_g TYPE j_1bnfe_ms_error VALUE 'G',
               gc_err_v TYPE j_1bnfe_ms_error VALUE 'V',
               gc_err_r TYPE j_1bnfe_ms_error VALUE 'R'.

    DATA lv_error TYPE j_1bnfe_message_type.

    DATA: lt_content TYPE j_1bnfe_t_ccecontent,
          ls_content TYPE j_1bnfe_s_ccecontent,
          ls_ccefld  TYPE j_1bnfe_d_ccefld.

    IF p_ccefld IS NOT INITIAL.

      CLEAR lt_content.
      LOOP AT p_ccefld INTO ls_ccefld.

        CLEAR ls_content.
        MOVE ls_ccefld-xml_group TO ls_content-grupo_alterado.
        MOVE ls_ccefld-xml_field TO ls_content-campo_alterado.
        MOVE ls_ccefld-newvalue  TO ls_content-valor_alterado.
        MOVE ls_ccefld-nroitem   TO ls_content-nro_item_alterado.
        APPEND ls_content TO lt_content.

      ENDLOOP.

      IF p_xnfeactive IS INITIAL.

*   CALL NON-GRC MESSAGING SYSTEM AND PASS DOC NUM
*   GV_CALLMODE CAN BE SWITCHED IN DEBBUGING TO CALL THE RFC SYNCHRONOUS
        IF p_callmode IS INITIAL.
          CALL FUNCTION 'J_1BNFE_CCEFLD_OUT'
            IN BACKGROUND TASK
            DESTINATION p_rfcdest
            EXPORTING
              iv_access_key               = p_acc_key44     "2324622
              iv_event_type               = p_event_type
              iv_internal_sequence_number = p_event_seqnum
              iv_timestamp                = p_event_issue_tmpl
              iv_timezone                 = p_time_zone
              it_content                  = lt_content      "2318549
              iv_resend                   = p_resend
              iv_docnum                   = p_event_docnum.
        ELSE.
          CALL FUNCTION 'J_1BNFE_CCEFLD_OUT'
            DESTINATION p_rfcdest
            EXPORTING
              iv_access_key               = p_acc_key44     "2324622
              iv_event_type               = p_event_type
              iv_internal_sequence_number = p_event_seqnum
              iv_timestamp                = p_event_issue_tmpl
              iv_timezone                 = p_time_zone
              it_content                  = lt_content      "2318549
              iv_resend                   = p_resend
              iv_docnum                   = p_event_docnum
            EXCEPTIONS
              communication_failure       = 1
              system_failure              = 2.
          p_subrc = sy-subrc.
          IF NOT sy-subrc IS INITIAL.
            p_event_ms_error = gc_err_r.
          ENDIF.
        ENDIF.

      ELSE.

*-#127611-17.11.2023-JT-inicio
        IF p_is_cloud_service = abap_true.                  "3047912

          CREATE OBJECT lo_cloud_cte_processor.                                                                 "3047912
                                                            "3047912
          ls_cte_event_parameters-docnum           = p_event_docnum. "3047912
          ls_cte_event_parameters-access_key       = p_acc_key44. "3047912
          ls_cte_event_parameters-int_event        = cl_nfe_constant_event_type=>cce. "3047912
          ls_cte_event_parameters-event_type       = p_event_type. "3047912
          ls_cte_event_parameters-event_seqnum     = p_event_seqnum. "3047912
          ls_cte_event_parameters-event_issue_tmpl = p_event_issue_tmpl. "3047912
          ls_cte_event_parameters-time_zone        = p_time_zone. "3047912
          ls_cte_event_parameters-resend           = p_resend. "3047912
          ls_cte_event_parameters-content          = lt_content. "3047912
          ls_cte_event_parameters-cloud_uuid       = p_event_uuid. "3047912
          ls_cte_event_parameters-XML_VERSION      =  P_VERSION . "IR170918
                                                            "3047912
          ls_cte_cce_error_info = lo_cloud_cte_processor->send_correction_letter( "3047912
                                                            is_event_parameters = ls_cte_event_parameters "3047912
                                                            iv_save_log         = abap_false ). "3047912
          p_event_ms_error = ls_cte_cce_error_info-ms_error. "3047912
          p_bapiret2       = ls_cte_cce_error_info-log.                                                         "3047912                                                                                                             "3047912

        ELSE.
*-#127611-17.11.2023-JT-fim
          SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
            FROM j_1bnfdoc
           WHERE docnum EQ @p_event_docnum.

          SELECT SINGLE * INTO @DATA(wa_j_1bnfe_active)
            FROM j_1bnfe_active
           WHERE docnum EQ @p_event_docnum.

          DATA: iv_dhemi          TYPE c LENGTH 14,
                iv_tpamb          TYPE c LENGTH 01,
                iv_cnpj_emit      TYPE c LENGTH 14,
                iv_nseqevento_cce TYPE zxnfe_ev_nseqevento.

          iv_dhemi = wa_j_1bnfdoc-cre_timestamp.

          CASE sy-sysid.
            WHEN 'PRD'.
              iv_tpamb = wa_j_1bnfe_active-tpamb.
            WHEN OTHERS.
              iv_tpamb = '2'.
          ENDCASE.

          iv_cnpj_emit = wa_j_1bnfe_active-stcd1.
          iv_nseqevento_cce = '00'.

          SELECT * INTO TABLE @DATA(it_carta)
            FROM zcarta_correcao
           WHERE docnum EQ @wa_j_1bnfe_active-docnum.

          CASE sy-sysid.
            WHEN 'QAS'.
              LOOP AT it_carta INTO DATA(wa_carta) WHERE ds_url CS '172.12.12.139'.
                ADD 1 TO iv_nseqevento_cce.
              ENDLOOP.
            WHEN OTHERS.
              LOOP AT it_carta INTO wa_carta WHERE ds_url CS 'SIMETRYA'.
                ADD 1 TO iv_nseqevento_cce.
              ENDLOOP.
          ENDCASE.

          "Cria Documento se Não Existe
          CALL FUNCTION 'ZNFE_INCLUIR_CTE_SIMETRYA'
            DESTINATION p_rfcdest
            EXPORTING
              iv_cte_access_key     = p_acc_key44
              iv_docnum             = wa_j_1bnfdoc-docnum
              iv_timestamp          = p_event_issue_tmpl
              iv_timezone           = p_time_zone
              iv_dhemi              = iv_dhemi
              iv_tpamb              = iv_tpamb
              iv_cnpj_emit          = iv_cnpj_emit
              iv_nseqevento_cce     = iv_nseqevento_cce
            IMPORTING
              ev_error_status       = lv_error
              et_bapiret2           = p_bapiret2
            EXCEPTIONS
              communication_failure = 1
              system_failure        = 2.

          IF lv_error EQ 'Y'.

            CLEAR: lv_error.

            CALL FUNCTION 'ZXCTE_ISSUE_CCE'
              DESTINATION p_rfcdest
              EXPORTING
                iv_cte_access_key           = p_acc_key44
                iv_event_type               = p_event_type
                iv_internal_sequence_number = p_event_seqnum
                iv_timestamp                = p_event_issue_tmpl
                iv_timezone                 = p_time_zone
                iv_content                  = lt_content
                iv_resend                   = p_resend
              IMPORTING
                ev_error_status             = lv_error
                et_bapiret2                 = p_bapiret2
              EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2.

          ELSE.

            CLEAR: lv_error, p_bapiret2[], p_bapiret2.

            CALL FUNCTION '/XNFE/EV_ISSUE_CTE_CCE'
              DESTINATION p_rfcdest
              EXPORTING
                iv_access_key               = p_acc_key44
                iv_event_type               = p_event_type
                iv_internal_sequence_number = p_event_seqnum
                iv_timestamp                = p_event_issue_tmpl
                iv_timezone                 = p_time_zone
                it_content                  = lt_content
                iv_resend                   = p_resend
              IMPORTING
                ev_error_status             = lv_error
                et_bapiret2                 = p_bapiret2
              EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2.

          ENDIF.

          p_subrc = sy-subrc.
          IF NOT sy-subrc IS INITIAL.
            p_event_ms_error = gc_err_r.  "RFC ERROR OCCURED
          ENDIF.

          IF NOT lv_error IS INITIAL.
            CASE lv_error.
              WHEN 'V'.
                p_event_ms_error = gc_err_v.
              WHEN 'G'.
                p_event_ms_error = gc_err_g.
              WHEN 'D'.
                p_event_ms_error = lv_error.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.

        ENDIF.  "*-#127611-17.11.2023-JT-inicio

      ENDIF.                                                "3047912

    ENDIF.

  ENDMETHOD.


  method CHECK_DISP_TRANSF.

    E_DISPONIVEL = ABAP_FALSE.

    ME->CHECK_GRAVADA( RECEIVING E_GRAVADA = DATA(_GRAVADA) ).
    CHECK _GRAVADA IS NOT INITIAL.

    IF ME->AT_CCE-MODEL NE '55'.
      MESSAGE 'Operação só permitida para NF-e!' TYPE 'S'.
      EXIT.
    ENDIF.

** Ajuste realizado US #84105 - 28/07/2022 - Anderson Oenning
*    SELECT SINGLE *
*      FROM ZSDT_RETLOTE INTO @DATA(_ZSDT_RETLOTE)
*     WHERE DOCNUM = @ME->AT_CCE-DOCNUM.
*
*    IF SY-SUBRC = 0.
*      MESSAGE | Nota já está vinculada a um Retorno de Formação de Lote: { _ZSDT_RETLOTE-DOCNUM_RET } ! | TYPE 'S'.
*    ELSE.
      E_DISPONIVEL = ABAP_TRUE.
*      MESSAGE 'Nota disponível para transferência!' TYPE 'S'.
*    ENDIF.
** Ajuste realizado US #84105 - 28/07/2022 - Anderson Oenning
  endmethod.


  method CHECK_GRAVADA.

    E_GRAVADA = ABAP_FALSE.

    IF ( ME->AT_CCE-DOCNUM IS INITIAL ) OR ( ME->AT_CCE-ID_CC IS INITIAL ).
      MESSAGE 'Carta de Correção ainda não foi gravada!' TYPE 'S'.
      EXIT.
    ENDIF.

    E_GRAVADA = ABAP_TRUE.


  endmethod.


  method CONSTRUCTOR.

    CLEAR: ME->AT_CCE, ME->AT_CAMPOS_CORRECAO[].

    CHECK ( I_DOCNUM IS NOT INITIAL ) AND ( I_ID_CC IS NOT INITIAL ).

    "Dados Carta Correção
    SELECT SINGLE *
      FROM ZCARTA_CORRECAO INTO ME->AT_CCE
     WHERE DOCNUM = I_DOCNUM
       AND ID_CC  = I_ID_CC.



  endmethod.


  method DEQUEUE_EVENT.

   CALL FUNCTION 'DEQUEUE_E_J_1BNFE_EVENT'
     EXPORTING
       mode_j_1bnfe_event = 'E'
       mandt              = sy-mandt
       docnum             = i_docnum.

  endmethod.


  METHOD ENVIAR.

    DATA: V_TEXTO_CORRECAO TYPE CHAR1000SF.

    E_ENVIADA = ABAP_FALSE.

    ME->CHECK_GRAVADA( RECEIVING E_GRAVADA = DATA(_GRAVADA) ).
    CHECK _GRAVADA IS NOT INITIAL.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(WL_DOC)
     WHERE DOCNUM EQ @ME->AT_CCE-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    CASE ME->AT_CCE-MODEL.
      WHEN '55'.

        CONCATENATE ME->AT_CCE-MSG_CORREC1
                    ME->AT_CCE-MSG_CORREC2
                    ME->AT_CCE-MSG_CORREC3
                    ME->AT_CCE-MSG_CORREC4 INTO V_TEXTO_CORRECAO.

*        SELECT SINGLE *
*          FROM SETLEAF INTO @DATA(WL_SETLEAF_MODEL_XI)
*         WHERE SETNAME EQ 'GRC_NFE_CALL_XI_BRANCH'
*           AND VALFROM EQ @WL_DOC-BRANCH.
*
*        IF SY-SUBRC EQ 0.

        "SELECT SINGLE * INTO @DATA(WA_URL)
        "  FROM ZIB_NFE
        " WHERE DOCNUM EQ @WL_DOC-DOCNUM
        "   AND DS_URL_DANFE NE @SPACE.

        "IF NOT ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
        ME->ENVIAR_GRC( I_MODE =  '1' ).
*        E_ENVIADA = ABAP_TRUE.
*        CHECK 1 = 2.
        "ENDIF.

*        ENDIF.

*        CALL FUNCTION 'Z_MONTA_XML_CTA_CORRECAO'
*          EXPORTING
*            P_DOCNUM     = ME->AT_CCE-DOCNUM
*            P_TXT_CORREC = V_TEXTO_CORRECAO.

      WHEN '57'.

*        SELECT SINGLE *
*          FROM SETLEAF INTO WL_SETLEAF_MODEL_XI
*         WHERE SETNAME EQ 'GRC_CTE_CALL_XI_BRANCH'
*           AND VALFROM EQ WL_DOC-BRANCH.
*
*        IF SY-SUBRC EQ 0.

        "SELECT SINGLE * INTO @DATA(WA_URL)
        "  FROM ZIB_NFE
        " WHERE DOCNUM EQ @WL_DOC-DOCNUM
        "   AND DS_URL_DANFE NE @SPACE.

        "IF NOT ( WA_URL-DS_URL_DANFE CS 'SIMETRYA' OR WA_URL-DS_URL_DANFE CS '172.12.12.139' ).
        ME->ENVIAR_GRC( I_MODE =  '1' ).
*        E_ENVIADA = ABAP_TRUE.
*        CHECK 1 = 2.
        "ENDIF.

*        ENDIF.

*        CALL FUNCTION 'Z_MONTA_XML_CTE_CORRECAO'
*          EXPORTING
*            P_DOCNUM = ME->AT_CCE-DOCNUM
*            P_ID_CC  = ME->AT_CCE-ID_CC.

    ENDCASE.

    E_ENVIADA = ABAP_TRUE.

  ENDMETHOD.


  METHOD enviar_grc.

    TYPES: BEGIN OF ty_text,
             text TYPE text100,
           END OF ty_text.

    CONSTANTS: gc_cce_create TYPE c VALUE '1',
               gc_cce_resend TYPE c VALUE '2',
               gc_cte_model  TYPE j_1bmodel VALUE '57',
               gc_nfe_model  TYPE j_1bmodel VALUE '55',
               gc_err_g      TYPE j_1bnfe_ms_error VALUE 'G',
               gc_err_v      TYPE j_1bnfe_ms_error VALUE 'V',
               gc_err_r      TYPE j_1bnfe_ms_error VALUE 'R',
               gc_cce        TYPE j_1bnfe_int_event VALUE 'EV_CCE'.

    DATA:  gv_callmode         TYPE flag  VALUE  space.

    DATA: lv_lines         TYPE i,
          ls_event         TYPE j_1bnfe_event,
          ls_orig_event    TYPE j_1bnfe_event,
          ls_cce           TYPE j_1bnfe_cce,
          ls_active        TYPE j_1bnfe_active,
          lv_new_sequence  TYPE flag,
          lv_first_event   TYPE flag,
          lv_resend        TYPE flag,
          lv_modify        TYPE flag,
          lv_display       TYPE flag,
          ls_cust3         TYPE j_1bnfe_cust3,
          ls_acc_key       TYPE j_1b_nfe_access_key,
          lv_acc_key44     TYPE j_1b_nfe_access_key_dtel44,
          lv_xnfeactive    TYPE j_1bxnfeactive,
          lv_rfcdest       TYPE rfcdest,
          lc_crlf          TYPE string VALUE cl_abap_char_utilities=>cr_lf,
          lv_len           TYPE i,
          lv_delta_length  TYPE i,
          lv_remain_length TYPE i,
          lv_last_length   TYPE i,
          iv_nprot         TYPE c LENGTH 15,
          lv_error         TYPE j_1bnfe_message_type,
          lt_bapiret2      TYPE bapirettab,
          ls_bapiret2      TYPE bapiret2,
          lv_msgid         TYPE symsgid,
          lv_message       TYPE c,
          ls_branch_sadr   TYPE sadr,
          lv_updmode       TYPE char1,
          lv_text          TYPE string,
          lv_content       TYPE string,      "content w/o ##
          lv_read_content  TYPE string, "content from DB
          lv_docnum        TYPE j_1bdocnum,
          lv_doc_string    TYPE string,
          lv_rest_string   TYPE string,
          lv_time_zone     TYPE tznzone,
          lv_event_type    TYPE num6,
          lv_date_check    TYPE dats,
          lv_index         TYPE sy-tabix,
          lv_service_type  TYPE string.                     "3047912

    DATA: lt_text TYPE TABLE OF ty_text.

    DATA: BEGIN OF ls_text,
            text TYPE text100,
          END OF ls_text.

    DATA: lc_max_number_chars TYPE i VALUE 1000.
    DATA: lc_max_seqnum TYPE j_1bnfe_event_seqno_ext VALUE 20. "1661138

    DATA: lt_ccefld           TYPE j_1bnfe_t_ccefld,        "2000504
          wl_ccefld           TYPE j_1bnfe_d_ccefld,
          lv_is_cloud_service TYPE char1.

    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(wl_active)
     WHERE docnum EQ @me->at_cce-docnum.

    CHECK sy-subrc EQ 0.

    IF wl_active-docsta <> 1
        OR NOT wl_active-cancel IS INITIAL
        OR wl_active-action_requ <> 'C'.
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '452' WITH wl_active-docnum.
    ENDIF.

    IF wl_active-form IS INITIAL.
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '455'.
    ENDIF.

*-#127611-17.11.2023-JT-inicio
    SELECT SINGLE *
      FROM j_1bnfdoc
      INTO @DATA(wl_jdoc)
     WHERE docnum EQ @me->at_cce-docnum.

    IF wl_active-model = gc_cte_model.                      "3047912
      lv_service_type = cl_nfe_cloud_cte_const_service=>correction_letter. "3047912
    ELSE.                                                   "3047912
      lv_service_type = cl_j_1bnfe_cf_constant=>c_service_event_cce. "3047912
    ENDIF.                                                  "3047912

    lo_service_locator = cl_j_1bnfe_cf_monitor=>create_service_locator_object(
                              iv_bukrs        = wl_active-bukrs
                              iv_branch       = wl_active-branch
                              iv_model        = wl_active-model
                              iv_nftype       = wl_jdoc-nftype
                              iv_service_type = lv_service_type ). "3047912

    IF lo_service_locator->is_cloud_service( ) = abap_true.
      lv_is_cloud_service = abap_true.
    ENDIF.
*-#127611-17.11.2023-JT-fim

* Enqueue J_1BNFE_EVENT
    CALL FUNCTION 'ENQUEUE_E_J_1BNFE_EVENT'
      EXPORTING
        mode_j_1bnfe_event = 'E'
        mandt              = sy-mandt
        docnum             = wl_active-docnum
      EXCEPTIONS
        foreign_lock       = 1
        system_failure     = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Prepare the event
    CALL FUNCTION 'J_1BNFE_EVENT_PREPARE'
      EXPORTING
        iv_docnum       = wl_active-docnum
        iv_event        = gc_cce
        iv_is_cloud     = lv_is_cloud_service  "*-#127611-17.11.2023-JT
      IMPORTING
        es_event        = ls_event
        es_orig_event   = ls_orig_event
        ev_first_event  = lv_first_event
        ev_new_sequence = lv_new_sequence
      EXCEPTIONS
        invalid_event   = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '458'.
      EXIT.
    ENDIF.

* Check whether max official sequence number 20 is reached
    IF ls_orig_event-ext_seqnum >= lc_max_seqnum.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '465'.
      EXIT.
    ENDIF.

    IF NOT ls_orig_event IS INITIAL. " it is not the first seq no
* Do not allow new CCe for MS status G
      IF ls_orig_event-ms_error = gc_err_g.
        me->dequeue_event( i_docnum =  wl_active-docnum ).
        CASE i_mode.
          WHEN 1.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '455'.
          WHEN 2.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '456'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
* Do not allow new CCe if another one is still processed
      IF ls_orig_event-docsta IS INITIAL AND ls_orig_event-ms_error IS INITIAL.
        me->dequeue_event( i_docnum =  wl_active-docnum ).
        CASE i_mode.
          WHEN 1.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '455'.
          WHEN 2.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '456'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.

    lv_updmode = 'I'. "database action: decide if it's insert or update

* do not proceed if this is the first CCE.
    IF lv_first_event IS INITIAL.

      IF lv_new_sequence IS INITIAL.
        lv_modify = 'X'.
        lv_updmode = 'U'.
      ENDIF.

      IF wl_active-model = gc_nfe_model.

*     Prepare the CCE
        CALL FUNCTION 'J_1BNFE_CCE_PREPARE'
          EXPORTING
            is_event_common  = ls_event
            iv_modify_cce    = lv_modify
          IMPORTING
            es_cce           = ls_cce
          EXCEPTIONS
            cce_not_existing = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          me->dequeue_event( i_docnum =  wl_active-docnum ).
          ROLLBACK WORK.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '460' WITH wl_active-docnum.
        ENDIF.

      ELSE.                                                 "2000504

        CALL FUNCTION 'J_1BNFE_CCEFLD_PREPARE'              "2000504
          EXPORTING                                         "2000504
            is_event_common  = ls_event                     "2000504
            iv_modify_cce    = lv_modify                    "2000504
          IMPORTING                                         "2000504
            et_ccefld        = lt_ccefld                    "2000504
          EXCEPTIONS                                        "2000504
            cce_not_existing = 1                            "2000504
            OTHERS           = 2.                           "2000504
        IF sy-subrc <> 0.                                   "2000504
          me->dequeue_event( i_docnum =  wl_active-docnum ).
          ROLLBACK WORK.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '460'        "2000504
            WITH wl_active-docnum.                          "2000504
        ENDIF.                                              "2000504

      ENDIF.
                                                            "2000504
    ELSE.
* this is done only for the first event
      IF wl_active-model = gc_nfe_model.                    "2000504
        MOVE-CORRESPONDING ls_event TO ls_cce.              "2000504
      ENDIF.                                                "2000504

    ENDIF.

* Allow resend only for errors R and V
    IF i_mode = gc_cce_resend.
      IF ls_orig_event-ms_error CA 'RV'.
        lv_read_content = ls_cce-text.
      ELSE.
        me->dequeue_event( i_docnum =  wl_active-docnum ).
        ROLLBACK WORK.
        MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '456'.
      ENDIF.
    ENDIF.

* Prepare the text from CCE
    IF NOT ls_cce-text IS INITIAL.
      SPLIT ls_cce-text AT lc_crlf INTO TABLE lt_text.
      CLEAR ls_cce-text.
    ENDIF.

* Read customizing --> needed for event group
    CALL FUNCTION 'J_1BNFE_CUST3_READ'
      EXPORTING
        iv_bukrs       = wl_active-bukrs
        iv_branch      = wl_active-branch
        iv_model       = wl_active-model
      IMPORTING
        es_cust3       = ls_cust3
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      EXIT.
    ENDIF.
* Read Bisuness Place address data
    CALL FUNCTION 'J_1B_BRANCH_READ'
      EXPORTING
        branch            = wl_active-branch
        company           = wl_active-bukrs
      IMPORTING
        address           = ls_branch_sadr
      EXCEPTIONS
        branch_not_found  = 1
        address_not_found = 2
        company_not_found = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Determine time zone
    IF NOT ls_branch_sadr-tzone IS INITIAL.
      lv_time_zone = ls_branch_sadr-tzone.
    ELSE.
      lv_time_zone = sy-zonlo.
    ENDIF.
* Check event group for CCe
    CALL FUNCTION 'J_1BNFE_EVENT_GROUP_CHECK'
      EXPORTING
        iv_event     = gc_cce
        iv_group     = ls_cust3-event_group
      EXCEPTIONS
        not_assigned = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '461'
              WITH gc_cce ls_cust3-event_group.
    ELSE.
      ls_event-eve_group = ls_cust3-event_group.
    ENDIF.

* For RFC error (R) only resend in read-only mode/ do not allow create.
    IF ls_orig_event-ms_error = gc_err_r.
      CASE i_mode.
        WHEN 1. "create
          me->dequeue_event( i_docnum =  wl_active-docnum ).
          ROLLBACK WORK.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '455'.
        WHEN 2. "resend
          lv_resend = 'X'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

*--------| CHAMAR EDITOR CCE |--------------------------------------------------------

    IF 1 = 2.

      IF wl_active-model = gc_cte_model.
*     Call the edior for CCE when Create
        IF i_mode = gc_cce_create.
          CALL FUNCTION 'J_1BNFE_CCEFLD_EDITOR_CALL'
            EXPORTING
              pi_docnum      = ls_event-docnum
              pi_int_event   = ls_event-int_event
              pi_seqnum      = ls_event-seqnum
              pi_model       = gc_cte_model
              pi_title       = TEXT-256
            CHANGING
              ct_ccefld      = lt_ccefld
            EXCEPTIONS
              user_cancelled = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            me->dequeue_event( i_docnum =  wl_active-docnum ).
            EXIT.
          ENDIF.

          IF lt_ccefld[] IS INITIAL.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '612'.
          ENDIF.
        ENDIF.

      ELSEIF wl_active-model = gc_nfe_model.

*   Call the edior for CCE when Create
        IF i_mode = gc_cce_create.
          CALL FUNCTION 'J_1BNFE_EDITOR_CALL'
            EXPORTING
              iv_titel            = TEXT-255
              iv_max_number_chars = lc_max_number_chars
            TABLES
              ct_textlines        = lt_text
            EXCEPTIONS
              user_cancelled      = 1
              OTHERS              = 2.
          IF sy-subrc <> 0.
            me->dequeue_event( i_docnum =  wl_active-docnum ).
            EXIT.
          ENDIF.

*     Check the lenght of 1000 charactes and cut the text if longer
          LOOP AT lt_text INTO ls_text.
            lv_index = sy-tabix.
            IF NOT ls_text IS INITIAL.
              IF lv_index = 1.
                CONCATENATE lv_text ls_text INTO lv_text.
              ELSE.
*     Measuring without the Carriage Return #
                CONCATENATE lv_text ls_text INTO lv_text SEPARATED BY space.
              ENDIF.
              lv_len = strlen( lv_text ).
              IF lv_len > lc_max_number_chars.
*     Mesure length of the last line
                lv_last_length = strlen( ls_text ).
*     Calcualate delta length to allowed 1000
                lv_delta_length = lv_len - lc_max_number_chars.
*     Determine remaining length
                lv_remain_length = lv_last_length - lv_delta_length.
                ls_text = ls_text(lv_remain_length).
                CONCATENATE ls_cce-text ls_text INTO ls_cce-text.

                MESSAGE ID 'J1B_NFE' TYPE 'W' NUMBER '453'.
                EXIT.
              ENDIF.
            ENDIF.
*     version of text to be saved later in the DB
*          CONCATENATE ls_cce-text ls_text lc_crlf INTO ls_cce-text.
            IF lv_index = 1.
              CONCATENATE ls_cce-text ls_text INTO ls_cce-text.
            ELSE.
              CONCATENATE ls_cce-text ls_text INTO ls_cce-text SEPARATED BY lc_crlf.
            ENDIF.
          ENDLOOP.
*     minimum length for the CCE content should be 15 characters
          IF lv_len < 15.
            ROLLBACK WORK.
            MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '463'.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    CASE wl_active-model.
      WHEN gc_cte_model.

        DATA lv_fldchgseq TYPE j_1bnfe_fld_chg_seq_no.

        CLEAR: lt_ccefld[].

        LOOP AT me->at_campos_correcao INTO DATA(wl_campo_correcao).

          CLEAR: wl_ccefld.

          ADD 1 TO lv_fldchgseq.

          SELECT SINGLE *
            FROM j_1bnfe_c_ccecfg INTO @DATA(wl_j_1bnfe_c_ccecfg)
           WHERE xml_group EQ @wl_campo_correcao-grupo
             AND xml_field EQ @wl_campo_correcao-campo.

          IF sy-subrc NE 0.
            MESSAGE e006 WITH wl_campo_correcao-grupo wl_campo_correcao-campo.
          ENDIF.

          wl_ccefld-docnum    = me->at_cce-docnum.
          wl_ccefld-int_event = 'EV_CCE'.
          wl_ccefld-seqnum    = ls_event-seqnum.
          wl_ccefld-fldchgseq = lv_fldchgseq.
          wl_ccefld-model     = gc_cte_model.
          wl_ccefld-ccefldid  = wl_j_1bnfe_c_ccecfg-ccefldid.

          wl_ccefld-xml_group = wl_campo_correcao-grupo.
          wl_ccefld-xml_field = wl_campo_correcao-campo.

          CONCATENATE wl_campo_correcao-valor
                      wl_campo_correcao-valor1
                      wl_campo_correcao-valor2
                      wl_campo_correcao-valor3 INTO wl_ccefld-newvalue SEPARATED BY space.

          wl_ccefld-nroitem = wl_campo_correcao-item.

          IF wl_ccefld-nroitem EQ '00'.
            wl_ccefld-nroitem = '01'.
          ENDIF.

          APPEND wl_ccefld TO lt_ccefld.

        ENDLOOP.

        IF lt_ccefld[] IS INITIAL.
          ROLLBACK WORK.
          MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '612'.
        ENDIF.

      WHEN gc_nfe_model.
        CONCATENATE me->at_cce-msg_correc1
                    me->at_cce-msg_correc2
                    me->at_cce-msg_correc3
                    me->at_cce-msg_correc4 INTO ls_cce-text.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

* Check if RFC is functioning
    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
      EXPORTING
        i_bukrs      = wl_active-bukrs
        i_branch     = wl_active-branch
        i_model      = wl_active-model
      IMPORTING
        e_rfcdest    = lv_rfcdest
        e_xnfeactive = lv_xnfeactive
      EXCEPTIONS
        rfc_error    = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      me->dequeue_event( i_docnum =  wl_active-docnum ).
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '066' WITH lv_rfcdest.
    ENDIF.

* check the value for the resend flag
    IF ls_orig_event-ms_error = gc_err_r.
      lv_resend = 'X'.
    ENDIF.

* move the text in case of resend
    IF i_mode = gc_cce_resend.
      ls_cce-text = lv_read_content.
    ENDIF.

    CLEAR lv_error.
    CLEAR lt_bapiret2.
* put the text w/o ## in the lv_content to pass to Msg Sys
    lv_content = ls_cce-text.
    REPLACE ALL OCCURRENCES OF lc_crlf IN lv_content WITH '# #'.
    REPLACE ALL OCCURRENCES OF '#' IN lv_content WITH ''.

    MOVE-CORRESPONDING wl_active TO ls_acc_key.
    lv_acc_key44 = ls_acc_key.

    lv_event_type = ls_event-ext_event.

    IF wl_active-model = gc_nfe_model.                      "2000504

      IF lv_xnfeactive IS INITIAL.

*   Call non-GRC messaging system and pass Doc Num
*   gv_callmode can be switched in debbuging to call the RFC synchronous

        IF gv_callmode IS INITIAL.

          CALL FUNCTION 'J_1BNFE_CCE_OUT'
            IN BACKGROUND TASK
            DESTINATION lv_rfcdest
            EXPORTING
              iv_nfe_access_key           = lv_acc_key44
              iv_event_type               = lv_event_type
              iv_internal_sequence_number = ls_event-seqnum
              iv_timestamp                = ls_event-issue_tmpl
              iv_timezone                 = lv_time_zone
              iv_content                  = lv_content
              iv_resend                   = lv_resend
              iv_docnum                   = ls_event-docnum. "1617819

        ELSE.
          CALL FUNCTION 'J_1BNFE_CCE_OUT'
            DESTINATION lv_rfcdest
            EXPORTING
              iv_nfe_access_key           = lv_acc_key44
              iv_event_type               = lv_event_type
              iv_internal_sequence_number = ls_event-seqnum
              iv_timestamp                = ls_event-issue_tmpl
              iv_timezone                 = lv_time_zone
              iv_content                  = lv_content
              iv_resend                   = lv_resend
              iv_docnum                   = ls_event-docnum  "1617819
            EXCEPTIONS
              communication_failure       = 1
              system_failure              = 2.
          IF NOT sy-subrc IS INITIAL.
            ls_event-ms_error = gc_err_r.  "RFC error occured
          ELSE.
            MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '457'.
          ENDIF.
        ENDIF.

      ELSE.
*-#127611-17.11.2023-JT-inicio
        me->service_event_cce( EXPORTING iv_active           = wl_active
                                         iv_access_key_44    = lv_acc_key44
                                         iv_event_type       = lv_event_type
                                         iv_content          = lv_content
                                         is_branch_sadr      = ls_branch_sadr
                                         is_event            = ls_event
                                CHANGING iv_error            = lv_error
                                         iv_is_cloud_service = lv_is_cloud_service
                                         ct_bapiret          = lt_bapiret2 ).
*-#127611-17.11.2023-JT-fim

        IF lv_is_cloud_service IS INITIAL.  "*-#127611-17.11.2023-JT-inicio

          SELECT SINGLE * INTO @DATA(wa_j_1bnfdoc)
            FROM j_1bnfdoc
           WHERE docnum EQ @wl_active-docnum.

          DATA: iv_dhemi          TYPE c LENGTH 14,
                iv_tpamb          TYPE c LENGTH 01,
                iv_cnpj_emit      TYPE c LENGTH 14,
                iv_nseqevento_cce TYPE zxnfe_ev_nseqevento.

          iv_dhemi     = wa_j_1bnfdoc-cre_timestamp.

          CASE sy-sysid.
            WHEN 'PRD'.
              iv_tpamb = '1'.
            WHEN OTHERS.
              iv_tpamb = '2'.
          ENDCASE.

          iv_nprot     = wl_active-authcod.
          iv_cnpj_emit = wl_active-stcd1.
          iv_nseqevento_cce = '00'.

          SELECT * INTO TABLE @DATA(it_carta)
            FROM zcarta_correcao
           WHERE docnum EQ @wl_active-docnum.

          CASE sy-sysid.
            WHEN 'QAS'.
              LOOP AT it_carta INTO DATA(wa_carta) WHERE ds_url CS '172.12.12.139'.
                ADD 1 TO iv_nseqevento_cce.
              ENDLOOP.
            WHEN OTHERS.
              LOOP AT it_carta INTO wa_carta WHERE ds_url CS 'SIMETRYA'.
                ADD 1 TO iv_nseqevento_cce.
              ENDLOOP.
          ENDCASE.

          "Cria Documento se Não Existe
          CALL FUNCTION 'ZNFE_INCLUIR_NFE_SIMETRYA'
            DESTINATION lv_rfcdest
            EXPORTING
              iv_nfe_access_key     = lv_acc_key44
              iv_docnum             = wl_active-docnum
              iv_timestamp          = ls_event-issue_tmpl
              iv_timezone           = lv_time_zone
              iv_dhemi              = iv_dhemi
              iv_tpamb              = iv_tpamb
              iv_cnpj_emit          = iv_cnpj_emit
              iv_nseqevento_cce     = iv_nseqevento_cce
              iv_nprot              = iv_nprot
            IMPORTING
              ev_error_status       = lv_error
              et_bapiret2           = lt_bapiret2
            EXCEPTIONS
              communication_failure = 1
              system_failure        = 2.

          IF lv_error EQ 'Y'.
            CLEAR: lv_error.

            "Call GRC
            CALL FUNCTION 'ZXNFE_ISSUE_CCE'
              DESTINATION lv_rfcdest
              EXPORTING
                iv_nfe_access_key           = lv_acc_key44
                iv_event_type               = lv_event_type
                iv_internal_sequence_number = ls_event-seqnum
                iv_timestamp                = ls_event-issue_tmpl
                iv_timezone                 = lv_time_zone
                iv_content                  = lv_content
                iv_resend                   = lv_resend
              IMPORTING
                ev_error_status             = lv_error
                et_bapiret2                 = lt_bapiret2
              EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2.

          ELSE.
            CLEAR: lv_error, lt_bapiret2[], lt_bapiret2.

            "Call GRC
            CALL FUNCTION '/XNFE/ISSUE_CCE'
              DESTINATION lv_rfcdest
              EXPORTING
                iv_nfe_access_key           = lv_acc_key44
                iv_event_type               = lv_event_type
                iv_internal_sequence_number = ls_event-seqnum
                iv_timestamp                = ls_event-issue_tmpl
                iv_timezone                 = lv_time_zone
                iv_content                  = lv_content
                iv_resend                   = lv_resend
              IMPORTING
                ev_error_status             = lv_error
                et_bapiret2                 = lt_bapiret2
              EXCEPTIONS
                communication_failure       = 1
                system_failure              = 2.
          ENDIF.

          IF NOT sy-subrc IS INITIAL.
            ls_event-ms_error = gc_err_r.  "RFC error occured
          ELSE.
            MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '457'.
          ENDIF.
        ENDIF.  "*-#127611-17.11.2023-JT

        IF NOT lv_error IS INITIAL.
          CASE lv_error.
            WHEN 'V'.
              ls_event-ms_error = gc_err_v.
            WHEN 'G'.
              ls_event-ms_error = gc_err_g.
            WHEN 'D'.                                       "1747221
              ls_event-ms_error = lv_error.                 "1747221
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

      ENDIF.

    ELSE.

      CLEAR ls_cce.
*   CC-e CT-e Call

      DATA(_sy_subrc) = 0.
      me->cce_cte_out(  EXPORTING
                          p_xnfeactive       =   lv_xnfeactive
                          p_callmode         =   gv_callmode
                          p_rfcdest          =   lv_rfcdest
                          p_acc_key44        =   lv_acc_key44
                          p_event_type       =   lv_event_type
                          p_event_seqnum     =   ls_event-seqnum
                          p_event_issue_tmpl =   ls_event-issue_tmpl
                          p_time_zone        =   lv_time_zone
                          p_resend           =   lv_resend
                          p_event_docnum     =   ls_event-docnum
                          p_event_uuid       =   ls_event-cloud_uuid "3047912
                          p_is_cloud_service =   lv_is_cloud_service "3047912
                          P_VERSION          =  ls_cust3-VERSION

                        CHANGING
                          p_ccefld           =   lt_ccefld[]
                          p_bapiret2         =   lt_bapiret2[]
                          p_subrc            =   _sy_subrc
                          p_event_ms_error   =   ls_event-ms_error   ).

      IF _sy_subrc IS INITIAL.
        MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '457'.
      ENDIF.

    ENDIF.

* Update the event tables
    CALL FUNCTION 'J_1BNFE_EVENT_UPDATE' IN UPDATE TASK
      EXPORTING
        is_event_common = ls_event
        iv_updmode      = lv_updmode
        is_cce          = ls_cce                            "2000504
        iv_cce_cte      = lt_ccefld.                        "2000504

* Read ACTIVE table
    CALL FUNCTION 'J_1B_NFE_XML_RAED_ACTIVE_TAB'
      EXPORTING
        i_docnum = wl_active-docnum
*       I_ACCKEY =
      IMPORTING
        e_acttab = ls_active
      EXCEPTIONS
        no_entry = 1
        OTHERS   = 2.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              RAISING document_not_found.
    ENDIF.

    ls_active-event_flag = 'X'.

* Update the Events Flag in ACTIVE
    CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE' IN UPDATE TASK
      EXPORTING
        i_acttab           = ls_active
        i_skip_action_data = 'X'                  "1810199
        i_updmode          = 'U'.

* Save into the database
    COMMIT WORK AND WAIT.

* Fire the RFC error, if any occured
    IF ls_event-ms_error = gc_err_r.
      ROLLBACK WORK.
      MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '066' WITH lv_rfcdest.
    ENDIF.

* Display error from messaging system
    IF NOT lt_bapiret2 IS INITIAL.
      LOOP AT lt_bapiret2 INTO ls_bapiret2
       WHERE type CA 'AXE'.
        IF ls_bapiret2-id = '/XNFE/APPEVENT'.
          lv_msgid = 'J1B_NFE_ERP_GRC'.
        ELSE.
* Unexpected message ID
          lv_msgid = ls_bapiret2-id.
        ENDIF.
* In case of validation error - write messages into NFe log
        IF ls_event-ms_error = gc_err_v OR
           ls_event-ms_error = 'D'.                         "1747221
          CLEAR: lv_doc_string, lv_rest_string, lv_docnum.
          SPLIT ls_bapiret2-message_v4 AT space INTO lv_doc_string lv_rest_string.
          lv_docnum  = lv_doc_string.
          ROLLBACK WORK.
          MESSAGE ID lv_msgid TYPE ls_bapiret2-type NUMBER ls_bapiret2-number
          WITH ls_bapiret2-message_v1 ls_bapiret2-message_v2 ls_bapiret2-message_v3
               ls_bapiret2-message_v4 INTO lv_message.
          CALL FUNCTION 'J_1B_NFE_ERROR_PROTOKOLL'
            EXPORTING
              i_docnum = wl_active-docnum.
*            i_docnum = lv_docnum.
          "PERFORM grid_refresh. " show the red flag for the existing log entries
        ENDIF.

        ROLLBACK WORK.
        MESSAGE ID lv_msgid TYPE ls_bapiret2-type NUMBER ls_bapiret2-number
        WITH ls_bapiret2-message_v1 ls_bapiret2-message_v2 ls_bapiret2-message_v3
             ls_bapiret2-message_v4.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  method IMPRIMIR.

    DATA : V_DS_URL TYPE AGR_URL.

    ME->CHECK_GRAVADA( RECEIVING E_GRAVADA = DATA(_GRAVADA) ).
    CHECK _GRAVADA IS NOT INITIAL.

    V_DS_URL = ME->AT_CCE-DS_URL.

    CHECK V_DS_URL IS NOT INITIAL.

    CALL FUNCTION 'PRGN_GENER_EXECUTE_URL'
      EXPORTING
        NODE_DATA = V_DS_URL.

  endmethod.


  method SET_CAMPOS_CORRECAO_CTE.

    ME->CK_ALTEROU = ABAP_TRUE.

    ME->AT_CAMPOS_CORRECAO = I_ZSDT0081_T.

  endmethod.


  method SET_DADOS_TRANSF_AUTO.

    ME->AT_CCE-TRANSF_AUTO = 'X'.
    ME->AT_CCE-TP_TRANSF   = I_TP_TRANSF.
    ME->AT_CCE-WERKS_O     = I_CENTRO_ORIGEM.
    ME->AT_CCE-LGORT_O     = I_DEPOSITO_ORIGEM.
    ME->AT_CCE-WERKS_D     = I_CENTRO_DESTINO.
    ME->AT_CCE-LGORT_D     = I_DEPOSITO_DESTINO.

  endmethod.


  method SET_DOCNUM.

    IF ME->AT_CCE-DOCNUM NE I_DOCNUM.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-DOCNUM = I_DOCNUM.

  endmethod.


  method SET_NOVO_AGENTE.

    IF ME->AT_CCE-NOVO_AGENTE NE I_NOVO_AGENTE.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-NOVO_AGENTE = I_NOVO_AGENTE.

  endmethod.


  method SET_NOVO_LOC_COLETA.

    IF ME->AT_CCE-NOVO_LOC_COLETA NE I_NOVO_LOC_COLETA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-NOVO_LOC_COLETA = I_NOVO_LOC_COLETA.

  endmethod.


  method SET_NOVO_LOC_ENTREGA.

    IF ME->AT_CCE-NOVO_LOC_ENTREGA NE I_NOVO_LOC_ENTREGA.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-NOVO_LOC_ENTREGA = I_NOVO_LOC_ENTREGA.


  endmethod.


  method SET_NOVO_TERMINAL.

    IF ME->AT_CCE-NOVO_TERMINAL NE I_NOVO_TERMINAL.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-NOVO_TERMINAL = I_NOVO_TERMINAL.

  endmethod.


  method SET_TEXTO_CORRECAO.

    DATA: V_TEXTO_CORRECAO TYPE CHAR1000SF.

    V_TEXTO_CORRECAO = I_TEXTO_CORRECAO.

    REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN V_TEXTO_CORRECAO WITH 'a' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN V_TEXTO_CORRECAO WITH 'e' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'í'     IN V_TEXTO_CORRECAO WITH 'i' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN V_TEXTO_CORRECAO WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN V_TEXTO_CORRECAO WITH 'u' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN V_TEXTO_CORRECAO WITH 'c' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        '&'     IN V_TEXTO_CORRECAO WITH '&#38;'.
    REPLACE ALL OCCURRENCES OF        ''''    IN V_TEXTO_CORRECAO WITH '&#39;'.
    REPLACE ALL OCCURRENCES OF        'º'     IN V_TEXTO_CORRECAO WITH 'o' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF        'ª'     IN V_TEXTO_CORRECAO WITH 'a' IGNORING CASE.

    V_TEXTO_CORRECAO = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = ZCL_STRING=>CONVERT_TO_UTF8( I_TEXTO = CONV #( V_TEXTO_CORRECAO ) ) ).

    IF ME->AT_CCE-MSG_CORREC1 NE V_TEXTO_CORRECAO(250).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->AT_CCE-MSG_CORREC2 NE V_TEXTO_CORRECAO+250(250).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->AT_CCE-MSG_CORREC3 NE V_TEXTO_CORRECAO+500(250).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    IF ME->AT_CCE-MSG_CORREC4 NE V_TEXTO_CORRECAO+750(250).
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.

    ME->AT_CCE-MSG_CORREC1 = V_TEXTO_CORRECAO(250).
    ME->AT_CCE-MSG_CORREC2 = V_TEXTO_CORRECAO+250(250).
    ME->AT_CCE-MSG_CORREC3 = V_TEXTO_CORRECAO+500(250).
    ME->AT_CCE-MSG_CORREC4 = V_TEXTO_CORRECAO+750(250).

  endmethod.


  METHOD transf_estoque.

    DATA: vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
          vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year.

    DATA: dia(2)     TYPE c,
          mes(2)     TYPE c,
          ano(4)     TYPE c,
          data(10)   TYPE c,
          flag_error TYPE c,
          var_answer TYPE c.

    DATA: gmhead TYPE bapi2017_gm_head_01,
          gmcode TYPE bapi2017_gm_code.

    DATA: it_gmcreate TYPE TABLE OF bapi2017_gm_item_create,
          wa_gmcreate TYPE bapi2017_gm_item_create,
          it_errmsg   TYPE TABLE OF bapiret2,
          wa_errmsg   TYPE bapiret2.

    DATA: lva_werks_d TYPE zsdt_depara_depo-werks_v.

    CLEAR: gmhead, gmcode, it_gmcreate[], it_errmsg[], flag_error, vl_mat_doc, vl_matdocumentyear.

    e_gravou = abap_false.

*---------------------------------------------------------------------------------------------*
*   Inicio Validações
*---------------------------------------------------------------------------------------------*

    me->check_gravada( RECEIVING e_gravada = DATA(_gravada) ).
    CHECK _gravada IS NOT INITIAL.

    IF me->at_cce-model NE '55'.
      MESSAGE 'Operação só permitida para NF-e!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF me->at_cce-novo_terminal IS INITIAL.
      MESSAGE 'Operação permitida somente para CC-e com correção de parceiro Z1 - Terminal Porto!' TYPE 'S'.
      EXIT.
    ENDIF.

    "Check Disponibilidade Trâsnferencia
    me->check_disp_transf( RECEIVING e_disponivel = DATA(_disponivel) ).
    CHECK _disponivel IS NOT INITIAL.

    IF ( me->at_cce-authcode IS INITIAL ).
      MESSAGE 'Carta de Correção não está autorizada' TYPE 'S'.
      EXIT.
    ENDIF.

    IF me->at_cce-status_trans IS NOT INITIAL."Transferencia já realizada

      IF i_job IS NOT INITIAL. "Execução por Job
        MESSAGE 'Transferência de Estoque já realizada para a carta de correção' TYPE 'S'.
        EXIT.
      ELSE.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Transferência de Estoque'
            text_question         = 'Já existem movimentações de estoque para esta carta de correção, estornar via MBST. Fazer nova movimentação?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '1'
            display_cancel_button = ''
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        CHECK var_answer = '1'.
      ENDIF.

    ENDIF.

    CASE i_tp_transf.
      WHEN '1'. "Trânsferência por Deposito
      WHEN '2'. "Trânsferência por Centro

        SELECT SINGLE *
          FROM zsdt_depara_cen INTO @DATA(_zsdt_depara_cen1)
         WHERE centrov_1 = @i_centro_origem.

        IF sy-subrc NE 0.
          MESSAGE 'Centro Origem deve ser Virtual' TYPE 'S'.
          EXIT.
        ENDIF.

        SELECT SINGLE *
          FROM zsdt_depara_cen INTO @DATA(_zsdt_depara_cen2)
         WHERE centrov_1 = @i_centro_destino.

        IF sy-subrc NE 0.
          MESSAGE 'Centro Destino deve ser Virtual' TYPE 'S'.
          EXIT.
        ENDIF.

        IF _zsdt_depara_cen1-centro_real NE _zsdt_depara_cen2-centro_real.
          MESSAGE 'Centro Origem/Destino devem pertencer ao mesmo Centro Real!' TYPE 'S'.
          EXIT.
        ENDIF.

      WHEN OTHERS.
        MESSAGE 'Tipo de Trânsferência não previsto!' TYPE 'W'.
        EXIT.
    ENDCASE.

    IF i_centro_origem IS INITIAL.
      MESSAGE 'Centro Origem não informado' TYPE 'S'.
      EXIT.
    ENDIF.

    IF i_deposito_origem IS INITIAL.
      MESSAGE 'Deposito Origem não informado' TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( i_centro_destino IS INITIAL ) AND ( i_tp_transf = '2' ).
      MESSAGE 'Centro Destino não informado' TYPE 'S'.
      EXIT.
    ENDIF.

    IF i_deposito_destino IS INITIAL.
      MESSAGE 'Deposito Destino não informado' TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnflin INTO @DATA(_wl_lin)
     WHERE docnum EQ @me->at_cce-docnum.

    IF sy-subrc NE 0.
      MESSAGE 'Item do documento Fiscal vinculado a Carta de Corração não encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM j_1bnfdoc INTO @DATA(_wl_doc)
     WHERE docnum EQ @me->at_cce-docnum.

    IF sy-subrc NE 0.
      MESSAGE 'Cabeçalho do documento Fiscal vinculado a Carta de Corração não encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF i_centro_destino IS NOT INITIAL.
      lva_werks_d = i_centro_destino.
    ELSE.
      lva_werks_d = i_centro_origem.
    ENDIF.

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255 - INICIO

*    SELECT SINGLE *
*      FROM ZSDT_DEPARA_DEPO INTO @DATA(SL_ZSDT_DEPARA_DEPO)
*     WHERE LGORT   EQ @I_DEPOSITO_DESTINO
*       AND WERKS_V EQ @LVA_WERKS_D.

    DATA(_dep_eudr) = 'N'.
    SELECT SINGLE *
       FROM zmmt0190 INTO @DATA(lwa_zmmt0190)
      WHERE lgort EQ @i_deposito_destino.

    IF sy-subrc eq 0 AND i_deposito_destino is NOT INITIAL AND lwa_zmmt0190-eudr = 'S'.
      _dep_eudr = 'S'.
    ENDIF.

    zcl_depara_centro_fixo_virtual=>get_dados_depara(
       EXPORTING
         i_lgort          = i_deposito_destino
         i_werks_v        = lva_werks_d
         i_eudr           = _dep_eudr
       IMPORTING
         e_single_depara  = DATA(sl_zsdt_depara_depo)  ).
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM


    IF sl_zsdt_depara_depo IS INITIAL.
      MESSAGE |Deposito { i_deposito_destino } Centro { lva_werks_d } EUDR: { _dep_eudr }, não cadastrado na ZSDT0020!| TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( me->at_cce-novo_terminal NE sl_zsdt_depara_depo-lifnr ).
      MESSAGE |Terminal CC-e { me->at_cce-novo_terminal } diferente do cadastrado na ZSDT0020 { sl_zsdt_depara_depo-lifnr }!| TYPE 'S'.
      EXIT.
    ENDIF.

*---------------------------------------------------------------------------------------------*
*   Inicio Preenchimento Bapi
*---------------------------------------------------------------------------------------------*

    gmhead-pstng_date = sy-datum.
    gmhead-doc_date   = sy-datum.

    dia  = _wl_doc-docdat+6(2).
    mes  = _wl_doc-docdat+4(2).
    ano  = _wl_doc-docdat(4).

    CONCATENATE dia '.' mes '.' ano INTO data.
    CONCATENATE _wl_doc-nfenum data INTO gmhead-header_txt SEPARATED BY space.

    gmcode-gm_code = '06'.

*<--- 01/07/2023 - Migração S4 - JP - Tamanho do campo maior.
*   WA_GMCREATE-MATERIAL   = _WL_LIN-MATNR.
    CLEAR: wa_gmcreate-material, wa_gmcreate-material_long.
    IF strlen( _wl_lin-matnr ) > 18.
      wa_gmcreate-material_long   = _wl_lin-matnr.
    ELSE.
      wa_gmcreate-material   = _wl_lin-matnr.
    ENDIF.
*<--- 01/07/2023 - Migração S4 - JP - Fim

    wa_gmcreate-plant      = i_centro_origem.
    wa_gmcreate-stge_loc   = i_deposito_origem.
    wa_gmcreate-batch      = _wl_lin-charg.

    CASE i_tp_transf.
      WHEN '1'. "Trânsferência por Deposito
        wa_gmcreate-move_type  = 'ZA5'.
        CLEAR wa_gmcreate-move_plant.
      WHEN '2'. "Trânsferência por Centro
        wa_gmcreate-move_type  = '301'.
        wa_gmcreate-move_plant = i_centro_destino.
    ENDCASE.

    wa_gmcreate-entry_qnt  = _wl_lin-menge.
    wa_gmcreate-entry_uom  = _wl_lin-meins.
    wa_gmcreate-move_stloc = i_deposito_destino.

    APPEND wa_gmcreate TO it_gmcreate.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = gmhead
        goodsmvt_code    = gmcode
      IMPORTING
        materialdocument = vl_mat_doc
        matdocumentyear  = vl_matdocumentyear
      TABLES
        goodsmvt_item    = it_gmcreate
        return           = it_errmsg.

    CLEAR: flag_error.

    LOOP AT it_errmsg INTO wa_errmsg.
      IF wa_errmsg-type EQ 'E'.
        flag_error = 'X'.
      ENDIF.
    ENDLOOP.

    IF ( flag_error EQ 'X' ).
      MESSAGE wa_errmsg-message TYPE 'S'.
      EXIT.
    ENDIF.

    COMMIT WORK AND WAIT.

    IF ( sy-subrc EQ 0 ) AND ( vl_matdocumentyear IS NOT INITIAL ).
      UPDATE zcarta_correcao SET status_trans  = 'X'
                                 "NOVO_TERMINAL = SL_ZSDT_DEPARA_DEPO-LIFNR
                                 doc_material  = vl_mat_doc
                                 ano_material  = vl_matdocumentyear
       WHERE docnum EQ me->at_cce-docnum
         AND id_cc  EQ me->at_cce-id_cc.

      IF ( sy-subrc EQ 0 ).
        MESSAGE 'Transferência de Estoque com Sucesso.' TYPE 'S'.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    e_gravou = abap_true.


  ENDMETHOD.


  method ZIF_CADASTRO~GRAVAR_REGISTRO.

    I_GRAVOU = ABAP_FALSE.

    CHECK ME->CK_ALTEROU EQ ABAP_TRUE.

    CHECK ME->VALIDAR_REGISTRO( ) EQ ABAP_TRUE.

    IF ME->AT_CCE-ID_CC IS INITIAL.
      SELECT SINGLE MAX( ID_CC )
        FROM ZCARTA_CORRECAO INTO @DATA(VL_ID)
       WHERE DOCNUM = @ME->AT_CCE-DOCNUM.

      ADD 1 TO VL_ID.

      ME->AT_CCE-ID_CC = VL_ID.

* Inicio - Falheiros - 22.12.2022
       ME->AT_CCE-tcode = sy-tcode.
* Fim - Falheiros - 22.12.2022
    ENDIF.

    IF ME->AT_CCE-ID_CC IS INITIAL.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao obter o identificador da Carta de Correção!' TYPE 'S'.
      RETURN.
    ENDIF.

    ME->AT_CCE-USUARIO     = SY-UNAME.

    MODIFY ZCARTA_CORRECAO FROM ME->AT_CCE.

    IF SY-SUBRC NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve um erro ao gravar a Carta de Correção!' TYPE 'S'.
      RETURN.
    ENDIF.

    LOOP AT ME->AT_CAMPOS_CORRECAO ASSIGNING FIELD-SYMBOL(<FS_CAMPOS_CCE>).
      <FS_CAMPOS_CCE>-ID_CC = ME->AT_CCE-ID_CC.
    ENDLOOP.

    IF ME->AT_CAMPOS_CORRECAO[] IS NOT INITIAL.
      MODIFY ZSDT0081 FROM TABLE ME->AT_CAMPOS_CORRECAO.

      IF SY-SUBRC NE 0 .
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao gravar os campos de correção da Carta de Correção!' TYPE 'S'.
        RETURN.
      ENDIF.
    ENDIF.

    ME->ENVIAR( RECEIVING E_ENVIADA = DATA(_ENVIADA) ).

    IF _ENVIADA EQ ABAP_FALSE.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK.

    ME->CK_ALTEROU = ABAP_FALSE.

    I_GRAVOU = ABAP_TRUE.

    MESSAGE 'Carta de Correção gravada com sucesso!' TYPE 'S'..

  endmethod.


  method ZIF_CADASTRO~LIMPAR_REGISTRO.

    CLEAR: ME->AT_CCE, ME->AT_CAMPOS_CORRECAO[].

  endmethod.


  method ZIF_CADASTRO~NOVO_REGISTRO.

    ME->LIMPAR_REGISTRO( ).

    ME->CK_ALTEROU = ABAP_FALSE.

  endmethod.


  METHOD ZIF_CADASTRO~SET_REGISTRO.

    DATA: LC_ID_REGISTTRO TYPE CHAR12.

    LC_ID_REGISTTRO = I_ID_REGISTRO.

    SELECT SINGLE * INTO @ME->AT_CCE
      FROM ZCARTA_CORRECAO
     WHERE DOCNUM EQ @LC_ID_REGISTTRO(10)
       AND ID_CC  EQ @LC_ID_REGISTTRO+10(02).

  ENDMETHOD.


  method ZIF_CADASTRO~VALIDAR_EXCLUSAO.
  endmethod.


  method ZIF_CADASTRO~VALIDAR_REGISTRO.

    DATA: V_TEXTO_CORRECAO TYPE CHAR1000SF.

    E_VALIDOU = ABAP_FALSE.

    IF ME->AT_CCE-DOCNUM IS INITIAL.
      MESSAGE 'Documento Fiscal da CC-e não foi informado!' TYPE 'S'.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM J_1BNFDOC INTO @DATA(_WL_DOC)
     WHERE DOCNUM = @ME->AT_CCE-DOCNUM.

    IF SY-SUBRC NE 0.
      MESSAGE 'Documento Fiscal da CC-e não foi encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF _WL_DOC-MODEL IS INITIAL.
      MESSAGE 'Modelo do Documento Fiscal da CC-e não foi encontrado!' TYPE 'S'.
      EXIT.
    ENDIF.

    IF ( _WL_DOC-MODEL NE '55' ) AND ( _WL_DOC-MODEL NE '57' ).
      MESSAGE 'CC-e só permitida para Documento Fiscal com modelo 55 e 57!' TYPE 'S'.
      EXIT.
    ENDIF.

    ME->AT_CCE-MODEL = _WL_DOC-MODEL.

    IF ( _WL_DOC-MODEL EQ '55' ).
      CONCATENATE ME->AT_CCE-MSG_CORREC1
                  ME->AT_CCE-MSG_CORREC2
                  ME->AT_CCE-MSG_CORREC3
                  ME->AT_CCE-MSG_CORREC4 INTO V_TEXTO_CORRECAO.

      IF ( STRLEN( V_TEXTO_CORRECAO ) < 15 ) OR ( STRLEN( V_TEXTO_CORRECAO ) > 1000 ).
        MESSAGE 'A CC-e deve ter entre 15 a 1000 caracteres!' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    E_VALIDOU = ABAP_TRUE.

  endmethod.


  METHOD read_doc_da.

    DATA lo_doc_da TYPE REF TO cl_j_1bnf_doc_da.

    CREATE OBJECT lo_doc_da.
    cs_doc = lo_doc_da->find( iv_docnum ).

  ENDMETHOD.


  METHOD service_cce_map.

    DATA lo_cce      TYPE REF TO cl_j_1bnfe_cf_cce.
    DATA lt_cce      TYPE j_1bnfe_t_cce.
    DATA lv_dh_str   TYPE string.
    DATA lv_time(14) TYPE c.

    lv_dh_str = is_event-issue_tmpl.
    lv_time = lv_dh_str(14).

    CREATE OBJECT lo_cce.

    lt_cce = lo_cce->structure_map(
        iv_docnum      = iv_active-docnum
        iv_accesskey   = iv_access_key_44
        iv_timestamp   = lv_time
        is_branch_sadr = is_branch_sadr
        iv_event_type  = iv_event_type
        iv_seqnum      = is_event-seqnum
        iv_offseqnum   = is_event-ext_seqnum
        iv_content     = iv_content
        iv_authcod     = iv_active-authcod
        iv_tpamb       = iv_active-tpamb
        iv_dhevent     = lv_time
        iv_nfe_version = lv_nfe_version
    ).

    iv_request_json = lo_cce->structure_serialization( lt_cce ).

  ENDMETHOD.


  METHOD service_event_cce.

    DATA lo_service_locator       TYPE REF TO cl_j_1bnfe_cf_service_loc.
    DATA lo_service_communicator  TYPE REF TO cl_j_1bnfe_cf_service_comm.
    DATA lo_service_exception     TYPE REF TO cx_j_1bnfe_cf.
    DATA ls_doc                   TYPE j_1bnfdoc.
    DATA ls_bapiret2              TYPE bapiret2.
    DATA lv_request_json          TYPE string.
    DATA lv_http_status_response  TYPE i.

    me->read_doc_da( EXPORTING iv_docnum = iv_active-docnum
                      CHANGING cs_doc    = ls_doc ).

    lo_service_locator = cl_j_1bnfe_cf_monitor=>create_service_locator_object(
      iv_bukrs        = iv_active-bukrs
      iv_branch       = iv_active-branch
      iv_model        = iv_active-model
      iv_nftype       = ls_doc-nftype
      iv_service_type = cl_j_1bnfe_cf_constant=>c_service_event_cce
    ).

    CHECK lo_service_locator->is_cloud_service( ) = abap_true.

    iv_is_cloud_service = abap_true.

    TRY .
        me->service_cce_map( EXPORTING iv_active        = iv_active
                                       iv_access_key_44 = iv_access_key_44
                                       iv_event_type    = iv_event_type
                                       iv_content       = iv_content
                                       is_branch_sadr   = is_branch_sadr
                                       is_event         = is_event
                                       lv_nfe_version   = ls_doc-xmlvers
                              CHANGING iv_request_json  = lv_request_json ).

*     Create Client and Oa2c for nfe cloud services
        CREATE OBJECT lo_service_communicator
          EXPORTING
            iv_service_url        = lo_service_locator->get_service_url_with_path( )
            iv_oa2c_profile       = lo_service_locator->get_service_oa2c_profile( )
            iv_oa2c_configuration = lo_service_locator->get_service_oa2c_configurator( ).

*     Set JSON to client.
        lo_service_communicator->set_request_json( lv_request_json ).

*     Send Request to Cloud Services.
        lo_service_communicator->send_request_to_cloud( ).

*     Get HTTP response.
        lv_http_status_response = lo_service_communicator->get_http_status_from_cloud( ).

        IF lv_http_status_response = cl_j_1bnfe_cf_constant=>c_service_status_accepted. "202
          MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '457'.
        ELSE.
*       Raise HTTP response exception.
          iv_error = gc_err_v.
          lo_service_communicator->http_error_handler( lv_http_status_response ).
        ENDIF.

      CATCH cx_j_1bnfe_cf INTO lo_service_exception.
        ls_bapiret2 = cl_j_1bnfe_external_cf=>fill_bapiret_w_j_1bnfe_cf_excp( lo_service_exception ).
        APPEND ls_bapiret2 TO ct_bapiret.
        iv_error = gc_err_r.                                "2980984
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
