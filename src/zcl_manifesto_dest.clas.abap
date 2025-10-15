class ZCL_MANIFESTO_DEST definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_CHAVE type ZDE_CHAVE_DOC_E optional
      !I_DOC_MANIFESTO type J_1BDOCNUM optional .
  methods SET_CNPJ_DEST
    importing
      !I_CNPJ_DEST type STCD1 .
  methods SET_IE_DEST
    importing
      !I_IE_DEST type STCD3 .
  methods SET_CHAVE
    importing
      !I_CHAVE type ZDE_CHAVE_DOC_E .
  methods SET_CD_OPERACAO
    importing
      !I_CD_OPERACAO type ZOPR_MDEST .
  methods SET_JUSTIFICATIVA
    importing
      !I_JUSTIFICATIVA type CHAR255 .
  methods SET_BUKRS
    importing
      !I_BUKRS type BUKRS .
  methods SET_BRANCH
    importing
      !I_BRANCH type J_1BBRANC_ .
  methods GET_CNPJ_DEST
    returning
      value(E_CNPJ_DEST) type STCD1 .
  methods GET_IE_DEST
    returning
      value(E_IE_DEST) type STCD3 .
  methods GET_CHAVE
    returning
      value(E_CHAVE) type ZDE_CHAVE_DOC_E .
  methods GET_CD_OPERACAO
    returning
      value(E_CD_OPERACAO) type ZOPR_MDEST .
  methods GET_JUSTIFICATIVA
    returning
      value(E_JUSTIFICATIVA) type CHAR255 .
  methods GRAVAR_MANIFESTO
    returning
      value(E_DOC_MANIFESTO) type J_1BDOCNUM
    raising
      ZCX_MANIFESTO_DEST .
  methods ENVIAR_MANIFESTO
    importing
      !I_SEM_CONFIRMACAO type CHAR01 optional
    raising
      ZCX_MANIFESTO_DEST .
  methods SET_VALIDA_ENVIAR_DRC
    importing
      !I_ZSDT0127 type ZSDT0127
      !I_ACTION type EDOC_ACTION optional
    returning
      value(E_ENVIA_DRC) type CHAR1
    exceptions
      CX_EDOCUMENT
      ZCX_MANIFESTO_DEST .
  methods SET_VALIDA_ANULACAO_REJEICAO
    importing
      !I_ZSDT0127 type ZSDT0127
    returning
      value(E_OK_ANULAR) type CHAR1 .
  methods SET_VALIDA_ANULACAO_CONF_OPER
    importing
      !I_ZSDT0127 type ZSDT0127
    returning
      value(E_OK_ANULAR) type CHAR1 .
protected section.
private section.

  data AT_CNPJ_DEST type STCD1 .
  data AT_IE_DEST type STCD3 .
  data AT_CHAVE type ZDE_CHAVE_DOC_E .
  data AT_CD_OPERACAO type ZOPR_MDEST .
  data AT_JUSTIFICATIVA type CHAR255 .
  data AT_DATA_EMI type RRSELDATE .
  data AT_HORA_EMI type RRSELTIME .
  data AT_DOC_MANIFESTO type J_1BDOCNUM .
  data AT_BUKRS type BUKRS .
  data AT_BRANCH type J_1BBRANC_ .

  methods SET_DATA_EMI
    importing
      !I_DATA_EMI type RRSELDATE .
  methods SET_HORA_EMI
    importing
      !I_HORA_EMI type RRSELTIME .
  methods SET_DOC_MANIFESTO
    importing
      !I_DOC_MANIFESTO type J_1BDOCNUM .
  methods MONTA_XML
    returning
      value(E_XML) type STRING .
ENDCLASS.



CLASS ZCL_MANIFESTO_DEST IMPLEMENTATION.


  method CONSTRUCTOR.

    DATA: WA_ZSDT0127 TYPE ZSDT0127.

    CHECK ( I_CHAVE IS NOT INITIAL ) AND ( I_DOC_MANIFESTO IS NOT INITIAL ).

    "Busca dados Manifesto.
    SELECT SINGLE *
      FROM ZSDT0127 INTO WA_ZSDT0127
     WHERE CHAVE         = I_CHAVE
       AND DOC_MANIFESTO = I_DOC_MANIFESTO.

    CHECK SY-SUBRC = 0.

    ME->SET_CHAVE( WA_ZSDT0127-CHAVE ).
    ME->SET_DOC_MANIFESTO( WA_ZSDT0127-DOC_MANIFESTO ).
    ME->SET_CNPJ_DEST( WA_ZSDT0127-CNPJ_DEST ).
    ME->SET_IE_DEST( WA_ZSDT0127-IE_DEST ).
    ME->SET_BUKRS( WA_ZSDT0127-BUKRS ).
    ME->SET_BRANCH( WA_ZSDT0127-BRANCH ).
    ME->SET_CD_OPERACAO( WA_ZSDT0127-CD_OPERACAO ).
    ME->SET_JUSTIFICATIVA( WA_ZSDT0127-JUSTIFICATIVA ).
    ME->SET_DATA_EMI( WA_ZSDT0127-DATA_EMI ).
    ME->SET_HORA_EMI( WA_ZSDT0127-HORA_EMI ).


  endmethod.


METHOD enviar_manifesto.

  DATA: it_nota     TYPE TABLE OF zob_nota_fiscal_sap,
        "WA_NOTA TYPE ZOB_NOTA_FISCAL_SAP,
        xdhemi      TYPE c LENGTH 30,
        xdata       TYPE c LENGTH 30,
        xhora       TYPE c LENGTH 10,
        v_not_code  TYPE c LENGTH 8,
        v_overwrite TYPE char1,
        wa_zsdt0127 TYPE zsdt0127.

  DATA: v_destination TYPE rfcdest.

  DATA: var_answer TYPE c,
        xml_ret    TYPE string,
        xml        TYPE zxml.

*#127333 - 24.11.2023 - JT - inicio
  DATA: mo_action_handler TYPE REF TO cl_edoc_action,
        go_cockpit        TYPE REF TO cl_edoc_cockpit,
        go_criteria       TYPE REF TO cl_edoc_sel_criteria,
        lv_action         TYPE edoc_action,
        ls_edocument      TYPE edoc_reslist_field,
        lt_edocument      TYPE edoc_reslist_field_tab,
        it_range          TYPE rsds_trange,
        l_envia_drc       TYPE char1.
*#127333 - 24.11.2023 - JT - fim

  IF i_sem_confirmacao EQ abap_false.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmação'
        text_question         = 'Confirmar envio do manifesto?'
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

  CLEAR: wa_zsdt0127.
  SELECT SINGLE *
    INTO wa_zsdt0127
    FROM zsdt0127
   WHERE doc_manifesto = me->at_doc_manifesto.

  IF sy-subrc = 0.
    IF ( wa_zsdt0127-autorizado IS NOT INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_evento_vinc_nfe-msgid
                            msgno = zcx_manifesto_dest=>zcx_evento_vinc_nfe-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_evento_vinc_nfe-msgno
          msgid  = zcx_manifesto_dest=>zcx_evento_vinc_nfe-msgid.
    ENDIF.
  ENDIF.

*#127333 - 24.11.2023 - JT - inicio
*-------------------------
* verificar se enviar por DRC
*-------------------------
  TRY.
      l_envia_drc = set_valida_enviar_drc( wa_zsdt0127 ).

    CATCH zcx_manifesto_dest INTO DATA(ex_man_dest).
      ex_man_dest->published_erro( i_msgty = 'I' i_msgty_display = 'W' ).
    CATCH cx_edocument     INTO DATA(ex_edocument).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

  CHECK l_envia_drc = abap_false.
*#127333 - 24.11.2023 - JT - fim

*#127333 - 24.11.2023 - JT - inicio
  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(w_tvarv)
   WHERE name = 'DESTINATION_GRC'.

  IF sy-subrc = 0.
    v_destination = w_tvarv-low.
  ELSE.
    CALL FUNCTION 'J_1B_NFE_CHECK_RFC_DESTINATION'
      EXPORTING
        i_bukrs   = '0001'
        i_branch  = '0111'
        i_model   = '55'
      IMPORTING
        e_rfcdest = v_destination
      EXCEPTIONS
        rfc_error = 1
        OTHERS    = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.
  ENDIF.
*#127333 - 24.11.2023 - JT - fim

  CASE me->at_cd_operacao.
    WHEN '210210'.   "Ciência da Operação

      CALL FUNCTION 'ZNFE_EVENTS_INB'
        DESTINATION v_destination
        EXPORTING
          iv_nfeid              = me->at_chave
          iv_tpevento           = '210210'
        EXCEPTIONS
          communication_failure = 1
          system_failure        = 2
          error_reading_nfe     = 3
          technical_error       = 4
          error_creating_event  = 5
          OTHERS                = 6.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE s009(zgrc). "Erro de comunicação com GRC!
          RETURN.
        WHEN 2.
          MESSAGE s010(zgrc). "Houve uma falha no sistema!
          RETURN.
        WHEN 3.
          MESSAGE s011(zgrc). "Erro ao ler o documento NF-e!
          RETURN.
        WHEN 4.
          MESSAGE s012(zgrc). "Houve um erro técnico!
          RETURN.
        WHEN 5.
          MESSAGE s013(zgrc). "Houve um erro ao criar o evento!
          RETURN.
        WHEN 6.
          MESSAGE s015(zgrc). "Erro desconhecido!
          RETURN.
      ENDCASE.

    WHEN '210200'.   "Confirmação da Operação

      CALL FUNCTION 'ZXNFE_COMPLETED_EVENTS'
        DESTINATION v_destination
        EXPORTING
          iv_nfeid              = me->at_chave
          iv_infotext           = 'Confirmação da Operação'
          iv_trig_opco_evnt     = abap_true
        EXCEPTIONS
          communication_failure = 1
          no_proc_allowed       = 2
          system_failure        = 3
          error_reading_nfe     = 4
          technical_error       = 5
          error_creating_event  = 6
          OTHERS                = 7.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE s009(zgrc). "Erro de comunicação com GRC!
          RETURN.
        WHEN 2.
          MESSAGE s014(zgrc). "Processo não permitido
          RETURN.
        WHEN 3.
          MESSAGE s010(zgrc). "Houve uma falha no sistema!
          RETURN.
        WHEN 4.
          MESSAGE s011(zgrc). "Erro ao ler o documento NF-e!
          RETURN.
        WHEN 5.
          MESSAGE s012(zgrc). "Houve um erro técnico!
          RETURN.
        WHEN 6.
          MESSAGE s013(zgrc). "Houve um erro ao criar o evento!
          RETURN.
        WHEN 7.
          MESSAGE s015(zgrc). "Erro desconhecido!
          RETURN.
      ENDCASE.

    WHEN '210240' OR "Operação não Realizada
         '210220'.   "Desconhecimento da Operação

      CASE me->at_cd_operacao.
        WHEN '210240'. "Operação não Realizada
          v_not_code = '100'.
        WHEN '210220'. "Desconhecimento da Operação
          v_not_code = '99'.
      ENDCASE.

      CALL FUNCTION 'ZXNFE_REJECT_EVENTS'
        DESTINATION v_destination
        EXPORTING
          iv_nfeid              = me->at_chave
          iv_not_code           = v_not_code
        EXCEPTIONS
          communication_failure = 1
          no_proc_allowed       = 2
          system_failure        = 3
          error_reading_nfe     = 4
          technical_error       = 5
          error_creating_event  = 6
          OTHERS                = 7.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE s009(zgrc). "Erro de comunicação com GRC!
          RETURN.
        WHEN 2.
          MESSAGE s014(zgrc). "Processo não permitido
          RETURN.
        WHEN 3.
          MESSAGE s010(zgrc). "Houve uma falha no sistema!
          RETURN.
        WHEN 4.
          MESSAGE s011(zgrc). "Erro ao ler o documento NF-e!
          RETURN.
        WHEN 5.
          MESSAGE s012(zgrc). "Houve um erro técnico!
          RETURN.
        WHEN 6.
          MESSAGE s013(zgrc). "Houve um erro ao criar o evento!
          RETURN.
        WHEN 7.
          MESSAGE s015(zgrc). "Erro desconhecido!
          RETURN.
      ENDCASE.

*-CS2022000243-#76365-20.04.2022-JT-inicio
    WHEN '610110'.   "Desacordo de Entrega de Serviços (CT-e)

      CALL FUNCTION 'Z_CTE_SET_REJECTED'
        DESTINATION v_destination
        EXPORTING
          iv_chave_cte         = me->at_chave
          iv_not_code          = '99'
          iv_ovewrite          = abap_false
        EXCEPTIONS
          no_proc_allowed      = 1
          error_reading_cte    = 2
          error_creating_event = 3
          technical_error      = 4
          OTHERS               = 5.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE s014(zgrc). "Processo não permitido
          RETURN.
        WHEN 2.
          MESSAGE s011(zgrc). "Erro ao ler o documento CT-e!
          RETURN.
        WHEN 3.
          MESSAGE s013(zgrc). "Houve um erro ao criar o evento!
          RETURN.
        WHEN 4.
          MESSAGE s012(zgrc). "Houve um erro técnico!
          RETURN.
        WHEN 5.
          MESSAGE s015(zgrc). "Erro desconhecido!
          RETURN.
      ENDCASE.
*-CS2022000243-#76365-20.04.2022-JT-fim

  ENDCASE.




*  ME->MONTA_XML( RECEIVING E_XML = XML_RET ).
*
*  XML = XML_RET.
*
*  REPLACE ALL OCCURRENCES OF REGEX '[áàãâ]' IN XML WITH 'a' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[éê]'   IN XML WITH 'e' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        'í'     IN XML WITH 'i' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[óô]'   IN XML WITH 'o' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[üú]'   IN XML WITH 'u' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF REGEX '[ç]'    IN XML WITH 'c' IGNORING CASE.
*  REPLACE ALL OCCURRENCES OF        '&'     IN XML WITH '&#38;'.
*  REPLACE ALL OCCURRENCES OF        ''''    IN XML WITH '&#39;'.
*  REPLACE ALL OCCURRENCES OF        'º'     IN XML WITH 'o' IGNORING CASE.
*
*
*  CONCATENATE ME->AT_HORA_EMI(2) ':' ME->AT_HORA_EMI+2(2) ':' ME->AT_HORA_EMI+4(2) INTO XHORA.
*  CONCATENATE ME->AT_DATA_EMI(4) '-' ME->AT_DATA_EMI+4(2) '-' ME->AT_DATA_EMI+6(2) INTO XDATA.
*  CONCATENATE XDATA XHORA INTO XDHEMI SEPARATED BY SPACE.

*  WA_NOTA-TP_AUTHCOD       = '4'.
*  WA_NOTA-NU_DOCUMENTO_SAP = ME->AT_DOC_MANIFESTO.
*  WA_NOTA-ID_EMPRESA       = ME->AT_BUKRS.
*  WA_NOTA-ID_FILIAL        = ME->AT_BRANCH.
*  WA_NOTA-TB_DIRECAO       = '1'.
*  WA_NOTA-NR_NFE           = ME->AT_CHAVE+25(9).
*  WA_NOTA-SR_NFE           = ME->AT_CHAVE+22(3).
*  WA_NOTA-DT_EMISSAO       = ME->AT_DATA_EMI.
*  WA_NOTA-TX_XML           = XML(4000).
*  APPEND WA_NOTA TO IT_NOTA.
*
*  SELECT SINGLE *
*    FROM SETLEAF INTO @DATA(WL_SETLEAF)
*   WHERE SETNAME EQ 'MAGGI_CTG_NFE_SAP'.
*
*  IF ( SY-SUBRC = 0 ) AND ( WL_SETLEAF-VALFROM IS NOT INITIAL ).
*
*    DATA: WL_ZOB_NFE_SAP TYPE ZOB_NFE_SAP.
*
*    MOVE-CORRESPONDING WA_NOTA TO WL_ZOB_NFE_SAP.
*
*    CALL FUNCTION 'Z_GRAVAR_XML_NFE_CTE'
*      EXPORTING
*        I_XML01            = XML
*        I_TIPO             = '1' "NF-e
*      CHANGING
*        I_ZOB_NFE_SAP      = WL_ZOB_NFE_SAP.
*
*  ELSE.
*
*    CALL FUNCTION 'Z_SD_OUTBOUND_NFE_XML' IN BACKGROUND TASK
*      DESTINATION 'XI_XML'
*      AS SEPARATE UNIT
*      TABLES
*        IT_SAIDA = IT_NOTA.
*
*  ENDIF.
*
*  COMMIT WORK.

  MESSAGE s014(zman_dest).
  RETURN.

ENDMETHOD.


  method GET_CD_OPERACAO.

   E_CD_OPERACAO = ME->AT_CD_OPERACAO.

  endmethod.


  method GET_CHAVE.

    E_CHAVE = ME->AT_CHAVE.

  endmethod.


  method GET_CNPJ_DEST.

    E_CNPJ_DEST = ME->AT_CNPJ_DEST.

  endmethod.


  method GET_IE_DEST.

     E_IE_DEST = ME->AT_IE_DEST.

  endmethod.


  method GET_JUSTIFICATIVA.

    E_JUSTIFICATIVA = ME->AT_JUSTIFICATIVA.

  endmethod.


  METHOD gravar_manifesto.

    DATA: vl_msg_exibir TYPE string.

    DATA: wa_zsdt0127 TYPE zsdt0127.

    CLEAR: e_doc_manifesto, wa_zsdt0127.

    "Check de permissão de visão
    AUTHORITY-CHECK OBJECT 'ZNFE_INB2' ID 'ZACT_NF_IB' FIELD '001'. "Criar Eventos Manifesto Destinatário
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_sem_permissao-msgid
                            msgno = zcx_manifesto_dest=>zcx_sem_permissao-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_sem_permissao-msgno
          msgid  = zcx_manifesto_dest=>zcx_sem_permissao-msgid.
    ENDIF.

*---------------------------------------------------------------------------*
*   Gravar Manifesto Destinatario
*---------------------------------------------------------------------------*
    wa_zsdt0127-chave               = me->at_chave.
    wa_zsdt0127-cd_operacao         = me->at_cd_operacao.
    wa_zsdt0127-cnpj_dest           = me->at_cnpj_dest.
    wa_zsdt0127-ie_dest             = me->at_ie_dest.
    wa_zsdt0127-justificativa       = me->at_justificativa.
    wa_zsdt0127-bukrs               = me->at_bukrs.
    wa_zsdt0127-branch              = me->at_branch.

    IF ( wa_zsdt0127-chave IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_chave_nao_definida-msgid
                            msgno = zcx_manifesto_dest=>zcx_chave_nao_definida-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_chave_nao_definida-msgno
          msgid  = zcx_manifesto_dest=>zcx_chave_nao_definida-msgid.
    ENDIF.

    IF strlen( wa_zsdt0127-chave ) NE 44 .
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_chave_invalida-msgid
                            msgno = zcx_manifesto_dest=>zcx_chave_invalida-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_chave_invalida-msgno
          msgid  = zcx_manifesto_dest=>zcx_chave_invalida-msgid.
    ENDIF.

    IF ( wa_zsdt0127-cd_operacao IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_operacao_nao_informada-msgid
                            msgno = zcx_manifesto_dest=>zcx_operacao_nao_informada-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_operacao_nao_informada-msgno
          msgid  = zcx_manifesto_dest=>zcx_operacao_nao_informada-msgid.
    ENDIF.

*---------------------------------------------------------------------*
*    Operações Previstas
*    ---------------------------------------
*    210200 – Confirmação da Operação
*    210210 – Ciência da Operação
*    210220 – Desconhecimento da Operação
*    210240 – Operação não Realizada
*---------------------------------------------------------------------*

    IF ( wa_zsdt0127-cd_operacao NE '210200' ) AND
       ( wa_zsdt0127-cd_operacao NE '210210' ) AND
       ( wa_zsdt0127-cd_operacao NE '210220' ) AND
       ( wa_zsdt0127-cd_operacao NE '210240' ) AND
       ( wa_zsdt0127-cd_operacao NE '610110' ). "*-CS2022000243-#76365-26.04.2022-JT-inicio

      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_operacao_nao_prevista-msgid
                            msgno = zcx_manifesto_dest=>zcx_operacao_nao_prevista-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_operacao_nao_prevista-msgno
          msgid  = zcx_manifesto_dest=>zcx_operacao_nao_prevista-msgid.
    ENDIF.

    IF wa_zsdt0127-cd_operacao NE '610110'. "*-CS2022000243-#76365-26.04.2022-JT-inicio
      IF ( wa_zsdt0127-cnpj_dest IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_manifesto_dest
          EXPORTING
            textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_cnpj_dest_nao_definido-msgid
                              msgno = zcx_manifesto_dest=>zcx_cnpj_dest_nao_definido-msgno )
            msgty  = 'E'
            msgno  = zcx_manifesto_dest=>zcx_cnpj_dest_nao_definido-msgno
            msgid  = zcx_manifesto_dest=>zcx_cnpj_dest_nao_definido-msgid.
      ENDIF.

      IF ( wa_zsdt0127-ie_dest IS INITIAL ).
        RAISE EXCEPTION TYPE zcx_manifesto_dest
          EXPORTING
            textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_ie_dest_nao_definido-msgid
                              msgno = zcx_manifesto_dest=>zcx_ie_dest_nao_definido-msgno )
            msgty  = 'E'
            msgno  = zcx_manifesto_dest=>zcx_ie_dest_nao_definido-msgno
            msgid  = zcx_manifesto_dest=>zcx_ie_dest_nao_definido-msgid.
      ENDIF.
    ENDIF.

    "Justificativa é obrigatário para a evento 210240 ( Operação não Realizada ).
    IF ( wa_zsdt0127-justificativa IS INITIAL  ) AND
       ( wa_zsdt0127-cd_operacao   EQ '210240' ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_just_obrig_evento-msgid
                            msgno = zcx_manifesto_dest=>zcx_just_obrig_evento-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_just_obrig_evento-msgno
          msgid  = zcx_manifesto_dest=>zcx_just_obrig_evento-msgid.
    ENDIF.

    IF ( wa_zsdt0127-bukrs IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_empresa_nao_definida-msgid
                            msgno = zcx_manifesto_dest=>zcx_empresa_nao_definida-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_empresa_nao_definida-msgno
          msgid  = zcx_manifesto_dest=>zcx_empresa_nao_definida-msgid.
    ENDIF.

    IF ( wa_zsdt0127-branch IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_filial_nao_definida-msgid
                            msgno = zcx_manifesto_dest=>zcx_filial_nao_definida-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_filial_nao_definida-msgno
          msgid  = zcx_manifesto_dest=>zcx_filial_nao_definida-msgid.
    ENDIF.

    SELECT SINGLE *
      FROM zib_nfe_forn INTO @DATA(wl_zib_nfe_forn)
     WHERE nu_chave EQ @me->at_chave.

    IF ( sy-subrc EQ 0 ) AND ( wl_zib_nfe_forn-docnum IS NOT INITIAL ).

      IF wa_zsdt0127-cd_operacao EQ '210220' OR "Desconhecimento da Operação
         wa_zsdt0127-cd_operacao EQ '210240'.   "Operação não Realizada

        RAISE EXCEPTION TYPE zcx_manifesto_dest
          EXPORTING
            textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_operation_not_allowed-msgid
                              msgno = zcx_manifesto_dest=>zcx_operation_not_allowed-msgno )
            msgty  = 'E'
            msgno  = zcx_manifesto_dest=>zcx_operation_not_allowed-msgno
            msgid  = zcx_manifesto_dest=>zcx_operation_not_allowed-msgid.

      ENDIF.

    ENDIF.

    "Atribui Numero Documento para o Manifesto
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZMAN_DEST'
      IMPORTING
        number      = me->at_doc_manifesto.

    IF ( sy-subrc <> 0 ) OR (  me->at_doc_manifesto IS INITIAL ).
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_erro_determinar_doc_man-msgid
                            msgno = zcx_manifesto_dest=>zcx_erro_determinar_doc_man-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_erro_determinar_doc_man-msgno
          msgid  = zcx_manifesto_dest=>zcx_erro_determinar_doc_man-msgid.
    ENDIF.

    wa_zsdt0127-doc_manifesto = me->at_doc_manifesto.

    wa_zsdt0127-usnam         = sy-uname.
    wa_zsdt0127-data_emi      = sy-datum.

    IF sy-timlo IS NOT INITIAL.
      wa_zsdt0127-hora_emi  = sy-timlo.
    ELSE.
      wa_zsdt0127-hora_emi  = sy-uzeit.
    ENDIF.

    me->at_data_emi = wa_zsdt0127-data_emi.
    me->at_hora_emi = wa_zsdt0127-hora_emi.

    INSERT zsdt0127 FROM wa_zsdt0127.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_manifesto_dest
        EXPORTING
          textid = VALUE #( msgid = zcx_manifesto_dest=>zcx_erro_gravar_manifesto-msgid
                            msgno = zcx_manifesto_dest=>zcx_erro_gravar_manifesto-msgno )
          msgty  = 'E'
          msgno  = zcx_manifesto_dest=>zcx_erro_gravar_manifesto-msgno
          msgid  = zcx_manifesto_dest=>zcx_erro_gravar_manifesto-msgid.
    ENDIF.

    e_doc_manifesto = wa_zsdt0127-doc_manifesto.
    MESSAGE s012(zman_dest).

  ENDMETHOD.


  method MONTA_XML.

    DEFINE CONC_XML.
      CONCATENATE E_XML &1 INTO E_XML.
    END-OF-DEFINITION.

    CLEAR: E_XML. "Limpar a variavel de retorno.

    CONC_XML '<intgManifestoNFe>'. " TAG de grupo das informações da Manifestação

      "----------------------------------------------------------*
      " Chave de acesso da NF-e
      "----------------------------------------------------------*
      CONC_XML     '<chNFe>'.
      CONC_XML        ME->AT_CHAVE.
      CONC_XML     '</chNFe>'.

      "----------------------------------------------------------*
      " Código do evento
      "----------------------------------------------------------*
      CONC_XML     '<tpEvento>'.
      CONC_XML        ME->AT_CD_OPERACAO.
      CONC_XML     '</tpEvento>'.

      "----------------------------------------------------------*
      " CNPJ do destinatário da NF-e
      "----------------------------------------------------------*

      CONC_XML     '<cnpj>'.
      CONC_XML        ME->AT_CNPJ_DEST.
      CONC_XML     '</cnpj>'.

      "----------------------------------------------------------*
      " Inscrição Estadual do destinatário da NF-e
      "----------------------------------------------------------*

      CONC_XML     '<inscEstd>'.
      CONC_XML        ME->AT_IE_DEST.
      CONC_XML     '</inscEstd>'.

      "----------------------------------------------------------*
      " Motivo da operação não realizada, texto livre.
      " Este campo deve ser informado somente no evento de Operação não Realizada (210240).
      "----------------------------------------------------------*
      IF ME->AT_CD_OPERACAO EQ '210240'.
        CONC_XML     '<xJust>'.
        CONC_XML        ME->AT_JUSTIFICATIVA.
        CONC_XML     '</xJust>'.
      ENDIF.

    CONC_XML '</intgManifestoNFe>'.

  endmethod.


  method SET_BRANCH.
    ME->AT_BRANCH = I_BRANCH.
  endmethod.


  method SET_BUKRS.

     ME->AT_BUKRS = I_BUKRS.

  endmethod.


  method SET_CD_OPERACAO.

    ME->AT_CD_OPERACAO = I_CD_OPERACAO.

  endmethod.


  method SET_CHAVE.

    ME->AT_CHAVE = I_CHAVE.

  endmethod.


  method SET_CNPJ_DEST.

    ME->AT_CNPJ_DEST = I_CNPJ_DEST.

  endmethod.


  method SET_DATA_EMI.

    ME->AT_DATA_EMI = I_DATA_EMI.

  endmethod.


  method SET_DOC_MANIFESTO.

    ME->AT_DOC_MANIFESTO = I_DOC_MANIFESTO.

  endmethod.


  method SET_HORA_EMI.

   ME->AT_HORA_EMI = I_HORA_EMI.

  endmethod.


  method SET_IE_DEST.

    ME->AT_IE_DEST = I_IE_DEST.

  endmethod.


  method SET_JUSTIFICATIVA.

    ME->AT_JUSTIFICATIVA =  I_JUSTIFICATIVA.

  endmethod.


  METHOD SET_VALIDA_ANULACAO_CONF_OPER.

    FREE: e_ok_anular.

*-------------------------
*-- verificar se DRC
*-------------------------
    SELECT SINGLE *
       FROM edobrincoming
       INTO @DATA(w_edoc)
      WHERE accesskey = @i_zsdt0127-chave.

    IF sy-subrc <> 0.
      SELECT SINGLE *
         FROM edobrcteincoming
         INTO @DATA(w_edoccte)
        WHERE accesskey = @i_zsdt0127-chave.
    ENDIF.

    CHECK sy-subrc = 0.

    SELECT *
      FROM zsdt0127
      INTO TABLE @DATA(t_0127)
     WHERE chave = @i_zsdt0127-chave.

    delete t_0127 WHERE autorizado = space.
    SORT t_0127 by dt_authcod DESCENDING
                   hr_authcod DESCENDING.

    CHECK t_0127[] is NOT INITIAL.
    READ TABLE t_0127  INTO DATA(w_0127) INDEX 1.
    CHECK sy-subrc eq 0.

    CASE w_0127-cd_operacao.
      WHEN '210200'. "Confirmação Operação.
        e_ok_anular = abap_true.
        EXIT.
    ENDCASE.


  ENDMETHOD.


  METHOD SET_VALIDA_ANULACAO_REJEICAO.

    FREE: e_ok_anular.

*-------------------------
*-- verificar se DRC
*-------------------------
    SELECT SINGLE *
       FROM edobrincoming
       INTO @DATA(w_edoc)
      WHERE accesskey = @i_zsdt0127-chave.

    IF sy-subrc <> 0.
      SELECT SINGLE *
         FROM edobrcteincoming
         INTO @DATA(w_edoccte)
        WHERE accesskey = @i_zsdt0127-chave.
    ENDIF.

    CHECK sy-subrc = 0.

    SELECT *
      FROM zsdt0127
      INTO TABLE @DATA(t_0127)
     WHERE chave = @i_zsdt0127-chave.

    delete t_0127 WHERE autorizado = space.
    SORT t_0127 by dt_authcod DESCENDING
                   hr_authcod DESCENDING.

    CHECK t_0127[] is NOT INITIAL.
    READ TABLE t_0127  INTO DATA(w_0127) INDEX 1.
    CHECK sy-subrc eq 0.

    CASE w_0127-cd_operacao.
      WHEN '210240' OR "Operação não Realizada
           '210220' OR "Desconhecimento da Operação
           '610110'.   "Desacordo prestacao de servicos
        e_ok_anular = abap_true.
        EXIT.
    ENDCASE.

  ENDMETHOD.


  METHOD set_valida_enviar_drc.

    DATA: mo_action_handler TYPE REF TO cl_edoc_action,
          go_cockpit        TYPE REF TO cl_edoc_cockpit,
          go_criteria       TYPE REF TO cl_edoc_sel_criteria,
          lv_action         TYPE edoc_action,
          ls_edocument      TYPE edoc_reslist_field,
          lt_edocument      TYPE edoc_reslist_field_tab,
          it_range          TYPE rsds_trange,
          wa_zsdt0127       TYPE zsdt0127.

    FREE: lt_edocument,
          lv_action,
          e_envia_drc.

    FREE MEMORY ID 'ZSDT0127'.

    wa_zsdt0127 = i_zsdt0127.

*-------------------------
*-- verificar se DRC
*-------------------------
    SELECT SINGLE *
       FROM edobrincoming
       INTO @DATA(w_edoc)
      WHERE accesskey = @i_zsdt0127-chave.

    IF sy-subrc <> 0.
      CLEAR w_edoc.
      SELECT SINGLE *
         FROM edobrcteincoming
         INTO @DATA(w_edoccte)
        WHERE accesskey = @i_zsdt0127-chave.
    ENDIF.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      from tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name = 'DRC_ACTIVE_MANIFESTO'
       AND LOW  = @ABAP_TRUE.

    CHECK SY-SUBRC EQ 0.

    e_envia_drc = abap_true.

    IF w_edoc IS NOT INITIAL.
      MOVE-CORRESPONDING w_edoc      TO ls_edocument.
    ELSE.
      MOVE-CORRESPONDING w_edoccte   TO ls_edocument.
    ENDIF.
    APPEND ls_edocument              TO lt_edocument.

    CREATE OBJECT: go_cockpit,
                   go_criteria EXPORTING io_cockpit = go_cockpit.

*-------------------------------------
*-- Exportar manifesto p/ memoria
*-------------------------------------
    EXPORT wa_zsdt0127 = wa_zsdt0127 TO MEMORY ID 'ZSDT0127'.

*-------------------------------------
*-- Setar parametroa para envio SEFAZ
*-------------------------------------
    TRY.
        go_cockpit->start_ui( EXPORTING iv_container_name = 'C_CONTAINER'
                                        it_range          = it_range ). "2765690
      CATCH cx_edocument.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.

*-------------------------------------
*-- gravar a rejeicao no DRC
*-------------------------------------
    IF i_action IS NOT INITIAL.
      lv_action     = i_action.
    ELSE.
      CASE me->at_cd_operacao.
        WHEN '210200'.   "Confirmação da Operação
          lv_action = 'ERP_TASKS'.
        WHEN '210210'.   "Ciencia Operação
          lv_action = 'CREA'.
        WHEN '210240' OR "Operação não Realizada
             '210220' OR "Desconhecimento da Operação
             '610110'.   "Desacordo prestacao de servicos
          lv_action = 'REJECT'.
      ENDCASE.
    ENDIF.

    IF lv_action IS NOT INITIAL.
      TRY .
          go_criteria->set_manifesto( COND #( WHEN w_edoc IS NOT INITIAL THEN 'BRBASIC'
                                                                         ELSE 'BRCTEFLEX' ) ).
          go_cockpit->mo_action->run(  iv_action    = lv_action
                                       it_edocument = lt_edocument ).
        CATCH cx_edocument.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.

      COMMIT WORK.
    ENDIF.

*-------------------------------------
*-- Enviar manifesto SEFAZ
*-------------------------------------
*    CASE me->at_cd_operacao.
*      WHEN '210200'.
*        lv_action = 'MD_CONF'.
*      WHEN '210220'.   "Desconhecimento da Operação
*        lv_action = 'MD_DENIAL'.
*      WHEN '210240'.   "Desconhecimento da Operação
*        lv_action = 'MD_NO_CONF'.
*    ENDCASE.

*   TRY .
*     go_cockpit->mo_action->run( EXPORTING iv_action    = lv_action
*                                           it_edocument = lt_edocument ).
*     CATCH cx_edocument.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*   ENDTRY.
*
*   COMMIT WORK.

    MESSAGE s014(zman_dest).

  ENDMETHOD.
ENDCLASS.
