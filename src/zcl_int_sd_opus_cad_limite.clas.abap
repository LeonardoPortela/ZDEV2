class ZCL_INT_SD_OPUS_CAD_LIMITE definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_tcurr,
        kurst TYPE tcurr-kurst,
        fcurr TYPE tcurr-fcurr,
        tcurr TYPE tcurr-tcurr,
        gdatu TYPE char10,
        ukurs TYPE tcurr-ukurs,
        ffact TYPE tcurr-ffact,
        TFACT TYPE tcurr-TFACT,
      END OF ty_tcurr .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '292' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.

  methods CONVERT_CULTURA
    importing
      !IV_VALUE type CHAR10
    returning
      value(RV_VALUE) type ZSDED001 .
  methods CONVERT_CURRENCY
    importing
      !IV_VALUE type CHAR30
    returning
      value(RV_VALUE) type DMBTR
    raising
      CX_SY_CONVERSION_NO_NUMBER .
  methods GET_ID_SAP
    returning
      value(RV_VALUE) type ZSDE_ID_LIMITE_SAP .
ENDCLASS.



CLASS ZCL_INT_SD_OPUS_CAD_LIMITE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD convert_cultura.

    DATA lv_cultura TYPE acc_txtlg.

    CHECK iv_value IS NOT INITIAL.

    lv_cultura = iv_value.

    TRANSLATE lv_cultura TO LOWER CASE.
    TRANSLATE lv_cultura(1) TO UPPER CASE.

    CONDENSE lv_cultura.

    lv_cultura = '%' && lv_cultura && '%'.

    SELECT SINGLE cultura FROM zsdt0038
      INTO rv_value
        WHERE descricao LIKE lv_cultura.

    IF sy-subrc NE 0.
      rv_value = iv_value.
    ENDIF.

  ENDMETHOD.


  METHOD convert_currency.

    DATA lo_exc TYPE REF TO cx_sy_conversion_no_number.
    DATA lv_string TYPE c LENGTH 30.

    lv_string = iv_value.

    REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN lv_string WITH '.'.

    CONDENSE lv_string NO-GAPS.

    TRY.
        rv_value = lv_string.
      CATCH cx_root.

        CREATE OBJECT lo_exc.
        RAISE EXCEPTION lo_exc.

    ENDTRY.

  ENDMETHOD.


  METHOD get_id_sap.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSDLIMITID'
      IMPORTING
        number                  = rv_value
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      CLEAR rv_value.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA lv_safra TYPE ajahr.
    DATA ls_data_response TYPE zsds_limite_opus.
    DATA lv_field TYPE c LENGTH 10.
    DATA lv_msgx TYPE c LENGTH 40.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = ls_data_response ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF ls_data_response IS INITIAL.
      r_msg_erro = 'Informar json'.
      RETURN.
    ENDIF.

    " ===== VALIDAÇÃO DADOS NULOS
    IF ls_data_response-id_limite CA sy-abcde OR ls_data_response-id_limite IS INITIAL.
      lv_field = 'id_limite'.
      lv_msgx = 'Valor inválido para'.
    ENDIF.

    IF ls_data_response-empresa CA sy-abcde OR ls_data_response-empresa IS INITIAL.
      lv_field = 'empresa'.
      lv_msgx = 'Valor inválido para'.
    ENDIF.

*    IF ls_data_response-emitente CA sy-abcde OR ls_data_response-emitente IS INITIAL.
*      lv_field = 'emitente'.
*      lv_msgx = 'Valor inválido para'.
*    ENDIF.

    IF ls_data_response-safra CA sy-abcde OR ls_data_response-safra  IS INITIAL.
      lv_field = 'safra'.
      lv_msgx = 'Valor inválido para'.
    ENDIF.

    IF ls_data_response-cultura IS INITIAL.
      lv_field = 'cultura'.
      lv_msgx = 'Informar valor para'.
    ENDIF.

    IF ls_data_response-valor_limite CA sy-abcde OR ls_data_response-valor_limite IS INITIAL.
      lv_field = 'valor_limite'.
      lv_msgx = 'Valor inválido para'.
    ENDIF.

    IF lv_field IS NOT INITIAL.
      r_msg_erro = lv_msgx && ` ` && lv_field.
      RETURN.
    ENDIF.

    " ===== VALIDAÇÃO DADOS EXISTENTES
    SELECT COUNT(*) FROM t001 WHERE bukrs = ls_data_response-empresa.

    IF sy-dbcnt = 0.
      lv_field = 'Empresa:'.
      lv_msgx = ls_data_response-empresa && ` ` && `não existe`.
    ENDIF.

*    SELECT COUNT(*) FROM tvbur WHERE vkbur = ls_data_response-filial.
*
*    IF sy-dbcnt = 0.
*      lv_field = 'Filial:'.
*      lv_msgx = ls_data_response-empresa && ` ` && `não existe`.
*    ENDIF.

    lv_safra = ls_data_response-safra.

    IF lv_safra < 2022.
      lv_field = 'Safra:'.
      lv_msgx = ls_data_response-cultura && ` ` && `inválida`.
    ENDIF.

    IF lv_field IS NOT INITIAL.
      r_msg_erro = lv_field && ` ` && lv_msgx.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


METHOD zif_integracao_inject~set_integrar_inbound.

  DATA: ls_data_request  TYPE zsds_limite_opus,
        ls_data_response TYPE zsds_limite_opus_resp.

  DATA:
    lra_kurst TYPE RANGE OF kurst_curr,
    lra_fcurr TYPE RANGE OF fcurr_curr,
    lra_tcurr TYPE RANGE OF tcurr_curr,
    lra_gdatu TYPE RANGE OF char10.

  DATA: v_gdatu_o    TYPE gdatu_inv,
        v_gdatu_u    TYPE char10,
        v_data_saida TYPE char10.

  r_if_integracao_inject = me.

  CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, ls_data_response.

  IF i_msg_inbound IS NOT INITIAL.
    /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = ls_data_request ).
  ENDIF.

  me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  DATA(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

  IF e_msg_erro IS NOT INITIAL.

    IF _status_code IS INITIAL .
      _status_code = '400'. "Bad Request
    ENDIF.

    e_sucesso      = abap_true.
    e_nm_code      = _status_code.
    e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                     '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                     '}'.
    RETURN.
  ENDIF.

  DATA ls_405 TYPE zsdt0405.
  DATA lt_emit TYPE TABLE OF zsdt0405_emit.

  ls_405-id_limite = ls_data_request-id_limite.
  ls_405-vkorg = ls_data_request-empresa.
  ls_405-safra = ls_data_request-safra.
  ls_405-cultura = ls_data_request-cultura.
  "ls_405-vlr_limite = ls_data_request-valor_limite.

  SELECT SINGLE * FROM zsdt0405
    INTO @DATA(ls_change)
    WHERE id_limite = @ls_405-id_limite
      AND vkorg = @ls_405-vkorg
      AND safra = @ls_405-safra
      AND cultura = @ls_405-cultura
      AND cancelado = @abap_false.

  IF sy-subrc EQ 0.

    ls_405-id_limite_sap = ls_change-id_limite_sap.

    ls_405-user_create = ls_change-user_create.
    ls_405-date_create = ls_change-date_create.
    ls_405-time_create = ls_change-time_create.

    ls_405-user_change = sy-uname.
    ls_405-date_change = sy-datum.
    ls_405-time_change = sy-uzeit.

  ELSE.

    ls_405-id_limite_sap = me->get_id_sap( ).

    IF ls_405-id_limite_sap IS INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_msg_erro = `Cadastrar SNRO no SAP: ZSDLIMITID: `.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.

    ENDIF.

    ls_405-user_create = sy-uname.
    ls_405-date_create = sy-datum.
    ls_405-time_create = sy-uzeit.

  ENDIF.

  ls_405-vkorg = ls_data_request-empresa.
  ls_405-safra = ls_data_request-safra.
  ls_405-cultura = me->convert_cultura( ls_data_request-cultura ).

  IF ls_data_request-moeda IS INITIAL.
    ls_405-waers = 'USD'.
  ELSE.
    ls_405-waers = ls_data_request-moeda.
  ENDIF.

  TRY.
      ls_405-vlr_limite = me->convert_currency( ls_data_request-valor_limite ).
    CATCH cx_sy_conversion_no_number.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      e_msg_erro = `Falha ao converter valor_limite: ` && ls_data_request-valor_limite.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.

  ENDTRY.

  CHECK ls_data_request-emitentes IS NOT INITIAL.

  IF ls_change-vlr_limite <> ls_405-vlr_limite AND ls_change-vlr_limite IS NOT INITIAL.

    " Cancela o limite se o valor foi alterado
    ls_change-cancelado = abap_true.

    ls_change-user_change = sy-uname.
    ls_change-date_change = sy-datum.
    ls_change-time_change = sy-uzeit.

    MODIFY zsdt0405 FROM ls_change.

    " atribui um novo id do sap
    ls_405-id_limite_sap = me->get_id_sap( ).

    " reinicia os campos de change
    ls_405-user_create = sy-uname.
    ls_405-date_create = sy-datum.
    ls_405-time_create = sy-uzeit.

    CLEAR ls_405-user_change.
    CLEAR ls_405-date_change.
    CLEAR ls_405-time_change.


  ENDIF.

  LOOP AT ls_data_request-emitentes ASSIGNING FIELD-SYMBOL(<fs_emitentes>).

    APPEND INITIAL LINE TO lt_emit ASSIGNING FIELD-SYMBOL(<fs_405_emit>).

    <fs_405_emit>-id_limite_sap = ls_405-id_limite_sap.
    <fs_405_emit>-id_limite = ls_405-id_limite.
    <fs_405_emit>-emitente = <fs_emitentes>-emitente.

  ENDLOOP.

  SORT lt_emit.

  DELETE ADJACENT DUPLICATES FROM lt_emit COMPARING ALL FIELDS.


  MODIFY zsdt0405 FROM ls_405.
  MODIFY zsdt0405_emit FROM TABLE lt_emit.

  COMMIT WORK AND WAIT.


  e_sucesso   = abap_true.
  ls_data_response-error = ''.
  ls_data_response-status_code = '200'.
  e_nm_code   = '200'.
  e_msg_erro  = 'Ok'.
  e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = ls_data_response ).

ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
