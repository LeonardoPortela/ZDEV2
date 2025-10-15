CLASS ZCL_NOTAS_ADMIN DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES ZIF_INTEGRACAO_INJECT .
    INTERFACES ZIF_INTEGRACAO_INBOUND .
TYPES:

  BEGIN OF TY_ERROS,
    MENSAGEM TYPE CHAR100,
  END OF TY_ERROS.


    DATA:
      BEGIN OF ZDE_DATA_REQUEST,
        DADOS_PROC TYPE TABLE OF ZMMT0024,
      END OF ZDE_DATA_REQUEST ,

      BEGIN OF ZDE_DATA_RESPONSE,
            ERROS TYPE TABLE OF TY_ERROS,
      END OF ZDE_DATA_RESPONSE .

    METHODS CONSTRUCTOR .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '298' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_NOTAS_ADMIN IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound. "Out
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_assincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'CADASTRO_CLIENTE'.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.


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

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

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

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

    DATA: W_DATA_INBOUND   TYPE ZSDT0325_INB_CLIENTE,
          LIT_DATA_INBOUND LIKE ZDE_DATA_REQUEST,
          V_OBJKEY         TYPE ZMMT0024-OBJKEY,
          V_NUM_DOC        TYPE ZMMT0024-NR_DOCUMENTO,
          IT_ZMM0124       TYPE TABLE OF ZMMT0024,
          L_ERRO(1).

    DATA: LWA_DATA_RESPONSE_ERRO LIKE ZDE_DATA_RESPONSE.


    TYPES:
      BEGIN OF TY_ERRO,
        MENSAGEM TYPE CHAR100,
      END OF TY_ERRO.

    DATA: IT_MENSAGEM TYPE TABLE OF  TY_ERRO,
          WA_MENSAGEM TYPE  TY_ERRO.

    CLEAR: R_MSG_ERRO.

    IF ( ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO NE 'POST' AND  ME->ZIF_INTEGRACAO_INJECT~AT_INFO_REQUEST_HTTP-DS_METODO NE 'PUT' ).
      R_MSG_ERRO = 'Metodo informado não reconhecido!'.
      E_STATUS_CODE  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF I_DATA_INBOUND IS INITIAL.
      R_MSG_ERRO = 'Payload Requisição não pode ser vazio!'.
      E_STATUS_CODE = '402'. "Payment Required
      RETURN.
    ENDIF.

    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_DATA_INBOUND CHANGING DATA = LIT_DATA_INBOUND ).

IT_ZMM0124 = LIT_DATA_INBOUND-DADOS_PROC.
    SORT IT_ZMM0124 BY OBJKEY.
APPEND INITIAL LINE TO LWA_DATA_RESPONSE_ERRO-ERROS ASSIGNING FIELD-SYMBOL(<FS_ERRO>).
    LOOP AT IT_ZMM0124 INTO DATA(LWA_DATA_INBOUND).


      IF V_OBJKEY IS INITIAL.
        V_OBJKEY = LWA_DATA_INBOUND-OBJKEY.
        V_NUM_DOC = LWA_DATA_INBOUND-NR_DOCUMENTO.
      ELSE.
        IF V_OBJKEY EQ LWA_DATA_INBOUND-OBJKEY.
          "IF V_NUM_DOC IS INITIAL.
           " V_NUM_DOC = LWA_DATA_INBOUND-NR_DOCUMENTO.
          "ELSE.
            IF V_NUM_DOC NE LWA_DATA_INBOUND-NR_DOCUMENTO.
              WA_MENSAGEM = 'Para cada Objkey o NR_DOCUMNETO deve ser igual'.
              <FS_ERRO>-MENSAGEM = WA_MENSAGEM.
              L_ERRO = 'X'.
              exit.
            ENDIF.
          "ENDIF.
        ELSE.
          V_OBJKEY = LWA_DATA_INBOUND-OBJKEY.
          V_NUM_DOC = LWA_DATA_INBOUND-NR_DOCUMENTO.
        ENDIF.
      ENDIF.
      IF LWA_DATA_INBOUND-OBJKEY IS INITIAL.
        WA_MENSAGEM = 'Objkey deve estar preenchido.'.
        <FS_ERRO>-MENSAGEM = WA_MENSAGEM.
        L_ERRO = 'X'.
        exit.
      ELSE.
        SELECT SINGLE * FROM ZMMT0024 INTO @DATA(wa_ZMMT0024) WHERE OBJKEY EQ @LWA_DATA_INBOUND-OBJKEY.
        IF SY-SUBRC IS INITIAL.
          WA_MENSAGEM = 'Objkey já existe na tabela.'.
          <FS_ERRO>-MENSAGEM = WA_MENSAGEM.
          L_ERRO = 'X'.
          exit.
        ENDIF.
      ENDIF.

    ENDLOOP.


    IF L_ERRO IS NOT INITIAL.
      R_MSG_ERRO = /UI2/CL_JSON=>SERIALIZE( EXPORTING DATA = LWA_DATA_RESPONSE_ERRO ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: LIT_DATA_INBOUND LIKE ZDE_DATA_REQUEST,
          IT_ZMM0124       TYPE TABLE OF ZMMT0024,
          LVA_ERRO(1)      TYPE C.
    DATA: LWA_DATA_RESPONSE_ERRO like ZDE_DATA_RESPONSE.



    DATA: E_JSON TYPE STRING.

    FIELD-SYMBOLS <ICONE>    LIKE ICON_CHECKED.

    DATA: LVA_TYPE TYPE DD01V-DATATYPE.

*----------------------------------------------------------------------*
* Declaração para função SD_CUSTOMER_MAINTAIN_ALL
*----------------------------------------------------------------------*

    DATA: VG_MESSAGE(220)  TYPE C.

    R_IF_INTEGRACAO_INJECT = ME.


    /UI2/CL_JSON=>DESERIALIZE( EXPORTING JSON = I_MSG_INBOUND CHANGING DATA = LIT_DATA_INBOUND ).

    ME->ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND( EXPORTING I_DATA_INBOUND =  I_MSG_INBOUND IMPORTING E_STATUS_CODE  =  DATA(_STATUS_CODE)  RECEIVING R_MSG_ERRO = E_MSG_ERRO ).

    IF E_MSG_ERRO IS NOT INITIAL.

      IF _STATUS_CODE IS INITIAL .
        _STATUS_CODE = '400'. "Bad Request
      ENDIF.

        E_SUCESSO      = ABAP_TRUE.
      E_NM_CODE      = _STATUS_CODE.
      E_MSG_OUTBOUND = E_MSG_ERRO.
      RETURN.
    ENDIF.
    IT_ZMM0124 = LIT_DATA_INBOUND-DADOS_PROC.


    IF IT_ZMM0124[] IS NOT INITIAL.

      LOOP AT  IT_ZMM0124 INTO DATA(W_DATA_INBOUND).
        MODIFY ZMMT0024 FROM W_DATA_INBOUND.

      ENDLOOP.


    ELSE.
     APPEND INITIAL LINE TO LWA_DATA_RESPONSE_ERRO-ERROS ASSIGNING FIELD-SYMBOL(<FS_ERRO>).
      VG_MESSAGE = 'Erro na desserialização JSON'.
       <FS_ERRO>-MENSAGEM =  VG_MESSAGE.
      LVA_ERRO = 'X'.
    ENDIF.


    IF LVA_ERRO IS NOT INITIAL.
      E_SUCESSO      = ABAP_FALSE.
    ELSE.

    E_SUCESSO   = ABAP_TRUE.
    E_NM_CODE   = '200'.
    E_MSG_ERRO  = 'Ok'.
    E_MSG_OUTBOUND = '{  "return": "'        && 'Dados gravados com Sucesso' && '" ,' && CL_ABAP_CHAR_UTILITIES=>NEWLINE &&
                       '}'.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
