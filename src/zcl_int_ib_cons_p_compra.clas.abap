class ZCL_INT_IB_CONS_P_COMPRA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_makt,
        maktg TYPE makt-maktg,
        maktx TYPE makt-maktx,
      END OF ty_makt .

  data:
    tb_ebeln TYPE TABLE OF ebeln .
  data:
    BEGIN OF z_ebeln,
        ebeln TYPE ekko-ebeln,
        aedat TYPE ekko-aedat,
        lifnr TYPE ekko-lifnr,
        "werks TYPE ekpo-werks,
        ekpo TYPE ekpo,
      END OF z_ebeln .
  data:
    BEGIN OF zde_data_request,
        ebeln      TYPE  ekko-ebeln,
        aedat_ini  TYPE ekko-aedat,
        aedat_fim  TYPE ekko-aedat,
        lifnr      TYPE ekko-lifnr,
        werks      TYPE ekpo-werks,
      END OF zde_data_request .
  data:
    BEGIN OF zde_data_response,
        pedidos LIKE TABLE OF z_ebeln,

      END OF zde_data_response .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '201' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_P_COMPRA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

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
*
    DATA: lwa_data_request LIKE zde_data_request.

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

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    IF lwa_data_request IS INITIAL.
      r_msg_erro = 'Nenhum filtro foi informado!'.
      RETURN.
    ENDIF.

    IF lwa_data_request IS NOT INITIAL.

      IF lwa_data_request-aedat_ini IS INITIAL OR lwa_data_request-aedat_fim IS INITIAL.
        r_msg_erro = 'Data de criação de documento de compras deve ser informado'.
        RETURN.
      ENDIF.

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

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    DATA: rg_ebeln TYPE RANGE OF ebeln,
          wa_ebeln LIKE LINE  OF rg_ebeln,
          rg_aedat TYPE RANGE OF aedat,
          wa_aedat LIKE LINE  OF rg_aedat,
          rg_lifnr TYPE RANGE OF lifnr,
          wa_lifnr LIKE LINE  OF rg_lifnr,
          rg_werks TYPE RANGE OF ewerk,
          wa_werks LIKE LINE  OF rg_werks.

    TYPES:
      BEGIN OF ty_ebeln,
        ebeln TYPE ebeln,
      END OF ty_ebeln.

    DATA: lra_ebeln  TYPE RANGE OF ebeln.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
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

    IF NOT lwa_data_request-ebeln IS INITIAL.
      wa_ebeln-sign   = 'I'.
      wa_ebeln-option = 'EQ'.
      wa_ebeln-low    = lwa_data_request-ebeln.
      APPEND wa_ebeln TO rg_ebeln.
    ENDIF.

    wa_aedat-sign   = 'I'.
    wa_aedat-option = 'BT'.
    CONCATENATE lwa_data_request-aedat_ini+04(04) lwa_data_request-aedat_ini+02(02) lwa_data_request-aedat_ini(02)
    INTO wa_aedat-low.
    CONCATENATE lwa_data_request-aedat_fim+04(04) lwa_data_request-aedat_fim+02(02) lwa_data_request-aedat_fim(02)
    INTO wa_aedat-high.
    APPEND wa_aedat TO rg_aedat.

    IF NOT lwa_data_request-lifnr IS INITIAL.
      wa_lifnr-sign   = 'I'.
      wa_lifnr-option = 'EQ'.
      wa_lifnr-low    = lwa_data_request-lifnr.
      UNPACK wa_lifnr-low TO wa_lifnr-low.
      APPEND wa_lifnr TO rg_lifnr.
    ENDIF.

    IF NOT lwa_data_request-werks IS INITIAL.
      wa_werks-sign   = 'I'.
      wa_werks-option = 'EQ'.
      wa_werks-low    = lwa_data_request-werks.
      APPEND wa_werks TO rg_werks.
    ENDIF.


    SELECT ebeln, aedat, lifnr
           FROM ekko
           INTO TABLE @DATA(tl_ekko)
           WHERE ebeln IN @rg_ebeln
           AND   aedat IN @rg_aedat
           AND   lifnr IN @rg_lifnr.

    IF tl_ekko[] IS NOT INITIAL.

      SORT tl_ekko BY ebeln.

      SELECT *
             FROM ekpo
             INTO TABLE @DATA(tl_ekpo)
             FOR ALL ENTRIES IN @tl_ekko
             WHERE ebeln EQ @tl_ekko-ebeln
             AND   werks IN @rg_werks.

      SORT tl_ekpo BY ebeln ebelp.

      LOOP AT tl_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).
        APPEND INITIAL LINE TO lwa_data_response-pedidos ASSIGNING FIELD-SYMBOL(<fs_response>).

        READ TABLE tl_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>) WITH KEY ebeln = <fs_ekpo>-ebeln BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_response>-ebeln   = <fs_ekko>-ebeln.
          <fs_response>-aedat   = <fs_ekko>-aedat.
          <fs_response>-lifnr   = <fs_ekko>-lifnr.
        ENDIF.

        <fs_response>-ekpo = <fs_ekpo>.
      ENDLOOP.

    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

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
