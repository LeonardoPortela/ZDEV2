CLASS zcl_int_ib_adto_prod_comp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_zfit0125,
        ch_referencia TYPE zfit0125-ch_referencia,
        nr_item       TYPE zfit0125-nr_item,
        bukrs         TYPE zfit0125-bukrs,
        lifnr         TYPE zfit0125-lifnr,
        belnr         TYPE zfit0125-belnr,
        waers         TYPE zfit0125-waers,
        dt_lcto       TYPE zfit0125-dt_lcto,
        umsks         TYPE zfit0125-umsks,
        umskz         TYPE zfit0125-umskz,
        dmbtr         TYPE zfit0125-dmbtr,
        tp_proc       TYPE zfit0125-tp_proc,
        tp_baixa      TYPE zfit0125-tp_baixa,
        rg_atualizado TYPE zfit0125-rg_atualizado,
        dt_atual      TYPE zfit0125-dt_atual,
        hr_atual      TYPE zfit0125-hr_atual,
      END OF ty_zfit0125 .

    DATA:
      tb_zfit0125 TYPE TABLE OF ty_zfit0125.
    DATA:
      BEGIN OF z_response,
       response TYPE String,
      END OF z_response .
    DATA:
      BEGIN OF zde_data_request,
        limites LIKE tb_zfit0125,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
       response LIKE TABLE OF z_response,

      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '164' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_ADTO_PROD_COMP IMPLEMENTATION.


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

    TYPES:
      BEGIN OF ty_zfit0125_1,
        mandt         TYPE zfit0125-mandt,
        ch_referencia TYPE zfit0125-ch_referencia,
        nr_item       TYPE zfit0125-nr_item,
        bukrs         TYPE zfit0125-bukrs,
        lifnr         TYPE zfit0125-lifnr,
        belnr         TYPE zfit0125-belnr,
        waers         TYPE zfit0125-waers,
        dt_lcto       TYPE zfit0125-dt_lcto,
        umsks         TYPE zfit0125-umsks,
        umskz         TYPE zfit0125-umskz,
        dmbtr         TYPE zfit0125-dmbtr,
        tp_proc       TYPE zfit0125-tp_proc,
        tp_baixa      TYPE zfit0125-tp_baixa,
        rg_atualizado TYPE zfit0125-rg_atualizado,
        dt_atual      TYPE zfit0125-dt_atual,
        hr_atual      TYPE zfit0125-hr_atual,
      END OF ty_zfit0125_1.

    DATA: it_zfit0125_1 TYPE STANDARD TABLE OF ty_zfit0125_1,
          ls_zfit0125   TYPE ty_zfit0125_1.
*          lra_limites  TYPE RANGE OF matnr.

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

    LOOP AT lwa_data_request-limites INTO DATA(wa_limites).

      MOVE-CORRESPONDING: wa_limites TO ls_zfit0125.
      APPEND ls_zfit0125 TO it_zfit0125_1.
      CLEAR ls_zfit0125.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_limites ) TO lra_limites.
    ENDLOOP.

    MODIFY zfit0125 FROM TABLE it_zfit0125_1.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
*      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).
      e_msg_outbound = ' { "protocolo" : "' && me->zif_integracao_inbound~at_id_integracao &&  '" , '  && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code     && '" ' && cl_abap_char_utilities=>newline && ' }'..
    ENDIF.




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
