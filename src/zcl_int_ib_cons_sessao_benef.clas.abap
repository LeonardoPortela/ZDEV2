CLASS zcl_int_ib_cons_sessao_benef DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES: BEGIN OF ty_sessoes_request,
             idsessao TYPE zppt0002-id_sessao,
           END OF ty_sessoes_request.

    TYPES: BEGIN OF ty_sessoes_response,
             id_sessao        TYPE zppt0002-id_sessao,
             algodao_caroco   TYPE mchb-clabs,
             fibrilha         TYPE mchb-clabs,
             caroco           TYPE mchb-clabs,
             pluma_elaboracao TYPE mchb-clabs,
           END OF ty_sessoes_response.

    DATA: lit_sessoes_request  TYPE TABLE OF ty_sessoes_request.
    DATA: lit_sessoes_response TYPE TABLE OF ty_sessoes_response.

    DATA:
      BEGIN OF zde_data_request ,
        sessoes LIKE lit_sessoes_request,
      END OF zde_data_request .

    DATA:
      BEGIN OF zde_data_response,
        sessoes LIKE lit_sessoes_response,
      END OF zde_data_response .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '216' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_SESSAO_BENEF IMPLEMENTATION.


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
    DELETE lwa_data_request-sessoes WHERE idsessao IS INITIAL.

    IF lwa_data_request-sessoes[] IS INITIAL.
      r_msg_erro = 'Sessão não foi informada!'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    TYPES: BEGIN OF ty_zppt0006,
             id_sessao           TYPE zppt0006-id_sessao,
             mblnr               TYPE zppt0006-mblnr,
             mjahr               TYPE zppt0006-mjahr,
             status_registro     TYPE zppt0006-status_registro,
             menge               TYPE zppt0006-menge,
             peso_algodao_caroco TYPE zppt0006-peso_algodao_caroco,
             peso_caroco         TYPE zppt0006-peso_caroco,
             peso_fibrilha       TYPE zppt0006-peso_fibrilha,
           END OF ty_zppt0006,

           BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             menge TYPE mseg-menge,
           END OF ty_mseg.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    DATA: lit_zppt0006 TYPE TABLE OF ty_zppt0006,
          lit_mseg     TYPE TABLE OF ty_mseg.


    DATA: lra_nro_romaneio   TYPE RANGE OF zsdt0001-nr_romaneio,
          lra_data_movimento TYPE RANGE OF zsdt0001-dt_movimento,
          lra_matnr          TYPE RANGE OF zsdt0001-matnr.

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

    SELECT id_sessao , mblnr, mjahr , status_registro, menge ,
           peso_algodao_caroco, peso_caroco, peso_fibrilha
      FROM zppt0006 AS a INTO TABLE @lit_zppt0006
      FOR ALL ENTRIES IN @lwa_data_request-sessoes
     WHERE id_sessao  EQ @lwa_data_request-sessoes-idsessao
       AND mblnr      NE @space
       AND NOT EXISTS (  SELECT mblnr
                           FROM mseg AS b
                          WHERE b~smbln EQ a~mblnr ).

    IF lit_zppt0006[] IS NOT INITIAL.
      SELECT mblnr mjahr menge
        FROM mseg INTO TABLE lit_mseg
         FOR ALL ENTRIES IN lit_zppt0006
       WHERE mblnr = lit_zppt0006-mblnr
         AND mjahr = lit_zppt0006-mjahr.
    ENDIF.


    SORT: lit_zppt0006 BY id_sessao mblnr mjahr.
    DELETE ADJACENT DUPLICATES FROM lit_zppt0006 COMPARING id_sessao mblnr mjahr.

    SORT: lit_mseg BY mblnr mjahr.
    DELETE ADJACENT DUPLICATES FROM lit_mseg COMPARING mblnr mjahr.

    LOOP AT lwa_data_request-sessoes INTO DATA(lwa_sessao).
      APPEND INITIAL LINE TO lwa_data_response-sessoes ASSIGNING FIELD-SYMBOL(<fs_sessao_response>).

      <fs_sessao_response>-id_sessao = lwa_sessao-idsessao.

      LOOP AT lit_zppt0006 INTO DATA(lwa_zppt0006) WHERE id_sessao = lwa_sessao-idsessao.

        READ TABLE lit_mseg INTO DATA(lwa_mseg) WITH KEY mblnr = lwa_zppt0006-mblnr
                                                         mjahr = lwa_zppt0006-mjahr BINARY SEARCH.
        CHECK sy-subrc EQ 0.

        CASE lwa_zppt0006-status_registro.
          WHEN '02'.

            SUBTRACT lwa_zppt0006-peso_algodao_caroco FROM  <fs_sessao_response>-algodao_caroco.
            ADD      lwa_zppt0006-menge               TO    <fs_sessao_response>-pluma_elaboracao.
            ADD      lwa_zppt0006-peso_fibrilha       TO    <fs_sessao_response>-fibrilha.
            ADD      lwa_zppt0006-peso_caroco         TO    <fs_sessao_response>-caroco.

          WHEN '06'.

            ADD      lwa_zppt0006-peso_algodao_caroco TO    <fs_sessao_response>-algodao_caroco.
            SUBTRACT lwa_zppt0006-menge               FROM  <fs_sessao_response>-pluma_elaboracao.
            SUBTRACT lwa_zppt0006-peso_fibrilha       FROM  <fs_sessao_response>-fibrilha.
            SUBTRACT lwa_zppt0006-peso_caroco         FROM  <fs_sessao_response>-caroco.

        ENDCASE.

      ENDLOOP.

    ENDLOOP.


    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data        = lwa_data_response
                                                        pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

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
