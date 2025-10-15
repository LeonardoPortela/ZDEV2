CLASS zcl_int_ib_cons_perm_ctb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

*    TYPES:
*      BEGIN OF ty_ref,
*        referencia TYPE zch_ref,
*      END OF ty_ref .
*    TYPES:
*      BEGIN OF ty_rem,
*        remessa TYPE vbeln,
*      END OF ty_rem .
*
*    TYPES:
*      BEGIN OF ty_group2,
*        nr_safra     TYPE zsdt0001-nr_safra,
*        bukrs        TYPE zsdt0001-bukrs,
*        branch       TYPE zsdt0001-branch,
*        tp_movimento TYPE zsdt0001-tp_movimento,
*        nr_romaneio  TYPE zsdt0001-nr_romaneio,
*        vbeln        TYPE zsdt0001-vbeln,
*        matnr        TYPE zsdt0001-matnr,
*        status       TYPE zsdt0001-status,
*      END OF ty_group2 .
*
*    DATA:
*      tb_referencia TYPE TABLE OF ty_ref .
*    DATA:
*      tb_remessa TYPE TABLE OF ty_rem .
*
*    TYPES:
*      BEGIN OF ty_group1,
*        ch_referencia LIKE tb_referencia,
*        doc_remessa   LIKE tb_remessa,
*      END OF ty_group1 .

    DATA:
      BEGIN OF zde_data_request ,
        useralias TYPE usalias,
        monat     TYPE monat,
        gjahr     TYPE gjahr,
      END OF zde_data_request .

*    DATA:
*      BEGIN OF zde_data_response,
*        romaneio TYPE zsdt0001,
*      END OF zde_data_response .


    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '120' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PERM_CTB IMPLEMENTATION.


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
    IF lwa_data_request-useralias IS INITIAL.
      r_msg_erro = 'User Alias é um campo obritario!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-monat IS INITIAL.
      r_msg_erro = 'Monat é um campo obritario!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-gjahr IS INITIAL.
      r_msg_erro = 'Gjahr é um campo obritario!'.
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

    TYPES:
      BEGIN OF ty_response,
        useralias      TYPE usalias,
        monat          TYPE monat,
        gjahr          TYPE gjahr,
        data_lim       TYPE zfit0033-data_lim,
        hora_lim       TYPE zfit0033-hora_lim,
        prazo_expirado TYPE char1,
      END OF ty_response.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response TYPE TABLE OF ty_response.

    DATA: lra_useralias TYPE RANGE OF usalias,
          lra_monat     TYPE RANGE OF monat,
          lra_gjahr     TYPE RANGE OF gjahr.

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

    IF lwa_data_request-useralias IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-useralias ) TO lra_useralias.
    ENDIF.

    IF lwa_data_request-monat IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-monat ) TO lra_monat.
    ENDIF.

    IF lwa_data_request-gjahr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_data_request-gjahr ) TO lra_gjahr.
    ENDIF.

    IF lra_useralias    IS NOT INITIAL OR
       lra_monat        IS NOT INITIAL OR
       lra_gjahr        IS NOT INITIAL.

      SELECT usr~useralias,
             z~gjahr,
             z~monat,
             z~data_lim,
             z~hora_lim
         FROM zfit0033 AS z INNER JOIN
              usrefus  AS usr ON z~usnam = usr~bname
        INTO TABLE @DATA(lt_dados)
        WHERE usr~useralias IN @lra_useralias
          AND z~monat IN @lra_monat
          AND z~gjahr IN @lra_gjahr.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

          APPEND INITIAL LINE TO lwa_data_response ASSIGNING FIELD-SYMBOL(<fs_response>).
          MOVE-CORRESPONDING <fs_dados> TO <fs_response>.

          CONCATENATE <fs_dados>-data_lim <fs_dados>-hora_lim INTO DATA(LVA_DATA_HORA_LIM).
          CONCATENATE SY-DATUM SY-UZEIT INTO DATA(LVA_DATA_HORA_ATUAL).

          IF LVA_DATA_HORA_LIM >= LVA_DATA_HORA_ATUAL.
            <fs_response>-prazo_expirado = 'N'.
          ELSE.
            <fs_response>-prazo_expirado = 'S'.
          ENDIF.

        ENDLOOP.

      ENDIF.

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
