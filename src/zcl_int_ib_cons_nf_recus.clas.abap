CLASS zcl_int_ib_cons_nf_recus DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_mess,
        werks    TYPE werks_d,
        nfenum   TYPE j_1bnfnum9,
        message  TYPE char50,
      END OF ty_mess .

    DATA:
      tb_werks   TYPE TABLE OF werks_d,
      tb_nfnum   TYPE TABLE OF j_1bnfnum9,
      tb_message TYPE TABLE OF ty_mess.

    DATA:
      BEGIN OF zde_data_request,
        werks  LIKE tb_werks,
        nfenum LIKE tb_nfnum,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        message LIKE tb_message,
      END OF zde_data_response .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '199' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_NF_RECUS IMPLEMENTATION.


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


  METHOD ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.
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

    DATA: lra_werks    TYPE RANGE OF werks_d,
          lra_nfenum   TYPE RANGE OF j_1bnfnum9,
          lv_nfenum(9) TYPE n,
          lv_werks(4)  TYPE n,
          it_message   LIKE me->tb_message,
          wa_message   TYPE ty_mess.

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

    LOOP AT lwa_data_request-werks INTO DATA(wa_werks).
      lv_werks = wa_werks.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_werks ) TO lra_werks.
    ENDLOOP.
    LOOP AT lwa_data_request-nfenum INTO DATA(wa_nfenum).
      lv_nfenum = wa_nfenum.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_nfenum ) TO lra_nfenum.
    ENDLOOP.

    SELECT werks, nfenum, docnum_ret
      INTO TABLE @DATA(it_ret)
      FROM zsdt_retlote
      WHERE werks  IN @lra_werks
        AND nfenum IN @lra_nfenum .

    IF sy-subrc IS INITIAL AND it_ret[] IS NOT INITIAL.
      SORT it_ret BY docnum_ret.

      SELECT docnum, werks, finalidade
        FROM zsdt_export
        INTO TABLE @DATA(it_export)
        FOR ALL ENTRIES IN @it_ret
        WHERE docnum EQ @it_ret-docnum_ret
          AND werks  EQ @it_ret-werks.

      IF sy-subrc IS INITIAL AND it_export[] IS NOT INITIAL.
        SORT it_export BY docnum werks.

        SELECT docnum, candat
          FROM j_1bnfdoc
          INTO TABLE @DATA(it_j_1bnfdoc)
          FOR ALL ENTRIES IN @it_export
          WHERE docnum    EQ @it_export-docnum.

        IF sy-subrc IS INITIAL AND it_j_1bnfdoc[] IS NOT INITIAL.
          SORT it_j_1bnfdoc BY docnum.

          LOOP AT it_ret ASSIGNING FIELD-SYMBOL(<fs_ret>).

            READ TABLE it_export INTO DATA(wa_export) WITH KEY docnum = <fs_ret>-docnum_ret
                                                               werks  = <fs_ret>-werks
                                                      BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              READ TABLE it_j_1bnfdoc INTO DATA(wa_j_1bnfdoc) WITH KEY docnum = wa_export-docnum
                                                        BINARY SEARCH.
              IF sy-subrc IS INITIAL.

                IF ( ( wa_export-finalidade EQ 'R' OR wa_export-finalidade EQ 'X' ) AND ( wa_j_1bnfdoc-candat EQ '00000000' ) ).
                  wa_message-werks = <fs_ret>-werks.
                  wa_message-nfenum = <fs_ret>-nfenum.
                  wa_message-message = 'Sucesso!'(r02).
                  APPEND wa_message TO it_message.
                  CLEAR wa_message.
                ELSE.
                  wa_message-werks = <fs_ret>-werks.
                  wa_message-nfenum = <fs_ret>-nfenum.
                  wa_message-message = 'Nf informada não possui recusa.'(r01).
                  APPEND wa_message TO it_message.
                  CLEAR wa_message.
                  DATA(lv_err) = abap_true.
                ENDIF.
              ELSE.
                wa_message-werks = <fs_ret>-werks.
                wa_message-nfenum = <fs_ret>-nfenum.
                wa_message-message = 'Nf informada não possui recusa.'(r01).
                APPEND wa_message TO it_message.
                CLEAR wa_message.
                lv_err = abap_true.
              ENDIF.
            ELSE.
              wa_message-werks = <fs_ret>-werks.
              wa_message-nfenum = <fs_ret>-nfenum.
              wa_message-message = 'Nf informada não possui recusa.'(r01).
              APPEND wa_message TO it_message.
              CLEAR wa_message.
              lv_err = abap_true.
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT it_ret ASSIGNING <fs_ret>.
            wa_message-werks = <fs_ret>-werks.
            wa_message-nfenum = <fs_ret>-nfenum.
            wa_message-message = 'Nf informada não possui recusa.'(r01).
            APPEND wa_message TO it_message.
            CLEAR wa_message.
          ENDLOOP.
          lv_err = abap_true.
        ENDIF.
      ELSE.
        LOOP AT it_ret ASSIGNING <fs_ret>.
          wa_message-werks = <fs_ret>-werks.
          wa_message-nfenum = <fs_ret>-nfenum.
          wa_message-message = 'Nf informada não possui recusa.'(r01).
          APPEND wa_message TO it_message.
          CLEAR wa_message.
        ENDLOOP.
        lv_err = abap_true.
      ENDIF.
    ELSE.
      LOOP AT lra_nfenum INTO DATA(wr_nfnum).
        wa_message-nfenum = wr_nfnum-low.
        wa_message-message = 'Nf informada não possui recusa.'(r01).
        APPEND wa_message TO it_message.
        CLEAR wa_message.
      ENDLOOP.
      lv_err = abap_true.
    ENDIF.

    SORT it_message BY werks ASCENDING nfenum ASCENDING message DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_message COMPARING werks nfenum.

    lwa_data_response-message = it_message.
*    IF lv_err IS INITIAL.
      e_sucesso   = abap_true.
*    ENDIF.
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
