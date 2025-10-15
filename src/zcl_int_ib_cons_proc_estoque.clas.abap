CLASS zcl_int_ib_cons_proc_estoque DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    DATA:
      tb_obj_key TYPE TABLE OF awkey,
      tb_OBJ_KEY_LIKE TYPE TABLE OF awkey.

  data:
      BEGIN OF zde_data_request,
        tipo_proc       TYPE char1,
        return_data_doc TYPE char1,
        obj_key         LIKE tb_obj_key,
        OBJ_KEY_LIKE    LIKE tb_OBJ_KEY_LIKE,
      END OF zde_data_request .

    TYPES:
      BEGIN OF ty_doc_matareial,
        cabecalho TYPE  mkpf,
        itens     TYPE  MB_MSEG,
      END OF ty_doc_matareial.

    TYPES:
      BEGIN OF ty_tipo_proc1,
        dados        TYPE zpps_ximfbf_log,
        doc_material TYPE ty_doc_matareial,
      END OF ty_tipo_proc1.

    DATA:
      BEGIN OF zde_data_response1,
        tipo_proc TYPE TABLE OF ty_tipo_proc1,
      END OF zde_data_response1,

      BEGIN OF zde_data_response2,
        zmmt0006      TYPE TABLE OF  zmmt0006,
        zmmt0006_docs TYPE TABLE OF  zmmt0006_docs,
      END OF zde_data_response2.

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '132' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .

*    TYPES: BEGIN OF ty_doc_matareial.
*             types: mkpf type mkpf.
*             TYPES:  mseg TYPE MB_MSEG.
*    TYPES:  END OF ty_doc_matareial.
*
*    TYPES: BEGIN OF ty_tipo_proc1.
*             tyzpps_ximfbf_log.
*             TYPES:    doc_material TYPE ty_doc_matareial.
*    TYPES:    END OF ty_tipo_proc1.
*
*    DATA:
*      tb_obj_key TYPE TABLE OF awkey.
*
*    DATA:
*      BEGIN OF zde_data_request,
*        tipo_proc       TYPE char1,
*        return_data_doc TYPE char1,
*        obj_key         LIKE tb_obj_key,
*      END OF zde_data_request.
*
*
*    DATA:
*      BEGIN OF zde_data_response1,
*        tipo_proc TYPE TABLE OF ty_tipo_proc1,
*      END OF zde_data_response1,
*
*      BEGIN OF zde_data_response2,
*        zmmt0006      TYPE TABLE OF  zmmt0006,
*        zmmt0006_docs TYPE TABLE OF  zmmt0006_docs,
*      END OF zde_data_response2.
*
*    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '132' ##NO_TEXT.
*
*    METHODS constructor
*      RAISING
*        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PROC_ESTOQUE IMPLEMENTATION.


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


  method ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

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
      else.

      IF ( lwa_data_request-obj_key is INITIAL and lwa_data_request-obj_key_like is INITIAL ) or lwa_data_request-tipo_proc is INITIAL.
        r_msg_erro = 'Filtros insuficientes para a consulta'.
        RETURN.
      ENDIF.


    ENDIF.
*



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


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request   LIKE zde_data_request,
          lwa_data_response1 LIKE zde_data_response1,
          lwa_data_response2 LIKE zde_data_response2.

    DATA:
      lra_objkey TYPE RANGE OF awkey.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response1, lwa_data_response2.

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

    LOOP AT lwa_data_request-obj_key INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_objkey.
    ENDLOOP.

      LOOP AT lwa_data_request-obj_key_like INTO DATA(wa_filtro2).
      APPEND VALUE #( sign = 'I' option = 'CP' low = wa_filtro2 ) TO lra_objkey.

      ENDLOOP.

      data v_where TYPE string.



    IF lwa_data_request IS NOT INITIAL.

      IF lwa_data_request-tipo_proc EQ '1'.

        SELECT *
          FROM  zpps_ximfbf_log
          INTO TABLE @DATA(tl_zpps_ximfbf_log)
          WHERE obj_key IN @lra_objkey.

        IF lwa_data_request-return_data_doc EQ 'S'.

          IF tl_zpps_ximfbf_log[] IS NOT INITIAL.

            SELECT *
            FROM  mkpf
            INTO TABLE @DATA(tl_mkpf)
            FOR ALL ENTRIES IN @tl_zpps_ximfbf_log
            WHERE mblnr = @tl_zpps_ximfbf_log-mblnr .

            SELECT *
            FROM  mseg
            INTO TABLE @DATA(tl_mseg)
            FOR ALL ENTRIES IN @tl_zpps_ximfbf_log
            WHERE mblnr = @tl_zpps_ximfbf_log-mblnr.

          ENDIF.

        ENDIF.

        SORT tl_mkpf by mblnr.

        LOOP AT tl_zpps_ximfbf_log INTO DATA(w_zpps_ximfbf_log).

          APPEND INITIAL LINE TO lwa_data_response1-tipo_proc ASSIGNING FIELD-SYMBOL(<fs_response>).

          <fs_response>-dados = w_zpps_ximfbf_log.

          IF lwa_data_request-return_data_doc EQ 'S'.

            READ TABLE tl_mkpf INTO DATA(w_mkpf)
                   WITH KEY mblnr = w_zpps_ximfbf_log-mblnr
                   BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_response>-doc_material-cabecalho = w_mkpf.

              LOOP AT tl_mseg[] ASSIGNING FIELD-SYMBOL(<fs_mseg>) WHERE mblnr = w_zpps_ximfbf_log-mblnr.
                APPEND <fs_mseg> TO <fs_response>-doc_material-itens.
              ENDLOOP.


            ENDIF.
          ENDIF.
        ENDLOOP.

      ELSE.

        SELECT *
        FROM zmmt0006
        INTO TABLE lwa_data_response2-zmmt0006
        WHERE ch_referencia IN lra_objkey .

        SELECT *
        FROM zmmt0006_docs
        INTO TABLE lwa_data_response2-zmmt0006_docs
        WHERE ch_referencia IN lra_objkey.

      ENDIF.

    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    IF lwa_data_request-tipo_proc EQ '1'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response1 ).
    ELSE.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response2 ).
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
