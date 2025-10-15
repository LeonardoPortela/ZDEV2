CLASS zcl_int_ib_sle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '167' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_SLE IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.


    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
      ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

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

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: lwa_data_request TYPE zpme_sle,
          it_bloqueio      TYPE TABLE OF zsdt0001_bloq.


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



*-----------------------------------------------------------------------------------------------------------------------*
*     Valida Preenchimento Campos
*-----------------------------------------------------------------------------------------------------------------------*
    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lwa_data_request ).

*    IF lwa_data_request-COD_RECINTO IS INITIAL.
*      r_msg_erro = 'Preenchimento do código recinto é obrigatório!'.
*      RETURN.
*    ENDIF.


  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    CONSTANTS: c_delete TYPE c LENGTH 50 VALUE 'DELETE',
               c_post   TYPE c LENGTH 50 VALUE 'POST'.

    DATA: lit_data_inbound      TYPE zpme_sle,
          lwa_zsdt0001cg_gravar TYPE zsdt0001cg,
          lw_zsdt0111           TYPE zsdt0111,
          lw_zsdt0112           TYPE zsdt0112,
          lt_zsdt0111           TYPE TABLE OF zsdt0111,
          lt_zsdt0112           TYPE TABLE OF zsdt0112,
          lwa_eq_header         TYPE alm_me_tob_header,
          wa_iflo               TYPE iflo,
          it_return             TYPE TABLE OF bapiret2.


    DATA: it_dados  TYPE TABLE OF zpme_sle.

    DATA: object_eqpto TYPE REF TO zcl_pm_data_equipament.
    CREATE OBJECT object_eqpto.


    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
    ENDIF.


    IF lit_data_inbound-cod_recinto = ' '.

      DATA: lt_zsdt0168 TYPE TABLE OF zsdt0168,
            ws_zsdt0168 TYPE zsdt0168,
            lt_lfa1     TYPE TABLE OF lfa1,
            ws_lfa1     TYPE lfa1.


      SELECT * FROM zsdt0168
        INTO TABLE lt_zsdt0168.


      IF sy-subrc = 0.

        SELECT * FROM lfa1
          INTO TABLE lt_lfa1
          FOR ALL ENTRIES IN lt_zsdt0168
          WHERE lifnr = lt_zsdt0168-lifnr.


        IF sy-subrc  = 0.

          LOOP AT lt_zsdt0168 INTO ws_zsdt0168.


            READ TABLE lt_lfa1 INTO ws_lfa1 WITH KEY lifnr = ws_zsdt0168-lifnr.


            IF sy-subrc = 0.

              lw_zsdt0111-codigo_ra      = ws_zsdt0168-codigo_ra.
              lw_zsdt0111-ds_ra          = ws_zsdt0168-ds_ra.
              lw_zsdt0111-local_despacho = ws_zsdt0168-local_despacho.
              lw_zsdt0111-local_embarque = ws_zsdt0168-local_embarque.
              lw_zsdt0111-lifnr   = ws_lfa1-lifnr.
              lw_zsdt0111-name1   = ws_lfa1-name1.
              lw_zsdt0111-ort01   = ws_lfa1-ort01.
              lw_zsdt0111-regio   = ws_lfa1-regio.

              APPEND lw_zsdt0111 TO lt_zsdt0111.
              CLEAR:lw_zsdt0111.

            ENDIF.

          ENDLOOP.


          e_sucesso   = abap_true.
          e_nm_code   = '200'.
          e_msg_erro  = 'Ok'.
          e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lt_zsdt0111 ).

        ENDIF.

      ENDIF.

    ELSE.

      DATA:lt_zsdt0169 TYPE TABLE OF zsdt0169,
           lw_zsdt0169 TYPE zsdt0169.


      SELECT * FROM zsdt0169
        INTO TABLE lt_zsdt0169
        WHERE codigo_ra = lit_data_inbound-cod_recinto.

      IF sy-subrc = 0.

        LOOP AT lt_zsdt0169 INTO lw_zsdt0169.

          lw_zsdt0112-codigo_ra   = lw_zsdt0169-codigo_ra.
          lw_zsdt0112-codigo_urf  = lw_zsdt0169-codigo_urf.

          APPEND lw_zsdt0112 TO lt_zsdt0112.
          CLEAR:lw_zsdt0112.

        ENDLOOP.

        e_sucesso   = abap_true.
        e_nm_code   = '200'.
        e_msg_erro  = 'Ok'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lt_zsdt0112 ).

      ENDIF.

    ENDIF.



  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
