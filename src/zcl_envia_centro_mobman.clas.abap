CLASS zcl_envia_centro_mobman DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_envia_centro_mobman .

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ENVIA_CENTRO_MOBMAN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interf_centro_mobman.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD zif_envia_centro_mobman~get_instance.

    IF zif_envia_centro_mobman~at_envia_centro_mobman IS NOT BOUND.
      CREATE OBJECT zif_envia_centro_mobman~at_envia_centro_mobman
        TYPE zcl_envia_centro_mobman.
    ENDIF.
    r_if_envia_centro_mobma = zif_envia_centro_mobman~at_envia_centro_mobman.

  ENDMETHOD.


  METHOD zif_envia_centro_mobman~get_json.

    CLEAR: e_json.

    r_if_envia_centro_mobma = me.

    CHECK me->zif_envia_centro_mobman~at_centro_trab IS NOT INITIAL.

** Construindo JSON.

    LOOP AT me->zif_envia_centro_mobman~at_centro_trab ASSIGNING FIELD-SYMBOL(<fs_centro>).

      AT LAST.

        e_json = e_json && ' { ' && ' "CREATED_AT": "'  && <fs_centro>-created_at  && '", '
                      && ' "UPDATED_aT": "'  && <fs_centro>-updated_at  && '", '
                      && ' "WERKS": "'       && <fs_centro>-werks       && '", '
                      && ' "ARBPL": "'       && <fs_centro>-arbpl       && '", '
                      && ' "KTEXT": "'       && <fs_centro>-ktext       && '", '
                      && ' "ISTAT": "'       && <fs_centro>-istat       && '" '
                      && ' } '
                     .
        EXIT.
      ENDAT.

      e_json = e_json && '{ ' && ' "CREATED_AT": "'  && <fs_centro>-created_at  && '", '
                    && ' "UPDATED_aT": "'  && <fs_centro>-updated_at  && '", '
                    && ' "WERKS": "'       && <fs_centro>-werks       && '", '
                    && ' "ARBPL": "'       && <fs_centro>-arbpl       && '", '
                    && ' "KTEXT": "'       && <fs_centro>-ktext       && '", '
                    && ' "ISTAT": "'       && <fs_centro>-istat       && '" '
                    && ' }, '
                   .
*                         .


    ENDLOOP.
  ENDMETHOD.


  METHOD zif_envia_centro_mobman~post_envia_centro.

    r_if_envia_centro_mobma = me.

    me->zif_envia_centro_mobman~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).

  ENDMETHOD.


  METHOD zif_envia_centro_mobman~set_dados_centro.

    r_if_envia_centro_mobma = me.

    me->zif_envia_centro_mobman~at_centro_trab = i_data.
  ENDMETHOD.


  METHOD zif_envia_centro_mobman~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_envia_centro_mobma = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.


  ENDMETHOD.


  METHOD zif_envia_centro_mobman~set_ds_url.

    r_if_envia_centro_mobma = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '17'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_envia_centro_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_envia_centro_mobman~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_envia_centro_mobman~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_envia_centro_mobma = me.

    CREATE OBJECT lc_integrar.

    "Cria MSG para Integração via HTTP
    lc_integrar->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = e_id_integracao
      )->set_outbound_msg(
      )->set_processar_retorno(
      )->set_integrar_retorno(
      )->get_registro( IMPORTING e_integracao = e_integracao
      )->free(
      ).

    CLEAR: lc_integrar.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.

*    DATA: lva_id_referencia TYPE zintegracao-id_referencia,
*          lva_id_integracao TYPE zintegracao-id_integracao.
*
*    lva_id_integracao =  c_integracao-id_integracao.
*    lva_id_referencia =  me->zif_integracao_tcot_contratos~at_id_referencia.
*
*    UPDATE zintegracao
*         SET id_referencia = lva_id_referencia
*           WHERE id_integracao        = lva_id_integracao
*           AND   id_interface         = '040'.
*    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.
    r_if_integracao_inject = me.

    TRY .
        CAST zcl_integracao_token_ordem(
               zcl_integracao_token_ordem=>zif_integracao_token_ordem~get_instance(
                 )->get_token( )
             )->zif_integracao_inject~get_header_request_http(
          IMPORTING
            e_header_fields = DATA(e_header_fields) ).

        me->zif_integracao_inject~set_header_request_http( i_header_fields = e_header_fields ).

      CATCH zcx_error INTO DATA(ex_erro).

        RAISE EXCEPTION TYPE zcx_integracao
          EXPORTING
            textid = VALUE #( msgid = ex_erro->zif_error~msgid
                              msgno = ex_erro->zif_error~msgno
                              attr1 = CONV #( ex_erro->zif_error~msgv1 )
                              attr2 = CONV #( ex_erro->zif_error~msgv2 )
                              attr3 = CONV #( ex_erro->zif_error~msgv3 )
                              attr4 = CONV #( ex_erro->zif_error~msgv4 ) )
            msgid  = ex_erro->zif_error~msgid
            msgno  = ex_erro->zif_error~msgno
            msgty  = 'E'
            msgv1  = ex_erro->zif_error~msgv1
            msgv2  = ex_erro->zif_error~msgv2
            msgv3  = ex_erro->zif_error~msgv3
            msgv4  = ex_erro->zif_error~msgv4.

    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
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
