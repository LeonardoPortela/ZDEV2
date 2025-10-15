class ZCL_PERMISSAO_USUARIOS definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_PERMISSAO_USUARIOS .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PERMISSAO_USUARIOS IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_interface_permissao_user.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
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
        CAST zcl_integracao_token_nota(
               zcl_integracao_token_nota=>zif_integracao_token_nota~get_instance(
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


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.

  ENDMETHOD.


  method ZIF_PERMISSAO_USUARIOS~GET_INSTANCE.
    IF zif_permissao_usuarios~at_if_permissao_usuario IS NOT BOUND.
      CREATE OBJECT zif_permissao_usuarios~at_if_permissao_usuario
        TYPE zcl_permissao_usuarios.
    ENDIF.
    r_if_permissao_usuario = zif_permissao_usuarios~at_if_permissao_usuario.
  endmethod.


  METHOD zif_permissao_usuarios~get_json.


    CLEAR: e_json.

    r_if_permissao_usuario = me.

    CHECK me->zif_permissao_usuarios~at_dados_usuario IS NOT INITIAL.

* Construindo JSON.
    e_json = '{ ' && ' "pernr": " ' && me->zif_permissao_usuarios~at_dados_usuario-pernr  && '", '
                   && ' "cname": " ' && me->zif_permissao_usuarios~at_dados_usuario-cname && '", '
                   && ' "cpf_nr": " ' && me->zif_permissao_usuarios~at_dados_usuario-cpf_nr  && '", '
                   && ' "departamento": " ' && me->zif_permissao_usuarios~at_dados_usuario-departamento  && '", '
                   && ' "funcao": " ' && me->zif_permissao_usuarios~at_dados_usuario-funcao  && '", '
                   && ' "ativo": " ' && me->zif_permissao_usuarios~at_dados_usuario-ativo  && '", '
                   && ' "login": " ' && me->zif_permissao_usuarios~at_dados_usuario-login  && '", '
                   && ' "samaccountname": " ' && me->zif_permissao_usuarios~at_dados_usuario-samaccountname  && '", '
                   && ' "email_ad": " ' && me->zif_permissao_usuarios~at_dados_usuario-email_ad  && '", '
                   && ' "bukrs": " ' && me->zif_permissao_usuarios~at_dados_usuario-bukrs  && '", '
                   && ' "werks": " ' && me->zif_permissao_usuarios~at_dados_usuario-werks  && '", '
                   && ' "cent_trab": [ '.

    LOOP AT me->zif_permissao_usuarios~at_dados_usuario-cent_trab ASSIGNING FIELD-SYMBOL(<fs_centro>).

      AT LAST.

        e_json = e_json && '{ '
                      && '"iwerk": "' && <fs_centro>-iwerk && '", '
                      && '"objty": "' && <fs_centro>-objty && '", '
                      && '"arbpl": "' && <fs_centro>-arbpl && '" }'.
        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                      && '"iwerk": "' && <fs_centro>-iwerk && '", '
                      && '"objty": "' && <fs_centro>-objty && '", '
                      && '"arbpl": "' && <fs_centro>-arbpl && '" },'.
    ENDLOOP.

    e_json = e_json && '], '
                    && '"perfil": [ '.

    LOOP AT me->zif_permissao_usuarios~at_dados_usuario-perfil ASSIGNING FIELD-SYMBOL(<fs_perfil>).


      AT LAST.
        e_json = e_json && '{ '
                            && '"iwerk": "'     && <fs_perfil>-iwerk && '", '
                            && '"transacao": "' && <fs_perfil>-transacao && '", '
                            && '"exibir": "'    && <fs_perfil>-exibir && '", '
                            && '"modificar": "' && <fs_perfil>-modificar && '", '
                            && '"criar": "'     && <fs_perfil>-criar && '", '
                            && '"liberar": "'   && <fs_perfil>-liberar && '", '
                            && '"encerrar": "'  && <fs_perfil>-encerrar && '", '
                            && '"aprovar_orc": "'  && <fs_perfil>-aprovar_orc && '" }'.

        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                          && '"iwerk": "'     && <fs_perfil>-iwerk && '", '
                          && '"transacao": "' && <fs_perfil>-transacao && '", '
                          && '"exibir": "'    && <fs_perfil>-exibir && '", '
                          && '"modificar": "' && <fs_perfil>-modificar && '", '
                          && '"criar": "'     && <fs_perfil>-criar && '", '
                          && '"liberar": "'  && <fs_perfil>-liberar && '", '
                          && '"encerrar": "'  && <fs_perfil>-encerrar && '",'
                          && '"aprovar_orc": "'  && <fs_perfil>-aprovar_orc && '" },'.


    ENDLOOP.

    e_json = e_json && '] '
                    && '}'.


  ENDMETHOD.


  METHOD zif_permissao_usuarios~post_permissao_usuario.

    r_if_permissao_usuarios = me.
*
*    DATA: e_comboio TYPE TABLE OF zpmt0058.
*
*    "Inclui Json na Mesagem a Ser Enviada
    me->zif_permissao_usuarios~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).

  ENDMETHOD.


  METHOD zif_permissao_usuarios~set_dados_usuario.

    r_if_permissao_usuarios = me.

    me->zif_permissao_usuarios~at_dados_usuario = i_data.

  ENDMETHOD.


  METHOD zif_permissao_usuarios~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_permissao_usuario = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  method ZIF_PERMISSAO_USUARIOS~SET_DS_URL.

  r_if_permissao_usuario = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '11'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_permissao_usuarios~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_permissao_usuarios~at_servico ).
    ENDIF.

*    CONCATENATE wa_webservice-url 'usuarios' INTO wa_webservice-url.
*
*    CLEAR: me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = '~request_method'  value = 'POST' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = '~server_protocol' value = 'HTTP/1.1' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Content-Type'     value = 'application/json; charset=UTF-8' ) TO me->zif_integracao_inject~at_header_fields.
*    APPEND VALUE #( name = 'Token'            value = wa_webservice-senha ) TO me->zif_integracao_inject~at_header_fields.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  endmethod.


  method ZIF_PERMISSAO_USUARIOS~SET_ID_REFERENCIA.
  endmethod.


  method ZIF_PERMISSAO_USUARIOS~SET_SEND_MSG.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_permissao_usuario = me.

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

  endmethod.
ENDCLASS.
