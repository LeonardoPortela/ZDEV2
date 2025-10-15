CLASS zcl_cria_modifica_nota_mobman DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_cria_modifica_nota_mobman .

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cria_modifica_nota_mobman IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_cria_modifica_nota.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~get_instance.

    IF zif_cria_modifica_nota_mobman~at_cria_modifica_nota_mobman IS NOT BOUND.
      CREATE OBJECT zif_cria_modifica_nota_mobman~at_cria_modifica_nota_mobman
        TYPE zcl_cria_modifica_nota_mobman.
    ENDIF.
    r_if_cria_modifica_nota_mobman = zif_cria_modifica_nota_mobman~at_cria_modifica_nota_mobman.

  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~get_json.

    CLEAR: e_json.

    r_if_cria_modifica_nota_mobman = me.

    CHECK me->zif_cria_modifica_nota_mobman~at_cria_modifica_nota_mobman IS NOT INITIAL.

** Construindo JSON.

    e_json = '{ ' && ' "ID": "'          && me->zif_cria_modifica_nota_mobman~at_nota-qmnum        && '", '
                  && ' "QMNUM": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmnum        && '", '
                  && ' "IWERK": "'       && me->zif_cria_modifica_nota_mobman~at_nota-iwerk        && '", '
                  && ' "TPLNR": "'       && me->zif_cria_modifica_nota_mobman~at_nota-tplnr        && '", '
                  && ' "EQUNR": "'       && me->zif_cria_modifica_nota_mobman~at_nota-equnr        && '", '
                  && ' "ARBPL": "'       && me->zif_cria_modifica_nota_mobman~at_nota-arbpl        && '", '
                  && ' "AUSVN": "'       && me->zif_cria_modifica_nota_mobman~at_nota-ausvn(4)   && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ausvn+4(2) && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ausvn+6(2) && '", '
                  && ' "AUSBS": "'       && me->zif_cria_modifica_nota_mobman~at_nota-ausbs(4)   && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ausbs+4(2) && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ausbs+6(2) && '", '
                  && ' "AUZTV": "'       && me->zif_cria_modifica_nota_mobman~at_nota-auztv(2)   && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-auztv+2(2) && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-auztv+4(2) && '", '
                  && ' "AUZTB": "'       && me->zif_cria_modifica_nota_mobman~at_nota-auztb(2)   && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-auztb+2(2) && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-auztb+4(2) && '", '
                  && ' "STRMN": "'       && me->zif_cria_modifica_nota_mobman~at_nota-strmn(4)   && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-strmn+4(2) && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-strmn+6(2) && '", '
                  && ' "STRUR": "'       && me->zif_cria_modifica_nota_mobman~at_nota-strur(2)   && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-strur+2(2) && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-strur+4(2) && '", '
                  && ' "LTRMN": "'       && me->zif_cria_modifica_nota_mobman~at_nota-ltrmn(4)   && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ltrmn+4(2) && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ltrmn+6(2) && '", '
                  && ' "LTRUR": "'       && me->zif_cria_modifica_nota_mobman~at_nota-ltrur(2)   && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ltrur+2(2) && ':'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-ltrur+4(2) && '", '
                  && ' "QMDAT": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmdat(4)   && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-qmdat+4(2) && '-'
                                         && me->zif_cria_modifica_nota_mobman~at_nota-qmdat+6(2) && '", '
                  && ' "BTPLN": "'       && me->zif_cria_modifica_nota_mobman~at_nota-btpln        && '", '
                  && ' "BEQUI": "'       && me->zif_cria_modifica_nota_mobman~at_nota-bequi        && '", '
                  && ' "QMART": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmart        && '", '
                  && ' "QMTXT": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmtxt        && '", '
                  && ' "TXTNT": "'       && me->zif_cria_modifica_nota_mobman~at_nota-txtnt        && '", '
                  && ' "PRIOK": "'       && me->zif_cria_modifica_nota_mobman~at_nota-priok        && '", '
                  && ' "AUFNR": "'       && me->zif_cria_modifica_nota_mobman~at_nota-aufnr        && '", '
                  && ' "QMNAM": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmnam        && '", '
                  && ' "MSAUS": "'       && me->zif_cria_modifica_nota_mobman~at_nota-msaus        && '", '
                  && ' "ISTAT": "'       && me->zif_cria_modifica_nota_mobman~at_nota-istat        && '", '
                  && ' "INGRP": "'       && me->zif_cria_modifica_nota_mobman~at_nota-ingrp        && '", '
                  && ' "QMGRP": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmgrp        && '", '
                  && ' "QMCOD": "'       && me->zif_cria_modifica_nota_mobman~at_nota-qmcod        && '", '
                  && ' "CREATED_AT": "'  && me->zif_cria_modifica_nota_mobman~at_nota-created_at        && '", '
                  && ' "UPDATED_AT": "'  && me->zif_cria_modifica_nota_mobman~at_nota-updated_at        && '", '
                  && ' "PART_OBJNR": ['.

    LOOP AT me->zif_cria_modifica_nota_mobman~at_nota-part_objnr ASSIGNING FIELD-SYMBOL(<fs_part_objnr>).

      AT LAST.

        e_json = e_json && '{ '
                        && '"numero_item": "' && <fs_part_objnr>-numero_item && '", '
                        && '"fecod": "' && <fs_part_objnr>-fecod && '", '
                        && '"fegrp": "' && <fs_part_objnr>-fegrp && '", '
                        && '"fekat": "' && <fs_part_objnr>-fekat && '", '
                        && '"fetxt": "' && <fs_part_objnr>-fetxt && '", '
                        && '"mncod": "' && <fs_part_objnr>-mncod && '", '
                        && '"mngrp": "' && <fs_part_objnr>-mngrp && '", '
                        && '"mnkat": "' && <fs_part_objnr>-mnkat && '", '
                        && '"oteil": "' && <fs_part_objnr>-oteil && '", '
                        && '"otgrp": "' && <fs_part_objnr>-otgrp && '", '
                        && '"otkat": "' && <fs_part_objnr>-otkat && '", '
                        && '"urcod": "' && <fs_part_objnr>-urcod && '", '
                        && '"urgrp": "' && <fs_part_objnr>-urgrp && '", '
                        && '"urstx": "' && <fs_part_objnr>-urstx && '", '
                        && '"urkat": "' && <fs_part_objnr>-urkat && '", '
                        && '"eliminado": "' && <fs_part_objnr>-eliminado && '" } '.

        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                      && '"numero_item": "' && <fs_part_objnr>-numero_item && '", '
                      && '"fecod": "' && <fs_part_objnr>-fecod && '", '
                      && '"fegrp": "' && <fs_part_objnr>-fegrp && '", '
                      && '"fekat": "' && <fs_part_objnr>-fekat && '", '
                      && '"fetxt": "' && <fs_part_objnr>-fetxt && '", '
                      && '"mncod": "' && <fs_part_objnr>-mncod && '", '
                      && '"mngrp": "' && <fs_part_objnr>-mngrp && '", '
                      && '"mnkat": "' && <fs_part_objnr>-mnkat && '", '
                      && '"oteil": "' && <fs_part_objnr>-oteil && '", '
                      && '"otgrp": "' && <fs_part_objnr>-otgrp && '", '
                      && '"otkat": "' && <fs_part_objnr>-otkat && '", '
                      && '"urcod": "' && <fs_part_objnr>-urcod && '", '
                      && '"urgrp": "' && <fs_part_objnr>-urgrp && '", '
                      && '"urstx": "' && <fs_part_objnr>-urstx && '", '
                      && '"urkat": "' && <fs_part_objnr>-urkat && '", '
                      && '"eliminado": "' && <fs_part_objnr>-eliminado && '" }, '.


    ENDLOOP.

    e_json = e_json && '] }'.

  ENDMETHOD.

  METHOD zif_cria_modifica_nota_mobman~get_json_ativ.

    CLEAR: e_json.

    r_if_cria_modifica_nota_mobman = me.

    CHECK me->zif_cria_modifica_nota_mobman~at_cria_modifica_nota_mobman IS NOT INITIAL.

** Construindo JSON para envio das atividades/Ações.

    LOOP AT me->zif_cria_modifica_nota_mobman~at_atividades ASSIGNING FIELD-SYMBOL(<fs_ativ>).

      AT LAST.

*        e_json = e_json && '[ { '
        e_json = e_json && '{ '
                        && '"id": "'     && <fs_ativ>-id && '", '
                        && '"qmnum": "'  && <fs_ativ>-qmnum && '", '
                        && '"n_item": "' && <fs_ativ>-n_item && '", '
                        && '"vornr": "'  && <fs_ativ>-vornr && '", '
                        && '"pernr": "'  && <fs_ativ>-pernr && '", '
                        && '"mnkat": "'  && <fs_ativ>-mnkat && '", '
                        && '"mngrp": "'  && <fs_ativ>-mngrp && '", '
                        && '"mncod": "'  && <fs_ativ>-mncod && '", '

                        && ' "isdd": "'  && <fs_ativ>-isdd(4)   && '-'
                                         && <fs_ativ>-isdd+4(2) && '-'
                                         && <fs_ativ>-isdd+6(2) && '", '

                        && ' "isdz": "'  && <fs_ativ>-isdz(2)   && ':'
                                         && <fs_ativ>-isdz+2(2) && ':'
                                         && <fs_ativ>-isdz+4(2) && '", '

                        && ' "iedd": "'  && <fs_ativ>-iedd(4)   && '-'
                                         && <fs_ativ>-iedd+4(2) && '-'
                                         && <fs_ativ>-iedd+6(2) && '", '

                        && ' "iedz": "'  && <fs_ativ>-iedz(2)   && ':'
                                         && <fs_ativ>-iedz+2(2) && ':'
                                         && <fs_ativ>-iedz+4(2) && '" } '.

        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                      && '"id": "'     && <fs_ativ>-id && '", '
                      && '"qmnum": "'  && <fs_ativ>-qmnum && '", '
                      && '"n_item": "' && <fs_ativ>-n_item && '", '
                      && '"vornr": "'  && <fs_ativ>-vornr && '", '
                      && '"pernr": "'  && <fs_ativ>-pernr && '", '
                      && '"mnkat": "'  && <fs_ativ>-mnkat && '", '
                      && '"mngrp": "'  && <fs_ativ>-mngrp && '", '
                      && '"mncod": "'  && <fs_ativ>-mncod && '", '

                      && ' "isdd": "'  && <fs_ativ>-isdd(4)   && '-'
                                       && <fs_ativ>-isdd+4(2) && '-'
                                       && <fs_ativ>-isdd+6(2) && '", '

                      && ' "isdz": "'  && <fs_ativ>-isdz(2)   && ':'
                                       && <fs_ativ>-isdz+2(2) && ':'
                                       && <fs_ativ>-isdz+4(2) && '", '

                      && ' "iedd": "'  && <fs_ativ>-iedd(4)   && '-'
                                       && <fs_ativ>-iedd+4(2) && '-'
                                       && <fs_ativ>-iedd+6(2) && '", '

                      && ' "iedz": "'  && <fs_ativ>-iedz(2)   && ':'
                                       && <fs_ativ>-iedz+2(2) && ':'
                                       && <fs_ativ>-iedz+4(2) && '" }, '.


    ENDLOOP.

*    e_json = e_json && ']'.

  ENDMETHOD.

  METHOD zif_cria_modifica_nota_mobman~post_cria_modifica_nota_mobman.

    r_if_cria_modifica_nota_mobman = me.
*
*    DATA: e_comboio TYPE TABLE OF zpmt0058.
*
*    "Inclui Json na Mesagem a Ser Enviada

    "Fabrício Fonseca - 29/04/2024 - inicio #131818
    IF i_atividades[] IS NOT INITIAL.

      me->zif_cria_modifica_nota_mobman~set_ds_url( EXPORTING i_ativ = 'X'
        )->get_json_ativ( IMPORTING e_json = DATA(lc_json)
        )->set_ds_data( EXPORTING i_json = lc_json
        )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
        ).


    ELSE.

      me->zif_cria_modifica_nota_mobman~set_ds_url(
        )->get_json( IMPORTING e_json = lc_json
        )->set_ds_data( EXPORTING i_json = lc_json
        )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
        ).


    ENDIF.
    "Fabrício Fonseca - 29/04/2024 - fim


    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).


  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~set_dados_nota.

    r_if_cria_modifica_nota_mobman = me.

    me->zif_cria_modifica_nota_mobman~at_nota = i_data.
    me->zif_cria_modifica_nota_mobman~at_atividades = i_atividades.

  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_cria_modifica_nota_mobman = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~set_ds_url.

    r_if_cria_modifica_nota_mobman = me.

    "FF - 23/04/2024 - inicio #131818
    IF i_ativ IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(wa_webservice)
        FROM zciot_webservice
       WHERE tipo    EQ '9'
         AND servico EQ '18'.

    ELSE.
      "FF - 23/04/2024 - fim #131818

      SELECT SINGLE * INTO wa_webservice
        FROM zciot_webservice
       WHERE tipo    EQ '9'
         AND servico EQ '13'. "@me->zif_integracao_comb~at_servico.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_cria_modifica_nota_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_cria_modifica_nota_mobman~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_cria_modifica_nota_mobman~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_cria_modifica_nota_mobman = me.

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
