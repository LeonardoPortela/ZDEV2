CLASS zcl_cria_modifica_ordem_mobman DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_cria_modifica_ordem_mobman .

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRIA_MODIFICA_ORDEM_MOBMAN IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface      = zif_integracao=>at_id_cria_modifica_ordem.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_outbound.
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = ''.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~get_instance.

    IF zif_cria_modifica_ordem_mobman~at_cria_modifica_ordem_mobman IS NOT BOUND.
      CREATE OBJECT zif_cria_modifica_ordem_mobman~at_cria_modifica_ordem_mobman
        TYPE zcl_cria_modifica_ordem_mobman.
    ENDIF.
    r_if_cria_modifica_ordem_mobma = zif_cria_modifica_ordem_mobman~at_cria_modifica_ordem_mobman.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~get_json.

    CLEAR: e_json.

    r_if_cria_modifica_ordem_mobma = me.

    CHECK me->zif_cria_modifica_ordem_mobman~at_ordem IS NOT INITIAL.

** Construindo JSON.

    e_json = '{ ' && ' "CREATED_AT": "'  && me->zif_cria_modifica_ordem_mobman~at_ordem-created_at  && '", '
                  && ' "UPDATED_aT": "'  && me->zif_cria_modifica_ordem_mobman~at_ordem-updated_at  && '", '
                  && ' "AUART": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-auart       && '", '
                  && ' "AUFNR": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-aufnr       && '", '
                  && ' "QMNUM": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-qmnum       && '", '
                  && ' "ISTAT": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-istat       && '", '
                  && ' "KTEXT": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-ktext       && '", '
                  && ' "IWERK": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-iwerk       && '", '
                  && ' "INGPR": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-ingpr       && '", '
                  && ' "ARBPL": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-arbpl       && '", '
                  && ' "USER4": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-user4       && '", '
                  && ' "ILART": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-ilart       && '", '
                  && ' "IDEQUIPE": "'    && me->zif_cria_modifica_ordem_mobman~at_ordem-idequipe    && '", '
                  && ' "ANLZU": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-anlzu       && '", '
                  && ' "PRIOK": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-priok       && '", '
                  && ' "TPLNR": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-tplnr       && '", '
                  && ' "EQUNR": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-equnr       && '", '
                  && ' "GSTRP": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-gstrp(4)    && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-gstrp+4(2)  && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-gstrp+6(2)  && '", '
                  && ' "GLTRP": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-gltrp(4)    && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-gltrp+4(2)  && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-gltrp+6(2)  && '", '
                  && ' "ERDAT": "'       && me->zif_cria_modifica_ordem_mobman~at_ordem-erdat(4)    && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-erdat+4(2)  && '-'
                                         && me->zif_cria_modifica_ordem_mobman~at_ordem-erdat+6(2)  && '", '
                  && '"NOTAS": ['.

    LOOP AT me->zif_cria_modifica_ordem_mobman~at_ordem-notas ASSIGNING FIELD-SYMBOL(<fs_notas>).
      AT LAST.

        e_json = e_json && '{ '
                        && ' "QMNUM": "'       && <fs_notas>-qmnum       && '",'
                        && ' "DESVINCULAR": "' && <fs_notas>-desvincular       && '" }'.
        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                        && ' "QMNUM": "'       && <fs_notas>-qmnum       && '" ,'
                        && ' "DESVINCULAR": "' && <fs_notas>-desvincular       && '" },'.
    ENDLOOP.

    e_json = e_json && '], "OPERACAO": ['.

    LOOP AT me->zif_cria_modifica_ordem_mobman~at_ordem-operacao ASSIGNING FIELD-SYMBOL(<fs_operacao>).

      AT LAST.
        e_json = e_json && '{ '
                       && '"VORNR": "'  && <fs_operacao>-vornr && '", '
                       && '"QMNUM": "'  && <fs_operacao>-qmnum && '", ' "FF - 12/04/2024 - #131818
                       && '"LTXA1": "'  && <fs_operacao>-ltxa1 && '", '
                       && '"AUFNR": "'  && <fs_operacao>-aufnr && '", '
                       && '"WERKS": "'  && <fs_operacao>-werks && '", '
                       && '"ARBPL": "'  && <fs_operacao>-arbpl && '", '
                       && '"STEUS": "'  && <fs_operacao>-steus && '", '
                       && '"ARBEI": "'  && <fs_operacao>-arbei && '", '
                       && '"ARBEH": "'  && <fs_operacao>-arbeh && '", '
                       && '"ANZZL": "'  && <fs_operacao>-anzzl && '", '
                       && '"PERNR": "'  && <fs_operacao>-pernr && '", '
                       && '"EINSA": "'  && <fs_operacao>-einsa && '", '
                       && '"EINSE": "'  && <fs_operacao>-einse && '", '
                       && '"NTANF": "'  && <fs_operacao>-ntanf(4)   && '-'
                                        && <fs_operacao>-ntanf+4(2) && '-'
                                        && <fs_operacao>-ntanf+6(2) && '", '
                       && '"NTEND": "'  && <fs_operacao>-ntend(4)   && '-'
                                        && <fs_operacao>-ntend+4(2) && '-'
                                        && <fs_operacao>-ntend+6(2) && '", '
                       && '"CONFNL": "' && <fs_operacao>-confnl && '", '
                       && '"APONTAMENTOS": [ '.

        LOOP AT <fs_operacao>-apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).

          AT LAST.
            e_json = e_json && '{ '
                            && '"RUECK": "' && <fs_apontamentos>-rueck && '", '
                            && '"RMZHL": "' && <fs_apontamentos>-rmzhl && '", '
                            && '"WERKS": "' && <fs_apontamentos>-werks && '", '
                            && '"PERNR": "' && <fs_apontamentos>-pernr && '", '
                            && '"ARBPL": "' && <fs_apontamentos>-arbpl && '", '
                            && '"VORNR": "' && <fs_apontamentos>-vornr && '", '
                            && '"BUDAT": "' && <fs_apontamentos>-budat(4)   && '-'
                                            && <fs_apontamentos>-budat+4(2) && '-'
                                            && <fs_apontamentos>-budat+6(2) && '", '
                            && '"ISDD": "'  && <fs_apontamentos>-isdd(4)    && '-'
                                            && <fs_apontamentos>-isdd+4(2)  && '-'
                                            && <fs_apontamentos>-isdd+6(2)  && '", '
                            && '"ISDZ": "'  && <fs_apontamentos>-isdz(2)    && ':'
                                            && <fs_apontamentos>-isdz+2(2)  && ':'
                                            && <fs_apontamentos>-isdz+4     && '", '
                            && '"IEDD": "'  && <fs_apontamentos>-iedd(4)    && '-'
                                            && <fs_apontamentos>-iedd+4(2)  && '-'
                                            && <fs_apontamentos>-iedd+6(2)  && '", '
                            && '"IEDZ": "'  && <fs_apontamentos>-iedz(2)    && ':'
                                            && <fs_apontamentos>-iedz+2(2)  && ':'
                                            && <fs_apontamentos>-iedz+4     && '", '
                            && '"ISMNW": "' && <fs_apontamentos>-ismnw && '", '
                            && '"ISMNE": "' && <fs_apontamentos>-ismne && '", '
                            && '"LTXA1": "' && <fs_apontamentos>-ltxa1 && '", '
                            && '"AUERU": "' && <fs_apontamentos>-aueru && '", '
                            && '"GRUND": "' && <fs_apontamentos>-grund && '" } '.

            EXIT.
          ENDAT.

          e_json = e_json && '{ '
                          && '"RUECK": "' && <fs_apontamentos>-rueck && '", '
                          && '"RMZHL": "' && <fs_apontamentos>-rmzhl && '", '
                          && '"WERKS": "' && <fs_apontamentos>-werks && '", '
                          && '"PERNR": "' && <fs_apontamentos>-pernr && '", '
                          && '"ARBPL": "' && <fs_apontamentos>-arbpl && '", '
                          && '"VORNR": "' && <fs_apontamentos>-vornr && '", '
                          && '"BUDAT": "' && <fs_apontamentos>-budat(4)   && '-'
                                          && <fs_apontamentos>-budat+4(2) && '-'
                                          && <fs_apontamentos>-budat+6(2) && '", '
                          && '"ISDD": "'  && <fs_apontamentos>-isdd(4)    && '-'
                                          && <fs_apontamentos>-isdd+4(2)  && '-'
                                          && <fs_apontamentos>-isdd+6(2)  && '", '
                          && '"ISDZ": "'  && <fs_apontamentos>-isdz && '", '
                          && '"IEDD": "'  && <fs_apontamentos>-iedd(4)    && '-'
                                          && <fs_apontamentos>-iedd+4(2)  && '-'
                                          && <fs_apontamentos>-iedd+6(2)  && '", '
                          && '"IEDZ": "'  && <fs_apontamentos>-iedz && '", '
                          && '"ISMNW": "' && <fs_apontamentos>-ismnw && '", '
                          && '"ISMNE": "' && <fs_apontamentos>-ismne && '", '
                          && '"LTXA1": "' && <fs_apontamentos>-ltxa1 && '", '
                          && '"AUERU": "' && <fs_apontamentos>-aueru && '", '
                          && '"GRUND": "' && <fs_apontamentos>-grund && '" }, '.

        ENDLOOP.

        e_json = e_json && '] }'.

        EXIT.
      ENDAT.

      e_json = e_json && '{ '
                     && '"VORNR": "'  && <fs_operacao>-vornr && '", '
                     && '"QMNUM": "'  && <fs_operacao>-qmnum && '", ' "FF - 12/04/2024 - #131818
                     && '"LTXA1": "'  && <fs_operacao>-ltxa1 && '", '
                     && '"AUFNR": "'  && <fs_operacao>-aufnr && '", '
                     && '"WERKS": "'  && <fs_operacao>-werks && '", '
                     && '"ARBPL": "'  && <fs_operacao>-arbpl && '", '
                     && '"STEUS": "'  && <fs_operacao>-steus && '", '
                     && '"ARBEI": "'  && <fs_operacao>-arbei && '", '
                     && '"ARBEH": "'  && <fs_operacao>-arbeh && '", '
                     && '"ANZZL": "'  && <fs_operacao>-anzzl && '", '
                     && '"PERNR": "'  && <fs_operacao>-pernr && '", '
                     && '"EINSA": "'  && <fs_operacao>-einsa && '", '
                     && '"EINSE": "'  && <fs_operacao>-einse && '", '
**  Begin of    #96115  FF
*                     && '"NTANF": "'  && <fs_operacao>-ntanf && '", '
*                     && '"NTEND": "'  && <fs_operacao>-ntend && '", '
                       && '"NTANF": "'  && <fs_operacao>-ntanf(4)   && '-'
                                        && <fs_operacao>-ntanf+4(2) && '-'
                                        && <fs_operacao>-ntanf+6(2) && '", '
                       && '"NTEND": "'  && <fs_operacao>-ntend(4)   && '-'
                                        && <fs_operacao>-ntend+4(2) && '-'
                                        && <fs_operacao>-ntend+6(2) && '", '
** End of FF
                     && '"CONFNL": "' && <fs_operacao>-confnl && '", '
                     && '"APONTAMENTOS": [ '.

      LOOP AT <fs_operacao>-apontamentos ASSIGNING <fs_apontamentos>.

        AT LAST.
          e_json = e_json && '{ '
                          && '"RUECK": "' && <fs_apontamentos>-rueck && '", '
                          && '"RMZHL": "' && <fs_apontamentos>-rmzhl && '", '
                          && '"WERKS": "' && <fs_apontamentos>-werks && '", '
                          && '"PERNR": "' && <fs_apontamentos>-pernr && '", '
                          && '"ARBPL": "' && <fs_apontamentos>-arbpl && '", '
                          && '"VORNR": "' && <fs_apontamentos>-vornr && '", '
                          && '"BUDAT": "' && <fs_apontamentos>-budat(4)   && '-'
                                          && <fs_apontamentos>-budat+4(2) && '-'
                                          && <fs_apontamentos>-budat+6(2) && '", '
                          && '"ISDD": "'  && <fs_apontamentos>-isdd(4)    && '-'
                                          && <fs_apontamentos>-isdd+4(2)  && '-'
                                          && <fs_apontamentos>-isdd+6(2)  && '", '
                          && '"ISDZ": "'  && <fs_apontamentos>-isdz && '", '
                          && '"IEDD": "'  && <fs_apontamentos>-iedd(4)    && '-'
                                          && <fs_apontamentos>-iedd+4(2)  && '-'
                                          && <fs_apontamentos>-iedd+6(2)  && '", '
                          && '"IEDZ": "'  && <fs_apontamentos>-iedz && '", '
                          && '"ISMNW": "' && <fs_apontamentos>-ismnw && '", '
                          && '"ISMNE": "' && <fs_apontamentos>-ismne && '", '
                          && '"LTXA1": "' && <fs_apontamentos>-ltxa1 && '", '
                          && '"AUERU": "' && <fs_apontamentos>-aueru && '", '
                          && '"GRUND": "' && <fs_apontamentos>-grund && '" } '.

          EXIT.
        ENDAT.

        e_json = e_json && '{ '
                        && '"RUECK": "' && <fs_apontamentos>-rueck && '", '
                        && '"RMZHL": "' && <fs_apontamentos>-rmzhl && '", '
                        && '"WERKS": "' && <fs_apontamentos>-werks && '", '
                        && '"PERNR": "' && <fs_apontamentos>-pernr && '", '
                        && '"ARBPL": "' && <fs_apontamentos>-arbpl && '", '
                        && '"VORNR": "' && <fs_apontamentos>-vornr && '", '
                        && '"BUDAT": "' && <fs_apontamentos>-budat(4)   && '-'
                                        && <fs_apontamentos>-budat+4(2) && '-'
                                        && <fs_apontamentos>-budat+6(2) && '", '
                        && '"ISDD": "'  && <fs_apontamentos>-isdd(4)    && '-'
                                        && <fs_apontamentos>-isdd+4(2)  && '-'
                                        && <fs_apontamentos>-isdd+6(2)  && '", '
                        && '"ISDZ": "'  && <fs_apontamentos>-isdz && '", '
                        && '"IEDD": "'  && <fs_apontamentos>-iedd(4)    && '-'
                                        && <fs_apontamentos>-iedd+4(2)  && '-'
                                        && <fs_apontamentos>-iedd+6(2)  && '", '
                        && '"IEDZ": "'  && <fs_apontamentos>-iedz && '", '
                        && '"ISMNW": "' && <fs_apontamentos>-ismnw && '", '
                        && '"ISMNE": "' && <fs_apontamentos>-ismne && '", '
                        && '"LTXA1": "' && <fs_apontamentos>-ltxa1 && '", '
                        && '"AUERU": "' && <fs_apontamentos>-aueru && '", '
                        && '"GRUND": "' && <fs_apontamentos>-grund && '" }, '.

      ENDLOOP.

      e_json = e_json && '] },'.

    ENDLOOP.

    e_json = e_json && '] }'.

*                  && ' "PART_OBJNR": ['.
*
*    LOOP AT me->zif_cria_modifica_ordem_mobman~at_ordem-part_objnr ASSIGNING FIELD-SYMBOL(<fs_part_objnr>).
*
*      AT LAST.
*
*        e_json = e_json && '{ '
*                        && '"numero_item": "' && <fs_part_objnr>-numero_item && '", '
*                        && '"fecod": "' && <fs_part_objnr>-fecod && '", '
*                        && '"fegrp": "' && <fs_part_objnr>-fegrp && '", '
*                        && '"fekat": "' && <fs_part_objnr>-fekat && '", '
*                        && '"fetxt": "' && <fs_part_objnr>-fetxt && '", '
*                        && '"mncod": "' && <fs_part_objnr>-mncod && '", '
*                        && '"mngrp": "' && <fs_part_objnr>-mngrp && '", '
*                        && '"mnkat": "' && <fs_part_objnr>-mnkat && '", '
*                        && '"oteil": "' && <fs_part_objnr>-oteil && '", '
*                        && '"otgrp": "' && <fs_part_objnr>-otgrp && '", '
*                        && '"otkat": "' && <fs_part_objnr>-otkat && '", '
*                        && '"urcod": "' && <fs_part_objnr>-urcod && '", '
*                        && '"urgrp": "' && <fs_part_objnr>-urgrp && '", '
*                        && '"urstx": "' && <fs_part_objnr>-urstx && '", '
*                        && '"urkat": "' && <fs_part_objnr>-urkat && '", '
*                        && '"eliminado": "' && <fs_part_objnr>-eliminado && '" } '.
*
*        EXIT.
*      ENDAT.
*
*      e_json = e_json && '{ '
*                      && '"numero_item": "' && <fs_part_objnr>-numero_item && '", '
*                      && '"fecod": "' && <fs_part_objnr>-fecod && '", '
*                      && '"fegrp": "' && <fs_part_objnr>-fegrp && '", '
*                      && '"fekat": "' && <fs_part_objnr>-fekat && '", '
*                      && '"fetxt": "' && <fs_part_objnr>-fetxt && '", '
*                      && '"mncod": "' && <fs_part_objnr>-mncod && '", '
*                      && '"mngrp": "' && <fs_part_objnr>-mngrp && '", '
*                      && '"mnkat": "' && <fs_part_objnr>-mnkat && '", '
*                      && '"oteil": "' && <fs_part_objnr>-oteil && '", '
*                      && '"otgrp": "' && <fs_part_objnr>-otgrp && '", '
*                      && '"otkat": "' && <fs_part_objnr>-otkat && '", '
*                      && '"urcod": "' && <fs_part_objnr>-urcod && '", '
*                      && '"urgrp": "' && <fs_part_objnr>-urgrp && '", '
*                      && '"urstx": "' && <fs_part_objnr>-urstx && '", '
*                      && '"urkat": "' && <fs_part_objnr>-urkat && '", '
*                      && '"eliminado": "' && <fs_part_objnr>-eliminado && '" }, '.
*
*
*    ENDLOOP.

*    e_json = e_json && '] }'.


  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~post_atualiza_status_ordem.

    DATA lw_json TYPE string.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~set_ds_url_ordem_status(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lw_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).


  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~post_cria_modifica_ordem_app.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~set_ds_url(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lc_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).


  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~post_estornar_apont.

    DATA lw_json TYPE string.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~set_ds_url_estornar_apont(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lw_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).



  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~post_excluir_oper.

    DATA lw_json TYPE string.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~set_ds_url_excluir_oper(
      )->get_json( IMPORTING e_json = DATA(lc_json)
      )->set_ds_data( EXPORTING i_json = lw_json
      )->set_send_msg( IMPORTING e_id_integracao = e_id_integracao e_integracao  = e_integracao
      ).

    /ui2/cl_json=>deserialize( EXPORTING json = e_integracao-ds_data_retorno CHANGING data = e_data  ).


  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_dados_confirmation.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~at_confirmation = i_data.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_dados_ordem.

    r_if_cria_modifica_ordem_mobma = me.

    me->zif_cria_modifica_ordem_mobman~at_ordem = i_data.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_ds_data.

    "Incluir Texto JSON para integração
    r_if_cria_modifica_ordem_mobma = me.
    me->zif_integracao_inject~at_info_request_http-ds_body = i_json.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_ds_url.

    r_if_cria_modifica_ordem_mobma = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '14'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_cria_modifica_ordem_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_cria_modifica_ordem_mobman~at_servico ).
    ENDIF.

    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_ds_url_estornar_apont.

    DATA: param     TYPE char255,
          vg_apont  TYPE co_rmzhl,
          vg_codigo TYPE co_rueck.
    r_if_cria_modifica_ordem_mobma = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '14'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_cria_modifica_ordem_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_cria_modifica_ordem_mobman~at_servico ).
    ENDIF.

    CLEAR: vg_apont, vg_codigo.
    vg_apont  = |{ me->zif_cria_modifica_ordem_mobman~at_confirmation-rmzhl ALPHA = IN }|.
    vg_codigo = |{ me->zif_cria_modifica_ordem_mobman~at_confirmation-rueck ALPHA = IN }|.

    CLEAR: param.

    param = '/apontamento/' && vg_apont && '/codigo/' && vg_codigo.
    CONDENSE param NO-GAPS.


    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url && param.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.


  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_ds_url_excluir_oper.

    DATA: param    TYPE char255,
          vg_ordem TYPE aufnr.
    r_if_cria_modifica_ordem_mobma = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '14'.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_cria_modifica_ordem_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_cria_modifica_ordem_mobman~at_servico ).
    ENDIF.

    CLEAR: vg_ordem.
    vg_ordem = |{ me->zif_cria_modifica_ordem_mobman~at_ordem-aufnr ALPHA = IN }|.
    CLEAR: param.

    param = '/ordem/' && vg_ordem && '/operacao/' && me->zif_cria_modifica_ordem_mobman~at_ordem-vornr.
    CONDENSE param NO-GAPS.


    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url && param.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_ds_url_ordem_status.
    DATA: param    TYPE char255,
          vg_ordem TYPE aufnr.
    r_if_cria_modifica_ordem_mobma = me.

    SELECT SINGLE * INTO @DATA(wa_webservice)
      FROM zciot_webservice
     WHERE tipo    EQ '9'
       AND servico EQ '14'. "@me->zif_integracao_comb~at_servico.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_integracao
        EXPORTING
          textid = VALUE #( msgid = zcx_integracao=>zcx_servico_http_config-msgid
                            msgno = zcx_integracao=>zcx_servico_http_config-msgno
                            attr1 = 'UN'
                            attr2 = me->zif_cria_modifica_ordem_mobman~at_servico )
          msgid  = zcx_integracao=>zcx_servico_http_config-msgid
          msgno  = zcx_integracao=>zcx_servico_http_config-msgno
          msgty  = 'E'
          msgv1  = 'K'
          msgv2  = CONV #( me->zif_cria_modifica_ordem_mobman~at_servico ).
    ENDIF.

    CLEAR: vg_ordem.
    vg_ordem = |{ me->zif_cria_modifica_ordem_mobman~at_ordem-aufnr ALPHA = IN }|.
    CLEAR: param.

    param = '/' && vg_ordem && '/' && me->zif_cria_modifica_ordem_mobman~at_ordem-istat.
    CONDENSE param NO-GAPS.


    me->zif_integracao_inject~at_info_request_http-ds_formato            =  'JSON'.
    me->zif_integracao_inject~at_info_request_http-ds_content_type       = wa_webservice-content_type.
    me->zif_integracao_inject~at_info_request_http-ds_url                = wa_webservice-url && param.
    me->zif_integracao_inject~at_info_request_http-ds_metodo             =  'POST'.
    me->zif_integracao_inject~at_info_request_http-ds_not_content_length = abap_true.

  ENDMETHOD.


  METHOD zif_cria_modifica_ordem_mobman~set_send_msg.

    DATA: lc_integrar TYPE REF TO zcl_integracao.

    r_if_cria_modifica_ordem_mobma = me.

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
