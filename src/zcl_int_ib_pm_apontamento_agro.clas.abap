CLASS zcl_int_ib_pm_apontamento_agro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '206' ##NO_TEXT.

    METHODS constructor .
    METHODS apontamento_agro
      IMPORTING
        !i_importa_dados TYPE ztpm_d_m_apontamento_agro_t
      EXPORTING
        !e_retorno       TYPE ztpm_d_m_apont_retorno_agro_t .
***    METHODS valida_apontamento
***      IMPORTING
***        !i_apontamentos TYPE bapi_alm_timeconfirmation_t
***        !i_ordem        TYPE aufnr
***      EXPORTING
***        !e_msg          TYPE char255 .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_int_ib_pm_apontamento_agro IMPLEMENTATION.


  METHOD apontamento_agro.
    "Apontamento da açao e update da AFRU

    DATA: t_apontamento TYPE bapi_alm_timeconfirmation_t,
          _return       TYPE bapiret2,
          t_alm_return  TYPE TABLE OF bapi_alm_return,
          t_ordem       TYPE ztpm_d_m_ordem_t,
          lv_msg        TYPE char255,
          it_return     TYPE TABLE OF bapiret2.

    DATA:
      it_operat      TYPE TABLE OF bapi_alm_order_operation_e,
      it_olist       TYPE TABLE OF bapi_alm_order_objectlist,
      it_timetickets TYPE TABLE OF bapi_alm_timeconfirmation,
      header         TYPE bapi_alm_order_header_e.

    DATA:
      it_notiteme     TYPE TABLE OF bapi2080_notiteme, "notitem it_niobj e it_nidef
      it_notcause     TYPE TABLE OF bapi2080_notcause, "notifcaus IT_NICAU
      it_notactve     TYPE TABLE OF bapi2080_notactve, "notifactv IT_NIFAC
      it_nottaske     TYPE TABLE OF bapi2080_nottaske, "notiftask IT_NITAS
      it_nifac        TYPE TABLE OF znotifactv,
      ls_notif        TYPE  bapi2080_nothdre,          "notifheader_export
      ls_notif_h      TYPE  bapi2080_nothdtxte,
      it_texto_longo  TYPE TABLE OF ztpm_d_m_apontamento_agro_text,
      wa_return       TYPE bapiret2,
      it_return_add   TYPE TABLE OF bapiret2,
      t_return        TYPE TABLE OF bapiret2,
      tg_return       TYPE TABLE OF bapi_alm_return,

      tnotifactv      TYPE TABLE OF  bapi2080_notactvi,
      tnotifactv_x    TYPE TABLE OF  bapi2080_notactvi_x,
      lt_result       TYPE  bapiret2_t,
      lt_dados_catalo TYPE TABLE OF  zpme0072.

    DATA: ls_header     TYPE bapi_alm_order_header_e,
          lt_operations TYPE TABLE OF bapi_alm_order_operation_e,
          lt_return     TYPE TABLE OF bapiret2.

    LOOP AT i_importa_dados INTO DATA(wa_dados) WHERE qmnum IS NOT INITIAL.

      wa_dados-qmnum = |{ wa_dados-qmnum ALPHA = IN }|.

      DATA(lv_tabix) = sy-tabix.

      CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
        EXPORTING
          number             = wa_dados-qmnum
        IMPORTING
          notifheader_export = ls_notif
          notifhdtext        = ls_notif_h
        TABLES
          notitem            = it_notiteme
          notifcaus          = it_notcause
          notifactv          = it_notactve
          notiftask          = it_nottaske.

      DELETE it_notiteme WHERE delete_flag IS NOT INITIAL.
      DELETE it_notcause WHERE delete_flag IS NOT INITIAL.
      DELETE it_notactve WHERE delete_flag IS NOT INITIAL.
      DELETE it_nottaske WHERE delete_flag IS NOT INITIAL.

      SELECT rueck, rmzhl, aufnr, vornr, pernr,
             isdd, isdz, iedz
        INTO TABLE @DATA(lt_afru)
        FROM afru FOR ALL ENTRIES IN @i_importa_dados
        WHERE aufnr = @ls_notif-orderid
          AND vornr = @i_importa_dados-vornr.

      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
        EXPORTING
          number        = ls_notif-orderid
        IMPORTING
          es_header     = ls_header
        TABLES
          et_operations = lt_operations
          return        = lt_return.


********* UPDATE AFRU ************

      READ TABLE lt_operations INTO DATA(wa_operations) WITH KEY activity = wa_dados-vornr.
      IF sy-subrc <> 0.
        CLEAR wa_operations.
      ENDIF.

      READ TABLE lt_afru WITH KEY aufnr = ls_notif-orderid
                                  vornr = wa_dados-vornr
                                  pernr = wa_dados-pernr
                                  isdd  = wa_dados-isdd
                                  isdz  = wa_dados-isdz
                                  iedz  = wa_dados-iedz
                                  INTO DATA(wa_afru).
      IF sy-subrc = 0.

        UPDATE afru SET zzqmnum   = wa_dados-qmnum
                         zzltxa1  = wa_operations-description
                        zzmncod   = wa_dados-mncod
*                        zztxtcdma = wa_dados-txtcdma
                    WHERE rueck = wa_afru-rueck
                      AND rmzhl = wa_afru-rmzhl.

        COMMIT WORK.

      ENDIF.


********* Apontamento das Ações ************

      it_nifac = VALUE #( FOR l5 IN it_notactve WHERE ( delete_flag IS INITIAL ) ( CORRESPONDING #( l5 ) ) ).

      SORT it_nifac BY act_key DESCENDING.

      READ TABLE it_nifac INDEX 1 INTO DATA(wa_nifac).

      DATA(lv_ultimo_act_key) = wa_nifac-act_key + 1.

      IF lv_ultimo_act_key IS INITIAL.
        lv_ultimo_act_key = '0001'.
      ENDIF.

      CLEAR: it_nifac[], it_nifac.

      APPEND INITIAL LINE TO it_nifac ASSIGNING FIELD-SYMBOL(<fs_notactve>).

      IF <fs_notactve> IS ASSIGNED.
        MOVE:
        'X' TO <fs_notactve>-check,
        wa_dados-mncod    TO <fs_notactve>-act_code,
        wa_dados-mngrp    TO <fs_notactve>-act_codegrp,
        wa_dados-isdd     TO <fs_notactve>-start_date,
        wa_dados-isdz     TO <fs_notactve>-start_time,
        wa_dados-iedd     TO <fs_notactve>-end_date,
        wa_dados-iedz     TO <fs_notactve>-end_time.

        IF lv_tabix = 1.
          <fs_notactve>-act_key = lv_ultimo_act_key.
        ELSE.
          lv_ultimo_act_key = lv_ultimo_act_key + 1.
          <fs_notactve>-act_key = lv_ultimo_act_key.
        ENDIF.

        tnotifactv   = VALUE #( FOR l3 IN it_nifac WHERE ( act_key IS NOT INITIAL )
                                (
                                  act_key      = l3-act_key
                                  act_sort_no  = l3-act_key"ACT_SORT_NO
                                  acttext      = l3-acttext
                                  act_codegrp  = l3-act_codegrp
                                  act_code     = l3-act_code
                                  start_date   = l3-start_date
                                  start_time   = l3-start_time
                                  end_date     = l3-end_date
                                  end_time     = l3-end_time

                                 ) ).

        CLEAR it_return_add[].
        CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
          EXPORTING
            number    = wa_dados-qmnum
          TABLES
            notifactv = tnotifactv
            return    = it_return_add.


        APPEND LINES OF it_return_add    TO t_return.

        IF NOT line_exists( it_return_add[ type = 'E' ] ).
          DATA ls_return_commit  TYPE bapiret2.

          CLEAR it_return[].
          CALL FUNCTION 'ALM_PM_NOTIFICATION_SAVE'
            EXPORTING
              number = wa_dados-qmnum
            TABLES
              return = it_return.

          APPEND LINES OF it_return TO t_return.

          IF NOT line_exists( it_return[ type = 'E' ] ).

            CLEAR ls_return_commit.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = abap_true
              IMPORTING
                return = ls_return_commit.
          ENDIF.
          APPEND ls_return_commit TO t_return.
        ENDIF.
      ENDIF.


      READ TABLE t_return WITH KEY type = 'E' INTO DATA(w_return).
      IF sy-subrc = 0.

        APPEND VALUE #( item = space
                        success = abap_false
                        msg_error = w_return-message
                       ) TO e_retorno.

      ELSE.


        CLEAR w_return.
        APPEND VALUE #( item = lv_ultimo_act_key
                        success = abap_true
                        msg_error = w_return-message
                       ) TO e_retorno.
      ENDIF.

      CLEAR t_return[].

    ENDLOOP.



  ENDMETHOD.


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

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA: lwa_data_request TYPE ztpm_d_m_apontamento_agro_t.

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
*    IF lwa_data_request-empresa IS INITIAL.
*      r_msg_erro = 'Empresa não Informada!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*      ELSEIF lwa_data_request-mes_ano IS INITIAL.
*      r_msg_erro = 'Mes_Ano não Informado!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*     ELSEIF lwa_data_request-moeda IS INITIAL.
*      r_msg_erro = 'Moeda não Informada!'.
*      e_status_code = '402'. "Payment Required
*      RETURN.
*
*      else.
*
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

    DATA: lwa_data_request  TYPE ztpm_d_m_apontamento_agro_t,
          lwa_data_retorno  TYPE ztpm_d_m_apont_retorno_agro_t,
          lwa_data_response TYPE zfie0014_t.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound. "lwa_data_response.

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

    me->apontamento_agro(
      EXPORTING
        i_importa_dados = lwa_data_request    " Estrutura de apontamentos
      IMPORTING
        e_retorno       = lwa_data_retorno   " Apontamento Ordem PM
    ).

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_retorno ). "enviar para essa tabela lwa_data_response

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
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
