class ZCL_INT_IB_CONS_RFO_JORNADA definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '182' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_RFO_JORNADA IMPLEMENTATION.


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


    DATA: lwa_data_request TYPE zde_date_0030.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_get.
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
    IF lwa_data_request-begda IS INITIAL.
      r_msg_erro = 'filtro data inicio não foi informado!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-endda IS INITIAL.
      r_msg_erro = 'filtro data fim não foi informado!'.
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
    TYPES: tt_str_tab TYPE STANDARD TABLE OF string WITH EMPTY KEY
           ,
           BEGIN OF ts_data,
             matricula                TYPE string,
             data                     TYPE string,
             inicio_jornada           TYPE beguz,
             inicio_intervalo         TYPE pabeg,
             fim_intervalo            TYPE paend,
             fim_jornada              TYPE enduz,
             total_jornada_registrada TYPE string,
             total_jornada_esperada   TYPE string,
             hora_calculo             TYPE string,
             batida                   TYPE tt_str_tab,
           END OF ts_data.

    DATA: lwa_data_request  TYPE zde_rfo_par_faltas.
    DATA: lv_id_referencia TYPE zsdt0001-id_referencia.

    DATA: lit_saida       TYPE TABLE OF zhcms_batida_list,
          lit_saida_aux   TYPE TABLE OF zhcms_batida_list,
          lit_saida_abono TYPE TABLE OF zhcms_abonos_list_re,
          lit_objid       TYPE TABLE OF zhcms_objid,
          lit_pernr       TYPE TABLE OF zhcms_ret_pernr,
          lwa_pernr       LIKE LINE OF lit_pernr.

    DATA: lit_horarios   TYPE TABLE OF zhcms_horarios_excecao.

    DATA: lva_dtbatida(10) TYPE c.
    DATA: lva_hora_diario(6) TYPE c,
          lva_part1(3)       TYPE c,
          lva_part2(2)       TYPE c,
          lva_stadz          TYPE zhcms_abonos_list_re-stdaz.

    DATA: lva_tot_atu TYPE anzhl,
          lva_hora    TYPE c LENGTH 8,
          lva_time    TYPE c LENGTH 8.

    DATA: lva_time1 LIKE sy-uzeit,
          lva_time2 LIKE sy-uzeit.

    DATA:
      lwa_time1  TYPE t,
      lwa_time2  TYPE t,
      lwa_result TYPE t,
      lwa_int1   TYPE i,
      lwa_int2   TYPE i.

    DATA: lo_sap_hcm TYPE REF TO zcl_hcm_util.
    CREATE OBJECT lo_sap_hcm.

    DATA: r_subty TYPE RANGE OF pa0030-subty.
    DATA: r_pernr TYPE RANGE OF persno.

    DATA: git_saida TYPE TABLE OF ts_data,
          gwa_saida TYPE ts_data.

    DATA: lit_text_tab_local  TYPE hrpad_text_tab,
          lwa_text_tab_local  LIKE LINE OF lit_text_tab_local,
          lva_message_handler TYPE REF TO  if_hrpa_message_handler,
          lva_no_auth_check   TYPE  boole_d,
          lva_is_ok           TYPE  boole_d.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

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


    SELECT 'I' AS sign, 'EQ' AS option, awart AS low, awart AS high
      INTO TABLE @DATA(r_awart_x)
    FROM zhcmt_re_0002
      WHERE check_quantidade = 'X'.

    SELECT 'I' AS sign, 'EQ' AS option, awart AS low, awart AS high
      INTO TABLE @DATA(r_awart)
    FROM zhcmt_re_0002
      WHERE check_quantidade NE 'X'.



    lo_sap_hcm->get_func_rem_operadores( EXPORTING
                                                 ir_pernr  = lwa_data_request-pernr
                                       IMPORTING et_return = DATA(lit_return) ).
    IF  lit_return IS NOT INITIAL.

      LOOP AT lit_return INTO DATA(lwa_return).
        lwa_pernr-pernr = lwa_return-matricula.
        APPEND lwa_pernr TO lit_pernr.
        CLEAR: lwa_pernr.
      ENDLOOP.

      CALL FUNCTION 'ZHCMF_RETURN_BATIDAS'
        EXPORTING
          "pernr   = lwa_return-matricula
          begda   = lwa_data_request-begda
          endda   = lwa_data_request-endda
        TABLES
          t_saida = lit_saida
          t_objid = lit_objid
          t_pernr = lit_pernr.

      IF lit_saida IS NOT  INITIAL.

        SORT   lit_saida     BY pernr data_batida.
        LOOP AT lit_saida INTO DATA(lwa_saida).

          gwa_saida-matricula     = lwa_saida-pernr.

          CALL FUNCTION 'ZHCMF_RET_ABONOS_RE'
            EXPORTING
              pernr   = lwa_saida-pernr
              begda   = lwa_saida-data_batida
              endda   = lwa_saida-data_batida
            TABLES
              t_saida = lit_saida_abono.

          IF lit_saida_abono IS NOT INITIAL.
            CLEAR: lva_stadz.
            LOOP AT lit_saida_abono INTO DATA(lwa_saida_abono) .
              IF lwa_saida_abono-codabono IN r_awart.
                DATA(lva_not_jorn) = 'X'.
              ELSE.
                IF lwa_saida_abono-codabono IN r_awart_x.
                  DATA(lva_jorn_qnt) = 'X'.
                  lva_stadz = lva_stadz + lwa_saida_abono-stdaz.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          CONCATENATE  lwa_saida-data_batida+6(2) '/'  lwa_saida-data_batida+4(2) '/' lwa_saida-data_batida+0(4) INTO lva_dtbatida .

          gwa_saida-data = lva_dtbatida.

*          IF lva_not_jorn NE 'X'.

          CALL FUNCTION 'ZHCMF_RETURN_HORARIOS_FUNC'
            EXPORTING
              pernr   = lwa_saida-pernr
              begda   = lwa_saida-data_batida
              endda   = lwa_saida-data_batida
            TABLES
              t_saida = lit_horarios.      " Tabela Horários Calendário - Ponto Exceção

          IF ( lit_horarios[] IS NOT INITIAL ).
            READ TABLE  lit_horarios INTO DATA(lwa_horarios) INDEX 1.
            gwa_saida-inicio_jornada   = lwa_horarios-hora_inicio.
            gwa_saida-inicio_intervalo = lwa_horarios-intervalo_ini.
            gwa_saida-fim_intervalo    = lwa_horarios-intervalo_fim.
            gwa_saida-fim_jornada      = lwa_horarios-hora_fim.

            CLEAR: lva_hora, lva_time, lva_tot_atu.
            lva_tot_atu = lwa_horarios-hora_diario.
            CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
              EXPORTING
                v_hora     = lva_tot_atu
              IMPORTING
                v_ret_hora = lva_hora
                v_ret_time = lva_time.

            gwa_saida-total_jornada_esperada = lva_hora.

          ENDIF.
*          ENDIF.

          gwa_saida-total_jornada_registrada = lwa_saida-htrab.

          IF lva_jorn_qnt = 'X'.
            CLEAR: lva_hora, lva_time, lva_tot_atu.
            lva_tot_atu = lva_stadz.
            CALL FUNCTION 'ZHCMF_CONVERT_NUM_HORA'
              EXPORTING
                v_hora     = lva_tot_atu
              IMPORTING
                v_ret_hora = lva_hora
                v_ret_time = lva_time.

            IF lva_hora >= gwa_saida-total_jornada_esperada.
              lva_not_jorn = 'X'.
*              CLEAR: gwa_saida-inicio_jornada,
*                     gwa_saida-inicio_intervalo,
*                     gwa_saida-fim_intervalo,
*                     gwa_saida-fim_jornada,
*                     gwa_saida-total_jornada_esperada.
            ENDIF.
          ENDIF.

          IF lwa_saida-htrab >= gwa_saida-total_jornada_esperada."  lwa_saida-base .
            REPLACE ALL OCCURRENCES OF ':' IN lwa_saida-htrab WITH space.
            CONDENSE lwa_saida-htrab NO-GAPS.
            CONCATENATE lwa_saida-htrab+0(2) ':' lwa_saida-htrab+2(2) INTO gwa_saida-hora_calculo.
          ELSE.
            gwa_saida-hora_calculo = gwa_saida-total_jornada_esperada. "lwa_saida-base.
          ENDIF.

          IF lwa_saida-ent1 IS NOT INITIAL.
            APPEND  lwa_saida-ent1 TO gwa_saida-batida.
          ENDIF.
          IF lwa_saida-sai1 IS NOT INITIAL.
            APPEND  lwa_saida-sai1 TO gwa_saida-batida.
          ENDIF.
          IF lwa_saida-ent2 IS NOT INITIAL.
            APPEND  lwa_saida-ent2 TO gwa_saida-batida.
          ENDIF.
          IF lwa_saida-sai2 IS NOT INITIAL.
            APPEND  lwa_saida-sai2 TO gwa_saida-batida.
          ENDIF.
          IF lwa_saida-ent3 IS NOT INITIAL.
            APPEND  lwa_saida-ent3 TO gwa_saida-batida.
          ENDIF.
          IF lwa_saida-sai3 IS NOT INITIAL.
            APPEND  lwa_saida-sai3 TO gwa_saida-batida.
          ENDIF.

          CLEAR: lwa_saida, lva_dtbatida.

          IF gwa_saida-inicio_jornada = 0 AND
              gwa_saida-inicio_intervalo  = 0 AND
              gwa_saida-fim_intervalo = 0 AND
              gwa_saida-fim_jornada = 0 AND
              gwa_saida-total_jornada_registrada IS INITIAL AND
              gwa_saida-total_jornada_esperada IS INITIAL.
            lva_not_jorn = 'X'.
          ENDIF.

          IF  lva_not_jorn NE 'X'. " Possui ausência que não conta dia.
            APPEND gwa_saida TO git_saida.
          ENDIF.


          CLEAR: gwa_saida,
                 lit_saida_abono,
                 lwa_time1,
                 lwa_time2,
                 lwa_result,
                 lwa_int1,
                 lwa_int2,
                 lit_horarios,
                 lva_dtbatida,
                 lva_hora_diario,
                 lva_part1,
                 lva_part2,
                 lva_not_jorn,
                 lva_jorn_qnt.

        ENDLOOP.
      ENDIF.
      CLEAR: lwa_return,
             lit_saida,
             lit_objid.

      CLEAR: lit_return.
    ENDIF.

    IF git_saida IS NOT INITIAL.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = git_saida ).
    ELSE.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Dados não econtrados'.
      e_msg_outbound =  '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                        '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                        '}'.
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
