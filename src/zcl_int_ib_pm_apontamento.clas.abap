class ZCL_INT_IB_PM_APONTAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '197' ##NO_TEXT.

  methods CONSTRUCTOR .
  methods CRIA_APONTAMENTO
    importing
      !I_IMPORTA_DADOS type ZTPM_D_M_APONTAMENTO_T
    exporting
      !E_RETORNO type ZTPM_D_M_APONT_RETORNO_T .
  methods VALIDA_APONTAMENTO
    importing
      !I_APONTAMENTOS type BAPI_ALM_TIMECONFIRMATION_T
      !I_ORDEM type AUFNR
    exporting
      !E_MSG type CHAR255 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_PM_APONTAMENTO IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD cria_apontamento.

    DATA: t_apontamento TYPE bapi_alm_timeconfirmation_t,
          _return       TYPE bapiret2,
          t_alm_return  TYPE TABLE OF bapi_alm_return,
          t_ordem       TYPE ztpm_d_m_ordem_t,
          lv_msg        TYPE char255.

    LOOP AT i_importa_dados INTO DATA(wa_dados).

      CLEAR t_apontamento[].

      t_apontamento =
             VALUE #( (
                                orderid         = |{ wa_dados-aufnr ALPHA = IN }|
                                operation       = wa_dados-vornr
                                postg_date      = wa_dados-budat
                                work_cntr       = wa_dados-arbpl
                                pers_no         = wa_dados-pernr
                                exec_start_date = wa_dados-isdd
                                exec_start_time = wa_dados-isdz
                                exec_fin_date   = wa_dados-iedd
                                exec_fin_time   = wa_dados-iedz
                                fin_conf        = wa_dados-aueru
                                act_work        = wa_dados-ismnw
                                un_work         = wa_dados-ismne
                                dev_reason      = wa_dados-grund
                   ) ).

      IF t_apontamento[] IS NOT INITIAL.

        CLEAR lv_msg.
        me->valida_apontamento(
          EXPORTING
            i_apontamentos =   t_apontamento " Tipo tabela dados importado apontamento hr/home  PM x Mobile
            i_ordem        =   wa_dados-aufnr " Nº ordem
          IMPORTING
            e_msg          =   lv_msg         " retorno da validação
        ).

        IF lv_msg IS INITIAL.
          CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
            IMPORTING
              return        = _return
            TABLES
              timetickets   = t_apontamento
              detail_return = t_alm_return.
        ELSE.
          _return-message = lv_msg.
        ENDIF.

        IF _return IS INITIAL.
          READ TABLE t_alm_return INTO DATA(wa_alm_return) INDEX 1.
          APPEND VALUE #( success = abap_true
                          id_mobman = wa_dados-id_mobman
                          rueck     = wa_alm_return-conf_no
                          rmzhl     = wa_alm_return-conf_cnt
                          msg_error = '' ) TO e_retorno.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

        ELSE.
          APPEND VALUE #(  success = abap_false
                           id_mobman =  wa_dados-id_mobman
                           rueck     = wa_dados-rueck
                           rmzhl     = wa_dados-rmzhl
                           msg_error =  _return-message ) TO e_retorno.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD valida_apontamento.

    DATA: zretun    TYPE p DECIMALS 2,
          v_ponta   TYPE p DECIMALS 2,
          cont_erro TYPE p DECIMALS 2,
          lv_aufnr  TYPE aufk-aufnr.

    IF i_apontamentos IS NOT INITIAL.

      DATA(lt_apont) = i_apontamentos.
      SORT lt_apont BY pers_no.
      DELETE ADJACENT DUPLICATES FROM lt_apont COMPARING pers_no.

      DELETE lt_apont WHERE pers_no IS INITIAL.
      IF sy-subrc IS INITIAL.
        MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 000 WITH 'Existem apontamentos sem Nº Pessoal ' 'favor corrigir'  INTO e_msg.
        EXIT.
      ENDIF.
*    READ TABLE p_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>) INDEX 1.
      IF i_ordem IS NOT INITIAL.

        lv_aufnr = |{ i_ordem ALPHA = IN }|.

        DATA(lt_apont_aux) = lt_apont.
        DELETE lt_apont_aux WHERE operation IS INITIAL.

        IF lt_apont_aux IS NOT INITIAL.

          SELECT *
            FROM afru
            INTO TABLE @DATA(lt_afru)
            FOR ALL ENTRIES IN @lt_apont_aux
            WHERE aufnr = @lv_aufnr
              AND vornr = @lt_apont_aux-operation.
          IF sy-subrc IS INITIAL.

            SORT lt_afru BY aueru.
            READ TABLE lt_afru TRANSPORTING  NO FIELDS
            WITH KEY aueru = 'X'
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 000 WITH 'Não é possível realizar apontamento ' 'ordem com confirmação final!'  INTO e_msg.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT *
      FROM afru
      INTO TABLE @DATA(t_afru)
        FOR ALL ENTRIES IN @lt_apont
      WHERE pernr EQ @lt_apont-pers_no
        AND stokz NE @abap_true
        AND stzhl EQ ' '.

      SORT t_afru ASCENDING BY pernr.
      CLEAR zretun.
      CLEAR v_ponta.


      LOOP AT i_apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).

        READ TABLE t_afru ASSIGNING FIELD-SYMBOL(<fs_afru>)
        WITH KEY pernr = <fs_apontamentos>-pers_no
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          LOOP AT t_afru ASSIGNING <fs_afru> FROM sy-tabix.

            IF <fs_afru>-pernr <> <fs_apontamentos>-pers_no.
              EXIT.
            ENDIF.

            IF ( <fs_afru>-isdd = <fs_apontamentos>-exec_start_date OR <fs_afru>-iedd = <fs_apontamentos>-exec_fin_date ) ."AND <fs_apontamentos>-stokz IS INITIAL.
              IF  <fs_apontamentos>-exec_start_date BETWEEN <fs_afru>-isdd AND <fs_afru>-iedd.
                IF <fs_apontamentos>-exec_start_time BETWEEN <fs_afru>-isdz AND <fs_afru>-iedz.
                  ADD 1 TO zretun.
                  EXIT.
                ENDIF.
              ENDIF.
            ENDIF.

          ENDLOOP.

          IF zretun <> 0.
            MESSAGE ID 'ZPMMSG' TYPE 'E' NUMBER 009 WITH <fs_apontamentos>-pers_no  INTO e_msg.
            CONCATENATE e_msg <fs_apontamentos>-pers_no INTO e_msg SEPARATED BY space.
            EXIT.
          ENDIF.

        ENDIF.

      ENDLOOP.
    ENDIF.
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

    DATA: lwa_data_request TYPE ztpm_d_m_apontamento_t.

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


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  TYPE ztpm_d_m_apontamento_t,
          lwa_data_retorno  TYPE ztpm_d_m_apont_retorno_t,
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

    me->cria_apontamento(
      EXPORTING
        i_importa_dados = lwa_data_request    " Estrutura de apontamentos
      IMPORTING
        e_retorno       =  lwa_data_retorno   " Apontamento Ordem PM
    ).

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_retorno ). "enviar para essa tabela lwa_data_response

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
