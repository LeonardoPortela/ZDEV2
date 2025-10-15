class ZCL_INT_IB_CONS_IT0030 definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  data:
    BEGIN OF zde_data_response,
        romaneio TYPE zsdt0001,
      END OF zde_data_response .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '173' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_IT0030 IMPLEMENTATION.


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
    IF lwa_data_request-begda IS INITIAL AND lwa_data_request-endda IS INITIAL .
      r_msg_erro = 'Nenhum filtro foi informado!'.
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


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    TYPES:
      BEGIN OF ty_response.
        INCLUDE TYPE zhcmt_pa_0036.
      TYPES END OF ty_response.
    TYPES:
      BEGIN OF ty_pa0030,
        pernr TYPE  pa0001-pernr,
        subty TYPE  pa0030-subty,
        objps TYPE  pa0030-objps,
        begda TYPE  pa0030-begda,
        endda TYPE  pa0030-endda,
        seqnr TYPE  pa0030-seqnr,

      END OF ty_pa0030,

      BEGIN OF ty_saida,
        pernr      TYPE  pa0001-pernr,
        subty      TYPE  pa0030-subty,
        desc_tipo  TYPE  t591s-stext,
        begda      TYPE  pa0030-begda,
        endda      TYPE  pa0030-endda,
        texto(400) TYPE c,
      END OF ty_saida.

    DATA: lwa_data_request  TYPE zde_date_0030,
          lwa_data_response TYPE TABLE OF ty_response.

    DATA: lv_id_referencia TYPE zsdt0001-id_referencia.


    DATA: r_subty TYPE RANGE OF pa0030-subty.
    DATA: lit_set TYPE TABLE OF rgsb4,
          lwa_set TYPE rgsb4.

    DATA: r_kostl  TYPE RANGE OF kostl,
          ls_kostl LIKE LINE OF r_kostl.


    DATA: git_pa0030 TYPE TABLE OF ty_pa0030.

    DATA: lit_text_tab_local  TYPE hrpad_text_tab,
          lwa_text_tab_local  LIKE LINE OF lit_text_tab_local,
          lva_message_handler TYPE REF TO  if_hrpa_message_handler,
          lva_no_auth_check   TYPE  boole_d,
          lva_is_ok           TYPE  boole_d.

    DATA: git_saida  TYPE TABLE OF zhcmt_pa_0036.
    DATA: gwa_saida TYPE zhcmt_pa_0036.


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


    r_subty[] = VALUE #(
    ( sign = 'I' option = 'EQ' low = '01' ) "ADVERTENCIA
    ( sign = 'I' option = 'EQ' low = '10' ) )."SUSPENSAO


    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        client        = sy-mandt
        setnr         = 'ZHCM_PT0021_KOSTL'
        table         = 'CSKS'
        class         = '0000'
        fieldname     = 'KOSTL'
      TABLES
        set_values    = lit_set
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.

    IF sy-subrc IS INITIAL AND lit_set[] IS NOT INITIAL.
      LOOP AT lit_set INTO lwa_set.
        IF lwa_set-to NE lwa_set-from.
          ls_kostl = VALUE #( sign = 'I' option = 'BT' low = lwa_set-from high = lwa_set-to ).
          APPEND ls_kostl TO r_kostl.
          CLEAR ls_kostl.
        ELSE.
          ls_kostl = VALUE #( sign = 'I' option = 'EQ' low = lwa_set-from ).
          APPEND ls_kostl TO r_kostl.
          CLEAR ls_kostl.
        ENDIF.
      ENDLOOP.

      SELECT pa0001~pernr
             pa0030~subty
             pa0030~objps
             pa0030~begda
             pa0030~endda
             pa0030~seqnr
        INTO TABLE git_pa0030
        FROM pa0001
       INNER JOIN pa0030  ON  pa0030~pernr = pa0001~pernr
          WHERE pa0001~kostl  IN r_kostl
            AND pa0030~subty  IN r_subty
            AND pa0030~begda >= lwa_data_request-begda
            AND pa0030~endda <= lwa_data_request-endda
            AND pa0001~endda = '99991231'.


      IF git_pa0030 IS NOT INITIAL..

        LOOP AT git_pa0030 INTO DATA(lwa_pa0030).

          CALL FUNCTION 'HR_ECM_READ_TEXT_INFOTYPE'
            EXPORTING
              pernr           = lwa_pa0030-pernr
              infty           = '0030'
              subty           = lwa_pa0030-subty
              objps           = lwa_pa0030-objps
              begda           = lwa_pa0030-begda
              endda           = lwa_pa0030-endda
              seqnr           = lwa_pa0030-seqnr
              no_auth_check   = lva_no_auth_check
              message_handler = lva_message_handler
            IMPORTING
              text_tab        = lit_text_tab_local
              is_ok           = lva_is_ok.

          LOOP AT lit_text_tab_local  INTO lwa_text_tab_local.
            CASE sy-tabix.
              WHEN 1.
                gwa_saida-texto = lwa_text_tab_local.
              WHEN 2.
                CONCATENATE  gwa_saida-texto '-' lwa_text_tab_local INTO gwa_saida-texto  SEPARATED BY space.
              WHEN 3.
                CONCATENATE  gwa_saida-texto '-' lwa_text_tab_local INTO  gwa_saida-texto SEPARATED BY space.
              WHEN 4.
                CONCATENATE  gwa_saida-texto '-' lwa_text_tab_local INTO  gwa_saida-texto SEPARATED BY space.
              WHEN 5.
                CONCATENATE  gwa_saida-texto '-' lwa_text_tab_local INTO  gwa_saida-texto SEPARATED BY space.
            ENDCASE.
          ENDLOOP.

          SELECT SINGLE stext
             INTO @DATA(lva_stext)
             FROM t591s
            WHERE sprsl = @sy-langu
              AND infty = '0030'
              AND subty = @lwa_pa0030-subty.

          gwa_saida-tipo = lwa_pa0030-subty.
          gwa_saida-desc_tipo = lva_stext.

          MOVE-CORRESPONDING lwa_pa0030 TO gwa_saida.

          gwa_saida-data_inicio = lwa_pa0030-begda.
          gwa_saida-data_fim    = lwa_pa0030-endda.
          gwa_saida-data_atu    = sy-datum.
          gwa_saida-hora_atu    = sy-uzeit.
          gwa_saida-user_atu    = sy-uname.

          APPEND gwa_saida TO git_saida.

          CLEAR:gwa_saida, lwa_pa0030.

        ENDLOOP.
      ENDIF.
    ENDIF.

    IF  git_saida  IS NOT INITIAL.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = git_saida ).
    ELSE.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Dados não econtrados'.
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
