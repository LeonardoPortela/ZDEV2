class ZCL_INT_IB_CONS_DADOS_PERFIL definition
  public
  final
  create public .

  public section.

    interfaces ZIF_INTEGRACAO_INJECT .
    interfaces ZIF_INTEGRACAO_INBOUND .

    DATA:
      BEGIN OF z_table,
        table TYPE string,
      END OF z_table.

    DATA:
      BEGIN OF zde_data_request,
        table TYPE string,
        pages TYPE string,
      END OF zde_data_request.

    DATA:
      BEGIN OF zde_data_response,
        agr_users    TYPE TABLE OF agr_users,
        agr_tcodes   TYPE TABLE OF agr_tcodes,
        adcp         TYPE TABLE OF adcp,
        zcrtran      TYPE TABLE OF /virsa/zcrtran,
        usr21        TYPE TABLE OF usr21,
        agr_define   TYPE TABLE OF agr_define,
        agr_agrs     TYPE TABLE OF agr_agrs,
        usr02        TYPE TABLE OF usr02,
        zpft0001     TYPE TABLE OF zpft0001,
        ztd_opns_018 TYPE TABLE OF ztd_opns_018,
      END OF zde_data_response.

    CONSTANTS AT_ID_INTERFACE TYPE ZDE_ID_INTERFACE VALUE '279' ##NO_TEXT.

    METHODS CONSTRUCTOR
      RAISING
        ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_DADOS_PERFIL IMPLEMENTATION.


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
*
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
    ENDIF.

*    IF lwa_data_request IS NOT INITIAL.
*
*      IF lwa_data_request-table IS INITIAL.
*        r_msg_erro = 'Tabela deve ser informada'.
*        RETURN.
*      ENDIF.
*
*    ENDIF.




  endmethod.


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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: lwa_data_request  LIKE zde_data_request,
*          lwa_data_response LIKE zde_data_response.
          lwa_data_response TYPE REF TO data.

    DATA: it_table TYPE REF TO data.

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
      EXIT.
    ENDIF.

    DATA: lv_range          TYPE string,
          lv_page_from_str  TYPE string,
          lv_page_to_str    TYPE string,
          lv_page_from      TYPE i,
          lv_page_to        TYPE i,
          lv_page_num       TYPE i,
          lv_offset         TYPE i,
          lv_max_rows       TYPE i,
          lv_table_name     TYPE tabname,
          lr_data           TYPE REF TO data,
          lo_descr_base     TYPE REF TO cl_abap_typedescr,
          lo_descr          TYPE REF TO cl_abap_structdescr,
          lo_table          TYPE REF TO cl_abap_tabledescr,
          lt_allowed_tables TYPE STANDARD TABLE OF tabname WITH EMPTY KEY,
          lv_table_allowed  TYPE abap_bool.

    CONSTANTS: lc_page_size TYPE i VALUE 100,
               lc_max_total TYPE i VALUE 50000.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    " --------------------------
    " Paginação
    " --------------------------
    lv_range = lwa_data_request-pages.
    SPLIT lv_range AT '-' INTO lv_page_from_str lv_page_to_str.

    lv_page_from = lv_page_from_str.
    lv_page_to   = lv_page_to_str.

    IF lv_page_from > lv_page_to.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Página incorreta.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    DATA(lv_pages_requested) = lv_page_to - lv_page_from + 1.

    lv_offset = ( lv_page_from - 1 ) * lc_page_size.
    lv_max_rows = lv_pages_requested * lc_page_size.

    IF lv_max_rows > lc_max_total.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Máximo de registros ultrapassado. Total solicitado:' && lv_max_rows && '.' && ' Total de registros permitido:15000'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    " --------------------------
    " Montagem dinâmica da tabela
    " --------------------------
    lv_table_name = lwa_data_request-table.

    lt_allowed_tables = VALUE #(
  ( 'AGR_USERS' )
  ( 'AGR_TCODES' )
  ( 'ADCP' )
  ( '/VIRSA/ZCRTRAN' )
  ( 'USR21' )
  ( 'AGR_DEFINE' )
  ( 'AGR_AGRS' )
  ( 'USR02' )
  ( 'ZPFT0001' )
  ( 'ZTD_OPNS_018' )
).

    lv_table_name = to_upper( lwa_data_request-table ).

    READ TABLE lt_allowed_tables WITH KEY table_line = lv_table_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_table_allowed = abap_true.
    ELSE.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Tabela não permitida.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    TRY.
        lo_descr_base = cl_abap_typedescr=>describe_by_name( lv_table_name ).
        lo_descr = CAST cl_abap_structdescr( lo_descr_base ).
        lo_table = cl_abap_tabledescr=>create( lo_descr ).
        CREATE DATA lr_data TYPE HANDLE lo_table.
        ASSIGN lr_data->* TO <lt_table>.
      CATCH cx_sy_dyn_call_illegal_type INTO DATA(lx_type).
        e_sucesso   = abap_false.
        e_nm_code   = '400'.
        e_msg_erro  = 'Tabela não encontrada: ' && lv_table_name.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.
      CATCH cx_root INTO DATA(lx_root).
        e_sucesso   = abap_false.
        e_nm_code   = '500'.
        e_msg_erro  = lx_root->get_text( ).
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.
    ENDTRY.


    " --------------------------
    " SELECT dinâmico com paginação
    " --------------------------
    DATA(lv_total_rows) = 0.

    SELECT COUNT(*) INTO lv_total_rows FROM (lv_table_name).

    IF ( lv_offset + lv_max_rows ) > lv_total_rows.
      lv_max_rows = lv_total_rows - lv_offset.
      IF lv_max_rows < 0.
        e_sucesso   = abap_false.
        e_nm_code   = '400'.
        e_msg_erro  = 'Página não possui resultado.'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.
      ENDIF.
    ENDIF.

    IF lv_max_rows <= lv_total_rows.
      SELECT * FROM (lv_table_name)
        ORDER BY PRIMARY KEY
        INTO TABLE @<lt_table>
        UP TO @lv_max_rows ROWS
        OFFSET @lv_offset.
    ELSE.
      SELECT * FROM (lv_table_name)
       ORDER BY PRIMARY KEY
       INTO TABLE @<lt_table>
       UP TO @lv_total_rows ROWS.
    ENDIF.

    " --------------------------
    " Serialização da resposta
    " --------------------------
    IF <lt_table> IS ASSIGNED AND <lt_table> IS NOT INITIAL.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Dados retornados com sucesso'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = <lt_table> ).
    ELSE.
      e_sucesso   = abap_false.
      e_nm_code   = '404'.
      e_msg_erro  = 'Nenhum dado encontrado'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = <lt_table> ).
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
