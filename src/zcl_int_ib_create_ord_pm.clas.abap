class ZCL_INT_IB_CREATE_ORD_PM definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '166' ##NO_TEXT.

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CREATE_ORD_PM IMPLEMENTATION.


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

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.


    DATA: lwa_data_request TYPE zpme_ordem_pm,
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

    IF lwa_data_request-equnr IS INITIAL.
      r_msg_erro = 'Preenchimento do equipamento é obrigatório!'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  endmethod.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    CONSTANTS: c_delete TYPE c LENGTH 50 VALUE 'DELETE',
               c_post   TYPE c LENGTH 50 VALUE 'POST'.

    DATA: lit_data_inbound      TYPE zpme_ordem_pm,
          lwa_zsdt0001cg_gravar TYPE zsdt0001cg,
          lwa_eq_header         TYPE alm_me_tob_header,
          wa_iflo               TYPE iflo,
          it_return             TYPE TABLE OF bapiret2.

    DATA: object_eqpto TYPE REF TO zcl_pm_data_equipament.
    CREATE OBJECT object_eqpto.


    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
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

    IF lit_data_inbound IS NOT INITIAL.

      lit_data_inbound-equnr = |{ lit_data_inbound-equnr ALPHA = IN }|.

      "Buscar dados equipamento.
      CALL FUNCTION 'ALM_ME_EQUIPMENT_GETDETAIL'
        EXPORTING
          i_equipment    = lit_data_inbound-equnr
        IMPORTING
          e_equi_header  = lwa_eq_header
        TABLES
          return         = it_return
        EXCEPTIONS
          not_successful = 1
          OTHERS         = 2.

      READ TABLE it_return INTO DATA(ws_return) WITH KEY type = 'E'.
      IF sy-subrc EQ 0.

        e_msg_erro     = 'Equipamento não cadastrado no SAP'.
        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
        RETURN.
      ENDIF.

      lwa_eq_header-kostl = |{ lwa_eq_header-kostl ALPHA = IN }|.
      lit_data_inbound-kostl = |{ lit_data_inbound-kostl ALPHA = IN }|.

"FF - USER STORY 76104 - 02.02.2024
*      IF lwa_eq_header-kostl NE lit_data_inbound-kostl.
*
*        e_msg_erro     = 'Centro de custo ennviado é diferente do cadastro eqpto no SAP'.
*        e_sucesso      = abap_true.
*        e_nm_code      = '400'.
*        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
*                         '}'.
*        RETURN.
*      ENDIF.
"FF - USER STORY 76104 - 02.02.2024

      IF lwa_eq_header-wkctr IS INITIAL.
        e_msg_erro     = 'Centro de trabalho não cadastrado para equipamento->'  && lwa_eq_header-equnr.
        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
        RETURN.
      ENDIF.

      IF lit_data_inbound-tplnr IS NOT INITIAL AND lit_data_inbound-equnr IS INITIAL.
        "Bucas local de instalação.
        CLEAR: wa_iflo.
        SELECT SINGLE tplnr FROM iflo INTO wa_iflo WHERE tplnr EQ lit_data_inbound-tplnr.
        IF sy-subrc NE 0.
          e_msg_erro     = 'Local de instalação não cadastrado->'  && lit_data_inbound-tplnr.
          e_sucesso      = abap_true.
          e_nm_code      = '400'.
          e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                           '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                           '}'.
          RETURN.
        ENDIF.
      ENDIF.

      object_eqpto->zif_pm_data_equipament~get_instance(
          )->set_work_ctr( i_work_ctr = lwa_eq_header-wkctr i_werks = lwa_eq_header-swerk
          )->criar_ordem_manutencao(
          EXPORTING
         order_type   = 'SI01'
         short_text   = lit_data_inbound-ktext
         planplant    = lwa_eq_header-swerk
         funct_loc    = lwa_eq_header-tplnr
         bus_area     = lwa_eq_header-swerk
         mn_wk_ctr    = object_eqpto->zif_pm_data_equipament~at_work_ctr
         plant        = lwa_eq_header-swerk
         maintplant   = lwa_eq_header-swerk
         loc_bus_area = lwa_eq_header-swerk
         plangroup    = 'FRO'
         equipment    = lwa_eq_header-equnr
*         costcenter   = lwa_eq_header-kostl  "FF - USER STORY 76104 - 02.02.2024
         costcenter   = lit_data_inbound-kostl "FF - USER STORY 76104 - 02.02.2024
         pmacttype    = '011'
         priority     = '3'
         activity     = '0010'
         control_key  = 'PM01'
         text_longo   = lit_data_inbound-text_longo
         description  = lit_data_inbound-ktext
         sortfield    = lit_data_inbound-sas
         estimated_costs = lit_data_inbound-estimated_costs  "FF - USER STORY 76104 - 02.02.2024
         IMPORTING
           e_ordem = object_eqpto->zif_pm_data_equipament~at_numero_ordem
         ).
    ENDIF.


    IF object_eqpto->zif_pm_data_equipament~at_numero_ordem IS INITIAL.
      e_msg_erro     = 'Ñão foi possivél criar a ordem de sinistro para o equipamento'.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    lit_data_inbound-aufnr = object_eqpto->zif_pm_data_equipament~at_numero_ordem.
    lit_data_inbound-auart = 'SI01'.


    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lit_data_inbound ).

  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
