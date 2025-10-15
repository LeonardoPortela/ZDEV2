CLASS zcl_int_ib_cons_dados_ov DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_instrucao,
        objek      TYPE zsdt0045-objek,
        bukrs      TYPE zsdt0045-bukrs,
        werks      TYPE zsdt0045-werks,
        instrucao  TYPE zsdt0045-instrucao,
        charg      TYPE zsdt0045-charg,
        quantidade TYPE zsdt0045-quantidade,
        contrato   TYPE zsdt0045-contrato,
        safra      TYPE zsdt0045-safra,
        matnr      TYPE zsdt0045-matnr,
        maktx      TYPE makt-maktx,
        montante   TYPE zsdt0045-vlr_frete,
        btgew      TYPE zsdt0045-btgew,
        terminal   TYPE zsdt0045-terminal,
      END OF ty_instrucao.

    TYPES:
      tb_instrucao TYPE TABLE OF ty_instrucao.

    DATA:
      tb_erros  TYPE TABLE OF bapiret2-message .
    DATA:
      BEGIN OF zde_data_request,
        instrucao TYPE zsdt0045-instrucao,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        consulta_ov TYPE tb_instrucao,
      END OF zde_data_response .

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '264' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_DADOS_OV IMPLEMENTATION.


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
*
    DATA: lwa_data_request LIKE zde_data_request,
          wa_tx            TYPE zsdt0327tx,
          lv_message       TYPE string,
          lva_type         TYPE dd01v-datatype,
          lv_objnr         TYPE fleet-objnr.

    DATA: lr_chave_banco  TYPE RANGE OF bnka-bankl,
          lr_chave_banco2 TYPE RANGE OF bnka-bankl.

    CLEAR: r_msg_erro.

    IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE zif_integracao_inject~co_request_method_post.
      r_msg_erro     = 'Metodo informado não previsto!'.
      e_status_code  = '405'. "Method Not Allowed

      wa_tx-id_integracao     = me->zif_integracao_inbound~at_id_integracao.
      wa_tx-id_origem         = me->at_id_interface.
      wa_tx-origem_cadastro   = 'LK'.
      wa_tx-msg_processamento = 'Metodo informado não previsto!'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro = sy-datum.
      wa_tx-hr_registro = sy-uzeit.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.

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



  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response,
          ls_addr1          TYPE addr1_data,
          ls_lfa1           TYPE lfa1,
          ls_kna1           TYPE kna1,
          wa_tx             TYPE zsdt0327tx,
          lt_tx             TYPE TABLE OF zsdt0327tx,
          ls_itobattr       TYPE itobattr,
          ls_equi_aux       TYPE equi,
          ls_eqkt_aux       TYPE eqkt,
          ls_equz_aux       TYPE equz,
          ls_iloa_aux       TYPE iloa,
          ls_fleet_aux      TYPE fleet,
          ls_equi_aux2      TYPE equi,
          ls_eqkt_aux2      TYPE eqkt,
          ls_equz_aux2      TYPE equz,
          ls_iloa_aux2      TYPE iloa,
          ls_fleet_aux2     TYPE fleet.


    DATA:
      lra_mblnr TYPE RANGE OF mblnr,
      lra_mjahr TYPE RANGE OF mjahr,
      lra_budat TYPE RANGE OF budat,
      lra_bldat TYPE RANGE OF bldat,
      lra_xblnr TYPE RANGE OF xblnr,
      lra_smbln TYPE RANGE OF mblnr,
      lra_smblp TYPE RANGE OF mblpo,
      lra_mat   TYPE RANGE OF mblnr.

    DATA: gwa_datageneral     TYPE bapi_itob,
          gwa_data_generalx   TYPE bapi_itobx,
          gwa_datafleet       TYPE bapi_fleet,
          gwa_data_fleetx     TYPE bapi_fleetx,
          gwa_datageneralexp  TYPE bapi_itob,
          gwa_dataspecificexp TYPE bapi_itob_eq_only,
          gwa_datainstall     TYPE bapi_itob_eq_install,
          gwa_return          TYPE bapiret2,
          gwa_datafleetexp    TYPE bapi_fleet,
          gwa_externalnumber  TYPE bapi_itob_parms-equipment,
          git_xtensionin      TYPE TABLE OF bapiparex,
          gwa_dataspecific    TYPE bapi_itob_eq_only,
          gva_valid_date      TYPE sy-datum,
          lva_equipment       TYPE bapi_itob_parms-equipment,
          lv_ktx01            TYPE ktx01,
          lv_external         TYPE  bapi_itob_parms-equipment,
          lt_values           TYPE TABLE OF bapi1003_alloc_values_num,
          lt_values2          TYPE TABLE OF BAPI1003_ALLOC_VALUES_char,
          lt_values3          TYPE TABLE OF BAPI1003_ALLOC_VALUES_curr,
          lt_return           TYPE TABLE OF bapiret2,
          lv_objkey           TYPE bapi1003_key-object.

    DATA: lr_equnr  TYPE RANGE OF equi-equnr,
          lv_seq    TYPE numc4,
          lv_qtd    TYPE sy-tabix,
          lv_status TYPE c.

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

    SELECT a~objek,a~bukrs,a~werks,a~instrucao,a~charg,a~quantidade ,
           a~contrato,a~safra,a~matnr,b~maktx,a~vlr_frete,a~btgew,a~terminal
      FROM zsdt0045 AS a
      INNER JOIN makt AS b
      ON   a~matnr = b~matnr
       AND b~spras = @sy-langu
      INTO TABLE @DATA(lt_ZSDT0045)
      WHERE instrucao = @lwa_data_request-instrucao.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_zsdt0045 ASSIGNING FIELD-SYMBOL(<fs_zsdt0045>).
        APPEND INITIAL LINE TO lwa_data_response-consulta_ov ASSIGNING FIELD-SYMBOL(<fs_dados_ov>).
        MOVE-CORRESPONDING <fs_zsdt0045> TO <fs_dados_ov>.
      ENDLOOP.

      e_nm_code   = '400'.

    ELSE.

      e_nm_code   = '400'.
      e_msg_erro = 'Nenhum registro encontrado!'.

    ENDIF.

    e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response ).

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
