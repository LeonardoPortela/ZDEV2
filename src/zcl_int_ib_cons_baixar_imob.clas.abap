CLASS zcl_int_ib_cons_baixar_imob DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    DATA:
      BEGIN OF z_table,
        table TYPE string,
      END OF z_table .
    DATA:
      BEGIN OF zde_data_request,
        n_sba	           TYPE char15,
        cod_empresa      TYPE char4,
        cod_filial       TYPE char4,
        n_imobilizado	   TYPE char12,
        subn_imobilizado TYPE char4,
        n_equipamento_pm TYPE char18,
        data_baixa       TYPE char10,
        motivo_baixa     TYPE char100,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        sucesso TYPE string,
        erro    TYPE string,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '293' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_int_ib_cons_baixar_imob IMPLEMENTATION.


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




  ENDMETHOD.


  METHOD zif_integracao_inject~get_form_request_http.
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


  METHOD zif_integracao_inject~set_form_request_http.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

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

    DATA: wa_zimt0004   TYPE zimt0004,
          lv_dia        TYPE char2,
          lv_mes        TYPE char2,
          lv_ano        TYPE char4,
          lv_data_baixa TYPE sy-datum.

    " --------------------------
    " Verificar se algum campo está vazio
    " --------------------------
    IF lwa_data_request-n_sba             IS INITIAL OR
       lwa_data_request-cod_empresa       IS INITIAL OR
*       lwa_data_request-cod_filial        IS INITIAL OR
       lwa_data_request-n_imobilizado     IS INITIAL OR
*       lwa_data_request-subn_imobilizado  IS INITIAL OR
*       lwa_data_request-n_equipamento_pm  IS INITIAL OR
       lwa_data_request-data_baixa        IS INITIAL OR
       lwa_data_request-motivo_baixa      IS INITIAL.

      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Preencher todos os campos.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.

    ENDIF.

    "FF #179192 - inicio
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_data_request-n_imobilizado
      IMPORTING
        output = lwa_data_request-n_imobilizado.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_data_request-subn_imobilizado
      IMPORTING
        output = lwa_data_request-subn_imobilizado.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_data_request-cod_empresa
      IMPORTING
        output = lwa_data_request-cod_empresa.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_data_request-n_equipamento_pm
      IMPORTING
        output = lwa_data_request-n_equipamento_pm.
    "FF #179192 - fim


    " --------------------------
    " Validação dos dados enviados
    " --------------------------
    "Validar campo cod_empresa
    SELECT SINGLE bukrs FROM t001 INTO @DATA(ls_bukrs) WHERE bukrs = @lwa_data_request-cod_empresa.
    IF sy-subrc IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Cod. Empresa não é válido.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    "Validar campo cod_filial
    SELECT SINGLE werks FROM t001w INTO @DATA(ls_werks) WHERE werks = @lwa_data_request-cod_filial.
    IF sy-subrc IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Cod. Filial não é válido.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    "Validar campo n_imobilizado
    SELECT SINGLE anln1, xloev FROM anla INTO @DATA(ls_anla) WHERE anln1 = @lwa_data_request-n_imobilizado.
    IF sy-subrc IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Nº do Imobilizado não é válido.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.

      "FF #179192 - inicio
    ELSEIF ls_anla-xloev IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Imobilizado já consta como baixado.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.

    ENDIF.
    "FF #179192 - fim

    "Validar campo subn_imobilizado
    IF lwa_data_request-subn_imobilizado IS NOT INITIAL AND lwa_data_request-subn_imobilizado <> 0.
      SELECT SINGLE anln2 FROM anla INTO @DATA(ls_anln2) WHERE anln2 = @lwa_data_request-subn_imobilizado.
      IF sy-subrc IS NOT INITIAL.
        e_sucesso   = abap_false.
        e_nm_code   = '400'.
        e_msg_erro  = 'Sub Nº do Imobilizado não é válido.'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.
      ENDIF.
    ENDIF.

    "Validar campo n_equipamento
    IF lwa_data_request-n_equipamento_pm IS NOT INITIAL.
      lwa_data_request-n_equipamento_pm = |{ lwa_data_request-n_equipamento_pm ALPHA = IN }|.
      SELECT SINGLE equnr FROM equi INTO @DATA(ls_equnr) WHERE equnr = @lwa_data_request-n_equipamento_pm.
      IF sy-subrc IS NOT INITIAL.
        e_sucesso   = abap_false.
        e_nm_code   = '400'.
        e_msg_erro  = 'Equipamento não é válido.'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.
      ENDIF.
    ENDIF.

    "Validar campo data_baixa
    SPLIT lwa_data_request-data_baixa AT '/' INTO lv_dia lv_mes lv_ano.
    lv_data_baixa = lv_ano && lv_mes && lv_dia.

    CLEAR: lwa_data_request-data_baixa.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = lv_data_baixa
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = 'Data informada não é válida.'.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.


    "FF #179192 - inicio
    "validar pendências no equipamento.

    "Notas
    DATA: ls_return_nota TYPE bapireturn.

    DATA(lt_notas) = zcl_pm_util=>consultar_notas_pend(
                       EXPORTING
                         i_equnr        = lwa_data_request-n_equipamento_pm
                       IMPORTING
                         es_return_nota = ls_return_nota ).

    IF lt_notas[] IS NOT INITIAL.
      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = |Existem notas pendentes vinculadas ao equipamento | && |{ lwa_data_request-n_equipamento_pm ALPHA = OUT }.|.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.

    "Ordens
    DATA: lt_return_ordens TYPE bapiret2_t.

    DATA(lt_ordens) = zcl_pm_util=>consultar_ordens_pend(
                        EXPORTING
                          i_equnr          = lwa_data_request-n_equipamento_pm
                        IMPORTING
                          et_return_ordens = lt_return_ordens ).


    IF lt_ordens[] IS NOT INITIAL.
      e_sucesso     = abap_false.
      e_nm_code     = '400'.
      e_msg_erro    = |Existem ordens pendentes vinculadas ao equipamento { lwa_data_request-n_equipamento_pm ALPHA = OUT }.|.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.


    "Verifica se o equipamento já foi eliminado.
    DATA: ls_return      TYPE bapiret2,
          lv_sys_status  TYPE j_stext,
          lv_user_status TYPE asttx,
          lt_sys_status  TYPE TABLE OF bapi_itob_status,
          lt_usr_status  TYPE TABLE OF bapi_itob_status,
          lv_langu       TYPE sy-langu VALUE 'P'.


    CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
      EXPORTING
        equipment     = lwa_data_request-n_equipamento_pm
        language      = lv_langu
      IMPORTING
        systemstatus  = lv_sys_status
        userstatus    = lv_user_status
        return        = ls_return
      TABLES
        system_status = lt_sys_status
        user_status   = lt_usr_status.

    READ TABLE lt_sys_status WITH KEY status = 'I0076' INTO DATA(wa). "Marcado para eliminação
    IF sy-subrc = 0.
      e_sucesso      = abap_false.
      e_nm_code      = '400'.
      e_msg_erro     = |Equipamento { lwa_data_request-n_equipamento_pm ALPHA = OUT } já está marcado para eliminação.|.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.
    ENDIF.


    "-----Baixa do imobilizado-----

    DATA: ls_posting TYPE bapifapo_gen_info,
          ls_ret     TYPE bapifapo_ret,
          ls_acc     TYPE bapifapo_acc_ass,
          ls_add     TYPE bapifapo_add_info,
          ls_docref  TYPE bapifapo_doc_ref,
          lt_return  TYPE TABLE OF bapiret2.

    " --- Dados gerais ---
    ls_posting-comp_code     = lwa_data_request-cod_empresa.
    ls_posting-doc_date      = lv_data_baixa.
    ls_posting-pstng_date    = lv_data_baixa.
    ls_posting-fis_period    = lv_data_baixa+4(2). " mês da data de baixa
    ls_posting-doc_type      = 'AA'.                           " Tipo de documento
    ls_posting-assetmaino    = lwa_data_request-n_imobilizado.
    ls_posting-assetsubno    = lwa_data_request-subn_imobilizado.
*    ls_posting-assettrtyp    = '01'.                           " 01 = baixa total
    ls_posting-acc_principle = 'GAAP'.                         " princípio contábil
    ls_posting-depr_area     = '01'.                           " área de avaliação
    ls_posting-ledger_group  = '0L'.                           " ledger principal

    " --- Dados de baixa (financeiros) ---
    ls_ret-perc_rate  = '100'.                                 " baixa total
    ls_ret-valuedate = lv_data_baixa. " Data de referência (obrigatório)

    " --- Texto da transação ---
    ls_add-header_txt = |{ lwa_data_request-motivo_baixa } { lwa_data_request-n_sba }|.

    " --- Chamada da BAPI ---
    CALL FUNCTION 'BAPI_ASSET_RETIREMENT_POST'
      EXPORTING
        generalpostingdata = ls_posting
        retirementdata     = ls_ret
        accountassignments = ls_acc        " se houver centro de custo/ordem/WBS
        furtherpostingdata = ls_add        " textos/documento ref
      IMPORTING
        documentreference  = ls_docref
        return             = ls_return
      TABLES
        return_all         = lt_return.


    DATA(lv_has_error) = abap_false.

    LOOP AT lt_return INTO ls_return.
      IF ls_return-type = 'E' OR ls_return-type = 'A'.
        lv_has_error = abap_true.
      ENDIF.
      EXIT.
    ENDLOOP.

    IF lv_has_error = abap_true.

      e_sucesso   = abap_false.
      e_nm_code   = '400'.
      e_msg_erro  = ls_return-message.
      e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
      EXIT.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      DATA(lv_imobilizado_baixado) = abap_true.

      "----Marcar o equipamento para eliminação----

      DATA:
        lt_status TYPE TABLE OF jstat,
        ls_status TYPE jstat.

      SELECT SINGLE objnr
        INTO @DATA(lv_objnr)
        FROM equi
       WHERE equnr = @lwa_data_request-n_equipamento_pm.   " número do equipamento


      " Monta o status de eliminação
      CLEAR ls_status.
      ls_status-stat  = 'I0076'.   " Flag eliminação (DLFL)
      ls_status-inact = space.     " ativa o status
      APPEND ls_status TO lt_status.

      " Chama FM para alterar status
      CALL FUNCTION 'STATUS_CHANGE_INTERN'
        EXPORTING
          objnr               = lv_objnr
        TABLES
          status              = lt_status
        EXCEPTIONS
          object_not_found    = 1
          status_inconsistent = 2
          status_not_allowed  = 3
          OTHERS              = 4.

      IF sy-subrc IS NOT INITIAL.

        e_sucesso   = abap_false.
        e_nm_code   = '400'.
        e_msg_erro  = 'Houve um erro ao tentar desativar o equipamento.'.
        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
        EXIT.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        IF lv_imobilizado_baixado = abap_true.


          " --------------------------
          " Alimentar tabela ZMMT0004
          " --------------------------
          MOVE-CORRESPONDING lwa_data_request TO wa_zimt0004.
          wa_zimt0004-data_baixa = lv_data_baixa.
          wa_zimt0004-executado = abap_true.

          MODIFY zimt0004 FROM wa_zimt0004.
          COMMIT WORK.
          IF sy-subrc = 0.
            e_sucesso   = abap_true.
            e_nm_code   = '200'.
            e_msg_erro  = 'Baixa executada com sucesso!'.
            e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
          ELSE.
            ROLLBACK WORK.
            e_sucesso   = abap_false.
            e_nm_code   = '400'.
            e_msg_erro  = 'Houve um erro ao gravar os dados na tabela.'.
            e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = e_msg_erro ).
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "FF #179192 - fim


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
