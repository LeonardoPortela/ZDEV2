CLASS zcl_int_ib_migo_ret_fert DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_makt,
        maktg TYPE makt-maktg,
        maktx TYPE makt-maktx,
      END OF ty_makt .

    DATA:
      tb_matnr TYPE TABLE OF matnr .
    DATA:
      BEGIN OF z_response,
        documento TYPE mblnr, "Documento
        ano       TYPE mjahr, "Ano
*        mblnr   TYPE mblnr, "Documento
*        mjahr   TYPE mjahr, "Ano
        message   TYPE string,
        code      TYPE string,
      END OF z_response .
    DATA:
      BEGIN OF zde_data_request,
        filial           TYPE werks_d, " (1521) N CENTRO
        material         TYPE matnr, " (417827) N Material
        data_lancamento  TYPE bldat, " (20250121) Data Documento
        peso             TYPE mb_erfmg, " (100,000) QTD EM UMR
        lote_origem      TYPE charg_d, " (2021) LOTE
        lote_destino     TYPE umcha, " (2021) LOTE
        deposito_origem  TYPE lgort_d, " (IN03) N DEPOSITO DE
        deposito_destino TYPE umlgo, " (EQ01) N DEPOSITO PARA
        os               TYPE bktxt, "(30080) Txt.Cab.Documento
        rri              TYPE mtsnr1, "(2737) Nota Material
*        werks TYPE werks_d, " (1521) N CENTRO
*        matnr TYPE matnr, " (417827) N Material
*        bldat TYPE bldat, " (20250121) Data Documento
*        erfmg TYPE mb_erfmg, " (100,000) QTD EM UMR
*        charg TYPE charg_d, " (2021) LOTE
*        umcha TYPE umcha, " (2021) LOTE
*        lgort TYPE lgort_d, " (IN03) N DEPOSITO DE
*        umlgo TYPE umlgo, " (EQ01) N DEPOSITO PARA
*        bktxt TYPE bktxt, "(30080) Txt.Cab.Documento
*        mtsnr TYPE mtsnr1, "(2737) Nota Material
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        response LIKE TABLE OF z_response,
      END OF zde_data_response .
*    data: begin of  ty_ckmlcr,
*            matnr type ckmlhd-matnr,
*            bwkey type ckmlhd-bwkey,
*            kalnr type ckmlcr-kalnr,
*            bdatj type ckmlcr-bdatj,
*            poper type ckmlcr-poper,
*            curtp type ckmlcr-curtp,
*            pvprs type ckmlcr-pvprs,
*            waers type ckmlcr-waers,
*            salk3 type ckmlcr-salk3,
*          end of ty_ckmlcr.
*
*    data: it_ckmlcr type table of ty_ckmlcr.
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '265' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_MIGO_RET_FERT IMPLEMENTATION.


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

    IF lwa_data_request IS NOT INITIAL.

      IF lwa_data_request-filial IS INITIAL.
        r_msg_erro = 'Filial deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-material IS INITIAL.
        r_msg_erro = 'Material deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-data_lancamento IS INITIAL.
        r_msg_erro = 'Data do Documento deve ser informado'.
        RETURN.
      ELSE.
        REPLACE ALL OCCURRENCES OF '-' IN lwa_data_request-data_lancamento WITH ''.
        REPLACE ALL OCCURRENCES OF '.' IN lwa_data_request-data_lancamento WITH ''.
        REPLACE ALL OCCURRENCES OF '/' IN lwa_data_request-data_lancamento WITH ''.

        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = lwa_data_request-data_lancamento
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.

        IF sy-subrc <> 0.
          r_msg_erro = 'Data do Documento não é valida!'.
          RETURN.
        ENDIF.

      ENDIF.

      IF lwa_data_request-peso IS INITIAL.
        r_msg_erro = 'Peso deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-lote_origem IS INITIAL.
        r_msg_erro = 'Lote Origem deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-lote_destino IS INITIAL.
        r_msg_erro = 'Lote Destino deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-deposito_origem IS INITIAL.
        r_msg_erro = 'Deposito Origem deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-deposito_destino IS INITIAL.
        r_msg_erro = 'Deposito Destino deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-os IS INITIAL.
        r_msg_erro = 'Nº OS deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-rri IS INITIAL.
        r_msg_erro = 'Nota deve ser informado'.
        RETURN.
      ENDIF.

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
          lwa_data_response LIKE zde_data_response.

    DATA: lwa_matnr(18) TYPE n,
          lwa_qtde      TYPE labst.

    DATA: it_dados LIKE TABLE OF z_response.

    TYPES:
      BEGIN OF ty_material,
        material TYPE matnr,
      END OF ty_material.

*    DATA: lra_material TYPE RANGE OF matnr,
*          zva_matnr    TYPE char18,
*          "lra_centro   TYPE RANGE OF filial_d,
*          lra_deposito TYPE RANGE OF lgort_d,
*          lra_lote     TYPE RANGE OF charg_d.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound = i_msg_inbound IMPORTING e_status_code = DATA(_status_code) RECEIVING r_msg_erro = e_msg_erro ).

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

**********************************************************************
    DATA: ls_header  TYPE bapi2017_gm_head_01,
          ls_code    TYPE bapi2017_gm_code,
          ls_item    TYPE bapi2017_gm_item_create,
          lt_item    TYPE STANDARD TABLE OF bapi2017_gm_item_create,
          lt_return  TYPE STANDARD TABLE OF bapiret2,
          wa_return  TYPE bapiret2,
          lw_return  TYPE bapiret2,
          ls_testrun TYPE STANDARD TABLE OF bapi2017_gm_gen-testrun,
          lw_testrun TYPE bapi2017_gm_gen,
          lv_matkl   TYPE mara-matkl,
          lv_year    TYPE bapi2017_gm_head_ret-doc_year,
          lv_mater   TYPE bapi2017_gm_head_ret-mat_doc.

    ls_code = '06'.

    SELECT SINGLE vkorg FROM t001w WHERE werks = @lwa_data_request-filial INTO @DATA(_empresa).

    CALL FUNCTION 'Z_RET_DT_AJUSTADA_FI_MM'
      EXPORTING
        p_data_ent     = lwa_data_request-data_lancamento
        p_bukrs        = _empresa
        p_val_fi       = 'X'
        p_val_mm       = 'X'
      EXCEPTIONS
        data_fi_mm_nao = 1
        OTHERS         = 2.
    IF sy-subrc IS NOT INITIAL.
      e_msg_erro = |A data informada esta em um período bloqueado para a empresa { _empresa }.|.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                 '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                 '}'.
      RETURN.
    ENDIF.

    ls_header-pstng_date = sy-datum.
    ls_header-doc_date   = lwa_data_request-data_lancamento.
    ls_header-header_txt = lwa_data_request-os. "BKTXT->BKTXT(30080) Txt.Cab.Documento
    ls_header-ref_doc_no = lwa_data_request-rri. "MTSNR->MTSNR1(2737) Nota Material



*** Formata o código do material
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lwa_data_request-material
      IMPORTING
        output = ls_item-material.

    ls_item-plant        = lwa_data_request-filial.
    ls_item-move_plant   = lwa_data_request-filial.
    ls_item-move_type    = '311'.
    ls_item-entry_qnt    = lwa_data_request-peso.
    ls_item-batch    = lwa_data_request-lote_origem.
    ls_item-move_batch    = lwa_data_request-lote_destino.
    ls_item-stge_loc    = lwa_data_request-deposito_origem.
    ls_item-move_stloc   = lwa_data_request-deposito_destino.

    APPEND ls_item TO lt_item[].

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = ls_header
        goodsmvt_code    = ls_code
      IMPORTING
        materialdocument = lv_mater
        matdocumentyear  = lv_year
      TABLES
        goodsmvt_item    = lt_item
        return           = lt_return.

    CLEAR: ls_header, ls_code, lt_item[].

    IF lv_mater IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      lwa_data_response-response[] = VALUE #( ( ano = lv_year documento = lv_mater message = 'Sucesso!' code = e_nm_code ) ).

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      READ TABLE lt_return INTO wa_return INDEX 1.
      e_msg_erro     = wa_return-message.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                 '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                 '}'.
      e_sucesso   = abap_true.
      lwa_data_response-response[] = VALUE #( ( message = e_msg_erro code = e_nm_code ) ).
    ENDIF.

**********************************************************************

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
