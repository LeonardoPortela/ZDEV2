class zcl_int_ib_cons_zmm0132 definition
  public
  final
  create public .

  public section.

    interfaces zif_integracao_inject .
    interfaces zif_integracao_inbound .

    types:
      begin of ty_makt,
        maktg type makt-maktg,
        maktx type makt-maktx,
      end of ty_makt .

    data:
      tb_matnr type table of matnr,
      tb_werks type table of werks_d,
      tb_lgort type table of lgort_d,
      tb_matkl type table of matkl.
    data:
      begin of ty_zmm0132,
        matnr       type mard-matnr,
        maktx       type makt-maktx,
        mtart       type mara-mtart,
        meins       type mara-meins,
        werks       type mard-werks,
        lgort       type mard-lgort,
        charg       type mchb-charg,
        diasr       type i,
        clabs       type mchb-clabs,
        lifnr       type mslb-lifnr,
        lblab       type mslb-lblab,
        matkl       type mara-matkl,
        hsdat       type mcha-hsdat,
        licha       type mcha-licha,
        umlmc       type marc-umlmc,
        trame       type marc-trame,
        status(20),
        saldo_z(02),
        name1       type lfa1-name1,
        vfdat       type zppt0011-vfdat,
        pvprs       type ckmlcr-pvprs,
        salk3       type ckmlcr-salk3,
        lifnr_ped   type ekko-lifnr,
        name1_ped   type lfa1-name1, "
      end of ty_zmm0132 .
    data:
      begin of zde_data_request,
        werks   like tb_werks,
        matnr   like tb_matnr,
        lgort   like tb_lgort,
        matkl   like tb_matkl,
        agrupar type c,
      end of zde_data_request .
    data:
      begin of zde_data_response,
        estoques like table of ty_zmm0132,
      end of zde_data_response .
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
    constants at_id_interface type zde_id_interface value '259' ##NO_TEXT.

    methods constructor
      raising
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_ZMM0132 IMPLEMENTATION.


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

    IF lwa_data_request IS NOT INITIAL.

      IF lwa_data_request-werks is INITIAL.
        r_msg_erro = 'Centro deve ser informado'.
        RETURN.
      ENDIF.

    ENDIF.


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


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  method zif_integracao_inject~set_integrar_inbound.

    data: lwa_data_request  like zde_data_request,
          lwa_data_response like zde_data_response.

    data: lwa_matnr(18) type n,
          lwa_qtde      type labst.

    field-symbols  : <lt_pay_data>   type any table .
    field-symbols : <lt_test> type any .

    data lr_pay_data              type ref to data.

    data: lt_tab  type table of abaplist,
          lt_tab2 like table of zde_data_response.

    data: lr_werks type range of werks_d,
          lr_matnr type range of matnr,
          lr_lgort type range of lgort_d,
          lr_matkl type range of matkl.

    types:
      begin of ty_material,
        material type matnr,
      end of ty_material.

    data: lra_material type range of matnr,
          zva_matnr    type char18,
          lra_centro   type range of werks_d,
          lra_deposito type range of lgort_d,
          lra_lote     type range of charg_d.

    r_if_integracao_inject = me.

    clear: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    if i_msg_inbound is not initial.
      /ui2/cl_json=>deserialize( exporting json = i_msg_inbound changing data = lwa_data_request ).
    endif.

    me->zif_integracao_inbound~validar_dados_inbound( exporting i_data_inbound = i_msg_inbound importing e_status_code = data(_status_code) receiving r_msg_erro = e_msg_erro ).

    if e_msg_erro is not initial.

      if _status_code is initial .
        _status_code = '400'. "Bad Request
      endif.

      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      return.
    endif.

    if lwa_data_request-werks[] is not initial.
      lr_werks = value #( for ls_werks in lwa_data_request-werks[]
                                ( sign = 'I'
                                  option = 'EQ'
                                  low = ls_werks )
                            ).
    endif.

    if lwa_data_request-matnr[] is not initial.
      lr_matnr = value #( for ls_matnr in lwa_data_request-matnr[]
                                ( sign = 'I'
                                  option = 'EQ'
                                  low = ls_matnr )
                            ).
    endif.

    if lwa_data_request-lgort[] is not initial.
      lr_lgort = value #( for ls_lgort in lwa_data_request-lgort[]
                            ( sign = 'I'
                              option = 'EQ'
                              low = ls_lgort )
                        ).
    endif.

    if lwa_data_request-matkl[] is not initial.
      lr_matkl = value #( for ls_matkl in lwa_data_request-matkl[]
                            ( sign = 'I'
                              option = 'EQ'
                              low = ls_matkl )
                        ).
    endif.

    cl_salv_bs_runtime_info=>set( exporting display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).

    submit zmmr129 with s_cent  in lr_werks
                   with s_matn  in lr_matnr
                   with s_lgor  in lr_lgort
                   with s_matkl in lr_matkl
                   with p_group = lwa_data_request-agrupar
                   and return.


    try.

        cl_salv_bs_runtime_info=>get_data_ref( importing r_data = lr_pay_data ).
        assign lr_pay_data->* to <lt_pay_data>.
      catch cx_salv_bs_sc_runtime_info.

        e_sucesso   = abap_true.
        e_nm_code   = '400'.
        e_msg_erro  = 'Ok'.
        e_msg_erro  = 'Não localizado informações'.

    endtry.

    move-corresponding <lt_pay_data> to lwa_data_response-estoques.

    if lwa_data_response-estoques is not initial.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.

      e_msg_outbound = /ui2/cl_json=>serialize( exporting data = lwa_data_response-estoques ).
    else.
      e_sucesso   = abap_true.
      e_nm_code   = '400'.
      e_msg_erro  = 'Ok'.
      e_msg_erro  = 'Não localizado informações'.
    endif.

  endmethod.


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
