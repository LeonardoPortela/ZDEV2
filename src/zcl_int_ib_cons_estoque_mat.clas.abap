class zcl_int_ib_cons_estoque_mat definition
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
      tb_matnr type table of matnr .
    data:
      begin of z_estoque,
        material     type matnr,
        desc_mat     type maktx,
        centro       type werks_d,
        deposito     type lgort_d,
        lote         type charg_d,
        qtd_est      type labst,
        vlr_est_brl  type vklab,
        vlr_est_usd  type vklab,
        preco_brl    type ckmlcr-pvprs,
        preco_usd    type ckmlcr-pvprs,
        date_ult_mov type sy-datum,
      end of z_estoque .
    data:
      begin of zde_data_request,
        material type ztt_material,
        centro   type ztt_centro,
        deposito type ztt_deposito,
        lote     type ztt_lote,
      end of zde_data_request .

    data: begin of zde_data_response,
            estoques like table of z_estoque,
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

    constants at_id_interface type zde_id_interface value '202' ##NO_TEXT.

    methods constructor
      raising
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_ESTOQUE_MAT IMPLEMENTATION.


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

      IF lwa_data_request-centro is INITIAL.
        r_msg_erro = 'Centro deve ser informado'.
        RETURN.
      ENDIF.

      IF lwa_data_request-deposito is INITIAL.
        r_msg_erro = 'Deposito deve ser informado'.
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
*          it_ckmlcr         type table of ckmlcr.

    data: lwa_matnr(18) type n,
          lwa_qtde      type labst.

    data: it_dados like table of z_estoque.

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


    if lwa_data_request-centro is not initial.
      loop at lwa_data_request-centro into data(wa_centro).
        append value #( sign = 'I' option = 'EQ' low = wa_centro ) to lra_centro.

      endloop.
    else.
      e_msg_erro = 'Obrigatório preenchimento do centro'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

      return.
    endif.

    if lwa_data_request-deposito is not initial.
      loop at lwa_data_request-deposito into data(wa_deposito).
        append value #( sign = 'I' option = 'EQ' low = wa_deposito ) to lra_deposito.
      endloop.
    else.
      e_msg_erro = 'Obrigatório preenchimento do deposito'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.

      return.
    endif.

    if lwa_data_request-lote is not initial.
      loop at lwa_data_request-lote into data(wa_lote).
        append value #( sign = 'I' option = 'EQ' low = wa_lote ) to lra_lote.
      endloop.
    endif.


    if lwa_data_request-material is not initial.
      loop at lwa_data_request-material into lwa_matnr.
        clear: zva_matnr.
        zva_matnr = |{ lwa_matnr alpha = out }|.
        zva_matnr = |{ zva_matnr alpha = in }|.
        append value #( sign = 'I' option = 'EQ' low = zva_matnr ) to lra_material.
      endloop.
    endif.

    "IF lwa_data_request-material IS NOT INITIAL.

    data: lv_where     type string,
          v_maktx_like type maktx,
          v_language   type makt-spras.

    "Seleciona materiais controlado por lote.
    select distinct a~matnr as material, b~maktx as desc_mat, a~werks as centro,
           a~lgort as deposito,  a~charg as lote, a~clabs as qtd_est
    from  mchb as a
    left join makt as b on b~matnr eq a~matnr
    into table @it_dados
    where a~matnr in @lra_material
      and a~werks in @lra_centro
      and a~lgort in @lra_deposito
      and a~charg in @lra_lote
      and a~clabs > 0.                                      "658445.

    "Seleciona materiais não controlado por lote.
    select distinct a~matnr as material, b~maktx as desc_mat, a~werks as centro, a~lgort as deposito, a~labst as qtd_est
     from  mard as a
     left join makt as b on b~matnr eq a~matnr
    appending corresponding fields of table @it_dados
    where a~matnr in @lra_material
      and a~werks in @lra_centro
      and a~lgort in @lra_deposito
      and a~labst > 0.

    if it_dados is not initial.
      select matnr, werks, lgort, charg, budat_mkpf
      from mseg
      into table @data(it_mseg)
      for all entries in @it_dados
      where matnr eq @it_dados-material
      and werks eq @it_dados-centro
      and lgort eq @it_dados-deposito
      and charg eq @it_dados-lote.

      select *
      from ckmlhd
      into table @data(it_ckmlhd)
      for all entries in @it_dados
      where matnr eq @it_dados-material
        and bwkey eq @it_dados-centro.

      if sy-subrc eq 0.
*    "Seleciona preço atualizado do material.
        select *
        from ckmlcr
        into table @data(it_ckmlcr)
        for all entries in @it_ckmlhd
        where kalnr eq @it_ckmlhd-kalnr.
*          and poper eq '01'
*          and bdatj eq @sy-datum+0(4).
        if sy-subrc eq 0.
          sort  it_ckmlcr descending by bdatj poper.
        endif.
      endif.


      sort it_mseg by matnr werks lgort charg.
      data(it_mseg_aux) = it_mseg.
      delete adjacent duplicates from it_mseg comparing matnr werks lgort charg.

      sort it_mseg by matnr werks lgort charg budat_mkpf descending.
      sort it_mseg_aux by matnr werks lgort charg budat_mkpf descending.

      loop at it_mseg assigning field-symbol(<wa_mseg>).
        read table it_mseg_aux assigning field-symbol(<ws_mseg>) with key matnr = <wa_mseg>-matnr
                                                                          werks = <wa_mseg>-werks
                                                                          lgort = <wa_mseg>-lgort
                                                                          charg = <wa_mseg>-charg binary search.
        if sy-subrc eq 0.
          <wa_mseg>-budat_mkpf = <ws_mseg>-budat_mkpf.
        endif.
      endloop.


      sort it_dados ascending by material centro deposito lote.
      sort it_mseg ascending by matnr werks lgort charg.
      sort it_ckmlhd ascending by matnr bwkey.

      loop at it_dados assigning field-symbol(<fs_dados>).
        read table it_ckmlhd into data(ws_ckmlhd) with key matnr = <fs_dados>-material
                                                           bwkey = <fs_dados>-centro binary search.
        if sy-subrc eq 0.
          read table it_ckmlcr assigning field-symbol(<ckmlcr>) with key kalnr = ws_ckmlhd-kalnr
                                                                         waers = 'BRL'.
          if sy-subrc eq 0.

             if <ckmlcr>-stprs > 0.
              <fs_dados>-preco_brl = <ckmlcr>-stprs.
            elseif <ckmlcr>-pvprs > 0.
              <fs_dados>-preco_brl = <ckmlcr>-pvprs.
            endif.

            <fs_dados>-preco_brl = <ckmlcr>-stprs.
            if <fs_dados>-qtd_est > 0 and <fs_dados>-preco_brl > 0.
              <fs_dados>-vlr_est_brl = ( <fs_dados>-preco_brl * <fs_dados>-qtd_est ).
            endif.
          endif.

          read table it_ckmlcr assigning <ckmlcr> with key kalnr = ws_ckmlhd-kalnr
                                                           waers = 'USD'.
          if sy-subrc eq 0.

            if <ckmlcr>-stprs > 0.
              <fs_dados>-preco_usd = <ckmlcr>-stprs.
            elseif <ckmlcr>-pvprs > 0.
              <fs_dados>-preco_usd = <ckmlcr>-pvprs.
            endif.


            if <fs_dados>-qtd_est > 0 and <fs_dados>-preco_usd > 0.
              <fs_dados>-vlr_est_usd = ( <fs_dados>-preco_usd * <fs_dados>-qtd_est ).
            endif.
          endif.
        endif.

        read table it_mseg assigning <ws_mseg> with key matnr = <fs_dados>-material
                                                        werks = <fs_dados>-centro
                                                        lgort = <fs_dados>-deposito
                                                        charg = <fs_dados>-lote binary search.
        if sy-subrc eq 0.
          <fs_dados>-date_ult_mov = <ws_mseg>-budat_mkpf.
        endif.
      endloop.
    endif.

    if it_dados is not initial.
      sort it_dados by material centro deposito lote.
      lwa_data_response-estoques = it_dados.
    endif.

    if lwa_data_response-estoques[] is not initial.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
*      e_msg_erro  = 'Ok'.
    else.
      e_sucesso   = abap_true.
      e_nm_code   = '400'.
      e_msg_erro  = 'Ok'.
      e_msg_erro  = 'Não localizado informações'.
    endif.
    e_msg_outbound = /ui2/cl_json=>serialize( exporting data = lwa_data_response ).

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
