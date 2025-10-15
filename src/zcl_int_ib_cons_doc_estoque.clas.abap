CLASS zcl_int_ib_cons_doc_estoque DEFINITION
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
      tb_mblnr TYPE TABLE OF mblnr,
      tb_mjahr TYPE TABLE OF mjahr,
      tb_budat TYPE TABLE OF budat,
      tb_bldat TYPE TABLE OF bldat,
      tb_xblnr TYPE TABLE OF xblnr,
      tb_smbln TYPE TABLE OF mblnr,
      tb_smblp TYPE TABLE OF mblpo.

    DATA:
      BEGIN OF z_material,
        cabecalho TYPE mkpf,
        itens     TYPE  ty_t_mseg,
      END OF z_material .
    DATA:
      BEGIN OF zde_data_request,
        mblnr                   LIKE tb_mblnr,
        mjahr                   LIKE tb_mjahr,
        budat_ini               TYPE erdat,
        budat_fim               TYPE erdat,
        bldat_ini               TYPE erdat,
        bldat_fim               TYPE erdat,
        xblnr                   LIKE tb_xblnr,
        smbln                   LIKE tb_smbln,
        smblp                   like tb_smblp,
        estornado               TYPE flag,
        interface_graos         TYPE flag,
        nao_estornado_interface TYPE flag,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        docs_material LIKE TABLE OF z_material,

      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '136' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_DOC_ESTOQUE IMPLEMENTATION.


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

    DATA:
      lra_mblnr TYPE RANGE OF mblnr,
      lra_mjahr TYPE RANGE OF mjahr,
      lra_budat TYPE RANGE OF budat,
      lra_bldat TYPE RANGE OF bldat,
      lra_xblnr TYPE RANGE OF xblnr,
      lra_smbln TYPE RANGE OF mblnr,
      lra_smblp TYPE RANGE OF mblpo,
      lra_mat   TYPE RANGE OF mblnr.

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

    LOOP AT lwa_data_request-mblnr INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_mblnr.
    ENDLOOP.

    LOOP AT lwa_data_request-mjahr INTO DATA(wa_filtro2).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro2 ) TO lra_mjahr.
    ENDLOOP.

    IF lwa_data_request-budat_ini IS NOT INITIAL OR lwa_data_request-budat_fim IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'BT' low = lwa_data_request-budat_ini high = lwa_data_request-budat_fim ) TO lra_budat.
    ENDIF.

    IF lwa_data_request-bldat_ini IS NOT INITIAL OR lwa_data_request-bldat_fim IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'BT' low = lwa_data_request-bldat_ini high = lwa_data_request-bldat_fim ) TO lra_bldat.
    ENDIF.

    LOOP AT lwa_data_request-xblnr INTO DATA(wa_filtro5).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro5 ) TO lra_xblnr.
    ENDLOOP.

    LOOP AT lwa_data_request-smbln INTO DATA(wa_filtro6).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro6 ) TO lra_smbln.
    ENDLOOP.

    LOOP AT lwa_data_request-smblp INTO DATA(wa_filtro7).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro7 ) TO lra_smblp.
    ENDLOOP.

    IF lwa_data_request IS NOT INITIAL.

      DATA: lv_where TYPE string.

      CONCATENATE 'mblnr   IN @lra_mblnr AND mjahr   IN @lra_mjahr AND '
                  ' budat IN @lra_budat AND bldat   IN @lra_bldat AND xblnr   IN @lra_xblnr '
                   INTO lv_where.

      IF lwa_data_request-estornado IS NOT INITIAL.
        CONCATENATE lv_where ' and EXISTS ( select mblnr from mseg as b where b~smbln = a~mblnr )' INTO lv_where.
      ENDIF.

      IF lwa_data_request-smbln IS NOT INITIAL.
        CONCATENATE lv_where ' and EXISTS ( select mblnr from mseg as b where b~mblnr = a~mblnr and b~smbln in @lra_smbln and b~smblp in @lra_smblp )' INTO lv_where.
      ENDIF.

      IF lwa_data_request-interface_graos IS NOT INITIAL.
        CONCATENATE lv_where ' and EXISTS (  select ft_belnr from zmmt_ee_zgr_docs as b where b~MM_MBLNR = a~mblnr )' INTO lv_where.
      ENDIF.

      IF lwa_data_request-nao_estornado_interface IS NOT INITIAL.
        CONCATENATE lv_where ' and NOT EXISTS (  select MBLNR from zmmt_eee_zgr as b where b~MBLNR = a~mblnr )' INTO lv_where.
      ENDIF.

      SELECT *
        FROM mkpf AS a
        INTO TABLE @DATA(tl_mkpf)
        WHERE (lv_where).

      IF tl_mkpf[] IS NOT INITIAL.
        SELECT *
          FROM mseg
          INTO TABLE @DATA(tl_mseg)
          FOR ALL ENTRIES IN @tl_mkpf
          WHERE mblnr   = @tl_mkpf-mblnr AND
                mjahr   = @tl_mkpf-mjahr.
      ENDIF.

      LOOP AT tl_mkpf ASSIGNING FIELD-SYMBOL(<fs_mkpf>).
        APPEND INITIAL LINE TO lwa_data_response-docs_material ASSIGNING FIELD-SYMBOL(<fs_response>).

        <fs_response>-cabecalho = <fs_mkpf>.

        LOOP AT tl_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>) WHERE mblnr   EQ <fs_response>-cabecalho-mblnr AND
                                                                mjahr   EQ <fs_response>-cabecalho-mjahr.

          APPEND <fs_mseg> TO <fs_response>-itens.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    e_sucesso   = abap_true.
    e_nm_code   = '200'.
    e_msg_erro  = 'Ok'.
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
