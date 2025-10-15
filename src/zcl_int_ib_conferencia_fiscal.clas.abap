class ZCL_INT_IB_CONFERENCIA_FISCAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_makt,
        maktg TYPE makt-maktg,
        maktx TYPE makt-maktx,
      END OF ty_makt .

  data:
    tb_bankl  TYPE TABLE OF bankl .
  data:
    tb_swift  TYPE TABLE OF swift .
  data:
    tb_bancos TYPE TABLE OF bnka .
  data:
    BEGIN OF z_bancos,
        bancos TYPE bnka,
      END OF z_bancos .
  data:
    BEGIN OF zde_data_request,
        bankl      LIKE tb_bankl,
        bankl_like TYPE bankl,
        swift      LIKE tb_swift,
      END OF zde_data_request .
  data:
    BEGIN OF zde_data_response,
        bancos LIKE tb_bancos,
      END OF zde_data_response .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '175' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONFERENCIA_FISCAL IMPLEMENTATION.


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
    DATA: lwa_data_request TYPE zfie0008.

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

    DATA(it_parceiro) = lwa_data_request-parceiro-codigo[].
    IF it_parceiro IS INITIAL.
      r_msg_erro = 'Preencha o codigo do parceiro!'.
      RETURN.
    ENDIF.


    IF lwa_data_request-data_emissao-inicio IS INITIAL.
      r_msg_erro = 'Preencha a data inicio de emissão!'.
      RETURN.
    ENDIF.

    IF lwa_data_request-data_emissao-fim IS INITIAL.
      r_msg_erro = 'Preencha a data fim de emissão!'.
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

    DATA: lwa_data_request  TYPE zfie0008,
          it_select         TYPE TABLE OF rsselect,
          it_saida          TYPE TABLE OF zdados_saida,
          dt_inicio         TYPE sy-datum,
          dt_fim            TYPE sy-datum,
          it_kna1           TYPE TABLE OF kna1,
          it_kna1_aux       TYPE TABLE OF kna1,
          rg_kunnr          TYPE RANGE OF kunnr,
          rg_stcd1          TYPE RANGE OF stcd1,
          rg_stcd3          TYPE RANGE OF stcd3,
          vg_mat            TYPE matnr,
          lwa_data_response LIKE zde_data_response.

    DATA:
      lra_filtro1 TYPE RANGE OF bankl.

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

    DATA(it_parceiro) = lwa_data_request-parceiro-codigo[].
    rg_kunnr = VALUE #( FOR l IN it_parceiro ( sign = 'I' option = 'EQ' low = |{ l-id ALPHA = IN }| ) ).

    IF it_parceiro IS NOT INITIAL.
      FREE: it_kna1 .
      SELECT * FROM kna1 INTO TABLE it_kna1
        WHERE kunnr IN rg_kunnr.

      IF sy-subrc NE 0.
        e_msg_erro = 'Dados do parceiro não encontrado'.
        e_sucesso      = abap_true.
        e_nm_code      = _status_code.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
        RETURN.
      ENDIF.
    ENDIF.

**======================================================================================================================================
    "Se Marcado o campo, fazer a seleção na tabela KNA1 com o código CNPJ do parteciro utilizando apenas as 8 casas decimais,
    "para localizar todos que fazer parte do mesmo CNPJ.

    IF lwa_data_request-parceiro-busca_by_cnpj_raiz EQ abap_true.
      rg_stcd1 = VALUE #( FOR t IN it_kna1 WHERE ( stcd1 NE space ) ( sign = 'I' option = 'CP' low = |{ t-stcd1(8) }*| ) ).
      IF rg_stcd1 IS NOT INITIAL.
        FREE: it_kna1_aux .
        SELECT * FROM kna1 INTO TABLE it_kna1_aux
          WHERE stcd1 IN rg_stcd1.
      ENDIF.
      SORT: it_kna1_aux BY stcd1, it_kna1 BY stcd1.
      LOOP AT it_kna1_aux ASSIGNING FIELD-SYMBOL(<ws_kan1>).
        READ TABLE it_kna1 INTO DATA(ws_kn) WITH KEY stcd1+0(8) = <ws_kan1>-stcd1+0(8).
        IF sy-subrc NE 0.
          <ws_kan1>-stcd1 = ''.
        ENDIF.
      ENDLOOP.

      SORT: it_kna1_aux BY stcd1.
      DELETE it_kna1_aux WHERE stcd1 EQ ''.

      "Excluir da tabela os parceiros que tenham a escrição diferente do filtro no body.
      IF lwa_data_request-inscricao_estadual IS NOT INITIAL.
        rg_stcd3 = VALUE #( FOR y IN lwa_data_request-inscricao_estadual ( sign = 'I' option = 'EQ' low = y-codigo ) ).
        IF it_kna1_aux IS NOT INITIAL.
          SORT: it_kna1_aux BY stcd3, it_kna1 BY stcd3.
          DELETE it_kna1_aux WHERE stcd3 NOT IN rg_stcd3.
          DELETE it_kna1 WHERE stcd3 NOT IN rg_stcd3.
        ENDIF.
      ENDIF.
    ENDIF.

    IF it_kna1 IS INITIAL AND it_kna1_aux IS INITIAL.
      e_msg_erro = 'Dados do parceiro não encontrado'.
      e_sucesso      = abap_true.
      e_nm_code      = _status_code.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.


    LOOP AT it_kna1 INTO DATA(ws_kan1).
      APPEND VALUE #( fieldnm = 'parid' sign = 'I' option = 'EQ' low = ws_kan1-kunnr ) TO it_select[].
    ENDLOOP.

    CLEAR: ws_kan1.
    LOOP AT it_kna1_aux INTO ws_kan1.
      APPEND VALUE #( fieldnm = 'parid' sign = 'I' option = 'EQ' low = ws_kan1-kunnr ) TO it_select[].
    ENDLOOP.

    DATA(it_material) = lwa_data_request-material[].
    LOOP AT it_material INTO DATA(ws_material).
      CLEAR: vg_mat.
      IF ws_material-codigo IS NOT INITIAL.
        vg_mat = ws_material-codigo.
        vg_mat = |{ vg_mat ALPHA = IN }|.
        APPEND VALUE #( fieldnm = 'mat' sign = 'I' option = 'EQ' low = |{ ws_material-codigo ALPHA = IN }| ) TO it_select[].
      ENDIF.
    ENDLOOP.
    CLEAR: ws_material.

    "Data
    dt_inicio = |{ lwa_data_request-data_emissao-inicio(4) }{ lwa_data_request-data_emissao-inicio+5(2) }{ lwa_data_request-data_emissao-inicio+8(2) }|.
    dt_fim    = |{ lwa_data_request-data_emissao-fim(4) }{ lwa_data_request-data_emissao-fim+5(2) }{ lwa_data_request-data_emissao-fim+8(2) }|.
    IF dt_inicio IS NOT INITIAL.
      APPEND VALUE #( fieldnm = 'pstdat' sign = 'I' option = 'BT' low = dt_inicio high = dt_fim ) TO it_select[].
    ENDIF.

*    APPEND VALUE #( fieldnm = 'bukrs'  sign = 'I' option = 'EQ' low = '*' ) TO it_select[].
*    APPEND VALUE #( fieldnm = 'branch' sign = 'I' option = 'EQ' low = '*' ) TO it_select[].
    APPEND VALUE #( fieldnm = 'direct' sign = 'I' option = 'EQ' low = '2' ) TO it_select[].
    APPEND VALUE #( fieldnm = 'tmptod' sign = 'I' option = 'EQ' low = 'X' ) TO it_select[].
    APPEND VALUE #( fieldnm = 'model'  sign = 'I' option = 'NE' low = '58') TO it_select[].


    TRY .


        "Seleção dados.
        CALL FUNCTION 'ZF_BW_FI_05'
          EXPORTING
            p_ativas                     = abap_true
*           p_nativa                     =
            p_autor                      = abap_true
*           p_rejeit                     =
*           p_recus                      =
*           p_cancel                     =
*           p_agres                      =
*           p_nenv                       =
*           p_gravr                      =
*           p_frepro                     =
          TABLES
            i_t_select                   = it_select
*           i_t_fields                   =
            e_t_data                     = it_saida
          EXCEPTIONS
            no_more_data                 = 1
            error_passed_to_mess_handler = 2
            OTHERS                       = 3.

        IF sy-subrc IS INITIAL.
          e_sucesso   = abap_true.
          e_nm_code   = '200'.
          e_msg_erro  = 'Ok'.
          e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = it_saida ).
        ENDIF.

      CATCH zcx_error.
        IF sy-subrc IS INITIAL.
          e_sucesso   = abap_true.
          e_nm_code   = '200'.
          e_msg_erro  = 'Ok'.
          e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data = it_saida ).
        ENDIF.
    ENDTRY.


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
