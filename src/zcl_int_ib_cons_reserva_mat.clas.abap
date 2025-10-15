CLASS zcl_int_ib_cons_reserva_mat DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:

      BEGIN OF ty_table,
        mblnr     TYPE mseg-mblnr,
        matnr     TYPE mseg-matnr,
        maktx     TYPE makt-maktx,
        menge     TYPE mseg-menge,
        rsnum     TYPE mseg-rsnum,
        uname     TYPE zmmt0009-uname,
        name_text TYPE adrp-name_text,
        pernr     TYPE zmmt0087-pernr,
        cname     TYPE pa0002-cname,

      END OF ty_table.

    DATA:
      tb_mblnr  TYPE TABLE OF mblnr,
      it_table TYPE STANDARD TABLE OF ty_table.
    DATA:
      BEGIN OF z_gen_table,
        mblnr                   TYPE mseg-mblnr,
        matnr                   TYPE mseg-matnr,
        maktx                   TYPE makt-maktx,
        menge                   TYPE mseg-menge,
        rsnum                   TYPE mseg-rsnum,
        uname                   TYPE zmmt0009-uname,
        nome_aprovador          TYPE adrp-name_text,
        matricula_resp_retirada TYPE zmmt0087-pernr,
        nome_resp_retirada      TYPE pa0002-cname,
      END OF z_gen_table.
    DATA:
      BEGIN OF zde_data_request,
        mblnr LIKE tb_mblnr,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        reservas LIKE TABLE OF z_gen_table,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '163' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_RESERVA_MAT IMPLEMENTATION.


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


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    TYPES:
      BEGIN OF ty_adrp,
        bname      TYPE usr21-bname,
        persnumber TYPE adrp-persnumber,
        name_text  TYPE adrp-name_text,
      END OF ty_adrp,

      BEGIN OF ty_material,
        material TYPE matnr,
      END OF ty_material.

    DATA: it_adrp   TYPE STANDARD TABLE OF ty_adrp,
          lra_mblnr TYPE RANGE OF mblnr.

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

    LOOP AT lwa_data_request-mblnr INTO DATA(wa_mblnr).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_mblnr ) TO lra_mblnr.
    ENDLOOP.

    SELECT mv~mblnr,
           mv~matnr,
           ma~maktx,
           mv~menge,
           mv~rsnum,
           ap~uname,
           re~pernr
      INTO TABLE @DATA(lit_table)
        FROM mseg AS mv
       INNER JOIN  makt AS ma
       ON ma~matnr EQ mv~matnr
      AND ma~spras EQ 'P'
      LEFT OUTER JOIN zmmt0009 AS ap
      ON mv~rsnum EQ ap~rsnum
     AND mv~rspos EQ ap~rspos
      LEFT OUTER JOIN zmmt0087 AS re
      ON mv~rsnum EQ re~rsnum
     AND mv~rspos EQ re~rspos
     AND mv~mblnr EQ re~mblnr
*     INNER JOIN usr21 AS u
*      ON u~bname EQ ap~uname
*      INNER JOIN adrp AS a
*       ON a~persnumber EQ u~persnumber
*      INNER JOIN pa0002 AS p2
*       ON p2~pernr EQ re~pernr
      WHERE mv~mblnr IN @lra_mblnr.


    IF lit_table[] IS NOT INITIAL.
      SELECT u~bname a~persnumber a~name_text
        INTO CORRESPONDING FIELDS OF TABLE it_adrp
        FROM usr21 AS u
        LEFT OUTER JOIN adrp AS a
        ON u~persnumber EQ a~persnumber
        FOR ALL ENTRIES IN lit_table
        WHERE u~bname EQ lit_table-uname.

      SELECT pernr , cname
        FROM pa0002
        INTO TABLE @DATA(it_pa002)
        FOR ALL ENTRIES IN @lit_table
             WHERE pernr = @lit_table-pernr.
    ENDIF.

    LOOP AT lit_table INTO DATA(ls_table).
      APPEND INITIAL LINE TO lwa_data_response-reservas ASSIGNING FIELD-SYMBOL(<fs_response>).

      <fs_response>-mblnr                     = ls_table-mblnr    .
      <fs_response>-matnr                     = ls_table-matnr    .
      <fs_response>-maktx                     = ls_table-maktx    .
      <fs_response>-menge                     = ls_table-menge    .
      <fs_response>-rsnum                     = ls_table-rsnum    .
      <fs_response>-uname                     = ls_table-uname    .


      READ TABLE it_adrp INTO DATA(ls_adrp) WITH KEY bname = ls_table-uname.
      IF sy-subrc IS INITIAL.
        <fs_response>-nome_aprovador            = ls_adrp-name_text.
      ELSE.
        <fs_response>-nome_aprovador            = 'N/A'.
      ENDIF.

      IF ls_table-pernr IS NOT INITIAL.
        <fs_response>-matricula_resp_retirada   = ls_table-pernr    .

      ELSE.
        <fs_response>-matricula_resp_retirada    = 'N/A'.
      ENDIF.

      READ TABLE it_pa002 INTO DATA(ls_pa002) WITH KEY pernr = ls_table-pernr.
      IF sy-subrc IS INITIAL.
        <fs_response>-nome_resp_retirada       = ls_pa002-cname    .
      ELSE.
        <fs_response>-nome_resp_retirada     = 'N/A'.
      ENDIF.


    ENDLOOP.


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
