class ZCL_INT_IB_CONS_DOC_FISCAL definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  data:
    TB_DOCNUM  TYPE TABLE OF J_1BDOCNUM,
    TB_NFENUM  TYPE TABLE OF J_1BNFNUM9,
    TB_NFNUM   TYPE TABLE OF J_1BNFNUMB,
    TB_SERIES  TYPE TABLE OF J_1BSERIES,
    TB_DIRECT  TYPE TABLE OF J_1BDIRECT,
    TB_PARID   TYPE TABLE OF J_1BPARID,
    TB_PARVW   TYPE TABLE OF J_1BPARVW ,
    TB_MODEL   TYPE TABLE OF J_1BMODEL ,
    TB_FORM    TYPE TABLE OF J_1BFORM ,
    TB_REFKEY  TYPE TABLE OF J_1BREFKEY,
    TB_MATNR   TYPE TABLE OF j_1bnflin-matnr,
    TB_partyp  TYPE TABLE OF j_1bnfdoc-partyp,
    tb_DOCTYP  TYPE TABLE OF j_1bDOCTYP .

  data:
    BEGIN OF zde_data_request,
      NFENUM     LIKE TB_NFENUM,
      NFNUM      LIKE TB_NFNUM ,
      SERIES     LIKE TB_SERIES,
      DIRECT     LIKE TB_DIRECT,
      PARID      LIKE TB_PARID ,
      PARVW      LIKE TB_PARVW ,
      PARTYP     LIKE TB_partyp ,
      MODEL      LIKE TB_MODEL ,
      FORM       LIKE TB_FORM ,
      CREDAT_INI TYPE DATS,
      CREDAT_FIM TYPE DATS,
      PSTDAT_INI TYPE DATS,
      PSTDAT_FIM TYPE DATS,
      DOCDAT_INI TYPE DATS,
      DOCDAT_FIM TYPE DATS,
      REFKEY     LIKE TB_REFKEY,
      MATNR      LIKE TB_MATNR,
      DOCNUM     LIKE TB_DOCNUM,
      DOCTYP     LIKE tb_doctyp,
   END OF zde_data_request .
  data:
    BEGIN OF TL_DOCS_FISCAL,
     CABECALHO TYPE zssd0001,
     ITENS     TYPE TY_J_1BNFLIN,
     END OF TL_DOCS_FISCAL .
  data:
    BEGIN OF zde_data_response,
        DOCS_FISCAL LIKE TABLE OF TL_DOCS_FISCAL,
      END OF zde_data_response .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '155' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_DOC_FISCAL IMPLEMENTATION.


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

    types: BEGIN OF ty_j_1bnfe_active,
              docnum     TYPE j_1bnfe_active-docnum,
              REGIO      TYPE j_1bnfe_active-REGIO,
              NFYEAR     TYPE j_1bnfe_active-NFYEAR,
              NFMONTH    TYPE j_1bnfe_active-NFMONTH,
              STCD1      TYPE j_1bnfe_active-STCD1,
              MODEL      TYPE j_1bnfe_active-MODEL,
              SERIE      TYPE j_1bnfe_active-SERIE,
              NFNUM9     TYPE j_1bnfe_active-NFNUM9,
              DOCNUM9    TYPE j_1bnfe_active-DOCNUM9,
              CDV        TYPE j_1bnfe_active-CDV,
           END OF ty_j_1bnfe_active.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    DATA: lit_J_1bnfe_active TYPE  TABLE OF ty_j_1bnfe_active.

    DATA:
      lra_nfenum TYPE RANGE OF j_1bnfnum9,
      lra_nfnum  TYPE RANGE OF j_1bnfnumb,
      lra_series TYPE RANGE OF j_1bseries,
      lra_direct TYPE RANGE OF j_1bdirect,
      lra_parid  TYPE RANGE OF j_1bparid,
      lra_parvw  TYPE RANGE OF j_1bparvw,
      lra_partyp TYPE RANGE OF J_1BPARTYP,
      lra_model  TYPE RANGE OF j_1bmodel,
      lra_form   TYPE RANGE OF j_1bform,
      lra_refkey TYPE RANGE OF j_1brefkey,
      lra_matnr  TYPE RANGE OF matnr,
      lra_docnum TYPE RANGE OF j_1bdocnum,
      lra_doctyp TYPE RANGE OF j_1bDOCTYP.


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


    LOOP AT lwa_data_request-docnum INTO DATA(wa_filtro0).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro0 ) TO lra_docnum.
    ENDLOOP.

        LOOP AT lwa_data_request-doctyp INTO DATA(wa_filtro01).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro01 ) TO lra_doctyp.
    ENDLOOP.

    LOOP AT lwa_data_request-nfenum INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_nfenum.
    ENDLOOP.

    LOOP AT lwa_data_request-nfnum INTO DATA(wa_filtro2).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro2 ) TO lra_nfnum.
    ENDLOOP.

    LOOP AT lwa_data_request-series INTO DATA(wa_filtro3).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro3 ) TO lra_series.
    ENDLOOP.

    LOOP AT lwa_data_request-direct INTO DATA(wa_filtro4).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro4 ) TO lra_direct.
    ENDLOOP.

    LOOP AT lwa_data_request-parid INTO DATA(wa_filtro5).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro5 ) TO lra_parid.
    ENDLOOP.

    LOOP AT lwa_data_request-parvw INTO DATA(wa_filtro6).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro6 ) TO lra_parvw.
    ENDLOOP.

    LOOP AT lwa_data_request-model INTO DATA(wa_filtro7).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro7 ) TO lra_model.
    ENDLOOP.

    LOOP AT lwa_data_request-form INTO DATA(wa_filtro8).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro8 ) TO lra_form.
    ENDLOOP.

    LOOP AT lwa_data_request-refkey INTO DATA(wa_filtro9).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro9 ) TO lra_refkey.
    ENDLOOP.

    LOOP AT lwa_data_request-matnr INTO DATA(wa_filtro10).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input     = wa_filtro10
        IMPORTING
          OUTPUT    = wa_filtro10.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro10 ) TO lra_matnr.
    ENDLOOP.

    LOOP AT lwa_data_request-partyp INTO DATA(wa_filtro11).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro11 ) TO lra_partyp.
    ENDLOOP.

    DATA v_where TYPE string.

    CONCATENATE
              'DOCNUM IN @lra_docnum AND'
              'DOCTYP IN @lra_doctyp AND'
              'NFENUM IN @lra_nfenum AND'
              'NFNUM  IN @lra_NFNUM AND'
              'SERIES IN @lra_SERIES and'
              'DIRECT IN @lra_DIRECT and'
              'PARID  IN @lra_PARID and'
              'PARVW  IN @lra_PARVW and'
              'PARTYP IN @lra_PARTYP and'
              'MODEL  IN @lra_MODEL and'
              'FORM   IN @lra_FORM ' INTO v_where SEPARATED BY space.


    IF  lwa_data_request-credat_ini IS NOT INITIAL AND lwa_data_request-credat_fim IS NOT INITIAL.
      CONCATENATE v_where 'and CREDAT BETWEEN @lwa_data_request-credat_ini AND @lwa_data_request-credat_fim' INTO v_where SEPARATED BY space.

    ELSE.

      IF  lwa_data_request-credat_ini IS NOT INITIAL.
        CONCATENATE v_where 'and CREDAT >= @lwa_data_request-credat_ini' INTO v_where SEPARATED BY space.
      ENDIF.

      IF  lwa_data_request-credat_fim IS NOT INITIAL .
        CONCATENATE v_where 'and credat <= @lwa_data_request-credat_fim' INTO v_where SEPARATED BY space.
      ENDIF.

    ENDIF.

    IF  lwa_data_request-pstdat_ini IS NOT INITIAL AND lwa_data_request-pstdat_fim IS NOT INITIAL.
      CONCATENATE v_where 'and pstdat BETWEEN @lwa_data_request-pstdat_ini and @lwa_data_request-pstdat_fim' INTO v_where SEPARATED BY space.

    ELSE.

      IF  lwa_data_request-pstdat_ini IS NOT INITIAL.
        CONCATENATE v_where 'and PSTDAT >= @lwa_data_request-PSTDAT_INI' INTO v_where SEPARATED BY space.
      ENDIF.

      IF  lwa_data_request-pstdat_fim IS NOT INITIAL.
        CONCATENATE v_where 'and PSTDAT  <= @lwa_data_request-PSTDAT_FIM ' INTO v_where SEPARATED BY space.
      ENDIF.

    ENDIF.


    IF  lwa_data_request-docdat_ini IS NOT INITIAL AND lwa_data_request-docdat_fim IS NOT INITIAL.
      CONCATENATE v_where 'and docdat BETWEEN @lwa_data_request-docdat_ini and @lwa_data_request-docdat_fim' INTO v_where SEPARATED BY space.

    ELSE.

      IF  lwa_data_request-docdat_ini IS NOT INITIAL.
        CONCATENATE v_where 'and DOCDAT >= @lwa_data_request-DOCDAT_INI' INTO v_where SEPARATED BY space.
      ENDIF.

      IF  lwa_data_request-docdat_fim IS NOT INITIAL.
        CONCATENATE v_where 'and DOCDAT <= @lwa_data_request-DOCDAT_FIM' INTO v_where SEPARATED BY space.
      ENDIF.

    ENDIF.

    IF lra_refkey IS NOT INITIAL or
       lra_matnr  is NOT INITIAL.
      CONCATENATE v_where 'and EXISTS ( select docnum from j_1bnflin where docnum eq a~docnum and refkey IN @lra_refkey  and matnr in @lra_matnr )' INTO v_where SEPARATED BY space.
    ENDIF .


    IF lwa_data_request IS NOT INITIAL.

      SELECT *
      FROM j_1bnfdoc AS a
      INTO TABLE @DATA(tl_1bnfdoc)
      WHERE (v_where).

      IF tl_1bnfdoc[] IS NOT INITIAL.

        SELECT * FROM j_1bnflin
        INTO TABLE @DATA(tl_1bnflin)
         FOR ALL ENTRIES IN @tl_1bnfdoc
          WHERE docnum EQ @tl_1bnfdoc-docnum
          AND refkey IN @lra_refkey .

        SELECT docnum REGIO  NFYEAR  NFMONTH STCD1  MODEL  SERIE  NFNUM9 DOCNUM9 CDV
           FROM j_1bnfe_active
        INTO TABLE lit_J_1bnfe_active
         FOR ALL ENTRIES IN tl_1bnfdoc
          WHERE docnum EQ tl_1bnfdoc-docnum.

      ENDIF.

      SORT lit_J_1bnfe_active by docnum.

      LOOP AT tl_1bnfdoc ASSIGNING FIELD-SYMBOL(<1bnfdoc>).
        APPEND INITIAL LINE TO lwa_data_response-docs_fiscal ASSIGNING FIELD-SYMBOL(<fs_response>).

        MOVE-CORRESPONDING <1bnfdoc> TO <fs_response>-cabecalho.

        if <fs_response>-cabecalho-nfe eq abap_true.
          READ TABLE lit_J_1bnfe_active ASSIGNING FIELD-SYMBOL(<fs_active>) WITH KEY docnum = <fs_response>-cabecalho-docnum BINARY SEARCH.
          IF sy-subrc EQ 0 .
            <fs_response>-cabecalho-chave_nfe = <fs_active>-REGIO  &&
                                                <fs_active>-NFYEAR  &&
                                                <fs_active>-NFMONTH &&
                                                <fs_active>-STCD1   &&
                                                <fs_active>-MODEL  &&
                                                <fs_active>-SERIE  &&
                                                <fs_active>-NFNUM9 &&
                                                <fs_active>-DOCNUM9 &&
                                                <fs_active>-CDV.
          ENDIF.
        endif.

        LOOP AT tl_1bnflin ASSIGNING FIELD-SYMBOL(<bnflin>) WHERE docnum EQ <1bnfdoc>-docnum.

          APPEND <bnflin> TO <fs_response>-itens.

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
