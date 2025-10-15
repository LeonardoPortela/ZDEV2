class ZCL_INT_IB_CONS_DOC_CTB definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  data:
    begin of ty_filter_header,
              bukrs         type bkpf-bukrs,
              belnr         type bkpf-belnr,
              gjahr         type bkpf-gjahr,
              awkey         type TABLE OF bkpf-awkey,
              xblnr         TYPE TABLE OF bkpf-xblnr,
              budat         TYPE bkpf-budat,
              bldat         TYPE bkpf-bldat,
              blart         TYPE TABLE OF bkpf-blart,
              sel_irf_doc   TYPE c,
         end of ty_filter_header ,

     begin of ty_filter_items,
              bukrs            TYPE bkpf-bukrs,
              belnr            TYPE bkpf-belnr,
              gjahr            TYPE bkpf-gjahr,
              blart            TYPE TABLE OF bsik-blart,
              buzei            TYPE bseg-buzei,
              budat            TYPE bsik-budat,
              bldat            TYPE bsik-bldat,
              augdt            TYPE bsak-augdt,
              augbl            TYPE bsak-augbl,
              lifnr            TYPE BSIK-lifnr,
              kunnr            TYPE BSID-kunnr,
              hkont            TYPE BSIS-hkont,
              status_partida   TYPE c, " 'A - Aberto (BSIK/BSID/BSIS) / F - Fechada (BSAK/BSAD/BSAS)'
              tipo_partida     TYPE c, " 'K - Fornecedor / D - Cliente / S - Conta Razão'
         end of ty_filter_items.

  data:
    begin of zde_data_request,
              header_filter LIKE ty_filter_header,
              items_filter  LIKE ty_filter_items,
        end of zde_data_request .
  data:
    begin of zde_data_response,
              header          TYPE bkpf_t,
              irf_doc         TYPE fiwtom_t_item,
              "itens           TYPE bseg_t,
              supplier_open   type tt_bsik,
              customer_open   type trty_bsid,
              account_open    type tt_bsis,
              supplier_closed type tt_bsak,
              customer_closed type tt_bsad,
              account_closed  type tt_bsas,
         end of zde_data_response .

  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '117' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_DOC_CTB IMPLEMENTATION.


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

    IF lwa_data_request-header_filter IS NOT INITIAL.
      IF lwa_data_request-header_filter-BUKRS IS INITIAL AND
         lwa_data_request-header_filter-BELNR IS INITIAL AND
         lwa_data_request-header_filter-GJAHR IS INITIAL AND
         lwa_data_request-header_filter-BLART IS INITIAL AND
         lwa_data_request-header_filter-AWKEY IS INITIAL AND
         lwa_data_request-header_filter-XBLNR IS INITIAL.
        r_msg_erro = 'Filtros de cabeçalho são insuficientes para uma pesquisa de performance!'.
        RETURN.
      ENDIF.
    ENDIF.

    IF lwa_data_request-items_filter IS NOT INITIAL.

      IF ( lwa_data_request-items_filter-tipo_partida is INITIAL ) AND
         ( lwa_data_request-items_filter-bukrs IS INITIAL OR lwa_data_request-items_filter-belnr IS INITIAL ).
        r_msg_erro = 'Tipo partida é um campo obrigátorio'.
        RETURN.
      ENDIF.

      IF lwa_data_request-items_filter-status_partida is INITIAL AND
         ( lwa_data_request-items_filter-bukrs IS INITIAL OR lwa_data_request-items_filter-belnr IS INITIAL ).
        r_msg_erro = 'Status partida é um campo obrigátorio'.
        RETURN.
      ENDIF.

      IF lwa_data_request-items_filter-tipo_partida IS NOT INITIAL AND
         lwa_data_request-items_filter-tipo_partida NE 'K' AND "Fornecedor
         lwa_data_request-items_filter-tipo_partida NE 'D' AND "Cliente
         lwa_data_request-items_filter-tipo_partida NE 'S'. "Razão

        r_msg_erro = 'Tipo de Partida Previstos-> K (Fornecedor ) / D ( Cliente ) / S ( Contas Razão )!'.
        RETURN.

      ENDIF.

      IF lwa_data_request-items_filter-status_partida IS NOT INITIAL AND
         lwa_data_request-items_filter-status_partida NE 'A' AND "Aberta
         lwa_data_request-items_filter-status_partida NE 'F'. "Fechada

        r_msg_erro = 'Status de Partida Previstos-> A ( Aberto ) / F ( Fechado ) !'.
        RETURN.

      ENDIF.


      CASE lwa_data_request-items_filter-status_partida.
        WHEN 'A'. "Partidas em aberto

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'K'.

            WHEN 'D'.

            WHEN 'S'.
          ENDCASE.

        WHEN 'F'. " Partidas Fechadas

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'K'.

            WHEN 'D'.

            WHEN 'S'.
          ENDCASE.

      ENDCASE.

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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

    DATA:   lra_bukrs        TYPE RANGE OF bkpf-bukrs,
            lra_belnr        TYPE RANGE OF bkpf-belnr,
            lra_gjahr        TYPE RANGE OF bkpf-gjahr,
            lra_blart        TYPE RANGE OF bkpf-blart,
            lra_awkey        TYPE RANGE OF bkpf-awkey,
            lra_xblnr        TYPE RANGE OF bkpf-xblnr,
            lra_budat        TYPE RANGE OF bkpf-budat,
            lra_bldat        TYPE RANGE OF bkpf-bldat,
            lra_bukrs_itm    TYPE RANGE OF bkpf-bukrs,
            lra_belnr_itm    TYPE RANGE OF bkpf-belnr,
            lra_gjahr_itm    TYPE RANGE OF bkpf-gjahr,
            lra_blart_itm    TYPE RANGE OF bsik-blart,
            lra_buzei_itm    TYPE RANGE OF bseg-buzei,
            lra_augbl_itm    TYPE RANGE OF bsak-belnr,
            lra_augdt_itm    TYPE RANGE OF bsak-augdt,
            lra_lifnr_itm    TYPE RANGE OF BSIK-lifnr,
            lra_kunnr_itm    TYPE RANGE OF BSID-kunnr,
            lra_hkont_itm    TYPE RANGE OF BSIS-hkont,
            lra_budat_itm    TYPE RANGE OF BSIS-budat,
            lra_bldat_itm    TYPE RANGE OF BSIS-bldat.

    r_if_integracao_inject = me.

    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound, lwa_data_response.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lwa_data_request ).
    ENDIF.

    ME->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound =  i_msg_inbound IMPORTING e_status_code  =  data(_status_code)  RECEIVING r_msg_erro = e_msg_erro ).

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

    "Campos Header
    IF lwa_data_request-header_filter-bukrs IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-header_filter-bukrs ) TO lra_bukrs.
    ENDIF.

    IF lwa_data_request-header_filter-belnr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-header_filter-belnr ) TO lra_belnr.
    ENDIF.

    IF lwa_data_request-header_filter-gjahr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-header_filter-gjahr ) TO lra_gjahr.
    ENDIF.

    LOOP at lwa_data_request-header_filter-blart INTO DATA(lwa_blart).
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_blart ) TO lra_blart.
    ENDLOOP.

    LOOP AT lwa_data_request-header_filter-awkey INTO DATA(lwa_awkey).
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_awkey ) TO lra_awkey.
    ENDLOOP.

    LOOP AT lwa_data_request-header_filter-xblnr INTO DATA(lwa_xblnr).
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_xblnr ) TO lra_xblnr.
    ENDLOOP.

    IF lwa_data_request-header_filter-budat IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-header_filter-budat ) TO lra_budat.
    ENDIF.

    IF lwa_data_request-header_filter-bldat IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-header_filter-bldat ) TO lra_bldat.
    ENDIF.


    "Campos Itens
    IF lwa_data_request-items_filter-bukrs IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-bukrs ) TO lra_bukrs_itm.
    ENDIF.

    IF lwa_data_request-items_filter-belnr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-belnr ) TO lra_belnr_itm.
    ENDIF.

    IF lwa_data_request-items_filter-gjahr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-gjahr ) TO lra_gjahr_itm.
    ENDIF.

    LOOP AT lwa_data_request-items_filter-blart INTO lwa_blart.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_blart ) TO lra_blart_itm.
    ENDLOOP.

    IF lwa_data_request-items_filter-buzei IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-buzei ) TO lra_buzei_itm.
    ENDIF.

    IF lwa_data_request-items_filter-augbl IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-augbl ) TO lra_augbl_itm.
    ENDIF.

    IF lwa_data_request-items_filter-augdt IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-augdt ) TO lra_augdt_itm.
    ENDIF.

    IF lwa_data_request-items_filter-lifnr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-lifnr ) TO lra_lifnr_itm.
    ENDIF.

    IF lwa_data_request-items_filter-kunnr IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-kunnr ) TO lra_kunnr_itm.
    ENDIF.

    IF lwa_data_request-items_filter-hkont IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-hkont ) TO lra_hkont_itm.
    ENDIF.

    IF lwa_data_request-items_filter-budat IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-budat ) TO lra_budat_itm.
    ENDIF.

    IF lwa_data_request-items_filter-bldat IS NOT INITIAL.
      APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = lwa_data_request-items_filter-bldat ) TO lra_bldat_itm.
    ENDIF.

    if lwa_data_request-header_filter is NOT INITIAL.
      SELECT *
        FROM BKPF INTO TABLE lwa_data_response-header
       WHERE bukrs in lra_bukrs
         AND belnr in lra_belnr
         and gjahr in lra_gjahr
         and blart in lra_blart
         and awkey in lra_awkey
         and xblnr in lra_xblnr
         and budat in lra_budat
         and bldat in lra_bldat.

       IF ( lwa_data_response-header[] is NOT INITIAL ) AND
          ( lwa_data_request-header_filter-sel_irf_doc EQ ABAP_TRUE ).

         SELECT *
           FROM with_item INTO TABLE lwa_data_response-irf_doc
            FOR ALL ENTRIES IN lwa_data_response-header
          WHERE bukrs EQ lwa_data_response-header-bukrs
            AND belnr EQ lwa_data_response-header-belnr
            and gjahr EQ lwa_data_response-header-gjahr.

       ENDIF.

    endif.

    IF lwa_data_request-items_filter is NOT INITIAL.

*      SELECT *
*        FROM BSEG INTO TABLE lwa_data_response-itens
*       WHERE bukrs in lra_bukrs_itm
*         AND belnr in lra_belnr_itm
*         and gjahr in lra_gjahr_itm
*         and buzei in lra_buzei_itm
*         and lifnr in lra_lifnr_itm.

      "--------------------------------------------------------------------------*
      " Partidas em aberto
      "--------------------------------------------------------------------------*

      CASE lwa_data_request-items_filter-status_partida.
        WHEN 'A' OR SPACE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'K' OR SPACE.

              SELECT *
               FROM BSIK INTO TABLE lwa_data_response-supplier_open
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and lifnr in lra_lifnr_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'D' OR SPACE.

              SELECT *
               FROM BSID INTO TABLE lwa_data_response-customer_open
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and kunnr in lra_kunnr_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'S' OR SPACE.

              SELECT *
               FROM BSIS INTO TABLE lwa_data_response-account_open
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and hkont in lra_hkont_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

      ENDCASE.

      "--------------------------------------------------------------------------*
      " Partidas Fechadas
      "--------------------------------------------------------------------------*
      CASE lwa_data_request-items_filter-status_partida.
        WHEN 'F' OR SPACE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'K' OR SPACE.

              SELECT *
               FROM BSAK INTO TABLE lwa_data_response-supplier_closed
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and lifnr in lra_lifnr_itm
                and augbl in lra_augbl_itm
                and augdt in lra_augdt_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'D' OR SPACE.

              SELECT *
               FROM BSAD INTO TABLE lwa_data_response-customer_closed
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and kunnr in lra_kunnr_itm
                and augbl in lra_augbl_itm
                and augdt in lra_augdt_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

          CASE lwa_data_request-items_filter-tipo_partida.
            WHEN 'S' OR SPACE.

              SELECT *
               FROM BSAS INTO TABLE lwa_data_response-account_closed
              WHERE bukrs in lra_bukrs_itm
                AND belnr in lra_belnr_itm
                and gjahr in lra_gjahr_itm
                and blart in lra_blart_itm
                and buzei in lra_buzei_itm
                and hkont in lra_hkont_itm
                and augbl in lra_augbl_itm
                and augdt in lra_augdt_itm
                and budat in lra_budat_itm
                and bldat in lra_bldat_itm.

          ENDCASE.

      ENDCASE.


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
