CLASS zcl_int_ib_proc_estorno_ee DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_request,
        obj_key TYPE awkey,
      END OF ty_request .
    TYPES:
      BEGIN OF ty_doc_fatura,
        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        stblg TYPE rbkp-stblg,
      END OF ty_doc_fatura .
    TYPES:
      BEGIN OF ty_doc_mat,
        mblnr     TYPE mkpf-mblnr,
        mjahr     TYPE mkpf-mjahr,
        estornado TYPE char1,
      END OF ty_doc_mat .
    TYPES:
      BEGIN OF ty_doc_aviso,
        vbeln     TYPE vbeln,
        eliminado TYPE char1,
      END OF ty_doc_aviso .
    TYPES:
      BEGIN OF ty_response,
        obj_key      TYPE awkey,
        doc_fatura   TYPE ty_doc_fatura,
        doc_material TYPE ty_doc_mat,
        doc_aviso    TYPE ty_doc_aviso,
      END OF ty_response .

    DATA: tb_obj_key TYPE TABLE OF awkey.

    DATA:
      BEGIN OF zde_data_request ,
        obj_key like  tb_obj_key,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        zmmt_eee_zgr TYPE ty_response,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '121' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_PROC_ESTORNO_EE IMPLEMENTATION.


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

    DATA(lt_obj_key) = lwa_data_request-obj_key.

    IF lt_obj_key IS INITIAL.
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

    TYPES:
      BEGIN OF ty_request,
        obj_key TYPE awkey,
      END OF ty_request.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response TYPE TABLE OF zde_data_response.

    DATA: lra_useralias TYPE RANGE OF usalias,
          lra_monat     TYPE RANGE OF monat,
          lra_gjahr     TYPE RANGE OF gjahr,
          lt_request    TYPE TABLE OF ty_request.

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

    IF lwa_data_request-obj_key IS NOT INITIAL.

      lt_request = lwa_data_request-obj_key.

      SELECT *
        FROM zmmt_eee_zgr
        INTO TABLE @DATA(lt_zgr)
        FOR ALL ENTRIES IN @lt_request
        WHERE obj_key = @lt_request-obj_key.
      IF sy-subrc IS INITIAL.

        DATA(lt_zgr_aux) = lt_zgr.
        SORT lt_zgr_aux BY re_belnr gjahr.

        DELETE ADJACENT DUPLICATES FROM lt_zgr_aux COMPARING re_belnr gjahr.

        IF lt_zgr_aux[] IS NOT INITIAL.
          SELECT belnr,
                 gjahr,
                 stblg
            FROM rbkp
            INTO TABLE @DATA(lt_rbkp)
            FOR ALL ENTRIES IN @lt_zgr_aux
            WHERE belnr = @lt_zgr_aux-re_belnr
              AND gjahr = @lt_zgr_aux-gjahr.
          IF sy-subrc IS INITIAL.
            SORT lt_rbkp BY belnr gjahr.
          ENDIF.
        ENDIF.

        lt_zgr_aux = lt_zgr.
        SORT lt_zgr_aux BY mblnr mjahr.

        DELETE ADJACENT DUPLICATES FROM lt_zgr_aux COMPARING mblnr mjahr.

        IF lt_zgr_aux[] IS NOT INITIAL.
          SELECT mblnr,
                 mjahr
            FROM mkpf
            INTO TABLE @DATA(lt_mkpf)
            FOR ALL ENTRIES IN @lt_zgr_aux
            WHERE mblnr = @lt_zgr_aux-mblnr
              AND mjahr = @lt_zgr_aux-mjahr.
          IF sy-subrc IS INITIAL.
            SORT lt_mkpf BY mblnr mjahr.

            DATA(lt_mkpf_aux) = lt_mkpf.
            SORT lt_mkpf_aux BY mblnr.
            DELETE ADJACENT DUPLICATES FROM lt_mkpf_aux COMPARING mblnr.

            SELECT smbln
              FROM mseg
              INTO TABLE @DATA(lt_mseg)
              FOR ALL ENTRIES IN @lt_mkpf_aux
              WHERE smbln = @lt_mkpf_aux-mblnr.
            IF sy-subrc IS INITIAL.
              SORT lt_mseg BY smbln.
            ENDIF.

          ENDIF.
        ENDIF.

        lt_zgr_aux = lt_zgr.
        SORT lt_zgr_aux BY vbeln_vl.

        DELETE ADJACENT DUPLICATES FROM lt_zgr_aux COMPARING vbeln_vl.

        IF lt_zgr_aux[] IS NOT INITIAL.
          SELECT vbeln
            FROM likp
            INTO TABLE @DATA(lt_likp)
            FOR ALL ENTRIES IN @lt_zgr_aux
            WHERE vbeln = @lt_zgr_aux-vbeln_vl.
          IF sy-subrc IS INITIAL.
            SORT lt_likp BY vbeln.
          ENDIF.
        ENDIF.

        lt_zgr_aux = lt_zgr.

        LOOP AT lt_zgr_aux ASSIGNING FIELD-SYMBOL(<fs_zgr>).

          APPEND INITIAL LINE TO lwa_data_response ASSIGNING FIELD-SYMBOL(<fs_response>).

          <fs_response>-obj_key = <fs_zgr>-obj_key.

          READ TABLE lt_rbkp ASSIGNING FIELD-SYMBOL(<fs_rbkp>)
          WITH KEY belnr = <fs_zgr>-re_belnr
                   gjahr = <fs_zgr>-gjahr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            <fs_response>-doc_fatura-belnr = <fs_rbkp>-belnr.
            <fs_response>-doc_fatura-gjahr = <fs_rbkp>-gjahr.
            <fs_response>-doc_fatura-stblg = <fs_rbkp>-stblg.

          ENDIF.

          READ TABLE lt_mkpf TRANSPORTING NO FIELDS
          WITH KEY mblnr = <fs_zgr>-mblnr
                   mjahr = <fs_zgr>-mjahr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            <fs_response>-doc_material-mblnr = <fs_zgr>-mblnr.
            <fs_response>-doc_material-mjahr = <fs_zgr>-mjahr.

            READ TABLE lt_mseg TRANSPORTING NO FIELDS
            WITH KEY smbln = <fs_zgr>-mblnr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_response>-doc_material-estornado = 'S'.
            ELSE.
              <fs_response>-doc_material-estornado = 'N'.
            ENDIF.

          ENDIF.

          READ TABLE lt_likp TRANSPORTING NO FIELDS
          WITH KEY vbeln = <fs_zgr>-vbeln_vl
          BINARY SEARCH.
          IF sy-subrc IS INITIAL  .

            <fs_response>-doc_aviso-vbeln = <fs_zgr>-vbeln_vl.
            <fs_response>-doc_aviso-eliminado = 'N'.

          ELSE.

            <fs_response>-doc_aviso-vbeln = <fs_zgr>-vbeln_vl.
            <fs_response>-doc_aviso-eliminado = 'S'.

          ENDIF.


        ENDLOOP.

      ENDIF.

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
