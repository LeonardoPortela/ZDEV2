CLASS zcl_int_ib_cons_proc_cont_corp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_itens,
        mandt        TYPE mandt,
        seq_lcto     TYPE zseq_lcto,
        seqitem      TYPE num06,
        operacao     TYPE zfiwed001,
        seqsub       TYPE zseqsub,
        lote         TYPE zlote_num,
        bukrs        TYPE bukrs,
        bschl        TYPE bschl,
        hkont        TYPE hkont,
        gsber        TYPE gsber,
        gsber_nf     TYPE gsber,
        kostl        TYPE kostl,
        vbeln        TYPE aufnr,
        budat        TYPE budat,
        bldat        TYPE bldat,
        cfop         TYPE j_1bcfop,
        menge        TYPE j_1bnetqty,
        meins        TYPE meins,
        netpr        TYPE j_1bnetpri,
        netwr        TYPE j_1bnetval,
        vlr_desconto TYPE j_1bnetval,
        itmtyp       TYPE j_1bitmtyp,
        ref_doc_no   TYPE xblnr,
        matnr        TYPE matnr,
        asnum        TYPE asnum,
        cprod        TYPE char18,
        sgtxt        TYPE sgtxt,
        part_forn    TYPE char01,
        docnum       TYPE j_1bdocnum,
        nfenum       TYPE j_1bnfnum9,
        series       TYPE j_1bseries,
        inco1        TYPE inco1,
        inco2        TYPE inco2,
        doc_lcto     TYPE numc10,
        belnr        TYPE belnr_d,
        dt_lcto_ctb  TYPE dats,
        cod_barras   TYPE zcod_barras,
        esrnr        TYPE esrnr,
        esrre        TYPE esrre,
        agrp_nf      TYPE numc3,
        estorno_nf   TYPE char01,
        objkey       TYPE awkey,
        bnfpo        TYPE bnfpo,
        vlr_liq_ret  TYPE j_1bnetval,
        sem_nf       TYPE char01,
        desconto     TYPE char01,
        loekz        TYPE loekz,
        id_lms       TYPE zid_lms,
        augbl        TYPE bsak-augbl,
        augdt        TYPE bsak-augdt,
      END OF ty_itens.

      data: tl_itens TYPE TABLE OF ty_itens,
            tl_zmmt0024 TYPE  TABLE OF zmmt0024.

      types: BEGIN OF ty_doc_gerado,
        itens     like tl_itens,
        cabecalho TYPE  zglt080, "TABLE OF
      END OF ty_doc_gerado,

      BEGIN OF ty_registros_proc,
        dados_proc like  tl_zmmt0024, " TABLE OF
        doc_gerado TYPE ty_doc_gerado,
        logs       TYPE zglt087_tt, "TABLE OF
      END OF ty_registros_proc.

    DATA:
      tb_objkey TYPE TABLE OF awkey,

      BEGIN OF zde_data_request,
        objkey LIKE tb_objkey,
      END OF zde_data_request,

      BEGIN OF zde_data_response,
        registros_proc TYPE TABLE OF ty_registros_proc,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '147' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PROC_CONT_CORP IMPLEMENTATION.


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
    TYPES:
      BEGIN OF ty_itens,
        mandt        TYPE mandt,
        seq_lcto     TYPE zseq_lcto,
        seqitem      TYPE num06,
        operacao     TYPE zfiwed001,
        seqsub       TYPE zseqsub,
        lote         TYPE zlote_num,
        bukrs        TYPE bukrs,
        bschl        TYPE bschl,
        hkont        TYPE hkont,
        gsber        TYPE gsber,
        gsber_nf     TYPE gsber,
        kostl        TYPE kostl,
        vbeln        TYPE aufnr,
        budat        TYPE budat,
        bldat        TYPE bldat,
        cfop         TYPE j_1bcfop,
        menge        TYPE j_1bnetqty,
        meins        TYPE meins,
        netpr        TYPE j_1bnetpri,
        netwr        TYPE j_1bnetval,
        vlr_desconto TYPE j_1bnetval,
        itmtyp       TYPE j_1bitmtyp,
        ref_doc_no   TYPE xblnr,
        matnr        TYPE matnr,
        asnum        TYPE asnum,
        cprod        TYPE char18,
        sgtxt        TYPE sgtxt,
        part_forn    TYPE char01,
        docnum       TYPE j_1bdocnum,
        nfenum       TYPE j_1bnfnum9,
        series       TYPE j_1bseries,
        inco1        TYPE inco1,
        inco2        TYPE inco2,
        doc_lcto     TYPE numc10,
        belnr        TYPE belnr_d,
        dt_lcto_ctb  TYPE dats,
        cod_barras   TYPE zcod_barras,
        esrnr        TYPE esrnr,
        esrre        TYPE esrre,
        agrp_nf      TYPE numc3,
        estorno_nf   TYPE char01,
        objkey       TYPE awkey,
        bnfpo        TYPE bnfpo,
        vlr_liq_ret  TYPE j_1bnetval,
        sem_nf       TYPE char01,
        desconto     TYPE char01,
        loekz        TYPE loekz,
        id_lms       TYPE zid_lms,
        augbl        TYPE bsak-augbl,
        augdt        TYPE bsak-augdt,
      END OF ty_itens.

    DATA:
      tl_itens   TYPE TABLE OF ty_itens,
      lra_objkey TYPE RANGE OF awkey.

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

    LOOP AT lwa_data_request-objkey INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_objkey.
    ENDLOOP.

    DATA: tl_itens_aux    TYPE TABLE OF ty_itens,
          tl_zmmt0024_aux TYPE TABLE OF zmmt0024.
    IF lwa_data_request IS NOT INITIAL.
      "select DADOS_PROC
      SELECT * FROM zmmt0024
        INTO TABLE @DATA(tl_zmmt0024)
        WHERE objkey IN @lra_objkey.


      IF tl_zmmt0024[] IS NOT INITIAL.

        tl_zmmt0024_aux = tl_zmmt0024.
        "select itens
        SELECT * FROM zglt081  "#EC CI_FLDEXT_OK[2215424]
          INTO CORRESPONDING FIELDS OF TABLE tl_itens
          FOR ALL ENTRIES IN tl_zmmt0024
          WHERE objkey EQ tl_zmmt0024-objkey.

        IF tl_itens[] IS NOT INITIAL.
          tl_itens_aux = tl_itens.

          IF tl_itens_aux[] IS NOT INITIAL.

            SORT tl_itens_aux BY objkey.
            DELETE ADJACENT DUPLICATES FROM tl_itens_aux COMPARING objkey.


            SELECT cp~bukrs, cp~belnr, cp~augbl , cp~augdt FROM bsak AS cp
            INTO TABLE @DATA(tl_bsak)
            FOR ALL ENTRIES IN @tl_itens
            WHERE cp~bukrs EQ @tl_itens-bukrs
            AND cp~belnr EQ @tl_itens-belnr
            AND cp~belnr NE cp~augbl.


            "select CABECALHO
            SELECT * FROM zglt080
            INTO TABLE @DATA(tl_zglt080)
            FOR ALL ENTRIES IN @tl_itens_aux
            WHERE seq_lcto EQ @tl_itens_aux-seq_lcto.
          ENDIF.

        ENDIF.

        "select LOG
        SELECT * FROM  zglt087
        INTO TABLE @DATA(tl_zglt087)
          FOR ALL ENTRIES IN @tl_zmmt0024
          WHERE objkey EQ @tl_zmmt0024-objkey.

      ENDIF.
      SORT: tl_itens BY objkey,
            tl_zmmt0024_aux BY objkey.
      DATA: v_objkey TYPE awkey.

      DELETE ADJACENT DUPLICATES FROM tl_zmmt0024_aux COMPARING objkey.

      LOOP AT tl_zmmt0024_aux ASSIGNING FIELD-SYMBOL(<fs_zmmt0024_aux>).

        APPEND INITIAL LINE TO lwa_data_response-registros_proc ASSIGNING FIELD-SYMBOL(<fs_response>).

        LOOP AT tl_zmmt0024 ASSIGNING FIELD-SYMBOL(<fs_zmmt0024>) WHERE objkey EQ <fs_zmmt0024_aux>-objkey.
          APPEND   <fs_zmmt0024> TO <fs_response>-dados_proc.
        ENDLOOP.

        LOOP AT tl_itens  ASSIGNING FIELD-SYMBOL(<fs_itens>) WHERE objkey EQ <fs_zmmt0024_aux>-objkey.
          IF v_objkey NE <fs_itens>-objkey.

            v_objkey = <fs_itens>-objkey.

            READ TABLE tl_zglt080 INTO DATA(w_zglt080)  WITH KEY seq_lcto =  <fs_itens>-seq_lcto.
            IF sy-subrc IS INITIAL.

              MOVE-CORRESPONDING   w_zglt080 TO <fs_response>-doc_gerado-cabecalho.

            ENDIF.
          ENDIF.



          READ TABLE tl_bsak INTO DATA(w_bsak)  WITH KEY bukrs = <fs_itens>-bukrs
                                                         belnr = <fs_itens>-belnr.
          IF sy-subrc IS INITIAL.
             MOVE-CORRESPONDING   w_bsak to <fs_itens>.
          ENDIF.

          APPEND  <fs_itens> TO <fs_response>-doc_gerado-itens.

        ENDLOOP.

        LOOP AT tl_zglt087 ASSIGNING FIELD-SYMBOL(<fs_zglt087>) WHERE objkey EQ <fs_zmmt0024_aux>-objkey.

          APPEND  <fs_zglt087> TO <fs_response>-logs.

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
