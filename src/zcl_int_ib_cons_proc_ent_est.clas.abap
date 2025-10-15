CLASS zcl_int_ib_cons_proc_ent_est DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

*    DATA:
*      BEGIN OF ty_filter_header,
*        obj_key TYPE zmmt_ee_zgr_docs-obj_key,
*      END OF ty_filter_header ,
*
*      BEGIN OF ty_filter_items,
*        obj_key TYPE zmmt_ee_zgr_docs-obj_key,
*      END OF ty_filter_items.
*
*    DATA:
*      BEGIN OF zde_data_request,
*        header_filter LIKE ty_filter_header,
*        items_filter  LIKE ty_filter_items,
*      END OF zde_data_request .
*    DATA:
*      BEGIN OF zde_data_response,
*        header                 TYPE zmmt_ee_zgr_t,
*        obj_key                TYPE zmmt_ee_zgr_docs-obj_key,
*        "DOC_FISCAL
*        docnum                 TYPE j_1bnfdoc-docnum,
*        cancel                 TYPE j_1bnfdoc-cancel,
*        candat                 TYPE j_1bnfdoc-candat,
*        "DOC_FATURA
*        belnr_fatura           TYPE  rbkp-belnr,
*        gjahr_fatura           TYPE rbkp-gjahr,
*        stblg                  TYPE rbkp-stblg,
*        "DOC_MATERIAL
*        belnr_material         TYPE mkpf-mblnr,
*        gjahr_material         TYPE mkpf-mjahr,
*        estornado(1),
*        "DOC_MATERIAL_SOBRA
*        belnr_sobra            TYPE mkpf-mblnr,
*        gjahr_sobra            TYPE mkpf-mjahr,
*        sobra_estornado(1),
*        "DOC_AVISO
*        av_vbeln               TYPE zmmt_ee_zgr_docs-av_vbeln,
*        doc_aviso_eliminado(1),
*      END OF zde_data_response .

    TYPES:
      BEGIN OF ty_request,
        obj_key TYPE awkey,
      END OF ty_request .
    TYPES:
      BEGIN OF ty_doc_fiscal,
        docnum TYPE j_1bnfdoc-docnum,
        cancel TYPE j_1bnfdoc-cancel,
        candat TYPE j_1bnfdoc-candat,
      END OF ty_doc_fiscal .
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
      BEGIN OF ty_doc_mat_sob,
        mblnr     TYPE mkpf-mblnr,
        mjahr     TYPE mkpf-mjahr,
        estornado TYPE char1,
      END OF ty_doc_mat_sob .
    TYPES:
      BEGIN OF ty_doc_aviso,
        vbeln     TYPE vbeln,
        eliminado TYPE char1,
      END OF ty_doc_aviso .
    TYPES:
      BEGIN OF ty_response,
        header       TYPE  zmmt_ee_zgr_t,
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

    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '122' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PROC_ENT_EST IMPLEMENTATION.


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

    DATA(lt_obj_key) = lwa_data_request-obj_key.

    "DELETE lt_obj_key WHERE obj_key IS INITIAL.

    IF lt_obj_key IS INITIAL.
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


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    TYPES:
      BEGIN OF ty_obj,
        obj_key TYPE awkey,
      END OF ty_obj.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response TYPE TABLE OF zde_data_response.

    DATA: lra_useralias TYPE RANGE OF usalias,
          lra_monat     TYPE RANGE OF monat,
          lra_gjahr     TYPE RANGE OF gjahr,
          lt_obj_key    TYPE TABLE OF ty_obj,
          lt_zgr        TYPE TABLE OF zmmt_ee_zgr.
    "lt_request     TYPE awkey.

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

      lt_obj_key = lwa_data_request-obj_key.

      SELECT *
        FROM zmmt_ee_zgr
        INTO TABLE lt_zgr
        FOR ALL ENTRIES IN lt_obj_key
        WHERE obj_key = lt_obj_key-obj_key.

      IF sy-subrc IS INITIAL.

        SELECT * FROM zmmt_ee_zgr_docs
        INTO TABLE  @DATA(lt_zmmt_ee_zgr_docs)
        FOR ALL ENTRIES IN @lt_zgr
        WHERE obj_key = @lt_zgr-obj_key.

        DATA(lt_zgr_aux) = lt_zgr.
        SORT lt_zmmt_ee_zgr_docs BY ft_belnr ft_gjahr.

*---> 04/07/2023 - Migração S4 - WS
        SORT lt_zgr_aux BY belnr gjhar.
*<--- 04/07/2023 - Migração S4 - WS

        DELETE ADJACENT DUPLICATES FROM lt_zgr_aux COMPARING belnr gjhar.

        "IF lt_zgr_aux[] IS NOT INITIAL.
        IF lt_zmmt_ee_zgr_docs[] IS NOT INITIAL.
          SELECT docnum ,
 cancel ,
 candat
 FROM j_1bnfdoc
 INTO TABLE @DATA(lt_1bnfdoc)
 FOR ALL ENTRIES IN @lt_zmmt_ee_zgr_docs
 WHERE docnum = @lt_zmmt_ee_zgr_docs-docnum
 ORDER BY PRIMARY KEY .

          SELECT belnr,
                 gjahr,
                 stblg
            FROM rbkp
            INTO TABLE @DATA(lt_rbkp)
            FOR ALL ENTRIES IN @lt_zmmt_ee_zgr_docs
            WHERE belnr = @lt_zmmt_ee_zgr_docs-ft_belnr
              AND gjahr = @lt_zmmt_ee_zgr_docs-ft_gjahr.
          IF sy-subrc IS INITIAL.
            SORT lt_rbkp BY belnr gjahr.
          ENDIF.
        ENDIF.

        "lt_zgr_aux = lt_zgr.
        DATA(lt_zmmt_ee_zgr_docs_aux) = lt_zmmt_ee_zgr_docs.
        SORT lt_zmmt_ee_zgr_docs_aux BY mm_mblnr mm_mjahr.

        DELETE ADJACENT DUPLICATES FROM lt_zmmt_ee_zgr_docs_aux COMPARING mm_mblnr mm_mjahr.

        IF lt_zgr_aux[] IS NOT INITIAL.
          SELECT mblnr,
                 mjahr
            FROM mkpf
            INTO TABLE @DATA(lt_mkpf)
            FOR ALL ENTRIES IN @lt_zmmt_ee_zgr_docs_aux
            WHERE mblnr = @lt_zmmt_ee_zgr_docs_aux-mm_mblnr
              AND mjahr = @lt_zmmt_ee_zgr_docs_aux-mm_mjahr.
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

        lt_zmmt_ee_zgr_docs_aux = lt_zmmt_ee_zgr_docs.
        SORT lt_zmmt_ee_zgr_docs_aux BY av_vbeln.

        DELETE ADJACENT DUPLICATES FROM lt_zmmt_ee_zgr_docs_aux COMPARING av_vbeln.

        IF lt_zgr_aux[] IS NOT INITIAL.
          SELECT vbeln
            FROM likp
            INTO TABLE @DATA(lt_likp)
            FOR ALL ENTRIES IN @lt_zmmt_ee_zgr_docs_aux
            WHERE vbeln = @lt_zmmt_ee_zgr_docs_aux-av_vbeln.
          IF sy-subrc IS INITIAL.
            SORT lt_likp BY vbeln.
          ENDIF.
        ENDIF.

        lt_zgr_aux = lt_zgr.
        lt_zmmt_ee_zgr_docs_aux = lt_zmmt_ee_zgr_docs.

        LOOP AT lt_zgr_aux ASSIGNING FIELD-SYMBOL(<fs_zgr>).

          APPEND INITIAL LINE TO lwa_data_response ASSIGNING FIELD-SYMBOL(<fs_response>).
          <fs_response>-zmmt_ee_zgr-obj_key         = <fs_zgr>-obj_key.
          <fs_response>-zmmt_ee_zgr-po_number       = <fs_zgr>-po_number.
          <fs_response>-zmmt_ee_zgr-opr_type        = <fs_zgr>-opr_type.
          <fs_response>-zmmt_ee_zgr-move_type       = <fs_zgr>-move_type.
          <fs_response>-zmmt_ee_zgr-nt_remessa      = <fs_zgr>-nt_remessa.
          <fs_response>-zmmt_ee_zgr-doc_date        = <fs_zgr>-doc_date.
          <fs_response>-zmmt_ee_zgr-pstng_date      = <fs_zgr>-pstng_date.
          <fs_response>-zmmt_ee_zgr-po_item         = <fs_zgr>-po_item.
          <fs_response>-zmmt_ee_zgr-entry_qnt       = <fs_zgr>-entry_qnt.
          <fs_response>-zmmt_ee_zgr-meins           = <fs_zgr>-meins.
          <fs_response>-zmmt_ee_zgr-in_aviso_receb  = <fs_zgr>-in_aviso_receb.
          <fs_response>-zmmt_ee_zgr-peso_bruto      = <fs_zgr>-peso_bruto.
          <fs_response>-zmmt_ee_zgr-comp_code       = <fs_zgr>-comp_code.
          <fs_response>-zmmt_ee_zgr-tp_operacao     = <fs_zgr>-tp_operacao.
          <fs_response>-zmmt_ee_zgr-ref_doc_no      = <fs_zgr>-ref_doc_no.
          <fs_response>-zmmt_ee_zgr-vr_bruto        = <fs_zgr>-vr_bruto.
          <fs_response>-zmmt_ee_zgr-del_costs_taxc  = <fs_zgr>-del_costs_taxc.
          <fs_response>-zmmt_ee_zgr-pmnt_block      = <fs_zgr>-pmnt_block.
          <fs_response>-zmmt_ee_zgr-pmnttrms        = <fs_zgr>-pmnttrms.
          <fs_response>-zmmt_ee_zgr-scbank_ind      = <fs_zgr>-scbank_ind.
          <fs_response>-zmmt_ee_zgr-plant           = <fs_zgr>-plant.
          <fs_response>-zmmt_ee_zgr-j_1bnftype      = <fs_zgr>-j_1bnftype.
          <fs_response>-zmmt_ee_zgr-alloc_nmbr      = <fs_zgr>-alloc_nmbr.
          <fs_response>-zmmt_ee_zgr-header_txt      = <fs_zgr>-header_txt.
          <fs_response>-zmmt_ee_zgr-gross_amount    = <fs_zgr>-gross_amount.
          <fs_response>-zmmt_ee_zgr-item_amount     = <fs_zgr>-item_amount.
          <fs_response>-zmmt_ee_zgr-amount_lc       = <fs_zgr>-amount_lc.
          <fs_response>-zmmt_ee_zgr-material        = <fs_zgr>-material.
          <fs_response>-zmmt_ee_zgr-text1           = <fs_zgr>-text1.
          <fs_response>-zmmt_ee_zgr-text2           = <fs_zgr>-text2.
          <fs_response>-zmmt_ee_zgr-text3           = <fs_zgr>-text3.
          <fs_response>-zmmt_ee_zgr-nfnum           = <fs_zgr>-nfnum.
          <fs_response>-zmmt_ee_zgr-authcod         = <fs_zgr>-authcod.
          <fs_response>-zmmt_ee_zgr-xmlvers         = <fs_zgr>-xmlvers.
          <fs_response>-zmmt_ee_zgr-code            = <fs_zgr>-code.
          <fs_response>-zmmt_ee_zgr-nfenum          = <fs_zgr>-nfenum.
          <fs_response>-zmmt_ee_zgr-docstat         = <fs_zgr>-docstat.
          <fs_response>-zmmt_ee_zgr-cdv             = <fs_zgr>-cdv.
          <fs_response>-zmmt_ee_zgr-profit_ctr      = <fs_zgr>-profit_ctr.
          <fs_response>-zmmt_ee_zgr-costobject      = <fs_zgr>-costobject.
          <fs_response>-zmmt_ee_zgr-nu_item         = <fs_zgr>-nu_item.
          <fs_response>-zmmt_ee_zgr-gl_credito      = <fs_zgr>-gl_credito.
          <fs_response>-zmmt_ee_zgr-gl_debito       = <fs_zgr>-gl_debito.
          <fs_response>-zmmt_ee_zgr-vl_cmv          = <fs_zgr>-vl_cmv.
          <fs_response>-zmmt_ee_zgr-vl_cmv_lc       = <fs_zgr>-vl_cmv_lc.
          <fs_response>-zmmt_ee_zgr-tax_code        = <fs_zgr>-tax_code.
          <fs_response>-zmmt_ee_zgr-taxjurcode      = <fs_zgr>-taxjurcode.
          <fs_response>-zmmt_ee_zgr-zdt_atlz        = <fs_zgr>-zdt_atlz.
          <fs_response>-zmmt_ee_zgr-zhr_atlz        = <fs_zgr>-zhr_atlz.
          <fs_response>-zmmt_ee_zgr-zrg_atlz        = <fs_zgr>-zrg_atlz.
          <fs_response>-zmmt_ee_zgr-pymt_meth       = <fs_zgr>-pymt_meth.
          <fs_response>-zmmt_ee_zgr-bus_area        = <fs_zgr>-bus_area.
          <fs_response>-zmmt_ee_zgr-gl_account      = <fs_zgr>-gl_account.
          <fs_response>-zmmt_ee_zgr-quantity        = <fs_zgr>-quantity.
          <fs_response>-zmmt_ee_zgr-batch           = <fs_zgr>-batch.
          <fs_response>-zmmt_ee_zgr-lgort           = <fs_zgr>-lgort.
          <fs_response>-zmmt_ee_zgr-lifnr           = <fs_zgr>-lifnr.
          <fs_response>-zmmt_ee_zgr-taxtyp_icms     = <fs_zgr>-taxtyp_icms.
          <fs_response>-zmmt_ee_zgr-taxtyp_ipi      = <fs_zgr>-taxtyp_ipi.
          <fs_response>-zmmt_ee_zgr-taxtyp_pis      = <fs_zgr>-taxtyp_pis.
          <fs_response>-zmmt_ee_zgr-taxtyp_cofins   = <fs_zgr>-taxtyp_cofins.
          <fs_response>-zmmt_ee_zgr-taxlw1          = <fs_zgr>-taxlw1.
          <fs_response>-zmmt_ee_zgr-taxlw2          = <fs_zgr>-taxlw2.
          <fs_response>-zmmt_ee_zgr-taxlw4          = <fs_zgr>-taxlw4.
          <fs_response>-zmmt_ee_zgr-taxlw5          = <fs_zgr>-taxlw5.
          <fs_response>-zmmt_ee_zgr-cfop            = <fs_zgr>-cfop.
          <fs_response>-zmmt_ee_zgr-vr_impostos     = <fs_zgr>-vr_impostos.
          <fs_response>-zmmt_ee_zgr-blart           = <fs_zgr>-blart.
          <fs_response>-zmmt_ee_zgr-belnr           = <fs_zgr>-belnr.
          <fs_response>-zmmt_ee_zgr-gjhar           = <fs_zgr>-gjhar.
          <fs_response>-zmmt_ee_zgr-desm_pagos      = <fs_zgr>-desm_pagos.
          <fs_response>-zmmt_ee_zgr-dt_vencimento   = <fs_zgr>-dt_vencimento.
          <fs_response>-zmmt_ee_zgr-cd_afip         = <fs_zgr>-cd_afip.
          <fs_response>-zmmt_ee_zgr-id_carga        = <fs_zgr>-id_carga.
          <fs_response>-zmmt_ee_zgr-id_nota         = <fs_zgr>-id_nota.
          <fs_response>-zmmt_ee_zgr-st_estorno      = <fs_zgr>-st_estorno.
          <fs_response>-zmmt_ee_zgr-interface_miro  = <fs_zgr>-interface_miro.
          <fs_response>-zmmt_ee_zgr-ch_referencia   = <fs_zgr>-ch_referencia.
          <fs_response>-zmmt_ee_zgr-cd_txt_sisa     = <fs_zgr>-cd_txt_sisa.
          <fs_response>-zmmt_ee_zgr-prod_date       = <fs_zgr>-prod_date.
          <fs_response>-zmmt_ee_zgr-objkey_np       = <fs_zgr>-objkey_np.

          <fs_response>-obj_key = <fs_zgr>-obj_key.

*          READ TABLE lt_zmmt_ee_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zmmt_ee_zgr_docs>)
*          WITH KEY obj_key = <fs_zgr>-obj_key
*          BINARY SEARCH.
          LOOP AT lt_zmmt_ee_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zmmt_ee_zgr_docs>) WHERE obj_key = <fs_zgr>-obj_key.



            READ TABLE lt_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_1bnfdoc>)
            WITH KEY docnum = <fs_zmmt_ee_zgr_docs>-docnum
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_response>-doc_fiscal-docnum = <fs_1bnfdoc>-docnum.
              <fs_response>-doc_fiscal-cancel = <fs_1bnfdoc>-cancel.
              <fs_response>-doc_fiscal-candat = <fs_1bnfdoc>-candat.

            ENDIF.


            READ TABLE lt_rbkp ASSIGNING FIELD-SYMBOL(<fs_rbkp>)
            WITH KEY belnr = <fs_zmmt_ee_zgr_docs>-ft_belnr
                     gjahr = <fs_zmmt_ee_zgr_docs>-ft_gjahr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_response>-doc_fatura-belnr = <fs_rbkp>-belnr.
              <fs_response>-doc_fatura-gjahr = <fs_rbkp>-gjahr.
              <fs_response>-doc_fatura-stblg = <fs_rbkp>-stblg.

            ENDIF.

            READ TABLE lt_mkpf TRANSPORTING NO FIELDS
            WITH KEY mblnr = <fs_zmmt_ee_zgr_docs>-mm_mblnr
                     mjahr = <fs_zmmt_ee_zgr_docs>-mm_mjahr
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_response>-doc_material-mblnr = <fs_zmmt_ee_zgr_docs>-mm_mblnr.
              <fs_response>-doc_material-mjahr = <fs_zmmt_ee_zgr_docs>-mm_mjahr.

              READ TABLE lt_mseg TRANSPORTING NO FIELDS
              WITH KEY smbln = <fs_zmmt_ee_zgr_docs>-mm_mblnr
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_response>-doc_material-estornado = 'S'.
              ELSE.
                <fs_response>-doc_material-estornado = 'N'.
              ENDIF.

            ENDIF.

            READ TABLE lt_mkpf TRANSPORTING NO FIELDS
            WITH KEY mblnr = <fs_zmmt_ee_zgr_docs>-mm_mblnr_sobra
                     mjahr = <fs_zmmt_ee_zgr_docs>-mm_mjahr_sobra
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_response>-doc_mat_sobra-mblnr = <fs_zmmt_ee_zgr_docs>-mm_mblnr_sobra.
              <fs_response>-doc_mat_sobra-mjahr = <fs_zmmt_ee_zgr_docs>-mm_mjahr_sobra.

              READ TABLE lt_mseg TRANSPORTING NO FIELDS
              WITH KEY smbln = <fs_zmmt_ee_zgr_docs>-mm_mblnr_sobra
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                <fs_response>-doc_material-estornado = 'S'.
              ELSE.
                <fs_response>-doc_material-estornado = 'N'.
              ENDIF.

            ENDIF.


            READ TABLE lt_likp TRANSPORTING NO FIELDS
            WITH KEY vbeln = <fs_zmmt_ee_zgr_docs>-av_vbeln
            BINARY SEARCH.
            IF sy-subrc IS INITIAL  .

              <fs_response>-doc_aviso-vbeln = <fs_zmmt_ee_zgr_docs>-av_vbeln.
              <fs_response>-doc_aviso-eliminado = 'N'.

            ELSE.

              <fs_response>-doc_aviso-vbeln = <fs_zmmt_ee_zgr_docs>-av_vbeln.
              <fs_response>-doc_aviso-eliminado = 'S'.

            ENDIF.
          ENDLOOP.

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


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
