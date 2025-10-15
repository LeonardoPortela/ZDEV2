CLASS zcl_int_ib_cons_simulador_vda DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_cabe,
        doc_simulacao      TYPE zsdt0040-doc_simulacao,
        kunnr              TYPE zsdt0040-kunnr,
        vkorg              TYPE zsdt0040-vkorg,
        vkbur              TYPE zsdt0040-vkbur,
        vtweg              TYPE zsdt0040-vtweg,
        spart              TYPE zsdt0040-spart,
        auart              TYPE zsdt0040-auart,
        venci              TYPE zsdt0040-venci,
        fazenda            TYPE zsdt0040-fazenda,
        tpsim              TYPE zsdt0040-tpsim,
        tpcult             TYPE zsdt0040-tpcult,
        fcult              TYPE zsdt0040-fcult,
        dtent              TYPE zsdt0040-dtent,
        vendedor           TYPE zsdt0040-vendedor,
        area_ha            TYPE zsdt0040-area_ha,
        waerk              TYPE zsdt0040-waerk,
        cultura            TYPE zsdt0040-cultura,
        safra              TYPE zsdt0040-safra,
        ernam              TYPE zsdt0040-ernam,
        erdat              TYPE zsdt0040-erdat,
        erzet              TYPE zsdt0040-erzet,
        prec_cult          TYPE zsdt0040-prec_cult,
        prec_ant_cult      TYPE zsdt0040-prec_ant_cult,
        juros_ano          TYPE zsdt0040-juros_ano,
        vlr_adto           TYPE zsdt0040-vlr_adto,
        adto_ha            TYPE zsdt0040-adto_ha,
        area_penhor        TYPE zsdt0040-area_penhor,
        trototsc           TYPE zsdt0040-trototsc,
        scha               TYPE zsdt0040-scha,
        status             TYPE zsdt0040-status,
        vlrtot             TYPE zsdt0040-vlrtot,
        comprsc            TYPE zsdt0040-comprsc,
        antec              TYPE zsdt0040-antec,
        dtpgtcult          TYPE zsdt0040-dtpgtcult,
        dtprevpgto         TYPE zsdt0040-dtprevpgto,
        dtinijuros         TYPE zsdt0040-dtinijuros,
        kursf              TYPE zsdt0040-kursf,
        taxa_frete         TYPE zsdt0040-taxa_frete,
        dt_entrega_sem     TYPE zsdt0040-dt_entrega_sem,
        dt_entrega_def     TYPE zsdt0040-dt_entrega_def,
        dt_entrega_fet     TYPE zsdt0040-dt_entrega_fet,
        juros_mora         TYPE zsdt0040-juros_mora,
        fiador_01          TYPE zsdt0040-fiador_01,
        fiador_02          TYPE zsdt0040-fiador_02,
        pag_prorrogado     TYPE zsdt0040-pag_prorrogado,
        meio_pago          TYPE zsdt0040-meio_pago,
        dtvencov           TYPE zsdt0040-dtvencov,
        repique            TYPE zsdt0040-repique,
        funrural           TYPE zsdt0040-funrural,
        funuser            TYPE zsdt0040-funuser,
        fundata            TYPE zsdt0040-fundata,
        funhora            TYPE zsdt0040-funhora,
        job                TYPE zsdt0040-job,
        hbkid              TYPE zsdt0040-hbkid,
        usnam              TYPE zsdt0040-usnam,
        data_atual         TYPE zsdt0040-data_atual,
        hora_atual         TYPE zsdt0040-hora_atual,
        inss               TYPE zsdt0040-inss,
        facs               TYPE zsdt0040-facs,
        ecommerce          TYPE zsdt0040-ecommerce,
        id_order_ecommerce TYPE zsdt0040-id_order_ecommerce,
      END OF ty_cabe,

      BEGIN OF ty_item,
        doc_simulacao TYPE zsdt0041-doc_simulacao,
        posnr         TYPE zsdt0041-posnr,
        auart         TYPE zsdt0041-auart,
        spart         TYPE zsdt0041-spart,
        inco1         TYPE zsdt0041-inco1,
        matnr         TYPE zsdt0041-matnr,
        dtvenc        TYPE zsdt0041-dtvenc,
        werks_fornec  TYPE zsdt0041-werks_fornec,
        werks         TYPE zsdt0041-werks,
        charg         TYPE zsdt0041-charg,
        zmeng         TYPE zsdt0041-zmeng,
        zieme         TYPE zsdt0041-zieme,
        desconto      TYPE zsdt0041-desconto,
        zwert         TYPE zsdt0041-zwert,
        calcu         TYPE zsdt0041-calcu,
        trunit        TYPE zsdt0041-trunit,
        siumb         TYPE zsdt0041-siumb,
        trtot         TYPE zsdt0041-trtot,
        compr         TYPE zsdt0041-compr,
        mgcad         TYPE zsdt0041-mgcad,
        mgefe         TYPE zsdt0041-mgefe,
        vlrtot        TYPE zsdt0041-vlrtot,
        waerk         TYPE zsdt0041-waerk,
        vbeln         TYPE zsdt0041-vbeln,
        vl_unit       TYPE zsdt0041-vl_unit,
        vlr_ajuste    TYPE zsdt0041-vlr_ajuste,
        desc_absoluto TYPE zsdt0041-desc_absoluto,
        vlr_frete     TYPE zsdt0041-vlr_frete,
        juros         TYPE zsdt0041-juros,
        status        TYPE zsdt0041-status,
        j_1btxsdc     TYPE zsdt0041-j_1btxsdc,
        j_1btaxlw1    TYPE zsdt0041-j_1btaxlw1,
        vlr_icms      TYPE zsdt0041-vlr_icms,
        j_1btaxlw4    TYPE zsdt0041-j_1btaxlw4,
        vlr_cofins    TYPE zsdt0041-vlr_cofins,
        j_1btaxlw5    TYPE zsdt0041-j_1btaxlw5,
        vlr_pis       TYPE zsdt0041-vlr_pis,
        zwert_liqdo   TYPE zsdt0041-zwert_liqdo,
        cultura_apl   TYPE zsdt0041-cultura_apl,
        safra_apl     TYPE zsdt0041-safra_apl,
      END OF ty_item,

      BEGIN OF ty_controle,
        doc_simulacao       TYPE zsdt0090-doc_simulacao,
        sequencia           TYPE zsdt0090-sequencia,
        seq_trava           TYPE zsdt0090-seq_trava,
        auart               TYPE zsdt0090-auart,
        vbeln               TYPE zsdt0090-vbeln,
        posnn               TYPE zsdt0090-posnn,
        spart               TYPE zsdt0090-spart,
        zmeng               TYPE zsdt0090-zmeng,
        zieme               TYPE zsdt0090-zieme,
        netpr               TYPE zsdt0090-netpr,
        kmein               TYPE zsdt0090-kmein,
        charg               TYPE zsdt0090-charg,
        matnr               TYPE zsdt0090-matnr,
        matkl               TYPE zsdt0090-matkl,
        inco1               TYPE zsdt0090-inco1,
        werks               TYPE zsdt0090-werks,
        kunnr               TYPE zsdt0090-kunnr,
        lgort               TYPE zsdt0090-lgort,
        flag_imposto        TYPE zsdt0090-flag_imposto,
        auartv              TYPE zsdt0090-auartv,
        vbelv               TYPE zsdt0090-vbelv,
        posnv               TYPE zsdt0090-posnv,
        spartv              TYPE zsdt0090-spartv,
        zmengv              TYPE zsdt0090-zmengv,
        ziemev              TYPE zsdt0090-ziemev,
        netprv              TYPE zsdt0090-netprv,
        kmeinv              TYPE zsdt0090-kmeinv,
        chargv              TYPE zsdt0090-chargv,
        matnrv              TYPE zsdt0090-matnrv,
        matklv              TYPE zsdt0090-matklv,
        inco1v              TYPE zsdt0090-inco1v,
        werksv              TYPE zsdt0090-werksv,
        kunnrv              TYPE zsdt0090-kunnrv,
        lgortv              TYPE zsdt0090-lgortv,
        netwr               TYPE zsdt0090-netwr,
        flag_impostov       TYPE zsdt0090-flag_impostov,
        categoria           TYPE zsdt0090-categoria,
        kurrf               TYPE zsdt0090-kurrf,
        valdt               TYPE zsdt0090-valdt,
        desc_absoluto       TYPE zsdt0090-desc_absoluto,
        desc_absoluto_lq    TYPE zsdt0090-desc_absoluto_lq,
        estorno             TYPE zsdt0090-estorno,
        usnam_e             TYPE zsdt0090-usnam_e,
        data_atual_e        TYPE zsdt0090-data_atual_e,
        hora_atual_e        TYPE zsdt0090-hora_atual_e,
        origem_est          TYPE zsdt0090-origem_est,
        route               TYPE zsdt0090-route,
        zpesagem            TYPE zsdt0090-zpesagem,
        parvw               TYPE zsdt0090-parvw,
        cod_parc            TYPE zsdt0090-cod_parc,
        usnam               TYPE zsdt0090-usnam,
        data_atual          TYPE zsdt0090-data_atual,
        hora_atual          TYPE zsdt0090-hora_atual,
        flag                TYPE zsdt0090-flag,
        flag1               TYPE zsdt0090-flag1,
        email               TYPE zsdt0090-email,
        dt_email            TYPE zsdt0090-dt_email,
        hr_email            TYPE zsdt0090-hr_email,
        job                 TYPE zsdt0090-job,
        zterm               TYPE zsdt0090-zterm,
        ztermv              TYPE zsdt0090-ztermv,
        dt_entrega          TYPE zsdt0090-dt_entrega,
        dt_entrega_v        TYPE zsdt0090-dt_entrega_v,
        data_prevpgto       TYPE zsdt0090-data_prevpgto,
        data_prevpgtov      TYPE zsdt0090-data_prevpgtov,
        prev_pgto_usd       TYPE zsdt0090-prev_pgto_usd,
        prev_pgto_brl       TYPE zsdt0090-prev_pgto_brl,
        prev_multa_usd      TYPE zsdt0090-prev_multa_usd,
        prev_juros_usd      TYPE zsdt0090-prev_juros_usd,
        prev_vl_liq_usd     TYPE zsdt0090-prev_vl_liq_usd,
        sequenciav          TYPE zsdt0090-sequenciav,
        trav_camb_utilizada TYPE zsdt0090-trav_camb_utilizada,
      END OF ty_controle.

    DATA:
      tb_simulacao TYPE TABLE OF zsdt0040-doc_simulacao .
    DATA:
      BEGIN OF z_gen_cabe,
        doc_simulacao      TYPE zsdt0040-doc_simulacao,
        kunnr              TYPE zsdt0040-kunnr,
        vkorg              TYPE zsdt0040-vkorg,
        vkbur              TYPE zsdt0040-vkbur,
        vtweg              TYPE zsdt0040-vtweg,
        spart              TYPE zsdt0040-spart,
        auart              TYPE zsdt0040-auart,
        venci              TYPE zsdt0040-venci,
        fazenda            TYPE zsdt0040-fazenda,
        tpsim              TYPE zsdt0040-tpsim,
        tpcult             TYPE zsdt0040-tpcult,
        fcult              TYPE zsdt0040-fcult,
        dtent              TYPE zsdt0040-dtent,
        vendedor           TYPE zsdt0040-vendedor,
        area_ha            TYPE zsdt0040-area_ha,
        waerk              TYPE zsdt0040-waerk,
        cultura            TYPE zsdt0040-cultura,
        safra              TYPE zsdt0040-safra,
        ernam              TYPE zsdt0040-ernam,
        erdat              TYPE zsdt0040-erdat,
        erzet              TYPE zsdt0040-erzet,
        prec_cult          TYPE zsdt0040-prec_cult,
        prec_ant_cult      TYPE zsdt0040-prec_ant_cult,
        juros_ano          TYPE zsdt0040-juros_ano,
        vlr_adto           TYPE zsdt0040-vlr_adto,
        adto_ha            TYPE zsdt0040-adto_ha,
        area_penhor        TYPE zsdt0040-area_penhor,
        trototsc           TYPE zsdt0040-trototsc,
        scha               TYPE zsdt0040-scha,
        status             TYPE zsdt0040-status,
        vlrtot             TYPE zsdt0040-vlrtot,
        comprsc            TYPE zsdt0040-comprsc,
        antec              TYPE zsdt0040-antec,
        dtpgtcult          TYPE zsdt0040-dtpgtcult,
        dtprevpgto         TYPE zsdt0040-dtprevpgto,
        dtinijuros         TYPE zsdt0040-dtinijuros,
        kursf              TYPE zsdt0040-kursf,
        taxa_frete         TYPE zsdt0040-taxa_frete,
        dt_entrega_sem     TYPE zsdt0040-dt_entrega_sem,
        dt_entrega_def     TYPE zsdt0040-dt_entrega_def,
        dt_entrega_fet     TYPE zsdt0040-dt_entrega_fet,
        juros_mora         TYPE zsdt0040-juros_mora,
        fiador_01          TYPE zsdt0040-fiador_01,
        fiador_02          TYPE zsdt0040-fiador_02,
        pag_prorrogado     TYPE zsdt0040-pag_prorrogado,
        meio_pago          TYPE zsdt0040-meio_pago,
        dtvencov           TYPE zsdt0040-dtvencov,
        repique            TYPE zsdt0040-repique,
        funrural           TYPE zsdt0040-funrural,
        funuser            TYPE zsdt0040-funuser,
        fundata            TYPE zsdt0040-fundata,
        funhora            TYPE zsdt0040-funhora,
        job                TYPE zsdt0040-job,
        hbkid              TYPE zsdt0040-hbkid,
        usnam              TYPE zsdt0040-usnam,
        data_atual         TYPE zsdt0040-data_atual,
        hora_atual         TYPE zsdt0040-hora_atual,
        inss               TYPE zsdt0040-inss,
        facs               TYPE zsdt0040-facs,
        ecommerce          TYPE zsdt0040-ecommerce,
        id_order_ecommerce TYPE zsdt0040-id_order_ecommerce,
      END OF z_gen_cabe,

      BEGIN OF z_gen_item,
        doc_simulacao      TYPE zsdt0041-doc_simulacao,
        posnr              TYPE zsdt0041-posnr,
        auart              TYPE zsdt0041-auart,
        spart              TYPE zsdt0041-spart,
        inco1              TYPE zsdt0041-inco1,
        matnr              TYPE zsdt0041-matnr,
        dtvenc             TYPE zsdt0041-dtvenc,
        werks_fornec       TYPE zsdt0041-werks_fornec,
        werks              TYPE zsdt0041-werks,
        charg              TYPE zsdt0041-charg,
        zmeng              TYPE zsdt0041-zmeng,
        zieme              TYPE zsdt0041-zieme,
        desconto           TYPE zsdt0041-desconto,
        zwert              TYPE zsdt0041-zwert,
        calcu              TYPE zsdt0041-calcu,
        trunit             TYPE zsdt0041-trunit,
        siumb              TYPE zsdt0041-siumb,
        trtot              TYPE zsdt0041-trtot,
        compr              TYPE zsdt0041-compr,
        mgcad              TYPE zsdt0041-mgcad,
        mgefe              TYPE zsdt0041-mgefe,
        vlrtot             TYPE zsdt0041-vlrtot,
        waerk              TYPE zsdt0041-waerk,
        vbeln              TYPE zsdt0041-vbeln,
        vl_unit            TYPE zsdt0041-vl_unit,
        vlr_ajuste         TYPE zsdt0041-vlr_ajuste,
        desc_absoluto      TYPE zsdt0041-desc_absoluto,
        vlr_frete          TYPE zsdt0041-vlr_frete,
        juros              TYPE zsdt0041-juros,
        status             TYPE zsdt0041-status,
        j_1btxsdc          TYPE zsdt0041-j_1btxsdc,
        j_1btaxlw1         TYPE zsdt0041-j_1btaxlw1,
        vlr_icms           TYPE zsdt0041-vlr_icms,
        j_1btaxlw4         TYPE zsdt0041-j_1btaxlw4,
        vlr_cofins         TYPE zsdt0041-vlr_cofins,
        j_1btaxlw5         TYPE zsdt0041-j_1btaxlw5,
        vlr_pis            TYPE zsdt0041-vlr_pis,
        zwert_liqdo        TYPE zsdt0041-zwert_liqdo,
        cultura_apl        TYPE zsdt0041-cultura_apl,
        safra_apl          TYPE zsdt0041-safra_apl,
        ds_setor_atividade TYPE tspat-vtext,
      END OF z_gen_item,

      BEGIN OF z_gen_controle,
        doc_simulacao       TYPE zsdt0090-doc_simulacao,
        sequencia           TYPE zsdt0090-sequencia,
        seq_trava           TYPE zsdt0090-seq_trava,
        auart               TYPE zsdt0090-auart,
        vbeln               TYPE zsdt0090-vbeln,
        posnn               TYPE zsdt0090-posnn,
        spart               TYPE zsdt0090-spart,
        zmeng               TYPE zsdt0090-zmeng,
        zieme               TYPE zsdt0090-zieme,
        netpr               TYPE zsdt0090-netpr,
        kmein               TYPE zsdt0090-kmein,
        charg               TYPE zsdt0090-charg,
        matnr               TYPE zsdt0090-matnr,
        matkl               TYPE zsdt0090-matkl,
        inco1               TYPE zsdt0090-inco1,
        werks               TYPE zsdt0090-werks,
        kunnr               TYPE zsdt0090-kunnr,
        lgort               TYPE zsdt0090-lgort,
        flag_imposto        TYPE zsdt0090-flag_imposto,
        auartv              TYPE zsdt0090-auartv,
        vbelv               TYPE zsdt0090-vbelv,
        posnv               TYPE zsdt0090-posnv,
        spartv              TYPE zsdt0090-spartv,
        zmengv              TYPE zsdt0090-zmengv,
        ziemev              TYPE zsdt0090-ziemev,
        netprv              TYPE zsdt0090-netprv,
        kmeinv              TYPE zsdt0090-kmeinv,
        chargv              TYPE zsdt0090-chargv,
        matnrv              TYPE zsdt0090-matnrv,
        matklv              TYPE zsdt0090-matklv,
        inco1v              TYPE zsdt0090-inco1v,
        werksv              TYPE zsdt0090-werksv,
        kunnrv              TYPE zsdt0090-kunnrv,
        lgortv              TYPE zsdt0090-lgortv,
        netwr               TYPE zsdt0090-netwr,
        flag_impostov       TYPE zsdt0090-flag_impostov,
        categoria           TYPE zsdt0090-categoria,
        kurrf               TYPE zsdt0090-kurrf,
        valdt               TYPE zsdt0090-valdt,
        desc_absoluto       TYPE zsdt0090-desc_absoluto,
        desc_absoluto_lq    TYPE zsdt0090-desc_absoluto_lq,
        estorno             TYPE zsdt0090-estorno,
        usnam_e             TYPE zsdt0090-usnam_e,
        data_atual_e        TYPE zsdt0090-data_atual_e,
        hora_atual_e        TYPE zsdt0090-hora_atual_e,
        origem_est          TYPE zsdt0090-origem_est,
        route               TYPE zsdt0090-route,
        zpesagem            TYPE zsdt0090-zpesagem,
        parvw               TYPE zsdt0090-parvw,
        cod_parc            TYPE zsdt0090-cod_parc,
        usnam               TYPE zsdt0090-usnam,
        data_atual          TYPE zsdt0090-data_atual,
        hora_atual          TYPE zsdt0090-hora_atual,
        flag                TYPE zsdt0090-flag,
        flag1               TYPE zsdt0090-flag1,
        email               TYPE zsdt0090-email,
        dt_email            TYPE zsdt0090-dt_email,
        hr_email            TYPE zsdt0090-hr_email,
        job                 TYPE zsdt0090-job,
        zterm               TYPE zsdt0090-zterm,
        ztermv              TYPE zsdt0090-ztermv,
        dt_entrega          TYPE zsdt0090-dt_entrega,
        dt_entrega_v        TYPE zsdt0090-dt_entrega_v,
        data_prevpgto       TYPE zsdt0090-data_prevpgto,
        data_prevpgtov      TYPE zsdt0090-data_prevpgtov,
        prev_pgto_usd       TYPE zsdt0090-prev_pgto_usd,
        prev_pgto_brl       TYPE zsdt0090-prev_pgto_brl,
        prev_multa_usd      TYPE zsdt0090-prev_multa_usd,
        prev_juros_usd      TYPE zsdt0090-prev_juros_usd,
        prev_vl_liq_usd     TYPE zsdt0090-prev_vl_liq_usd,
        sequenciav          TYPE zsdt0090-sequenciav,
        trav_camb_utilizada TYPE zsdt0090-trav_camb_utilizada,
      END OF z_gen_controle,

      BEGIN OF z_simuladores,
        cabecalho           LIKE z_gen_cabe,
        itens               LIKE TABLE OF z_gen_item,
        controle_documentos LIKE TABLE OF z_gen_controle,
      END OF z_simuladores.


    DATA: it_cabe     TYPE STANDARD TABLE OF ty_cabe,
          it_item     TYPE STANDARD TABLE OF ty_item,
          it_controle TYPE STANDARD TABLE OF ty_controle.


    DATA:
      BEGIN OF zde_data_request,
        doc_simulacao LIKE tb_simulacao,
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        simuladores LIKE TABLE OF z_simuladores,
*        cabecalho           LIKE TABLE OF z_gen_cabe,
*        itens               LIKE TABLE OF z_gen_item,
*        controle_documentos LIKE TABLE OF z_gen_controle,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '162' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_SIMULADOR_VDA IMPLEMENTATION.


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

*      IF lwa_data_request-material is INITIAL AND lwa_data_request-maktx_like is INITIAL.
*        r_msg_erro = 'Material ou Texto Breve deve ser informado'.
*        RETURN.
*      ENDIF.
*
*      IF lwa_data_request-language is INITIAL.
*        r_msg_erro = 'A linguagem deve ser informada'.
*        RETURN.
*      ENDIF.


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
      BEGIN OF ty_tspat,
        spart TYPE tspat-spart,
        vtext TYPE tspat-vtext,
      END OF ty_tspat.

    DATA: it_tspat TYPE STANDARD TABLE OF ty_tspat.

    DATA: lra_doc_simulacao  TYPE RANGE OF zsdt0041-doc_simulacao.

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

    LOOP AT lwa_data_request-doc_simulacao INTO DATA(wa_doc_simulacao).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_doc_simulacao ) TO lra_doc_simulacao.
    ENDLOOP.

    SELECT *
      FROM zsdt0040
      INTO CORRESPONDING FIELDS OF TABLE it_cabe
      WHERE doc_simulacao IN  lra_doc_simulacao.
    IF sy-subrc IS INITIAL.

      SELECT *
    FROM zsdt0041
    INTO CORRESPONDING FIELDS OF TABLE it_item
        FOR ALL ENTRIES IN it_cabe
    WHERE doc_simulacao EQ it_cabe-doc_simulacao.
      IF sy-subrc IS INITIAL.
        SORT it_item BY doc_simulacao posnr.
        SELECT spart vtext
          FROM tspat
          INTO TABLE it_tspat
          FOR ALL ENTRIES IN it_item
          WHERE spras EQ 'P'
            AND spart EQ it_item-spart.
        IF sy-subrc IS INITIAL.

        ENDIF.
      ENDIF.


      SELECT *
        FROM zsdt0090
        INTO CORRESPONDING FIELDS OF TABLE it_controle
        FOR ALL ENTRIES IN it_cabe
        WHERE doc_simulacao EQ it_cabe-doc_simulacao.
      IF sy-subrc IS INITIAL.
        SORT it_controle BY doc_simulacao sequencia.
      ENDIF.


      SORT it_cabe BY doc_simulacao.
      LOOP AT it_cabe INTO DATA(ls_cabe).
        APPEND INITIAL LINE TO lwa_data_response-simuladores ASSIGNING FIELD-SYMBOL(<fs_response>).
        MOVE-CORRESPONDING ls_cabe TO <fs_response>-cabecalho.
        READ TABLE it_item INTO DATA(ls_item) WITH KEY doc_simulacao = ls_cabe-doc_simulacao BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          DATA(l_tabix) = sy-tabix.
          LOOP AT it_item INTO DATA(ls_item1) FROM l_tabix.
            IF ls_item1-doc_simulacao <> ls_item-doc_simulacao.
              EXIT.
            ENDIF.
             APPEND ls_item1 TO <fs_response>-itens.
            READ TABLE it_tspat INTO DATA(ls_tspat) WITH KEY spart = ls_item1-spart.
            IF sy-subrc IS INITIAL.
              READ TABLE <fs_response>-itens ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY doc_simulacao = ls_item1-doc_simulacao
                                                                                         posnr        = ls_item1-posnr.
              IF sy-subrc IS INITIAL.
                <fs_item>-ds_setor_atividade =  ls_tspat-vtext.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

        READ TABLE it_controle INTO DATA(ls_controle1) WITH KEY doc_simulacao = ls_cabe-doc_simulacao.
        IF sy-subrc IS INITIAL.
          l_tabix = sy-tabix.
          LOOP AT it_controle INTO DATA(ls_controle) FROM l_tabix.
            IF ls_controle1-doc_simulacao <> ls_controle-doc_simulacao.
              EXIT.
            ENDIF.

            APPEND ls_controle TO <fs_response>-controle_documentos.
          ENDLOOP.

        ENDIF.

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
