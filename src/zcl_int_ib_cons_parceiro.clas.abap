CLASS zcl_int_ib_cons_parceiro DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_parceiros,
        parid        TYPE char10,
        land1        TYPE lfa1-land1,
        name1        TYPE lfa1-name1,
        name2        TYPE lfa1-name2,
        name3        TYPE lfa1-name3,
        name4        TYPE lfa1-name4,
        ort01        TYPE lfa1-ort01,
        ort02        TYPE lfa1-ort02,
        pfach        TYPE lfa1-pfach,
        pstl2        TYPE lfa1-pstl2,
        pstlz        TYPE lfa1-pstlz,
        regio        TYPE lfa1-regio,
        sortl        TYPE lfa1-sortl,
        stras        TYPE lfa1-stras,
        adrnr        TYPE lfa1-adrnr,
        mcod1        TYPE lfa1-mcod1,
        mcod2        TYPE lfa1-mcod2,
        mcod3        TYPE lfa1-mcod3,
        anred        TYPE lfa1-anred,
        bahns        TYPE lfa1-bahns,
        bbbnr        TYPE lfa1-bbbnr,
        bbsnr        TYPE lfa1-bbsnr,
        begru        TYPE lfa1-begru,
        brsch        TYPE lfa1-brsch,
        bubkz        TYPE lfa1-bubkz,
        datlt        TYPE lfa1-datlt,
        dtams        TYPE lfa1-dtams,
        dtaws        TYPE lfa1-dtaws,
        erdat        TYPE lfa1-erdat,
        ernam        TYPE lfa1-ernam,
        esrnr        TYPE lfa1-esrnr,
        konzs        TYPE lfa1-konzs,
        ktokk        TYPE lfa1-ktokk,
        ktokd        TYPE kna1-ktokd,
        kunnr        TYPE lfa1-kunnr,
        lnrza        TYPE lfa1-lnrza,
        loevm        TYPE lfa1-loevm,
        sperr        TYPE lfa1-sperr,
        sperm        TYPE lfa1-sperm,
        spras        TYPE lfa1-spras,
        stcd1        TYPE lfa1-stcd1,
        stcd2        TYPE lfa1-stcd2,
        stkza        TYPE lfa1-stkza,
        stkzu        TYPE lfa1-stkzu,
        telbx        TYPE lfa1-telbx,
        telf1        TYPE lfa1-telf1,
        telf2        TYPE lfa1-telf2,
        telfx        TYPE lfa1-telfx,
        teltx        TYPE lfa1-teltx,
        telx1        TYPE lfa1-telx1,
        xcpdk        TYPE lfa1-xcpdk,
        xzemp        TYPE lfa1-xzemp,
        vbund        TYPE lfa1-vbund,
        fiskn        TYPE lfa1-fiskn,
        stceg        TYPE lfa1-stceg,
        stkzn        TYPE lfa1-stkzn,
        sperq        TYPE lfa1-sperq,
        gbort        TYPE lfa1-gbort,
        gbdat        TYPE lfa1-gbdat,
        sexkz        TYPE lfa1-sexkz,
        kraus        TYPE lfa1-kraus,
        revdb        TYPE lfa1-revdb,
        qssys        TYPE lfa1-qssys,
        ktock        TYPE lfa1-ktock,
        pfort        TYPE lfa1-pfort,
        werks        TYPE lfa1-werks,
        ltsna        TYPE lfa1-ltsna,
        werkr        TYPE lfa1-werkr,
        plkal        TYPE lfa1-plkal,
        duefl        TYPE lfa1-duefl,
        txjcd        TYPE lfa1-txjcd,
        sperz        TYPE lfa1-sperz,
        scacd        TYPE lfa1-scacd,
        sfrgr        TYPE lfa1-sfrgr,
        lzone        TYPE lfa1-lzone,
        xlfza        TYPE lfa1-xlfza,
        dlgrp        TYPE lfa1-dlgrp,
        fityp        TYPE lfa1-fityp,
        stcdt        TYPE lfa1-stcdt,
        regss        TYPE lfa1-regss,
        actss        TYPE lfa1-actss,
        stcd3        TYPE lfa1-stcd3,
        stcd4        TYPE lfa1-stcd4,
        stcd5        TYPE lfa1-stcd5,
        ipisp        TYPE lfa1-ipisp,
        taxbs        TYPE lfa1-taxbs,
        profs        TYPE lfa1-profs,
        stgdl        TYPE lfa1-stgdl,
        emnfr        TYPE lfa1-emnfr,
        lfurl        TYPE lfa1-lfurl,
        j_1kfrepre   TYPE lfa1-j_1kfrepre,
        j_1kftbus    TYPE lfa1-j_1kftbus,
        j_1kftind    TYPE lfa1-j_1kftind,
        confs        TYPE lfa1-confs,
        updat        TYPE lfa1-updat,
        uptim        TYPE lfa1-uptim,
        nodel        TYPE lfa1-nodel,
        qssysdat     TYPE lfa1-qssysdat,
        podkzb       TYPE lfa1-podkzb,
        fisku        TYPE lfa1-fisku,
        stenr        TYPE lfa1-stenr,
        carrier_conf TYPE lfa1-carrier_conf,
        min_comp     TYPE lfa1-min_comp,
        term_li      TYPE lfa1-term_li,
        crc_num      TYPE lfa1-crc_num,
        cvp_xblck    TYPE lfa1-cvp_xblck,
        rg           TYPE lfa1-rg,
        exp          TYPE lfa1-exp,
        uf           TYPE lfa1-uf,
        rgdate       TYPE lfa1-rgdate,
        ric          TYPE lfa1-ric,
        rne          TYPE lfa1-rne,
        rnedate      TYPE lfa1-rnedate,
        cnae         TYPE lfa1-cnae,
        legalnat     TYPE lfa1-legalnat,
        crtn         TYPE lfa1-crtn,
        icmstaxpay   TYPE lfa1-icmstaxpay,
        indtyp       TYPE lfa1-indtyp,
        tdt          TYPE lfa1-tdt,
        comsize      TYPE lfa1-comsize,
        decregpc     TYPE lfa1-decregpc,
      END OF ty_parceiros,

      BEGIN OF ty_dados_bancarios,
        banks         TYPE lfbk-banks,
        bankl         TYPE lfbk-bankl,
        bankn         TYPE lfbk-bankn,
        bkont         TYPE lfbk-bkont,
        bvtyp         TYPE lfbk-bvtyp,
        xezer         TYPE lfbk-xezer,
        bkref         TYPE lfbk-bkref,
        koinh         TYPE lfbk-koinh,
        ebpp_accname  TYPE lfbk-ebpp_accname,
        ebpp_bvstatus TYPE lfbk-ebpp_bvstatus,
        kovon         TYPE lfbk-kovon,
        kobis         TYPE lfbk-kobis,
        "banco         LIKE td_banco,
      END OF ty_dados_bancarios,
*
      BEGIN OF ty_dados_irf,
        bukrs     TYPE lfbw-bukrs,
        witht     TYPE lfbw-witht,
        wt_withcd TYPE lfbw-wt_withcd,
        wt_agent  TYPE knbw-wt_agent,
        wt_agtdf  TYPE knbw-wt_agtdf,
        wt_agtdt  TYPE knbw-wt_agtdt,
        wt_wtstcd TYPE lfbw-wt_wtstcd,
        wt_exnr   TYPE lfbw-wt_exnr,
        wt_exrt   TYPE lfbw-wt_exrt,
        wt_exdf   TYPE lfbw-wt_exdf,
        wt_exdt   TYPE lfbw-wt_exdt,
        wt_wtexrs TYPE lfbw-wt_wtexrs,
        wt_subjct TYPE lfbw-wt_subjct,
        qsrec     TYPE lfbw-qsrec,
      END OF ty_dados_irf,

      BEGIN OF ty_dados_empresa,
        bukrs TYPE lfb1-bukrs,
        pernr TYPE lfb1-pernr,
        sperr TYPE lfb1-sperr,
        loevm TYPE lfb1-loevm,
        zuawa TYPE lfb1-zuawa,
        akont TYPE lfb1-akont,
        begru TYPE lfb1-begru,
        vzskz TYPE lfb1-vzskz,
        zwels TYPE lfb1-zwels,
        xverr TYPE lfb1-xverr,
        zahls TYPE lfb1-zahls,
        zterm TYPE lfb1-zterm,
        eikto TYPE lfb1-eikto,
        zsabe TYPE lfb1-zsabe,
        kverm TYPE lfb1-kverm,
        fdgrv TYPE lfb1-fdgrv,
        busab TYPE lfb1-busab,
        lnrze TYPE lfb1-lnrze,
        lnrzb TYPE lfb1-lnrzb,
        zindt TYPE lfb1-zindt,
        zinrt TYPE lfb1-zinrt,
        datlz TYPE lfb1-datlz,
        xdezv TYPE lfb1-xdezv,
        webtr TYPE lfb1-webtr,
        kultg TYPE lfb1-kultg,
        reprf TYPE lfb1-reprf,
        togru TYPE lfb1-togru,
        hbkid TYPE lfb1-hbkid,
        xpore TYPE lfb1-xpore,
        qsznr TYPE lfb1-qsznr,
        qszdt TYPE lfb1-qszdt,
        qsskz TYPE lfb1-qsskz,
        blnkz TYPE lfb1-blnkz,
        mindk TYPE lfb1-mindk,
        altkn TYPE lfb1-altkn,
        zgrup TYPE lfb1-zgrup,
        mgrup TYPE lfb1-mgrup,
        uzawe TYPE lfb1-uzawe,
        qsrec TYPE lfb1-qsrec,
        qsbgr TYPE lfb1-qsbgr,
        qland TYPE lfb1-qland,
        xedip TYPE lfb1-xedip,
        frgrp TYPE lfb1-frgrp,
        togrr TYPE lfb1-togrr,
        tlfxs TYPE lfb1-tlfxs,
        intad TYPE lfb1-intad,
        xlfzb TYPE lfb1-xlfzb,
        guzte TYPE lfb1-guzte,
        gricd TYPE lfb1-gricd,
        gridt TYPE lfb1-gridt,
        xausz TYPE lfb1-xausz,
        cerdt TYPE lfb1-cerdt,
        confs TYPE lfb1-confs,
        updat TYPE lfb1-updat,
        uptim TYPE lfb1-uptim,
        nodel TYPE lfb1-nodel,
        tlfns TYPE lfb1-tlfns,
        avsnd TYPE lfb1-avsnd,
      END OF ty_dados_empresa.

    DATA:
      tb_parid TYPE TABLE OF char15,
      tb_bukrs TYPE TABLE OF bukrs,
      tb_GRUPO_CONTA TYPE TABLE OF KTOKD.

      data:  tb_email        TYPE TABLE OF ad_smtpadr,
             td_banco       TYPE bnka .
    "DATA:

types:
      begin of z_dados_bancarios,
       banks         TYPE lfbk-banks,
        bankl         TYPE lfbk-bankl,
        bankn         TYPE lfbk-bankn,
        bkont         TYPE lfbk-bkont,
        bvtyp         TYPE lfbk-bvtyp,
        xezer         TYPE lfbk-xezer,
        bkref         TYPE lfbk-bkref,
        koinh         TYPE lfbk-koinh,
        ebpp_accname  TYPE lfbk-ebpp_accname,
        ebpp_bvstatus TYPE lfbk-ebpp_bvstatus,
        kovon         TYPE lfbk-kovon,
        kobis         TYPE lfbk-kobis,
        banco          TYPE bnka,
  END OF z_dados_bancarios.

    DATA:
      BEGIN OF z_parceiro,
        dados_parceiro  TYPE ty_parceiros,
        dados_bancarios TYPE TABLE OF z_dados_bancarios,"ty_dados_bancarios,
        dados_irf       TYPE TABLE OF ty_dados_irf,
        dados_empresa   TYPE TABLE OF ty_dados_empresa,
        dados_email     like tb_email,
      END OF z_parceiro .
    DATA:
      BEGIN OF zde_data_request,
        parid        LIKE tb_parid,
        bukrs        LIKE tb_bukrs,
        GRUPO_CONTA  LIKE tb_GRUPO_CONTA,
        NAME_LIKE    TYPE NAME1_GP,
        CPF_LIKE     TYPE STCD2,
        CNPJ_LIKE    TYPE STCD1,
        IE_LIKE      TYPE STCD3,
        partyp(1),
        dados_bancarios(1),
        dados_irf(1),
        dados_empresa(1),
        dados_email(1),
      END OF zde_data_request .
    DATA:
      BEGIN OF zde_data_response,
        parceiros LIKE TABLE OF z_parceiro,
      END OF zde_data_response .
    CONSTANTS at_id_interface TYPE zde_id_interface VALUE '127' ##NO_TEXT.

    METHODS constructor
      RAISING
        zcx_integracao .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_PARCEIRO IMPLEMENTATION.


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

    IF lwa_data_request IS NOT INITIAL.

      IF lwa_data_request-parid IS INITIAL and
        lwa_data_request-NAME_LIKE is INITIAL and
        lwa_data_request-CPF_LIKE is INITIAL and
        lwa_data_request-CNPJ_LIKE is INITIAL and
        lwa_data_request-ie_like is INITIAL.

        r_msg_erro = 'Necessário informar ao menos uma informação do parceiro (Código, nome, CPF, CNPJ ou IE)'.
        RETURN.
      ENDIF.

      IF lwa_data_request-partyp IS INITIAL.
        r_msg_erro = 'O Tipo do Parceiro deve ser informado'.
        RETURN.
      ENDIF.

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

    "
    TYPES:

      BEGIN OF ty_parceiros,
        kunnr        TYPE kna1-kunnr,
        lifnr        TYPE lfa1-lifnr,
        land1        TYPE lfa1-land1,
        name1        TYPE lfa1-name1,
        name2        TYPE lfa1-name2,
        name3        TYPE lfa1-name3,
        name4        TYPE lfa1-name4,
        ort01        TYPE lfa1-ort01,
        ort02        TYPE lfa1-ort02,
        pfach        TYPE lfa1-pfach,
        pstl2        TYPE lfa1-pstl2,
        pstlz        TYPE lfa1-pstlz,
        regio        TYPE lfa1-regio,
        sortl        TYPE lfa1-sortl,
        stras        TYPE lfa1-stras,
        adrnr        TYPE lfa1-adrnr,
        mcod1        TYPE lfa1-mcod1,
        mcod2        TYPE lfa1-mcod2,
        mcod3        TYPE lfa1-mcod3,
        anred        TYPE lfa1-anred,
        bahns        TYPE lfa1-bahns,
        bbbnr        TYPE lfa1-bbbnr,
        bbsnr        TYPE lfa1-bbsnr,
        begru        TYPE lfa1-begru,
        brsch        TYPE lfa1-brsch,
        bubkz        TYPE lfa1-bubkz,
        datlt        TYPE lfa1-datlt,
        dtams        TYPE lfa1-dtams,
        dtaws        TYPE lfa1-dtaws,
        erdat        TYPE lfa1-erdat,
        ernam        TYPE lfa1-ernam,
        esrnr        TYPE lfa1-esrnr,
        konzs        TYPE lfa1-konzs,
        ktokk        TYPE lfa1-ktokk,
        ktokd        TYPE kna1-ktokd,
        lnrza        TYPE lfa1-lnrza,
        loevm        TYPE lfa1-loevm,
        sperr        TYPE lfa1-sperr,
        sperm        TYPE lfa1-sperm,
        spras        TYPE lfa1-spras,
        stcd1        TYPE lfa1-stcd1,
        stcd2        TYPE lfa1-stcd2,
        stkza        TYPE lfa1-stkza,
        stkzu        TYPE lfa1-stkzu,
        telbx        TYPE lfa1-telbx,
        telf1        TYPE lfa1-telf1,
        telf2        TYPE lfa1-telf2,
        telfx        TYPE lfa1-telfx,
        teltx        TYPE lfa1-teltx,
        telx1        TYPE lfa1-telx1,
        xcpdk        TYPE lfa1-xcpdk,
        xzemp        TYPE lfa1-xzemp,
        vbund        TYPE lfa1-vbund,
        fiskn        TYPE lfa1-fiskn,
        stceg        TYPE lfa1-stceg,
        stkzn        TYPE lfa1-stkzn,
        sperq        TYPE lfa1-sperq,
        gbort        TYPE lfa1-gbort,
        gbdat        TYPE lfa1-gbdat,
        sexkz        TYPE lfa1-sexkz,
        kraus        TYPE lfa1-kraus,
        revdb        TYPE lfa1-revdb,
        qssys        TYPE lfa1-qssys,
        ktock        TYPE lfa1-ktock,
        pfort        TYPE lfa1-pfort,
        werks        TYPE lfa1-werks,
        ltsna        TYPE lfa1-ltsna,
        werkr        TYPE lfa1-werkr,
        plkal        TYPE lfa1-plkal,
        duefl        TYPE lfa1-duefl,
        txjcd        TYPE lfa1-txjcd,
        sperz        TYPE lfa1-sperz,
        scacd        TYPE lfa1-scacd,
        sfrgr        TYPE lfa1-sfrgr,
        lzone        TYPE lfa1-lzone,
        xlfza        TYPE lfa1-xlfza,
        dlgrp        TYPE lfa1-dlgrp,
        fityp        TYPE lfa1-fityp,
        stcdt        TYPE lfa1-stcdt,
        regss        TYPE lfa1-regss,
        actss        TYPE lfa1-actss,
        stcd3        TYPE lfa1-stcd3,
        stcd4        TYPE lfa1-stcd4,
        stcd5        TYPE lfa1-stcd5,
        ipisp        TYPE lfa1-ipisp,
        taxbs        TYPE lfa1-taxbs,
        profs        TYPE lfa1-profs,
        stgdl        TYPE lfa1-stgdl,
        emnfr        TYPE lfa1-emnfr,
        lfurl        TYPE lfa1-lfurl,
        j_1kfrepre   TYPE lfa1-j_1kfrepre,
        j_1kftbus    TYPE lfa1-j_1kftbus,
        j_1kftind    TYPE lfa1-j_1kftind,
        confs        TYPE lfa1-confs,
        updat        TYPE lfa1-updat,
        uptim        TYPE lfa1-uptim,
        nodel        TYPE lfa1-nodel,
        qssysdat     TYPE lfa1-qssysdat,
        podkzb       TYPE lfa1-podkzb,
        fisku        TYPE lfa1-fisku,
        stenr        TYPE lfa1-stenr,
        carrier_conf TYPE lfa1-carrier_conf,
        min_comp     TYPE lfa1-min_comp,
        term_li      TYPE lfa1-term_li,
        crc_num      TYPE lfa1-crc_num,
        cvp_xblck    TYPE lfa1-cvp_xblck,
        rg           TYPE lfa1-rg,
        exp          TYPE lfa1-exp,
        uf           TYPE lfa1-uf,
        rgdate       TYPE lfa1-rgdate,
        ric          TYPE lfa1-ric,
        rne          TYPE lfa1-rne,
        rnedate      TYPE lfa1-rnedate,
        cnae         TYPE lfa1-cnae,
        legalnat     TYPE lfa1-legalnat,
        crtn         TYPE lfa1-crtn,
        icmstaxpay   TYPE lfa1-icmstaxpay,
        indtyp       TYPE lfa1-indtyp,
        tdt          TYPE lfa1-tdt,
        comsize      TYPE lfa1-comsize,
        decregpc     TYPE lfa1-decregpc,
      END OF ty_parceiros,


      BEGIN OF ty_dados_bancarios,
        lifnr         TYPE lfbk-lifnr,
        kunnr         TYPE knbk-kunnr,
        banks         TYPE lfbk-banks,
        bankl         TYPE lfbk-bankl,
        bankn         TYPE lfbk-bankn,
        bkont         TYPE lfbk-bkont,
        bvtyp         TYPE lfbk-bvtyp,
        xezer         TYPE lfbk-xezer,
        bkref         TYPE lfbk-bkref,
        koinh         TYPE lfbk-koinh,
        ebpp_accname  TYPE lfbk-ebpp_accname,
        ebpp_bvstatus TYPE lfbk-ebpp_bvstatus,
        kovon         TYPE lfbk-kovon,
        kobis         TYPE lfbk-kobis,
      END OF ty_dados_bancarios,

      BEGIN OF ty_dados_irf,
        lifnr     TYPE lfbw-lifnr,
        kunnr     TYPE knbw-kunnr,
        bukrs     TYPE lfbw-bukrs,
        witht     TYPE lfbw-witht,
        wt_withcd TYPE lfbw-wt_withcd,
        wt_agent  TYPE knbw-wt_agent,
        wt_agtdf  TYPE knbw-wt_agtdf,
        wt_agtdt  TYPE knbw-wt_agtdt,
        wt_wtstcd TYPE lfbw-wt_wtstcd,
        wt_exnr   TYPE lfbw-wt_exnr,
        wt_exrt   TYPE lfbw-wt_exrt,
        wt_exdf   TYPE lfbw-wt_exdf,
        wt_exdt   TYPE lfbw-wt_exdt,
        wt_wtexrs TYPE lfbw-wt_wtexrs,
        wt_subjct TYPE lfbw-wt_subjct,
        qsrec     TYPE lfbw-qsrec,
      END OF ty_dados_irf,

      BEGIN OF ty_dados_empresa,
        lifnr TYPE lfb1-lifnr,
        kunnr TYPE knb1-kunnr,
        bukrs TYPE lfb1-bukrs,
        pernr TYPE lfb1-pernr,
        sperr TYPE lfb1-sperr,
        loevm TYPE lfb1-loevm,
        zuawa TYPE lfb1-zuawa,
        akont TYPE lfb1-akont,
        begru TYPE lfb1-begru,
        vzskz TYPE lfb1-vzskz,
        zwels TYPE lfb1-zwels,
        xverr TYPE lfb1-xverr,
        zahls TYPE lfb1-zahls,
        zterm TYPE lfb1-zterm,
        eikto TYPE lfb1-eikto,
        zsabe TYPE lfb1-zsabe,
        kverm TYPE lfb1-kverm,
        fdgrv TYPE lfb1-fdgrv,
        busab TYPE lfb1-busab,
        lnrze TYPE lfb1-lnrze,
        lnrzb TYPE lfb1-lnrzb,
        zindt TYPE lfb1-zindt,
        zinrt TYPE lfb1-zinrt,
        datlz TYPE lfb1-datlz,
        xdezv TYPE lfb1-xdezv,
        webtr TYPE lfb1-webtr,
        kultg TYPE lfb1-kultg,
        reprf TYPE lfb1-reprf,
        togru TYPE lfb1-togru,
        hbkid TYPE lfb1-hbkid,
        xpore TYPE lfb1-xpore,
        qsznr TYPE lfb1-qsznr,
        qszdt TYPE lfb1-qszdt,
        qsskz TYPE lfb1-qsskz,
        blnkz TYPE lfb1-blnkz,
        mindk TYPE lfb1-mindk,
        altkn TYPE lfb1-altkn,
        zgrup TYPE lfb1-zgrup,
        mgrup TYPE lfb1-mgrup,
        uzawe TYPE lfb1-uzawe,
        qsrec TYPE lfb1-qsrec,
        qsbgr TYPE lfb1-qsbgr,
        qland TYPE lfb1-qland,
        xedip TYPE lfb1-xedip,
        frgrp TYPE lfb1-frgrp,
        togrr TYPE lfb1-togrr,
        tlfxs TYPE lfb1-tlfxs,
        intad TYPE lfb1-intad,
        xlfzb TYPE lfb1-xlfzb,
        guzte TYPE lfb1-guzte,
        gricd TYPE lfb1-gricd,
        gridt TYPE lfb1-gridt,
        xausz TYPE lfb1-xausz,
        cerdt TYPE lfb1-cerdt,
        confs TYPE lfb1-confs,
        updat TYPE lfb1-updat,
        uptim TYPE lfb1-uptim,
        nodel TYPE lfb1-nodel,
        tlfns TYPE lfb1-tlfns,
        avsnd TYPE lfb1-avsnd,
      END OF ty_dados_empresa.

*
    DATA: tl_parceiros       TYPE TABLE OF ty_parceiros,
          tl_dados_bancarios TYPE TABLE OF ty_dados_bancarios,
          lt_dados_irf       TYPE TABLE OF ty_dados_irf,
          lt_dados_empresa   TYPE TABLE OF ty_dados_empresa.

    DATA v_cliente(1).
    DATA: w_dados_empresa   TYPE zde_data_response-dados_empresa,
          w_dados_bancarios TYPE zde_data_response-dados_bancarios,
          w_dados_irf       TYPE zde_data_response-dados_irf,

          lra_filtro1       TYPE RANGE OF kna1-ktokd,
          lra_parid         TYPE RANGE OF kna1-kunnr,
          lra_bukrs         TYPE RANGE OF lfb1-bukrs.

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

    LOOP AT lwa_data_request-grupo_conta INTO DATA(wa_filtro1).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_filtro1 ) TO lra_filtro1.
    ENDLOOP.

    LOOP AT lwa_data_request-parid INTO DATA(wa_parid).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_parid ) TO lra_parid.
    ENDLOOP.

     LOOP AT lwa_data_request-bukrs INTO DATA(wa_bukrs).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_bukrs ) TO lra_bukrs.
    ENDLOOP.

    DATA: v_where TYPE string,
          v_name  TYPE name1_gp,
          v_cpf   TYPE char20,
          v_cnpj  TYPE char20,
          v_ie    TYPE char20.
    IF lwa_data_request-partyp EQ 'C'.
      CONCATENATE 'kunnr in @lra_parid AND '
                  ' KTOKD IN @lra_filtro1'
      INTO v_where.
    ELSE.
      CONCATENATE 'lifnr in @lra_parid AND '
                  ' ktokk IN @lra_filtro1'
      INTO v_where.

    ENDIF.

    IF lwa_data_request-dados_empresa EQ 'S' AND lra_bukrs[] IS NOT INITIAL.
      CASE lwa_data_request-partyp.
        WHEN 'C'.
          CONCATENATE v_where ' and exists ( select bukrs from knb1 as l where l~kunnr = a~kunnr and l~bukrs in @lra_bukrs )' INTO v_where.
        WHEN 'F'.
          CONCATENATE v_where ' and exists ( select bukrs from lfb1 as l where l~lifnr = a~lifnr and l~bukrs in @lra_bukrs )' INTO v_where.
      ENDCASE.
    ENDIF.

    IF lwa_data_request-name_like IS NOT INITIAL.
      v_name = |'%{ lwa_data_request-name_like }%'|.
      CONCATENATE v_where ' and NAME1 LIKE '  v_name  INTO v_where SEPARATED BY space.
    ENDIF.

    IF lwa_data_request-cpf_like IS NOT INITIAL.
      v_cpf = |'%{ lwa_data_request-cpf_like }%'|.
      CONCATENATE v_where ' and STCD2 LIKE '  v_cpf   INTO v_where SEPARATED BY space.
    ENDIF.

    IF lwa_data_request-cnpj_like IS NOT INITIAL.
      v_cnpj = |'%{ lwa_data_request-cnpj_like }%'|.
      CONCATENATE v_where ' and  STCD1 LIKE '  v_cnpj  INTO v_where SEPARATED BY space.
    ENDIF.

    IF lwa_data_request-ie_like IS NOT INITIAL.
      v_ie = |'%{ lwa_data_request-ie_like }%'|.
      CONCATENATE v_where ' and STCD3 LIKE '  v_ie   INTO v_where SEPARATED BY space.
    ENDIF.

    IF lwa_data_request-partyp EQ 'C'.

      SELECT * FROM kna1 as a
      INTO CORRESPONDING FIELDS OF TABLE @tl_parceiros
      WHERE (v_where).

      v_cliente = 'X'.

    ELSE.

      SELECT * FROM lfa1 as a
      INTO CORRESPONDING FIELDS OF TABLE @tl_parceiros
      WHERE (v_where).


    ENDIF.
    IF  tl_parceiros[] IS NOT INITIAL.
      IF lwa_data_request-dados_bancarios EQ 'S'.

        IF v_cliente IS NOT INITIAL.

          SELECT *
          FROM  knbk
          INTO CORRESPONDING FIELDS OF TABLE tl_dados_bancarios
          FOR ALL ENTRIES IN tl_parceiros
          WHERE kunnr = tl_parceiros-kunnr.

        ELSE.

          SELECT *
          FROM  lfbk
          INTO CORRESPONDING FIELDS OF TABLE tl_dados_bancarios
          FOR ALL ENTRIES IN tl_parceiros
          WHERE lifnr = tl_parceiros-lifnr.
        ENDIF.

        IF tl_dados_bancarios[] IS NOT INITIAL.
          SELECT * FROM bnka INTO TABLE @DATA(tl_bnka)
            FOR ALL ENTRIES IN @tl_dados_bancarios
            WHERE banks = @tl_dados_bancarios-banks AND
                  bankl = @tl_dados_bancarios-bankl.
        ENDIF.

      ENDIF.

      IF lwa_data_request-dados_irf EQ 'S'.

        IF v_cliente IS NOT INITIAL.

          SELECT *
          FROM knbw
          INTO CORRESPONDING FIELDS OF TABLE lt_dados_irf
          FOR ALL ENTRIES IN tl_parceiros
          WHERE kunnr = tl_parceiros-kunnr.

        ELSE.

          SELECT *
          FROM lfbw
          INTO CORRESPONDING FIELDS OF TABLE lt_dados_irf
          FOR ALL ENTRIES IN tl_parceiros
          WHERE lifnr = tl_parceiros-lifnr.

        ENDIF.

      ENDIF.

      IF lwa_data_request-dados_empresa EQ 'S'.

        IF v_cliente IS NOT INITIAL.

          SELECT *
          FROM  knb1
          INTO CORRESPONDING FIELDS OF TABLE lt_dados_empresa
          FOR ALL ENTRIES IN tl_parceiros
          WHERE kunnr = tl_parceiros-kunnr
            AND BUKRS IN lra_bukrs.

        ELSE.

          SELECT *
          FROM  lfb1
          INTO CORRESPONDING FIELDS OF TABLE lt_dados_empresa
          FOR ALL ENTRIES IN tl_parceiros
          WHERE lifnr = tl_parceiros-lifnr
            AND BUKRS IN lra_bukrs.

        ENDIF.

      ENDIF.

      IF   lwa_data_request-dados_email EQ 'S'.

        SELECT addrnumber, smtp_addr
                FROM  adr6
                INTO TABLE @DATA(lt_email)
                FOR ALL ENTRIES IN @tl_parceiros
                WHERE addrnumber = @tl_parceiros-adrnr.

      ENDIF.

      LOOP AT tl_parceiros ASSIGNING FIELD-SYMBOL(<fs_parceiros>).

        APPEND INITIAL LINE TO lwa_data_response-parceiros ASSIGNING FIELD-SYMBOL(<fs_response>).


        MOVE-CORRESPONDING  <fs_parceiros> TO <fs_response>-dados_parceiro.
        IF lwa_data_request-partyp EQ 'C'.
          <fs_response>-dados_parceiro-parid = <fs_parceiros>-kunnr.
        ELSE.
          <fs_response>-dados_parceiro-parid = <fs_parceiros>-lifnr.
        ENDIF.

*        IF tl_knbk[] IS NOT INITIAL.
        LOOP AT tl_dados_bancarios ASSIGNING FIELD-SYMBOL(<fs_knbk>) WHERE  lifnr = <fs_response>-dados_parceiro-parid OR
                                                                            kunnr = <fs_response>-dados_parceiro-parid.


          READ TABLE  tl_bnka INTO DATA(w_bnka) WITH KEY banks = <fs_knbk>-banks
                                                         bankl = <fs_knbk>-bankl.

          IF sy-subrc IS INITIAL.
            MOVE-CORRESPONDING  w_bnka TO w_dados_bancarios-banco.
            "APPEND w_dados_bancarios-banco TO <fs_response>-dados_bancarios.
          ENDIF.

          MOVE-CORRESPONDING  <fs_knbk> TO w_dados_bancarios.
          APPEND w_dados_bancarios TO <fs_response>-dados_bancarios.
          CLEAR w_dados_bancarios.

        ENDLOOP.

        IF lt_dados_irf[] IS NOT INITIAL.
          LOOP AT lt_dados_irf ASSIGNING FIELD-SYMBOL(<fs_dados_irf>) WHERE lifnr = <fs_response>-dados_parceiro-parid OR
                                                                            kunnr = <fs_response>-dados_parceiro-parid.

            MOVE-CORRESPONDING <fs_dados_irf> TO w_dados_irf.
            APPEND w_dados_irf TO <fs_response>-dados_irf .
            CLEAR w_dados_irf.

          ENDLOOP.
        ENDIF.

        IF lt_dados_empresa[] IS NOT INITIAL.
          LOOP AT lt_dados_empresa ASSIGNING FIELD-SYMBOL(<fs_dados_empresa>) WHERE lifnr = <fs_response>-dados_parceiro-parid OR
                                                                                    kunnr = <fs_response>-dados_parceiro-parid.
            MOVE-CORRESPONDING <fs_dados_empresa> TO w_dados_empresa. "<fs_response>-dados_empresa.
            APPEND w_dados_empresa TO <fs_response>-dados_empresa .
            CLEAR w_dados_empresa.

          ENDLOOP.
        ENDIF.


        IF lt_email[] IS NOT INITIAL.
          LOOP AT lt_email ASSIGNING FIELD-SYMBOL(<fs_email>) WHERE addrnumber = <fs_parceiros>-adrnr.
            APPEND <fs_email> TO <fs_response>-dados_email.
            "MOVE-CORRESPONDING <fs_email> to <fs_response>-dados_email.
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
