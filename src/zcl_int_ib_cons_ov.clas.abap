class ZCL_INT_IB_CONS_OV definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  DATA:
      BEGIN OF z_ORDENS,
        "ORDENS  TYPE VBAK,
        VBELN       type  VBELN_VA          ,
ERDAT type  ERDAT                 ,
ERZET type  ERZET                 ,
ERNAM type  ERNAM                 ,
ANGDT type  ANGDT_V               ,
BNDDT type  BNDDT                 ,
AUDAT type  AUDAT                 ,
VBTYP type  VBTYP                 ,
TRVOG type  TRVOG                 ,
AUART type  AUART                 ,
AUGRU type  AUGRU                 ,
GWLDT type  GWLDT                 ,
SUBMI type  SUBMI_SD              ,
LIFSK type  LIFSK                 ,
FAKSK type  FAKSK                 ,
NETWR type  NETWR_AK              ,
WAERK type  WAERK                 ,
VKORG type  VKORG                 ,
VTWEG type  VTWEG                 ,
SPART type  SPART                 ,
VKGRP type  VKGRP                 ,
VKBUR type  VKBUR                 ,
GSBER type  GSBER                 ,
GSKST type  GSKST                 ,
GUEBG type  GUEBG                 ,
GUEEN type  GUEEN                 ,
KNUMV type  KNUMV                 ,
VDATU type  EDATU_VBAK            ,
VPRGR type  PRGRS_VBAK            ,
AUTLF type  AUTLF                 ,
VBKLA type  VBKLA                 ,
VBKLT type  VBKLT                 ,
KALSM type  KALSMASD              ,
VSBED type  VSBED                 ,
FKARA type  FKARA                 ,
AWAHR type  AWAHR_AK              ,
KTEXT type  KTEXT_V               ,
BSTNK type  BSTNK                 ,
BSARK type  BSARK                 ,
BSTDK type  BSTDK                 ,
BSTZD type  BSTZD                 ,
IHREZ type  IHREZ                 ,
BNAME type  BNAME_V               ,
TELF1 type  TELF1_VP              ,
MAHZA type  MAHZA                 ,
MAHDT type  MAHDT                 ,
KUNNR type  KUNAG                 ,
KOSTL type  KOSTL                 ,
STAFO type  STAFO                 ,
STWAE type  STWAE                 ,
AEDAT type  AEDAT                 ,
KVGR1 type  KVGR1                 ,
KVGR2 type  KVGR2                 ,
KVGR3 type  KVGR3                 ,
KVGR4 type  KVGR4                 ,
KVGR5 type  KVGR5                 ,
KNUMA type  KNUMA                 ,
KOKRS type  KOKRS                 ,
PS_PSP_PNR  type  PS_PSP_PNR        ,
KURST type  KURST                 ,
KKBER type  KKBER                 ,
KNKLI type  KNKLI                 ,
GRUPP type  GRUPP_CM              ,
SBGRP type  SBGRP_CM              ,
CTLPC type  CTLPC_CM              ,
CMWAE type  WAERS_CM              ,
CMFRE type  CMFRE                 ,
CMNUP type  CMNUP                 ,
CMNGV type  CMNGV                 ,
AMTBL type  AMTBL_CM              ,
HITYP_PR  type  HITYP_PR          ,
ABRVW type  ABRVW                 ,
ABDIS type  ABDIS                 ,
VGBEL type  VGBEL                 ,
OBJNR type  OBJKO                 ,
BUKRS_VF  type  BUKRS_VF          ,
TAXK1 type  TAXK1_AK              ,
TAXK2 type  TAXK2                 ,
TAXK3 type  TAXK3                 ,
TAXK4 type  TAXK4                 ,
TAXK5 type  TAXK5                 ,
TAXK6 type  TAXK6                 ,
TAXK7 type  TAXK7                 ,
TAXK8 type  TAXK8                 ,
TAXK9 type  TAXK9                 ,
XBLNR type  XBLNR_V1              ,
ZUONR type  ORDNR_V               ,
VGTYP type  VBTYP_V               ,
KALSM_CH  type  KALSMA_CH         ,
AGRZR type  AGRZR                 ,
AUFNR type  AUFNR                 ,
QMNUM type  QMNUM                 ,
VBELN_GRP type  VBELN_GRP         ,
SCHEME_GRP  type  SCHEME_GRP        ,
ABRUF_PART  type  ABRUF_PART        ,
ABHOD type  ABHOD                 ,
ABHOV type  ABHOZ                 ,
ABHOB type  ABHOZ                 ,
RPLNR type  RPLNR                 ,
VZEIT type  EZEIT_VBAK            ,
STCEG_L type  LAND1TX               ,
LANDTX  type  LANDTX                ,
XEGDR type  XEGDR                 ,
ENQUEUE_GRP type  ENQUEUE_GRP       ,
DAT_FZAU  type  DAT_FZAU          ,
FMBDAT  type  MBDAT                 ,
VSNMR_V type  VSNMR_V               ,
HANDLE  type  TSEGGUID_VBAK         ,
PROLI type  ADGE_PROLI            ,
CONT_DG type  ADGE_NCDG             ,
CRM_GUID  type  CHAR70            ,
UPD_TMSTMP  type  TIMESTAMPL        ,
MSR_ID  type  MSR_PROCESS_ID        ,
TM_CTRL_KEY type  TM_CTRL_KEY       ,
HANDOVERLOC type  HANDOVER_LOC      ,
PSM_BUDAT type  PSM_BUDAT         ,
SWENR type  SWENR, "HB_SWENR              ,
SMENR type  SMENR, "HB_SMENR              ,
PHASE type  ZHB_SPHSE, "HB_SPHSE              ,
MTLAUR  type  CRM_EWA_DHB_MTLAUR, "HB_MTLAUR             ,
STAGE type  STAGE, "HB_STAGE              ,
HB_CONT_REASON  type  CRM_EWA_DHB_CONT, "HB_CONT       ,
HB_EXPDATE  type  datum, "HB_EXP            ,
HB_RESDATE  type  datum, "HB_RES            ,
MILL_APPL_ID  type  MILL_APPL_ID  ,
TAS type  FMFG_TAS                  ,
BETC  type  FMFG_BETC             ,
MOD_ALLOW type  FMFG_MOD_ALLOW    ,
CANCEL_ALLOW  type  FMFG_CANCEL_ALLOW ,
PAY_METHOD  type  DZWELS,
BPN type  FMFG_BPN,
REP_FREQ  type  FMFG_REP_FREQ,
LOGSYSB type  LOGSYSB,
KALCD type  KALCD,
MULTI type  CMPC_MULT_CAMPAIGN,
SPPAYM  type  SPPAYM,
WTYSC_CLM_HDR type  WTYSC_CLM_HDR,
ZTROCANOTA  type  ZDE_TROCA_NOTA,
ZPESAGEM  type  ZPESAGEM,
TKNUM type  TKNUM,
ZLZONE_PC type  LZONE,
ZLZONE_LR type  LZONE,
        ITENS TYPE TABLE OF VBAP,
      END OF z_ORDENS .

  data:
    tb_ordens TYPE TABLE OF char15 .


  data:
    BEGIN OF zde_data_request,
        ORDENS        LIKE tb_ordens,
      END OF zde_data_request .
  data:
    BEGIN OF zde_data_response,
        ORDENS LIKE TABLE OF z_ORDENS,
      END OF zde_data_response .
  constants AT_ID_INTERFACE type ZDE_ID_INTERFACE value '188' ##NO_TEXT.

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_CONS_OV IMPLEMENTATION.


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

    IF lwa_data_request IS NOT INITIAL.

      IF
        lwa_data_request-ordens is INITIAL.

        r_msg_erro = 'Necessário informar ao menos uma Ordem de venda'.
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


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_INBOUND.

    DATA: lwa_data_request  LIKE zde_data_request,
          lwa_data_response LIKE zde_data_response.

*
*    DATA: tl_parceiros       TYPE TABLE OF ty_parceiros,
*          tl_dados_bancarios TYPE TABLE OF ty_dados_bancarios,
*          lt_dados_irf       TYPE TABLE OF ty_dados_irf,
*          lt_dados_empresa   TYPE TABLE OF ty_dados_empresa.
*
*    DATA v_cliente(1).
*    DATA: w_dados_empresa   TYPE zde_data_response-dados_empresa,
*          w_dados_bancarios TYPE zde_data_response-dados_bancarios,
*          w_dados_irf       TYPE zde_data_response-dados_irf,

          DATA: lra_ORDENS       TYPE RANGE OF VBAP-vbeln.

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

    LOOP AT lwa_data_request-ordens INTO DATA(wa_ORDENS).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_ordens ) TO lra_ordens.
    ENDLOOP.


select * from vbak
  into TABLE @data(it_vbak)
  WHERE vbeln in @lra_ordens.


  select * from vbap
    into TABLE @data(it_vbap)
  WHERE vbeln in @lra_ordens.


    if it_vbak[] is not INITIAL.

     LOOP AT it_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>).
       APPEND INITIAL LINE TO lwa_data_response-ordens ASSIGNING FIELD-SYMBOL(<fs_response>).


        MOVE-CORRESPONDING  <fs_vbak> TO <fs_response>.

        LOOP AT it_vbap ASSIGNING FIELD-SYMBOL(<fs_vbap>) WHERE  vbeln = <fs_response>-vbeln.
           APPEND <fs_vbap> TO <fs_response>-itens.
          ENDLOOP.

      ENDLOOP.

      endif.





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
