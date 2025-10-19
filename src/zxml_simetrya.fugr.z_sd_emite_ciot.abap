FUNCTION z_sd_emite_ciot.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_CTE_AVULSO) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(P_TELA_VISUALIZA) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(P_CD_CIOT) TYPE  ZCIOT OPTIONAL
*"     REFERENCE(EMITIR_VIAGEM_ADM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(CREDITAR_VIAGEM_ADM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(RESCINDIR_VIAGEM_ADM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(CONSULTAR_STATUS_VIAGEM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_FATURAMENTO_AUTOM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_CH_REFERENCIA) TYPE  ZCH_REF OPTIONAL
*"  CHANGING
*"     VALUE(P_PROTOCOLO) TYPE  ZPROTOCOLO OPTIONAL
*"  EXCEPTIONS
*"      SEM_DADOS_CIOT
*"      NAO_CIOT
*"      ERRO_STATUS
*"      ERRO_WEB_SERVICE
*"      ERRO_STATUS_CRED
*"      ERRO_STATUS_CANC
*"      ERRO_SOLICITACAO
*"----------------------------------------------------------------------

  DATA: viagem       TYPE REF TO zcl_ciot_viagem,
        wa_cte_ciot  TYPE zcte_ciot,
        wa_j_1bnfdoc TYPE j_1bnfdoc,
        p_emite_ciot TYPE char01,
        p_tknum      TYPE tknum.

  vg_faturamento_autom = p_faturamento_autom. "*-#133089-21.02.2024-JT
  vg_ch_referencia     = p_ch_referencia.     "*-#133089-21.02.2024-JT

*-#133089-12.02.2024-JT-inicio
  IF vg_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  IF ( p_cd_ciot IS NOT INITIAL ) AND ( p_cte_avulso IS INITIAL ).
    SELECT SINGLE docnum INTO p_cte_avulso
      FROM zcte_ciot
     WHERE cd_ciot EQ p_cd_ciot.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE e028(zciot) WITH p_cd_ciot RAISING sem_dados_ciot.
    ENDIF.
  ELSE.

    SELECT SINGLE * INTO cte_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ p_cte_avulso.

    MOVE cte_j_1bnfdoc TO wa_j_1bnfdoc.

    CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
      EXPORTING
        p_docnum = wa_j_1bnfdoc-docnum
      CHANGING
        p_bukrs  = wa_j_1bnfdoc-bukrs
        p_parid  = wa_j_1bnfdoc-parid
        p_partyp = wa_j_1bnfdoc-partyp
        p_tknum  = p_tknum.

    CALL FUNCTION 'Z_CIOT_EMPRESA_PARCEIRO'
      EXPORTING
        p_empresa    = wa_j_1bnfdoc-bukrs
        p_partyp     = wa_j_1bnfdoc-partyp
        p_parid      = wa_j_1bnfdoc-parid
        p_dt_posicao = wa_j_1bnfdoc-docdat
        p_tknum      = p_tknum
      IMPORTING
        p_emite      = p_emite_ciot.

    CHECK p_emite_ciot EQ 'X'.
  ENDIF.

*-US 140617-30.12.2024-#140617-JT-inicio
  CREATE OBJECT lc_integra_tip.
  lc_integra_tip->set_referencia( CONV #( p_cte_avulso ) ).
*-US 140617-30.12.2024-#140617-JT-fim

  IF p_emite_ciot EQ 'X'.

    SELECT SINGLE * INTO cte_j_1bnfdoc
      FROM j_1bnfdoc
     WHERE docnum EQ p_cte_avulso.

    MOVE cte_j_1bnfdoc TO wa_j_1bnfdoc.

    CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
      EXPORTING
        p_docnum = wa_j_1bnfdoc-docnum
      CHANGING
        p_bukrs  = wa_j_1bnfdoc-bukrs
        p_parid  = wa_j_1bnfdoc-parid
        p_partyp = wa_j_1bnfdoc-partyp
        p_tknum  = p_tknum.

    CALL FUNCTION 'Z_CIOT_EMPRESA_PARCEIRO'
      EXPORTING
        p_empresa    = wa_j_1bnfdoc-bukrs
        p_partyp     = wa_j_1bnfdoc-partyp
        p_parid      = wa_j_1bnfdoc-parid
        p_dt_posicao = wa_j_1bnfdoc-docdat
        p_tknum      = p_tknum
      IMPORTING
        p_emite      = p_emite_ciot.

  ENDIF.

  IF p_tela_visualiza IS NOT INITIAL.

    CALL FUNCTION 'Z_SD_INFO_CTE_AVULSO'
      EXPORTING
        p_cte_avulso      = p_cte_avulso
        p_chamar_tela     = space
        p_gravar_dados    = c_x
      TABLES
        it_ciot           = it_cte_ciot
        it_ciot_parceiros = cte_ciot_parceiros.

    CLEAR: cte_dynnr_000, cte_bloqueado.

    CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
      EXPORTING
        p_docnum       = p_cte_avulso
        p_uso          = 'N'
      CHANGING
        p_cabec        = cte_j_1bnfdoc
        p_active       = cte_active
      EXCEPTIONS
        cancelado      = 1
        nao_cancelado  = 2
        pendente       = 3
        nao_concluido  = 4
        nao_existe     = 5
        autorizado_uso = 6
        denegado       = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      cte_bloqueado = c_x.
    ENDIF.

    IF sy-subrc EQ 6.
      cte_autorizado_uso = abap_true.
    ELSE.
      cte_autorizado_uso = abap_false.
    ENDIF.

    LOOP AT it_cte_ciot INTO cte_ciot.
      "Pendente
      "Rejeitado
      IF cte_ciot-st_ciot NE '0' AND cte_ciot-st_ciot NE '3'.
        cte_bloqueado = c_x.
      ENDIF.
    ENDLOOP.

*-#133089-12.02.2024-JT-inicio
    IF vg_faturamento_autom = abap_true.
      PERFORM gerar_solicitacao_fatauto.
    ELSE.
      CALL SCREEN 9970 STARTING AT 07 05 ENDING AT 115 20.
    ENDIF.
*-#133089-12.02.2024-JT-fim

  ELSE.

    CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
      EXPORTING
        p_docnum       = p_cte_avulso
        p_uso          = 'N'
      CHANGING
        p_cabec        = cte_j_1bnfdoc
        p_active       = cte_active
      EXCEPTIONS
        cancelado      = 1
        nao_cancelado  = 2
        pendente       = 3
        nao_concluido  = 4
        nao_existe     = 5
        autorizado_uso = 6
        denegado       = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN abap_true.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

    CREATE OBJECT viagem.

    CALL METHOD viagem->popula_ciot
      EXPORTING
        cte_docnum              = p_cte_avulso
        emitir_viagem_adm       = emitir_viagem_adm
        creditar_viagem_adm     = creditar_viagem_adm
        rescindir_viagem_adm    = rescindir_viagem_adm
        consultar_status_viagem = consultar_status_viagem
        i_faturamento_autom     = p_faturamento_autom "*-#133089-21.02.2024-JT-inicio
        i_ch_referencia         = p_ch_referencia     "*-#133089-21.02.2024-JT-inicio
      IMPORTING
        p_protocolo             = p_protocolo
      EXCEPTIONS
        nao_ciot                = 1
        erro_status             = 2
        erro_status_cred        = 3
        erro_status_canc        = 4
        erro_web_service        = 5
        erro_solicitacao        = 6
        erro_xml_solicita       = 7
        OTHERS                  = 8.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e008(zciot) WITH p_cte_avulso RAISING sem_dados_ciot.
      WHEN 2.
        MESSAGE e012(zciot) WITH sy-msgv1 RAISING erro_status.
      WHEN 3.
        MESSAGE e017(zciot) WITH sy-msgv1 RAISING erro_status_cred.
      WHEN 4.
        MESSAGE e019(zciot) WITH sy-msgv1 RAISING erro_status_canc.
      WHEN 5.
        MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
      WHEN 6 OR 7.
        MESSAGE e000(zciot) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_solicitacao.
    ENDCASE.

  ENDIF.

ENDFUNCTION.
