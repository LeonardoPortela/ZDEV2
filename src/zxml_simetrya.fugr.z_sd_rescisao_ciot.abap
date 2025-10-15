FUNCTION z_sd_rescisao_ciot.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"  EXPORTING
*"     VALUE(P_PROTOCOLO) TYPE  ZPROTOCOLO
*"  EXCEPTIONS
*"      SEM_DADOS_CIOT
*"      ERRO_STATUS_CANC
*"      ERRO_WEB_SERVICE
*"      ERRO_SOLICITACAO
*"----------------------------------------------------------------------

  DATA: viagem       TYPE REF TO zcl_ciot_viagem,
        wa_cte_ciot  TYPE zcte_ciot,
        p_autorizado TYPE  char01.

*-US 140617-30.12.2024-#140617-JT-inicio
  CREATE OBJECT lc_integra_tip.
  lc_integra_tip->set_referencia( CONV #( p_cte_avulso ) ).
*-US 140617-30.12.2024-#140617-JT-fim

  CREATE OBJECT viagem.

  CALL METHOD viagem->popula_ciot
    EXPORTING
      cte_docnum           = p_cte_avulso
      rescindir_viagem_adm = c_x
    IMPORTING
      p_protocolo          = p_protocolo
    EXCEPTIONS
      nao_ciot             = 1
      erro_status_canc     = 2
      erro_web_service     = 3
      erro_solicitacao     = 4
      OTHERS               = 5.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE e008(zciot) WITH p_cte_avulso RAISING sem_dados_ciot.
    WHEN 2.
      MESSAGE e019(zciot) WITH sy-msgv1 RAISING erro_status_canc.
    WHEN 2.
      MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
    WHEN 4.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_solicitacao.
    WHEN 5.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

ENDFUNCTION.
