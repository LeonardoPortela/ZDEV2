FUNCTION z_sd_credita_ciot.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_CTE_AVULSO) TYPE  J_1BDOCNUM
*"     REFERENCE(P_FATURAMENTO_AUTOM) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_CH_REFERENCIA) TYPE  ZCH_REF OPTIONAL
*"  EXPORTING
*"     VALUE(P_PROTOCOLO) TYPE  ZPROTOCOLO
*"  EXCEPTIONS
*"      CTE_NAO_AUTORIZADO
*"      SEM_DADOS_CIOT
*"      ERRO_STATUS_CRED
*"      ERRO_WEB_SERVICE
*"----------------------------------------------------------------------

  DATA: viagem       TYPE REF TO zcl_ciot_viagem,
        wa_cte_ciot  TYPE zcte_ciot,
        p_autorizado TYPE  char01.

*-US 140617-30.12.2024-#140617-JT-inicio
  CREATE OBJECT lc_integra_tip.
  lc_integra_tip->set_referencia( CONV #( p_cte_avulso ) ).
*-US 140617-30.12.2024-#140617-JT-fim

  SELECT SINGLE * INTO cte_j_1bnfdoc
    FROM j_1bnfdoc
   WHERE docnum EQ p_cte_avulso.

  CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
    EXPORTING
      p_docnum     = p_cte_avulso
    IMPORTING
      p_autorizado = p_autorizado.

  IF p_autorizado NE c_x.
    MESSAGE e015(zciot) WITH wa_cte_ciot-docnum RAISING cte_nao_autorizado.
  ENDIF.

  CLEAR: p_autorizado.
  CALL FUNCTION 'Z_NFE_MDFE_AUTORIZADO'
    EXPORTING
      p_docnum     = p_cte_avulso  "docnum cte
    IMPORTING
      p_autorizado = p_autorizado
*   CHANGING
*     P_CABEC      =
*     P_ACTIVE     =
*   EXCEPTIONS
*     CANCELADO    = 1
*     NAO_CANCELADO         = 2
*     PENDENTE     = 3
*     NAO_CONCLUIDO         = 4
*     NAO_EXISTE   = 5
*     AUTORIZADO_USO        = 6
*     DENEGADO     = 7
*     MDFE_AUTORIZADO       = 8
*     OTHERS       = 9
    .

  IF p_autorizado EQ c_x.
    CREATE OBJECT viagem.

    CALL METHOD viagem->popula_ciot
      EXPORTING
        cte_docnum          = p_cte_avulso
        creditar_viagem_adm = c_x
        i_faturamento_autom = p_faturamento_autom "*-#133089-21.02.2024-JT-inicio
        i_ch_referencia     = p_ch_referencia     "*-#133089-21.02.2024-JT-inicio
      IMPORTING
        p_protocolo         = p_protocolo
      EXCEPTIONS
        nao_ciot            = 1
        erro_status_cred    = 2
        erro_web_service    = 3
        OTHERS              = 4.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e008(zciot) WITH p_cte_avulso RAISING sem_dados_ciot.
      WHEN 2.
        MESSAGE e017(zciot) WITH sy-msgv1 RAISING erro_status_cred.
      WHEN 2.
        MESSAGE e032(zsimetrya) WITH sy-msgv1 RAISING erro_web_service.
      WHEN 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

  ENDIF.

ENDFUNCTION.
