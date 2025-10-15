FUNCTION zsd_tela_inicial_zsdt0132.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_RENEW) TYPE  FLAG
*"  EXPORTING
*"     REFERENCE(E_BUKRS) TYPE  BUKRS
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"     REFERENCE(ET_DADOS_LOTE) TYPE  ZCDE0005
*"     REFERENCE(ET_DADOS_ORDENS) TYPE  ZCDE0006
*"  EXCEPTIONS
*"      ERRO_ENCONTRADO
*"      ERRO_AUTORIZACAO
*"      TELA_CANCELADA
*"      SEM_NOVO_PROCESSO
*"----------------------------------------------------------------------

  gv_renew = i_renew.

  CALL SCREEN 9200.

  et_dados_lote[] = gt_0005.
  et_dados_ordens[] = gt_0006_global.

  e_bukrs = gv_bukrs.
  e_matnr = gv_matnr.

  CASE gv_error_num.
    WHEN 1.

      MESSAGE ID gw_bapiret-id TYPE 'S' NUMBER gw_bapiret-number
        WITH gw_bapiret-message_v1 gw_bapiret-message_v2
             gw_bapiret-message_v3 gw_bapiret-message_v4
              DISPLAY LIKE gw_bapiret-type RAISING erro_encontrado.

    WHEN 2.

      MESSAGE ID gw_bapiret-id TYPE 'S' NUMBER gw_bapiret-number
        WITH gw_bapiret-message_v1 gw_bapiret-message_v2
             gw_bapiret-message_v3 gw_bapiret-message_v4
              DISPLAY LIKE gw_bapiret-type RAISING erro_autorizacao.

    WHEN 3.

      RAISE tela_cancelada.

    WHEN 4.
      RAISE sem_novo_processo.

  ENDCASE.

ENDFUNCTION.
