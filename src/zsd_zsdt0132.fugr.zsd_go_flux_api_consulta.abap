FUNCTION zsd_go_flux_api_consulta.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DADOS) TYPE  ZSDE0004 OPTIONAL
*"     REFERENCE(I_COM_TELA) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_DADOS_LOTE) TYPE  ZCDE0005
*"     REFERENCE(ET_DADOS_ORDENS) TYPE  ZCDE0006
*"  EXCEPTIONS
*"      ERRO_ENCONTRADO
*"      ERRO_AUTORIZACAO
*"      TELA_CANCELADA
*"----------------------------------------------------------------------

  PERFORM f_refresh_all.

  zsde0004 = i_dados.

  IF i_com_tela = 'X'.

    CALL SCREEN 9000.

  ELSE.

    PERFORM f_buscar_lote.
    PERFORM f_buscar_ordens.

  ENDIF.

  et_dados_lote[] = gt_0005.
  et_dados_ordens[] = gt_0006_global.

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

  ENDCASE.

ENDFUNCTION.
