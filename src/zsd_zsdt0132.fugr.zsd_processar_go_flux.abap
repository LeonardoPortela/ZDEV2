FUNCTION zsd_processar_go_flux.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_TITLE) TYPE  SY-TITLE OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_DADOS_LOTE) TYPE  ZCDE0005
*"     REFERENCE(ET_DADOS_ORDENS) TYPE  ZCDE0006
*"  EXCEPTIONS
*"      ERRO_ENCONTRADO
*"      ERRO_AUTORIZACAO
*"      TELA_CANCELADA
*"      SEM_NOVO_PROCESSO
*"----------------------------------------------------------------------

  DATA lw_popup TYPE zsde0003.
  DATA lw_dados TYPE zsde0004.
  DATA lv_selec TYPE c.
  DATA lv_existe TYPE c.

  CLEAR: et_dados_lote, et_dados_ordens.

  PERFORM f_check_bukrs_stvarv
    USING i_bukrs
 CHANGING lv_existe.

  " Se não estiver , abrir a tela da ZSDT0132 como é hoje.
  IF lv_existe IS INITIAL.
    RAISE sem_novo_processo.
  ENDIF.

  PERFORM f_check_matnr_stvarv
    USING i_matnr
 CHANGING lv_existe.

  " Se o material pertence a um dos grupos de mercadoria deve-se verificar
  " se ele não esta cadastrado como exceção na nova tabela ZSDT02xx
  IF lv_existe = 'X'.

    PERFORM f_check_matnr_excecao
      USING i_matnr
   CHANGING lv_existe.

    "Se estiver cadastrado como exceção , abrir a tela da ZSDT0132 como ela é atualmente.
    IF lv_existe = 'X'.

      RAISE sem_novo_processo.

    ENDIF.

  ELSE.

    RAISE sem_novo_processo.

  ENDIF.

  gv_title = i_title.

  CALL FUNCTION 'ZSD_POPUP_REFERENCIA_PRECO'
    EXPORTING
      i_bukrs        = i_bukrs
      i_matnr        = i_matnr
      i_com_tela     = 'X'
    IMPORTING
      ew_dados_popup = lw_popup
      ev_ret         = lv_selec.

  IF lv_selec = '2'.
    RAISE tela_cancelada.
  ENDIF.

  IF lw_popup-preco_afixar = 'X'.
    RAISE sem_novo_processo.
  ENDIF.

  IF lw_popup-preco_fixo = 'X' AND lw_popup-cif = 'X'.
    RAISE sem_novo_processo.
  ENDIF.

  lw_dados-bukrs = i_bukrs.
  lw_dados-matnr = i_matnr.

  CALL FUNCTION 'ZSD_GO_FLUX_API_CONSULTA'
    EXPORTING
      i_com_tela       = 'X'
      i_dados          = lw_dados
    TABLES
      et_dados_lote    = et_dados_lote
      et_dados_ordens  = et_dados_ordens
    EXCEPTIONS
      erro_encontrado  = 1
      erro_autorizacao = 2
      tela_cancelada   = 3
      OTHERS           = 4.

  CASE sy-subrc.
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
