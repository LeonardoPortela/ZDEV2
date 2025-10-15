FUNCTION zmm_popup_referencia_preco.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_TEXT) TYPE  MSGTX
*"  EXPORTING
*"     REFERENCE(EW_DADOS_POPUP) TYPE  ZSDE0003
*"     REFERENCE(EV_RET) TYPE  CHAR1
*"----------------------------------------------------------------------

  CLEAR zsde0003.

  CLEAR gv_9100_hide.

  gv_text_9100 = iv_text.

  CALL SCREEN 9100 STARTING AT 10 10.

  IF gv_9100_ucomm NE 'OK'.
    ev_ret = '2'.
  ENDIF.

  ew_dados_popup = zsde0003.

ENDFUNCTION.
