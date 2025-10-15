FUNCTION zsd_popup_referencia_preco.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_COM_TELA) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(EW_DADOS_POPUP) TYPE  ZSDE0003
*"     REFERENCE(EV_RET) TYPE  CHAR1
*"----------------------------------------------------------------------

  CLEAR zsde0003.
  CLEAR gv_9100_hide.

  IF i_com_tela = 'X'.

    gv_bukrs = i_bukrs.

    gv_matnr = i_matnr.

    SHIFT gv_matnr LEFT DELETING LEADING '0'.

    CALL SCREEN 9100 STARTING AT 10 10.

    IF gv_9100_ucomm NE 'OK'.
      ev_ret = '2'.
    ENDIF.

    ew_dados_popup = zsde0003.

  ENDIF.

ENDFUNCTION.
