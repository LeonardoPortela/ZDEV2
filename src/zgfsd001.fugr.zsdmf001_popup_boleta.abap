FUNCTION zsdmf001_popup_boleta.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_EDIT) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_VENC) TYPE  SYDATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(ES_LINE) TYPE  ZSDE013
*"     REFERENCE(EV_MOTIVO) TYPE  STRING
*"     REFERENCE(EV_CANCEL) TYPE  FLAG
*"  TABLES
*"      CT_BOLETA STRUCTURE  ZSDE013
*"----------------------------------------------------------------------

  PERFORM f_refresh_9000.

  gt_alv_9000 = ct_boleta[].

  gv_edit_9000 = iv_edit.

  gv_venc_9000 = iv_venc.

  CALL SCREEN 9000 STARTING AT 20 1.

  PERFORM f_remove_unicode USING gv_motivo CHANGING ev_motivo.

  " ev_motivo = gv_motivo.

  READ TABLE gt_alv_9000 INTO es_line WITH KEY selec = 'X'.

  IF gv_ucomm_9000 NE 'OK'.
    ev_cancel = 'X'.
  ENDIF.

ENDFUNCTION.
