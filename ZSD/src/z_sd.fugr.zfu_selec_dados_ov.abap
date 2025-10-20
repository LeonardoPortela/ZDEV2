FUNCTION ZFU_SELEC_DADOS_OV.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"  EXPORTING
*"     REFERENCE(E_DATA) TYPE  ZSDT0041
*"     REFERENCE(E_VBELV) TYPE  VBELN
*"----------------------------------------------------------------------

  DATA: wa_zsdt0090 TYPE zsdt0090.

  CLEAR:  e_data, e_vbelv.

  CHECK i_vbeln IS NOT INITIAL.

  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM zsdt0090 INTO wa_zsdt0090 WHERE vbeln EQ i_vbeln.
    IF sy-subrc EQ 0.
      SELECT SINGLE safra_apl FROM zsdt0041 INTO e_data WHERE vbeln EQ wa_zsdt0090-vbelv.
      IF sy-subrc NE 0.
        e_vbelv = wa_zsdt0090-vbelv.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
