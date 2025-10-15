FUNCTION zsd_selec_dados_ord_vend.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN
*"  EXPORTING
*"     REFERENCE(E_VBELV) TYPE  VBELN
*"     REFERENCE(E_DATA) TYPE  ZSDT0041
*"----------------------------------------------------------------------



  DATA: wa_zsdt0090 TYPE zsdt0090.

  CLEAR:  e_data.

  CHECK i_vbeln IS NOT INITIAL.

  SELECT SINGLE * FROM zsdt0090 INTO wa_zsdt0090 WHERE vbeln EQ i_vbeln AND estorno NE abap_true.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM zsdt0041 INTO e_data WHERE vbeln EQ wa_zsdt0090-vbelv.
    IF sy-subrc NE 0.
      e_vbelv = wa_zsdt0090-vbelv.
    ENDIF.
  ENDIF.

ENDFUNCTION.
