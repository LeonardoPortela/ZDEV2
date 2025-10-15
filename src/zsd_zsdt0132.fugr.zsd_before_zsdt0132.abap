FUNCTION zsd_before_zsdt0132.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_REINICIAR) TYPE  FLAG DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(E_BUKRS) TYPE  BUKRS
*"     REFERENCE(E_MATNR) TYPE  MATNR
*"     REFERENCE(E_RET) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA lw_info TYPE zsdt0002us.

  PERFORM f_seleciona_info CHANGING lw_info.

  IF lw_info-bukrs IS INITIAL OR i_reiniciar = 'X'.

    PERFORM f_poup_get_values CHANGING lw_info e_ret.

  ENDIF.

  CHECK e_ret NE 'A'.

  e_bukrs = lw_info-bukrs.
  e_matnr = lw_info-matnr.

ENDFUNCTION.
