*----------------------------------------------------------------------*
***INCLUDE LZSDF0128F01.
*----------------------------------------------------------------------*

FORM F_NEW_ENTRADA.

  ZSDT0128-DATA   = SY-DATUM.
  ZSDT0128-HORA   = SY-UZEIT.
  ZSDT0128-USNAME = SY-UNAME.

  DATA: TESTE TYPE C.

  TESTE = 'A'.

ENDFORM.
