*----------------------------------------------------------------------*
***INCLUDE LZFIT0086F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZFIT0086-DATA_ATUAL = SY-DATUM .
  ZFIT0086-HORA_ATUAL = SY-UZEIT .
  ZFIT0086-USUARIO = SY-UNAME.
ENDFORM.                    "fetch_value
