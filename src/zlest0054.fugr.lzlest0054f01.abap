*----------------------------------------------------------------------*
***INCLUDE LZLEST0054F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZLEST0054-DT_CADASTRO = SY-DATUM .
  ZLEST0054-HR_HORA = SY-UZEIT .
  ZLEST0054-USUARIO = SY-UNAME.
ENDFORM.                    "fetch_value
