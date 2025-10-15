*----------------------------------------------------------------------*
***INCLUDE LZMMT0074F01.
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZMMT0074-DATA_ATUAL = SY-DATUM .
  ZMMT0074-HORA_ATUAL = SY-UZEIT .
  ZMMT0074-USUARIO = SY-UNAME.
ENDFORM.                    "fetch_value
