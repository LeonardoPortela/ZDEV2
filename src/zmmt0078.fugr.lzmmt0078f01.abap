*----------------------------------------------------------------------*
***INCLUDE LZMMT0078F01.
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZMMT0078-DATA_ATUAL = SY-DATUM .
  ZMMT0078-HORA_ATUAL = SY-UZEIT .
  ZMMT0078-USUARIO = SY-UNAME.
ENDFORM.
