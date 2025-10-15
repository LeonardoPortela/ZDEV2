*----------------------------------------------------------------------*
***INCLUDE LZMMT0061F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZMMT0061-DATA_ATUAL = SY-DATUM .
  ZMMT0061-HORA_ATUAL = SY-UZEIT .
  ZMMT0061-USUARIO    = SY-UNAME.
ENDFORM.                    "fetch_value
