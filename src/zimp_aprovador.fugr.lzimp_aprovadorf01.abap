*----------------------------------------------------------------------*
***INCLUDE LZIMP_APROVADORF01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZIMP_APROVADOR-DATA_ATUAL = SY-DATUM .
  ZIMP_APROVADOR-HORA_ATUAL = SY-UZEIT .
  ZIMP_APROVADOR-USUARIO = SY-UNAME.
ENDFORM.                    "fetch_value
