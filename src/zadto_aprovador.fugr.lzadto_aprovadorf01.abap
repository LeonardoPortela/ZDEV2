*----------------------------------------------------------------------*
***INCLUDE LZADTO_APROVADORF01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZADTO_APROVADOR-DATA_ATUAL = SY-DATUM .
  ZADTO_APROVADOR-HORA_ATUAL = SY-UZEIT .
  ZADTO_APROVADOR-USUARIO    = SY-UNAME .
ENDFORM.                    "FETCH_VALUE
