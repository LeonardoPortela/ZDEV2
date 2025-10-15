*----------------------------------------------------------------------*
***INCLUDE LZFIT0089F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZFIT0089-DATA_ATUAL = SY-DATUM .
  ZFIT0089-HORA_ATUAL = SY-UZEIT .
  ZFIT0089-USUARIO    = SY-UNAME .
ENDFORM.                    "FETCH_VALUE
