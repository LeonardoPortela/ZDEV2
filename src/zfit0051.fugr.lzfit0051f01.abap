*----------------------------------------------------------------------*
***INCLUDE LZFIT0051F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZFIT0051-DATA_ATUAL = SY-DATUM .
  ZFIT0051-HORA_ATUAL = SY-UZEIT .
  ZFIT0051-USNAM = SY-UNAME.
ENDFORM.                    "fetch_value
