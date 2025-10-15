*----------------------------------------------------------------------*
***INCLUDE LZGLT037F01 .
*----------------------------------------------------------------------*

FORM FETCH_VALUE.
  ZGLT037-DATA_ATUAL = SY-DATUM .
  ZGLT037-HORA_ATUAL = SY-UZEIT .
  ZGLT037-USUARIO = SY-UNAME.
ENDFORM.                    "FETCH_VALUE
