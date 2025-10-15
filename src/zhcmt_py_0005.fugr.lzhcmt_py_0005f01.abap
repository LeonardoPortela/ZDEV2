*----------------------------------------------------------------------*
***INCLUDE LZHCMT_PY_0005F01.
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZHCMT_PY_0005-DATA_ATUAL = SY-DATUM .
  ZHCMT_PY_0005-HORA_ATUAL = SY-UZEIT .
  ZHCMT_PY_0005-USUARIO = SY-UNAME.
ENDFORM.                    "FETCH_VALUE
