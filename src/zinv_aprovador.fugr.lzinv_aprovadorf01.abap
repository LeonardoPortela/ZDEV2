*----------------------------------------------------------------------*
***INCLUDE LZINV_APROVADORF01 .
*----------------------------------------------------------------------*

FORM FETCH_VALUE.
  ZINV_APROVADOR-DATA_ATUAL = SY-DATUM .
  ZINV_APROVADOR-HORA_ATUAL = SY-UZEIT .
  ZINV_APROVADOR-USUARIO = SY-UNAME.

ENDFORM.                    "fetch_value
