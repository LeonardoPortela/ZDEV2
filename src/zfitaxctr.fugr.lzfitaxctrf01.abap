*----------------------------------------------------------------------*
***INCLUDE LZFITAXCTRF01.
*----------------------------------------------------------------------*

FORM FETCH_VALUE.
  ZFITAXCTR-DATA_ATUAL = SY-DATUM .
  ZFITAXCTR-HORA_ATUAL = SY-UZEIT .
  ZFITAXCTR-USUARIO = SY-UNAME.
ENDFORM.                    "fetch_value
