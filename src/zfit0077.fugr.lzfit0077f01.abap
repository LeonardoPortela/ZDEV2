*----------------------------------------------------------------------*
***INCLUDE LZFIT0077F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
  ZFIT0077-DT_ATUAL = SY-DATUM .
  ZFIT0077-HR_ATUAL = SY-UZEIT .
  ZFIT0077-USNAM    = SY-UNAME.
ENDFORM.                    "fetch_value
