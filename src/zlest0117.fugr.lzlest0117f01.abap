*----------------------------------------------------------------------*
***INCLUDE LZLEST0117F01.
*----------------------------------------------------------------------*

FORM FETCH_VALUE.
  ZLEST0117-DT_ULT_MOD = SY-DATUM .
  ZLEST0117-HR_ULT_MOD = SY-UZEIT .
  ZLEST0117-USUARIO_ULT_MOD = SY-UNAME.
ENDFORM.                    "fetch_value
