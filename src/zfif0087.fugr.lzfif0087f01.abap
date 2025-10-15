*----------------------------------------------------------------------*
***INCLUDE LZFIF0087F01 .
*----------------------------------------------------------------------*
FORM F_ATRIBUIR_INFO.
  ZFIT0087-USUARIO       = SY-UNAME.
  ZFIT0087-HORA_REGISTRO = SY-UZEIT.
  ZFIT0087-DATA_REGISTRO = SY-DATUM.
ENDFORM.                    "F_ATRIBUIR_INFO
