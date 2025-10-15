*----------------------------------------------------------------------*
***INCLUDE LZFIT0063F01 .
*----------------------------------------------------------------------*
FORM FETCH_VALUE.
     ZFIT0063-DATA_ATUAL = SY-DATUM .
     ZFIT0063-HORA_ATUAL = SY-UZEIT .
     ZFIT0063-USNAM = SY-UNAME.
ENDFORM.
