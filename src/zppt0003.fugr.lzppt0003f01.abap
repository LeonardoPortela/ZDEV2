*----------------------------------------------------------------------*
***INCLUDE LZPPT0003F01 .
*----------------------------------------------------------------------*
FORM fetch_value.
  zppt0003-data_atual = sy-datum .
  zppt0003-hora_atual = sy-uzeit .
  zppt0003-usnam = sy-uname.
ENDFORM.                    "fetch_value
