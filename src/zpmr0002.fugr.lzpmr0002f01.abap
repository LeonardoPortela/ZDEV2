*----------------------------------------------------------------------*
***INCLUDE LZPMR0002F01 .
*----------------------------------------------------------------------*
form fetch_value.
  zpmr0002-usuario    = sy-uname.
  zpmr0002-hora_atual = sy-uzeit.
  zpmr0002-data_atual = sy-datum.
endform.
