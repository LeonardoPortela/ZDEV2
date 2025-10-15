*----------------------------------------------------------------------*
***INCLUDE LZMMT0023F03 .
*----------------------------------------------------------------------*
form user_date_time.
  zmmt0023-usnam      = sy-uname.
  zmmt0023-dt_atul    = sy-datum.
  zmmt0023-hora_atual = sy-uzeit.
endform.                    "USER_DATE_TIME
