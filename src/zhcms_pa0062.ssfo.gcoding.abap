DATA t_month TYPE TABLE OF t247.
CALL FUNCTION 'MONTH_NAMES_GET'
  EXPORTING
    language              = sy-langu
  TABLES
    month_names           = t_month
  exceptions
    month_names_not_found = 1
    OTHERS                = 2.

READ TABLE t_month INTO DATA(s_month) WITH KEY mnr = sy-datum+4(2).
fv_month = s_month-ltx.

























