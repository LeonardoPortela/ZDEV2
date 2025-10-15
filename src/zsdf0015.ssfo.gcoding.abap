DATA: l_dt_ini TYPE sy-datum,
      l_dt_fim TYPE sy-datum,
      t_day    TYPE TABLE OF casdayattr.

wa_143 = i_zsdt0143.
nr_provisional = |{ header-nr_provisional ALPHA = OUT }|.

l_dt_ini = sy-datum.
l_dt_fim = l_dt_ini + 20.

CALL FUNCTION 'DAY_ATTRIBUTES_GET'
  EXPORTING
    factory_calendar = 'BR'
    date_from        = l_dt_ini
    date_to          = l_dt_fim
    language         = sy-langu
  TABLES
    day_attributes   = t_day.

DELETE t_day WHERE holiday = 'X' OR
                   weekday = 6   OR
                   weekday = 7.

READ TABLE t_day INTO DATA(w_day) INDEX 6.
IF i_formulario = '2'.
  data_pagamento = w_day-date+6(02) && '.' &&
                   w_day-date+4(02) && '.' &&
                   w_day-date(04).
ELSE.
  CLEAR data_pagamento.
ENDIF.
















