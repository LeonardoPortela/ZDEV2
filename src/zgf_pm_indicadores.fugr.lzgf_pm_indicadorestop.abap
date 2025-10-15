FUNCTION-POOL ZGF_PM_INDICADORES.           "MESSAGE-ID ..

  DATA: t_aufk TYPE TABLE OF aufk,
        t_cobk TYPE TABLE OF cobk.

  DATA: r_date TYPE RANGE OF dats,
        r_year TYPE RANGE OF gjahr.
* INCLUDE LZGF_PM_INDICADORESD...            " Local class definition
