DATA: _lifnr  TYPE lifnr.

CLEAR wa_total-bales.

_lifnr = |{ header-werks ALPHA = IN }|.

SELECT SINGLE *
FROM zsdt0143
INTO wa_143
WHERE contrato EQ header-contrato.

LOOP AT itens INTO DATA(wa).
  ADD wa-qtd_fardos  TO wa_total-bales.
ENDLOOP.

SHIFT wa_total-bales LEFT DELETING LEADING '0'.

SELECT SINGLE *
  FROM kna1
  INTO wa_kna1
  WHERE kunnr EQ header-kunnr.

SELECT SINGLE *
  FROM adrc
  INTO wa_adrc
  WHERE addrnumber EQ wa_kna1-adrnr
    AND date_to    >= sy-datum.

w_nome_client = wa_adrc-name1 && wa_adrc-name2.

SELECT SINGLE *
  FROM lfa1
  INTO wa_lfa1
  WHERE lifnr = _lifnr.
IF wa_lfa1-name4 IS INITIAL.
  wa_lfa1-name4 = wa_lfa1-name1.
ENDIF.
