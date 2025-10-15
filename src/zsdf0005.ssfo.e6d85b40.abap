DATA: _lifnr TYPE lifnr.

_lifnr = |{ header-werks ALPHA = IN }|.

SELECT SINGLE *
  FROM zsdt0143
  INTO wa_143
  WHERE contrato EQ header-contrato.

SELECT SINGLE *
  FROM kna1
  INTO wa_kna1
  WHERE kunnr EQ header-kunnr.

CHECK sy-subrc IS INITIAL.

SELECT SINGLE *
  FROM adr2
  INTO wa_adr2
  WHERE addrnumber EQ wa_kna1-adrnr.

SELECT SINGLE *
  FROM adrc
  INTO wa_adrc
  WHERE addrnumber EQ wa_kna1-adrnr
    AND date_to    >= sy-datum.

SELECT SINGLE *
  FROM lfa1
  INTO wa_lfa1
  WHERE lifnr = _lifnr.

wa_kna1-name1 = wa_adrc-name1.
wa_kna1-name2 = wa_adrc-name2.
