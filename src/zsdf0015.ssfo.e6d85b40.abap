DATA: _lifnr TYPE lifnr.

_lifnr = |{ header-werks ALPHA = IN }|.

wa_143 = i_zsdt0143.

*SELECT SINGLE *
*  FROM zsdt0143
*  INTO wa_143
*  WHERE contrato EQ header-contrato.

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

w_nome_cliente = wa_adrc-name1 && wa_adrc-name2.

*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Início de Alteração
DATA:
  lo_sd_header_to TYPE REF TO zcl_sd_header_to.

CREATE OBJECT lo_sd_header_to
  EXPORTING
    i_w_adrc = wa_adrc.

w_header_to = lo_sd_header_to->get_w_header_to( ).
*** Stefanini - IR195586 - 12/12/2024 - LAZAROSR - Fim de Alteração
