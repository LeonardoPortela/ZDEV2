DATA: _lifnr TYPE lifnr.

_lifnr = |{ wa_itens-werks ALPHA = IN }|.

SELECT SINGLE *
  INTO w_lfa1_table
  FROM lfa1
  WHERE lifnr = _lifnr.

















