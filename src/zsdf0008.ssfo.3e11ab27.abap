DATA: _lifnr TYPE lifnr.

_lifnr = |{ wa_itens-werks ALPHA = IN }|.

SELECT SINGLE *
  INTO w_lfa1_table
  FROM lfa1
  WHERE lifnr = _lifnr.

WA_GRID = VALUE #(
                    NAME4    = W_LFA1_TABLE-NAME4
                    MCOD3    = W_LFA1_TABLE-MCOD3
                    REGIO    = W_LFA1_TABLE-REGIO
                    LOT      = WA_ITENS-LOTE
                    SHIPPING = WA_ITENS-TIPO
                    BALES    = WA_ITENS-QTD_FARDOS
                    NET      = WA_ITENS-PESO_LOTE
                 ).



*ADD WA_GRID-BALES TO WA_TOTAL-BALES.
ADD WA_GRID-NET TO WA_TOTAL-NET.












