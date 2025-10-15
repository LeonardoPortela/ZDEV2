
WA_GRID = VALUE #(
                    LOT      = WA_ITENS-LOTE
                    SHIPPING = WA_ITENS-TIPO
                    BALES    = WA_ITENS-QTD_FARDOS
                    NET      = WA_ITENS-PESO_LOTE
                 ).

ADD WA_GRID-NET TO WA_TOTAL-NET.
