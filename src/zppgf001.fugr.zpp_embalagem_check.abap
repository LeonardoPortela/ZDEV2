FUNCTION zpp_embalagem_check.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(LOTES) TYPE  ZPPET011_T
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  ZPPET012_T
*"----------------------------------------------------------------------

  LOOP AT lotes[] INTO DATA(_lotes).

*    _LOTES-CODIGO = |{ _LOTES-CODIGO ALPHA = IN }|.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = _lotes-codigo
      IMPORTING
        output       = _lotes-codigo
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    SELECT SINGLE *
      FROM zppt0011
      INTO @DATA(_0011)
    WHERE matnr           = @_lotes-codigo
      AND werks           = @_lotes-centro
      AND lgort           = @_lotes-deposito_saida
      AND charg           = @_lotes-lote_individual
      AND id_movimentacao = @_lotes-id_movimentacao.

    IF sy-subrc IS INITIAL.
      APPEND VALUE #(
                        id              = _lotes-id_movimentacao
                        lote_individual = _lotes-lote_individual
                        doc_sap         = _0011-mblnr
                        msg             = 'OK!'
                    ) TO result.
    ELSE.

      SELECT SINGLE *
        FROM zppt0023
        INTO @DATA(_0023)
      WHERE matnr           = @_lotes-codigo
        AND werks           = @_lotes-centro
        AND lgort           = @_lotes-deposito_saida
        AND charg           = @_lotes-lote_individual
        AND id_movimentacao = @_lotes-id_movimentacao.

      IF sy-subrc IS INITIAL.
        APPEND VALUE #(
                          id              = _lotes-id_movimentacao
                          lote_individual = _lotes-lote_individual
                          doc_sap         = _0023-mblnr
                          msg             = 'OK!'
                      ) TO result.
      ELSE.
        APPEND VALUE #(
                          id              = _lotes-id_movimentacao
                          lote_individual = _lotes-lote_individual
                          msg             = 'Nenhum resultado encontrado!'
                      ) TO result.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
