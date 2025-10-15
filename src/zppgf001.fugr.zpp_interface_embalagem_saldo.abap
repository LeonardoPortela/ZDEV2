FUNCTION zpp_interface_embalagem_saldo.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(MATERIAL) TYPE  MATNR
*"     REFERENCE(CENTRO) TYPE  WERKS_D
*"     REFERENCE(DEPOSITO) TYPE  LGORT_D
*"     REFERENCE(LOTE) TYPE  ZDE_LOTE_FORN
*"  EXPORTING
*"     REFERENCE(_SALDO) TYPE  ZPPET008
*"--------------------------------------------------------------------


  DATA(_new_deposito) = |{ deposito CASE = UPPER }|.
  DATA(_new_lote) = |{ lote CASE = UPPER }|.

  DATA: it_zppt0015       TYPE TABLE OF zppt0015,
        vl_tipo_movimento TYPE bwart,
        _matnr            TYPE matnr,
*        _LGORT            TYPE LGORT_D,
        _charg            TYPE charg_d,
*** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Início de Alteração
        vl_lote_interno   TYPE boolean.
*** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Fim de Alteração

  "_MATNR = |{ MATERIAL ALPHA = IN }|.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input  = material
    IMPORTING
      output = _matnr
*   EXCEPTIONS
*     LENGTH_ERROR       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  SELECT COUNT(*) FROM mchb WHERE matnr EQ @_matnr.
  IF sy-subrc IS NOT INITIAL.
    _saldo = VALUE #(
                       material = material
                       value    = |Material não é controlado por Lote!|
                    ).
    EXIT.
  ENDIF.

**  Check material.
  SELECT SINGLE * FROM zppt0016
  INTO @DATA(wa_0016)
    WHERE matnr EQ @_matnr
     AND  werks EQ @centro
*     AND  CHARG EQ @_new_lote "LOTE
     AND  lgort EQ @_new_deposito "deposito
     AND zlicha EQ @_new_lote."lote.

  IF sy-subrc IS INITIAL.

*    _charg = wa_0016-charg. "Comentado BUG IMPEDITIVO 156497 / 25-10-2024 / AOENNING.
     _charg = wa_0016-chargd."Nova implementação BUG IMPEDITIVO 156497 / 25-10-2024 / AOENNING.

*    _charg = |{ wa_0016-chargd CASE = UPPER }|.
**  ELSE.
**    _charg = |{ _new_lote CASE = UPPER }|.
*  ENDIF.

    IF _matnr IS INITIAL.
      _saldo = VALUE #( value = |Material Obrigatório!| ).
    ELSE.
      IF centro IS INITIAL.
        _saldo = VALUE #( value = |Centro Obrigatório!| ).
      ELSE.
        IF _charg IS INITIAL.
          _saldo = VALUE #( value = |Lote Obrigatório!| ).
        ELSE.

**** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Início de Alteração
*        " Se for um lote interno, não deve trazer saldo
*        SELECT COUNT(*)
*          FROM zppt0016
*          WHERE matnr EQ @_matnr
*            AND werks EQ @centro
*            AND charg EQ @_new_lote"lote
*            AND lgort EQ @_new_deposito. "deposito.
*
*        IF sy-subrc IS INITIAL.
*          vl_lote_interno = abap_true.
*        ENDIF.
**** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Fim de Alteração

          SELECT SINGLE *
            FROM mchb
           INTO @DATA(wa_mchb)
            WHERE matnr EQ @_matnr
            AND werks EQ @centro
            "and LGORT eq _new_deposito "É campo chave coloquei aqui mas tem que ver porque não esta como condição!?
            AND charg EQ @_charg.

          _matnr = |{ material ALPHA = OUT }|.
          CONDENSE _matnr NO-GAPS.

          IF sy-subrc IS INITIAL.
**** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Início de Alteração
*        AND vl_lote_interno IS INITIAL.
**** Stefanini - IR198163 - 23/09/2024 - LAZAROSR - Fim de Alteração

            _saldo = VALUE #(
                              material   = wa_mchb-matnr
                              centro     = wa_mchb-werks
                              deposito   = wa_mchb-lgort
                              lote       = _new_lote"lote
                              vencimento = zcl_emb=>get_vencimento( vl_matnr = wa_mchb-matnr
                                                                       werks = wa_mchb-werks
                                                                       lgort = wa_mchb-lgort
                                                                       vl_charg = CONV #( wa_mchb-charg ) )
                              saldo      = wa_mchb-clabs
                              value      = 'OK'
                           ).
          ELSE.
            _saldo = VALUE #(
                               material = material
                               centro   = centro
                               lote     = _new_lote"lote
                               deposito = _new_deposito"deposito
                               value    = |Combinação { _matnr } { centro } { _new_deposito } { _new_lote } não encontrada!|
                           ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    _saldo = VALUE #(
                       material = material
                       centro   = centro
                       lote     = _new_lote"lote
                       deposito = _new_deposito"deposito
                       value    = |Combinação { _matnr } { centro } { _new_deposito } { _new_lote } não encontrada!|
                   ).
  ENDIF.


ENDFUNCTION.
