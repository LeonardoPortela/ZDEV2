class ZCL_DEPOSITO definition
  public
  final
  create public .

public section.

  interfaces ZIF_DEPOSITO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DEPOSITO IMPLEMENTATION.


  METHOD zif_deposito~get_deposito_material_filial.

    DATA: lra_eudr TYPE RANGE OF zeudr. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    r_instance = me.
                                                            "US128331
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
    IF i_eudr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = i_eudr ) TO lra_eudr.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' ) TO lra_eudr.
    ENDIF.
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
    SELECT COUNT(*)
       FROM tvarvc
      WHERE name = 'MAGGI_BIODIESEL'
      AND   low  = i_matnr.
    IF sy-subrc = 0.
      e_lgort = 'PR01'.
      EXIT.

      SELECT SINGLE * INTO @DATA(wa_zmmt0017)
         FROM zmmt0017
        WHERE matnr          EQ @i_matnr
          AND centro_fixo    EQ @i_branch
          AND centro_a_fixar EQ @space
          AND tp_produto     EQ @space
          AND eudr           IN @lra_eudr. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
      IF sy-subrc = 0.
        e_lgort = wa_zmmt0017-lgort.
        EXIT.
      ENDIF.
    ENDIF.
                                                            "US128331

    IF ( i_centro_a_fixar IS INITIAL AND i_branch IS NOT INITIAL     ) OR
       ( i_centro_a_fixar IS NOT INITIAL AND i_branch IS NOT INITIAL ).

      SELECT SINGLE * INTO @DATA(wa_centro)
        FROM zsdt_depara_cen
       WHERE vkorg             EQ @i_bukrs
         AND centro_real       EQ @i_branch
         AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_deposito
          EXPORTING
            textid    = VALUE #( msgid = zcx_deposito=>zcx_centro_a_fixar-msgid
                                 msgno = zcx_deposito=>zcx_centro_a_fixar-msgno
                                 attr1 = CONV #( i_branch )
                                 attr2 = 'ZSDT0036' )
            msgid     = zcx_deposito=>zcx_centro_a_fixar-msgid
            msgno     = zcx_deposito=>zcx_centro_a_fixar-msgno
            msgty     = 'E'
            msgv1     = CONV #( i_branch )
            msgv2     = 'ZSDT0036'
            transacao = 'ZSDT0036'.
      ENDIF.

      e_branch         = wa_centro-centro_real.
      e_centro_a_fixar = wa_centro-centrov_1.

    ELSEIF i_centro_a_fixar IS NOT INITIAL AND i_branch IS INITIAL.

      SELECT SINGLE * INTO @wa_centro
        FROM zsdt_depara_cen
       WHERE vkorg             EQ @i_bukrs
         AND centrov_1         EQ @i_centro_a_fixar
         AND tp_centro_virtual EQ @zcl_pedido_compra=>st_tp_centro_a_fixar.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_deposito
          EXPORTING
            textid    = VALUE #( msgid = zcx_deposito=>zcx_centro_fixo-msgid
                                 msgno = zcx_deposito=>zcx_centro_fixo-msgno
                                 attr1 = CONV #( i_centro_a_fixar )
                                 attr2 = 'ZSDT0036' )
            msgid     = zcx_deposito=>zcx_centro_fixo-msgid
            msgno     = zcx_deposito=>zcx_centro_fixo-msgno
            msgty     = 'E'
            msgv1     = CONV #( i_centro_a_fixar )
            msgv2     = 'ZSDT0036'
            transacao = 'ZSDT0036'.
      ENDIF.

      e_branch         = wa_centro-centro_real.
      e_centro_a_fixar = wa_centro-centrov_1.

    ENDIF.

    SELECT SINGLE * INTO @wa_zmmt0017
      FROM zmmt0017
     WHERE matnr          EQ @i_matnr
       AND centro_fixo    EQ @e_branch
       AND centro_a_fixar EQ @e_centro_a_fixar
       AND tp_produto     EQ @i_tp_produto
       AND eudr           IN @lra_eudr. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE * INTO @wa_zmmt0017
        FROM zmmt0017
       WHERE matnr          EQ @i_matnr
         AND centro_fixo    EQ @e_branch
         AND centro_a_fixar EQ @e_centro_a_fixar
         AND tp_produto     EQ @space
         AND eudr           IN @lra_eudr. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940

      IF sy-subrc IS NOT INITIAL.

        "SD - Geração Ordem Venda sem Deposito US #153341 - WPP --->>>
        DATA(lva_matnr) = i_matnr.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input         = lva_matnr
          IMPORTING
            OUTPUT        = lva_matnr.

        DATA(lva_msgv1) = |Centro: { e_branch } Centro a Fixar: { e_centro_a_fixar } |.
        DATA(lva_msgv2) = |Material: { lva_matnr } Class.EUDR: { i_eudr }|.
        DATA(lva_msgv3) = |Tp.Produto: { i_tp_produto } sem parametrização de depósito |.
        DATA(lva_msgv4) = |na transação ZMM0017!|.

        RAISE EXCEPTION TYPE zcx_deposito
          EXPORTING
            textid    = VALUE #( msgid = zcx_deposito=>ZCX_ERRO_GERAL-msgid
                                 msgno = zcx_deposito=>ZCX_ERRO_GERAL-msgno
                                 attr1 = CONV #( lva_msgv1 )
                                 attr2 = CONV #( lva_msgv2 )
                                 attr3 = CONV #( lva_msgv3 )
                                 attr4 = CONV #( lva_msgv4 )
                                 )
            msgid     = zcx_deposito=>ZCX_ERRO_GERAL-msgid
            msgno     = zcx_deposito=>ZCX_ERRO_GERAL-msgno
            msgty     = 'E'
            msgv1     = CONV #( lva_msgv1 )
            msgv2     = CONV #( lva_msgv2 )
            msgv3     = CONV #( lva_msgv3 )
            msgv4     = CONV #( lva_msgv4 ).
        "SD - Geração Ordem Venda sem Deposito US #153341 - WPP <<<---

      ELSE.
        "Não Existe depósito para este material e tipo, mais existe depósito
        e_lgort = wa_zmmt0017-lgort.
      ENDIF.

    ELSE.
      "Existe depósito para este material e tipo
      e_lgort = wa_zmmt0017-lgort.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_DEPOSITO~GET_INSTANCE.

    IF ZIF_DEPOSITO~AT_DEPOSITO IS NOT BOUND.
      CREATE OBJECT ZIF_DEPOSITO~AT_DEPOSITO TYPE ZCL_DEPOSITO.
    ENDIF.

    R_INSTANCE = ZIF_DEPOSITO~AT_DEPOSITO.

  ENDMETHOD.
ENDCLASS.
