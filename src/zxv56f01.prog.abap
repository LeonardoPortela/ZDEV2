*----------------------------------------------------------------------*
***INCLUDE ZXV56F01.
*----------------------------------------------------------------------*

FORM f_inf_car_frete_sem_pesagem TABLES T_LIKP STRUCTURE LIKP
                                  USING P_TKNUM TYPE VTTK-TKNUM.


  DATA: lva_peso_tara      TYPE zlest0109-peso_tara,
        lva_peso_liquido   TYPE zlest0109-peso_bruto,
        lva_id_ordem       TYPE zlest0108-id_ordem.

  CHECK T_LIKP[] IS NOT INITIAL.

  READ TABLE t_likp INTO DATA(lwa_likp) INDEX 1.

  CHECK SY-SUBRC EQ 0.

  CHECK zcl_faturamento=>zif_faturamento~get_romaneio_trocanota( EXPORTING i_vbeln = lwa_likp-vbeln ) = abap_false.

  SELECT SINGLE * from lips INTO @DATA(lwa_lips) WHERE vbeln eq @lwa_likp-vbeln.

  SELECT SINGLE *
    FROM zlest0108 INTO @DATA(lwa_zlest0108)
   WHERE vbeln EQ @lwa_likp-vbeln.

  IF sy-subrc EQ 0.

    SELECT SINGLE *
      FROM tvarvc INTO @DATA(lwa_tvarvc)
     WHERE name = 'MAGGI_GR_FERTILIZANTES'
       AND LOW  = @lwa_lips-matkl.

    CHECK sy-subrc eq 0.

    SELECT SINGLE *
      FROM zlest0109 INTO @DATA(lwa_zlest0109)
     WHERE vbeln EQ @lwa_likp-vbeln.

    CHECK sy-subrc eq 0.

    lva_id_ordem     = lwa_zlest0108-id_ordem.
    lva_peso_liquido = lwa_zlest0109-qtde_aviso.
    lva_peso_tara    = lwa_zlest0109-peso_tara.

  ELSE.

    SELECT SINGLE *
      from zlest0211 INTO @DATA(lwa_zlest0211)
     WHERE vbeln eq @lwa_likp-vbeln.

    CHECK sy-subrc eq 0.

    lva_id_ordem     = lwa_zlest0211-id_ordem.
    "lva_peso_liquido = lwa_zlest0211-peso_liq.  "Depois deve ser feito essa atribuição pelo fluxo da ZLES0200
    "lva_peso_tara    = lwa_zlest0211-peso_tara. "Depois deve ser feito essa atribuição pelo fluxo da ZLES0200

  ENDIF.

  CHECK lva_id_ordem     IS NOT INITIAL and
        lva_peso_liquido is NOT INITIAL and
        lva_peso_tara is NOT INITIAL.

  SELECT SINGLE *
    FROM vttk INTO @DATA(lwa_vttk)
   WHERE tknum = @p_tknum.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zlest0185 INTO @DATA(lwa_zlest0185)
   WHERE id_ordem = @lva_id_ordem.

  CHECK sy-subrc EQ 0.

  TRY .
      zcl_integracao_viagem_carregar=>zif_integracao_viagem_carregar~get_instance(
        )->set_viagem_carregar(
        EXPORTING
          i_viagem_id              = CONV #( lwa_vttk-id_viagem )
          i_dt_carregamento        = CONV #( lwa_vttk-erdat )    " Data Carregamento
          i_peso_tara              = CONV #( lva_peso_tara  )
          i_peso_liquido           = CONV #( lva_peso_liquido )
        ).
    CATCH zcx_integracao INTO DATA(ex_integracao).

    CATCH zcx_error INTO DATA(ex_error2).

  ENDTRY.




ENDFORM.
