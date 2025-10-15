*----------------------------------------------------------------------*
***INCLUDE LZFI_ZFI0064F01.

FORM f_build_desc_juros_cenario1  USING p_opcao
                                        p_tipo
                                        p_waers
                                        p_bukrs   TYPE range_c10_t
                                        p_augdt   TYPE range_c10_t
                                        p_auart   TYPE range_c10_t
                                        p_nro_ov  TYPE range_c10_t
                                        p_nro_sol TYPE range_c10_t
                                        p_kunnr   TYPE range_c10_t
                               CHANGING c_saida TYPE zfis_saida_juros_zfi0064_t.

  RANGES: lra_bukrs   FOR t001-bukrs,
          lra_augdt   FOR bsad-augdt,
          lra_kunnr   FOR bsad-kunnr,
          lra_nr_ov   FOR bsad-vbel2,
          lra_nr_sol  FOR zsdt0053-nro_sol_ov,
          lra_auart   FOR vbak-auart.


  DATA: lit_juros_descontos_cenario_1 TYPE TABLE OF zi_fi_juros_mi_c1.

  CHECK p_tipo EQ 'MI'. "Mercado Interno

  MOVE-CORRESPONDING:    p_bukrs[]   TO lra_bukrs[],
                         p_augdt[]   TO lra_augdt[],
                         p_auart[]   TO lra_auart[],
                         p_nro_ov[]  TO lra_nr_ov[],
                         p_nro_sol[] TO lra_nr_sol[],
                         p_kunnr[]   TO lra_kunnr[].

  CASE p_opcao.
    WHEN 'VC'. "Visão Caixa


      CASE p_waers.
        WHEN 'BRL'.


          "Cenário 1: Descontos de antecipação com Adiantamentos contra banco
          SELECT *
            FROM zi_fi_juros_mi_c1 INTO TABLE @lit_juros_descontos_cenario_1
            WHERE bukrs IN @lra_bukrs
              AND augdt IN @lra_augdt
              AND waers EQ @p_waers.



        WHEN 'USD'.

          "Cenário 1: Descontos de antecipação com Adiantamentos contra banco
          SELECT *
          FROM zi_fi_juros_mi_c1 AS a
          WHERE bukrs IN @lra_bukrs
            AND augdt IN @lra_augdt
            AND waers EQ @p_waers
            AND EXISTS  ( SELECT b~vbelv
                                  FROM zsdt0090 AS b
                                 WHERE b~vbelv     EQ a~vbel2
                                   AND b~categoria EQ 'C'
                                   AND b~estorno   EQ '' )
          INTO TABLE @lit_juros_descontos_cenario_1.


      ENDCASE.


      DELETE lit_juros_descontos_cenario_1 WHERE nro_sol NOT IN lra_nr_sol
                                             AND vbel2   NOT IN lra_nr_ov
                                             AND kunnr   NOT IN lra_kunnr.

    WHEN 'TR'. "Troca Acerto
  ENDCASE.


  LOOP AT lit_juros_descontos_cenario_1 INTO DATA(lwa_juros_descontos).
    APPEND INITIAL LINE TO c_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

    <fs_saida>-waers     = lwa_juros_descontos-waers.
    <fs_saida>-bukrs     = lwa_juros_descontos-bukrs.
    <fs_saida>-butxt     = lwa_juros_descontos-butxt.
    <fs_saida>-gsber     = lwa_juros_descontos-gsber.
    <fs_saida>-kunnr     = lwa_juros_descontos-kunnr.
    <fs_saida>-name1     = lwa_juros_descontos-ds_cliente.
    <fs_saida>-tipo      = lwa_juros_descontos-tipo.
    <fs_saida>-tpsim     = lwa_juros_descontos-tpsim.
    <fs_saida>-auart     = lwa_juros_descontos-auart.
    <fs_saida>-matnr     = lwa_juros_descontos-matnr.
    <fs_saida>-matkl     = lwa_juros_descontos-matkl.
    <fs_saida>-maktx     = lwa_juros_descontos-maktx.
    <fs_saida>-nr_sol    = lwa_juros_descontos-nro_sol.
    <fs_saida>-tp_venda  = lwa_juros_descontos-tp_venda.
    <fs_saida>-buzei     = lwa_juros_descontos-buzei.
    <fs_saida>-belnr     = lwa_juros_descontos-augbl.
    <fs_saida>-augbl     = lwa_juros_descontos-augbl.
    <fs_saida>-budat     = lwa_juros_descontos-augdt.
    <fs_saida>-augdt     = lwa_juros_descontos-augdt.
    <fs_saida>-vbel2     = lwa_juros_descontos-vbel2.
    <fs_saida>-banco_liq = lwa_juros_descontos-banco_liq.
    <fs_saida>-charg     = lwa_juros_descontos-charg.

    IF lwa_juros_descontos-desc_antecipacao_dmbtr >= 0.
      <fs_saida>-dmbtr = abs( lwa_juros_descontos-valor_desc_prop_dmbtr ).
      <fs_saida>-dmbe2 = abs( lwa_juros_descontos-valor_desc_prop_dmbe2 ).
    ELSE.
      <fs_saida>-dmbtr = abs( lwa_juros_descontos-valor_desc_prop_dmbtr ) * -1.
      <fs_saida>-dmbe2 = abs( lwa_juros_descontos-valor_desc_prop_dmbe2 ) * -1.
    ENDIF.


    CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 1.
      <fs_saida>-tx_camb =  <fs_saida>-dmbtr /  <fs_saida>-dmbe2.
    ENDCATCH.



  ENDLOOP.

ENDFORM.
