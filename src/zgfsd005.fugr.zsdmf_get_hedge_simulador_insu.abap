FUNCTION zsdmf_get_hedge_simulador_insu.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IS_VKORG) TYPE  CMM_T_VKORG_RANGE
*"     REFERENCE(IS_SIM) TYPE  ZSDC0171_DOC_SIMU
*"     REFERENCE(IS_DATA) TYPE  RANGES_BUDAT_TT
*"  TABLES
*"      ET_SAIDA_0171 STRUCTURE  ZSDE0171_SAIDA
*"      ET_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA lt_0090 TYPE TABLE OF zsdt0090.
  DATA lt_vbak TYPE zsdc0171_vbak.
  DATA lt_vbap TYPE zsdc0171_vbap.
  DATA lt_tipo_disparo TYPE TABLE OF zsded_tp_disparo.

  DATA lt_0040 TYPE TABLE OF zsdt0040.
  DATA lt_0117 TYPE TABLE OF zsdt0117.
  DATA lt_0041 TYPE TABLE OF zsdt0041.
  DATA lt_0094 TYPE TABLE OF zsdt0094.
  DATA lt_0037 TYPE TABLE OF zsdt0037.
  DATA lt_ovs TYPE RANGE OF vbeln.

  DATA lt_nro_sol_ov TYPE zrsdsselopts.

  DATA lv_vlr_sim TYPE zdmbtr.
  DATA lv_taxa TYPE kursf.

  SELECT 'I' AS sign, 'EQ' AS option, doc_simulacao AS low FROM zsdt0040
    INTO TABLE @lt_nro_sol_ov
      WHERE data_atual    IN @is_data
        AND vkorg         IN @is_vkorg
        AND doc_simulacao IN @is_sim.

  CHECK sy-subrc EQ 0.

  SORT lt_nro_sol_ov BY low.

  DELETE ADJACENT DUPLICATES FROM lt_nro_sol_ov COMPARING low.

  SELECT * FROM zsdt0040
   INTO TABLE lt_0040
    WHERE doc_simulacao IN lt_nro_sol_ov.

  CHECK lt_0040 IS NOT INITIAL.

  SELECT * FROM zsdt0117
    INTO TABLE lt_0117
    FOR ALL ENTRIES IN lt_0040
   WHERE bukrs      EQ lt_0040-vkorg
     AND desativado EQ abap_false.

  SELECT * FROM zsdt0041
    INTO TABLE lt_0041
   WHERE doc_simulacao IN lt_nro_sol_ov.

  SELECT * FROM zsdt0094
    INTO TABLE lt_0094
   WHERE nro_sol_ov IN lt_nro_sol_ov.

  SELECT *
    FROM zsdt0090
    INTO TABLE lt_0090
   WHERE doc_simulacao IN lt_nro_sol_ov
     AND estorno EQ abap_false
     AND categoria NOT IN ( 'F','G','P' ).

  LOOP AT lt_0041 ASSIGNING FIELD-SYMBOL(<fs_0041>).

    IF <fs_0041>-vbeln IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0041>-vbeln TO lt_ovs.
    ENDIF.

  ENDLOOP.

  LOOP AT lt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>).

    IF <fs_0090>-vbelv IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0090>-vbelv TO lt_ovs.
    ENDIF.

    IF <fs_0090>-vbeln IS NOT INITIAL.
      APPEND 'IEQ' && <fs_0090>-vbeln TO lt_ovs.
    ENDIF.

  ENDLOOP.

  SORT lt_ovs ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_ovs.

  IF lt_ovs IS NOT INITIAL.

    SELECT vbeln, vbtyp, vbeln AS vbeln_o, knumv FROM vbak
      INTO TABLE @lt_vbak
      FOR ALL ENTRIES IN @lt_ovs
     WHERE vbeln EQ @lt_ovs-low.
    "AND vbak~vbtyp = 'C'."<-- Somente OV

    DELETE lt_vbak WHERE vbtyp = 'L'. " <-- retira Nota de credito

    CHECK lt_vbak IS NOT INITIAL.

    LOOP AT lt_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs>).

      DATA(lv_vbeln) = <fs_ovs>-low.

      READ TABLE lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
        WITH KEY vbeln = lv_vbeln.

      CHECK sy-subrc EQ 0.

      READ TABLE lt_0090 ASSIGNING FIELD-SYMBOL(<fs_0090_o>)
        WITH KEY vbeln = lv_vbeln.

      IF sy-subrc EQ 0.

        <fs_vbak>-doc_sim = <fs_0090_o>-doc_simulacao.

        IF <fs_0090_o>-vbelv IS NOT INITIAL.
          <fs_vbak>-vbeln_o = <fs_0090_o>-vbelv.
        ELSE.
          <fs_vbak>-vbeln_o = lv_vbeln.
        ENDIF.
      ELSE.

        <fs_vbak>-vbeln_o = lv_vbeln.

        READ TABLE lt_0041 ASSIGNING <fs_0041>
          WITH KEY vbeln = lv_vbeln.

        IF sy-subrc EQ 0.
          <fs_vbak>-doc_sim = <fs_0041>-doc_simulacao.
        ENDIF.

      ENDIF.

    ENDLOOP.

    SELECT vbap~vbeln, vbap~posnr, vbap~matnr, spart, netwr,vbap~waerk,
      brgew, mwsbp, kwmeng, kmein, lifsp FROM vbap AS vbap
      LEFT JOIN vbep ON vbep~vbeln = vbap~vbeln
                     AND vbep~posnr = vbap~posnr
                     AND vbep~etenr = '0001'
      INTO TABLE @lt_vbap
        FOR ALL ENTRIES IN @lt_vbak
          WHERE vbap~vbeln EQ @lt_vbak-vbeln.

  ENDIF.

  "BREAK rblima.

  LOOP AT lt_tipo_disparo ASSIGNING FIELD-SYMBOL(<fs_disparo>).

    LOOP AT lt_0040 ASSIGNING FIELD-SYMBOL(<fs_0040>).

      CLEAR: lv_taxa, lv_vlr_sim.

      READ TABLE lt_0041 ASSIGNING <fs_0041>
        WITH KEY doc_simulacao = <fs_0040>-doc_simulacao.

      CHECK sy-subrc EQ 0.

      APPEND INITIAL LINE TO et_saida_0171 ASSIGNING FIELD-SYMBOL(<fs_alv>).

      <fs_alv>-vkorg = <fs_0040>-vkorg.
      <fs_alv>-vkbur = <fs_0040>-vkbur.
      <fs_alv>-data_atual = <fs_0040>-data_atual.
      <fs_alv>-area_proc = 'IN'.
      <fs_alv>-doc_simulacao = <fs_0040>-doc_simulacao.
      <fs_alv>-fixacao = '0000'. "<-- sem fixação para insumos
      <fs_alv>-tp_diparo = <fs_disparo>.
      <fs_alv>-spart = <fs_0041>-spart.
      <fs_alv>-tpsim = <fs_0040>-tpsim.
      <fs_alv>-waerk = <fs_0040>-waerk.
      <fs_alv>-status = <fs_0040>-status.
      <fs_alv>-bezei = SWITCH #( <fs_0041>-spart
                                      WHEN '03' THEN 'DF'
                                      WHEN '02' THEN 'FT'
                                      WHEN '04' THEN |S{ <fs_0040>-cultura(1) }|
                                    ).


      "<fs_alv>-TP_VENDA =  .

      CLEAR <fs_alv>-vl_t0094_brl.
      CLEAR <fs_alv>-vl_t0094_usd.
      CLEAR <fs_alv>-vl_t0059_brl.
      CLEAR <fs_alv>-vl_t0059_usd.

      IF <fs_0040>-kursf IS NOT INITIAL.
        lv_taxa = <fs_0040>-kursf.
      ENDIF.

      LOOP AT lt_0094 ASSIGNING FIELD-SYMBOL(<fs_0094>)
          WHERE nro_sol_ov = <fs_alv>-doc_simulacao
            AND tipo = <fs_alv>-tp_diparo.

        <fs_alv>-vl_t0094_brl = <fs_alv>-vl_t0094_brl + <fs_0094>-total_proporc.

        CHECK <fs_0094>-taxa_cambio IS NOT INITIAL.
        <fs_alv>-vl_t0094_usd = <fs_alv>-vl_t0094_brl / <fs_0094>-taxa_cambio.

      ENDLOOP.

      IF lv_taxa IS INITIAL AND <fs_0094> IS ASSIGNED.
        lv_taxa = <fs_0094>-taxa_cambio.
      ENDIF.

      " proteção do codigo contra dump
      IF lv_taxa IS INITIAL.
        lv_taxa = 1.
      ENDIF.

      IF <fs_alv>-tp_diparo = 'VDI'.

        PERFORM f_valor_simulador
          USING lt_0090
                lt_vbak
                lt_vbap
                <fs_alv>-doc_simulacao
       CHANGING lv_vlr_sim.

        " se o valor do simulador estiver vazio, significa que nao tem ov ainda, então pega o valor total da 0040
        IF lv_vlr_sim IS INITIAL.

          lv_vlr_sim = <fs_0040>-vlrtot.

        ENDIF.

      ELSE.

        PERFORM f_valor_frete
          USING <fs_alv>-doc_simulacao
                <fs_alv>-spart
       CHANGING lv_vlr_sim.

      ENDIF.

      IF <fs_alv>-waerk = 'USD'.

        <fs_alv>-vl_t0059_usd = lv_vlr_sim.
        <fs_alv>-vl_t0059_brl = lv_vlr_sim * lv_taxa.

      ELSEIF <fs_alv>-waerk = 'BRL'.

        <fs_alv>-vl_t0059_brl = lv_vlr_sim.
        <fs_alv>-vl_t0059_usd = lv_vlr_sim / lv_taxa.

      ENDIF.

      <fs_alv>-diferenca_brl = <fs_alv>-vl_t0094_brl - <fs_alv>-vl_t0059_brl.
      <fs_alv>-diferenca_usd = <fs_alv>-vl_t0094_usd - <fs_alv>-vl_t0059_usd.

*      IF <fs_alv>-waerk = 'BRL'.

      <fs_alv>-vl_t0094 = <fs_alv>-vl_t0094_brl .
      <fs_alv>-vl_t0059 = <fs_alv>-vl_t0059_brl.
      <fs_alv>-diferenca = <fs_alv>-diferenca_brl.

*      ELSEIF <fs_alv>-waerk = 'USD'.
*
*        <fs_alv>-vl_t0094 = <fs_alv>-vl_t0094_usd .
*        <fs_alv>-vl_t0059 = <fs_alv>-vl_t0059_usd.
*        <fs_alv>-diferenca = <fs_alv>-diferenca_usd.
*
*      ENDIF.

      PERFORM f_valida_exibicao
        USING lt_0090
     CHANGING <fs_alv>.

    ENDLOOP.

  ENDLOOP.



ENDFUNCTION.
