"Name: \PR:RFFOBR_D\FO:FILL_DETAILS_FEBRABAN\SE:END\EI
ENHANCEMENT 0 Z_MULTA_R.
*
***SELECT  *
***  FROM  bkpf
***  INTO TABLE lit_bkpf
***  WHERE bukrs EQ regup-bukrs
***    AND belnr EQ regup-belnr.
***
***  LOOP AT lit_bkpf INTO lwa_bkpf.
***    MOVE lwa_bkpf-awkey TO lwa_bkpf_62-awkey.
***    lwa_bkpf_62-belnr = lwa_bkpf-belnr.
***
***    APPEND lwa_bkpf_62 TO lit_bkpf_62.
***    CLEAR: lwa_bkpf_62.
***  ENDLOOP.
***
***  SELECT  *
***    FROM  vbrp
***    INTO TABLE lit_vbrp
***           FOR ALL ENTRIES IN lit_bkpf_62
***    WHERE vbeln = lit_bkpf_62-awkey.
***
***  IF lit_vbrp IS NOT INITIAL.
***    SELECT  *
***     FROM  zsdt0053
***     INTO TABLE lit_zsdt0053
***            FOR ALL ENTRIES IN lit_vbrp
***     WHERE vbeln = lit_vbrp-aubel.
***
***    SELECT  *
***     FROM  zsdt0051
***     INTO TABLE lit_zsdt0051
***            FOR ALL ENTRIES IN lit_zsdt0053
***     WHERE nro_sol_ov = lit_zsdt0053-nro_sol_ov.
***  ELSE.
***    READ TABLE lit_bkpf INTO wl_lit_bkpf INDEX 1.
***    IF wl_lit_bkpf-bstat EQ 'S'.
***      SELECT  *
***       FROM  zsdt0054
***       INTO TABLE lit_zsdt0054
***              FOR ALL ENTRIES IN lit_bkpf
***       WHERE ADIANT = lit_bkpf-belnr.
***
***      SELECT  *
***       FROM  zsdt0051
***       INTO TABLE lit_zsdt0051
***              FOR ALL ENTRIES IN lit_zsdt0054
***       WHERE nro_sol_ov = lit_zsdt0054-nro_sol_ov.
***    ENDIF.
***  ENDIF.
***
***  IF reguh-ubnkl(3) eq '001'."Banco do Brasil
***    IF lit_zsdt0051 IS NOT INITIAL.
***      READ TABLE lit_zsdt0051 INTO lwa_zsdt0051 INDEX 1.
***      IF lwa_zsdt0051-tx_multa > 0.
***          MOVE lwa_zsdt0051-tx_multa TO ZFI_J_1BDMEXR-r16.
***      ENDIF.
***    ELSE.
***      LOOP AT lit_bkpf INTO lwa_bkpf.
***        MOVE lwa_bkpf-awkey TO lwa_znfw-obj_key.
***
***        IF lwa_znfw-obj_key IS NOT INITIAL.
***          lwa_znfw-bukrs = lwa_bkpf-bukrs.
***          lwa_znfw-belnr = lwa_bkpf-belnr.
***
***          APPEND lwa_znfw TO lit_znfw.
***          CLEAR: lwa_znfw.
***        ENDIF.
***      ENDLOOP.
***
***      IF lit_znfw IS NOT INITIAL.
***        SELECT  *
***        FROM   zfiwrt0008
***        INTO TABLE lit_zfiwrt0008
***               FOR ALL ENTRIES IN lit_znfw
***        WHERE obj_key EQ lit_znfw-obj_key
***          AND bukrs = lit_znfw-bukrs .
***
***        IF lit_zfiwrt0008 IS NOT INITIAL.
***          SELECT  *
***            FROM zfiwrt0011
***         INTO TABLE lit_zfiwrt0011
***            FOR ALL ENTRIES IN lit_zfiwrt0008
***              WHERE seq_lcto  EQ lit_zfiwrt0008-seq_lcto
***                AND zlsch EQ 'D'
***                AND estorno NE 'X'.
***        ENDIF.
***      ENDIF.
***
***      IF lit_zfiwrt0011 IS NOT INITIAL.
***        READ TABLE lit_zfiwrt0011 INTO lwa_zfiwrt0011 INDEX 1.
***        IF lwa_zfiwrt0011-taxa_multa > 0.
***          MOVE lwa_zfiwrt0011-taxa_multa TO ZFI_J_1BDMEXR-r16.
***        ENDIF.
***      ENDIF.
***    ENDIF.

    CLEAR vl_valor.
    CALL FUNCTION 'Z_BUSCA_JUROS_MULTA_BOLETO'
      EXPORTING
        i_belnr        = regup-belnr
        i_bukrs        = regup-bukrs
     IMPORTING
        e_multa        = vl_valor.

    MOVE vl_valor to ZFI_J_1BDMEXR-r16.

    IF ZFI_J_1BDMEXR-r16 > 0.

* -----Details segment R -----------------------------------------------
      cnt_posten           = cnt_posten + 1.
      cnt_records          = cnt_records + 1.

      ZFI_J_1BDMEXR-r01 = reguh-ubnkl(3).   "Código do Banco
      ZFI_J_1BDMEXR-r02 = 1.                "Número do Lote
      ZFI_J_1BDMEXR-r03 = '3'.              "Tipo de Registro

      ZFI_J_1BDMEXR-r04 = cnt_posten.       "record number
      ZFI_J_1BDMEXR-r05 = 'R'.              "segment type
*      ZFI_J_1BDMEXR-r06 =

      ZFI_J_1BDMEXR-r07 = '01'.    "instruction
*      ZFI_J_1BDMEXR-r08 =
*      ZFI_J_1BDMEXR-r09 =
*      ZFI_J_1BDMEXR-r10 =
*      ZFI_J_1BDMEXR-r11 =
*      ZFI_J_1BDMEXR-r12 =
*      ZFI_J_1BDMEXR-r13 =
      ZFI_J_1BDMEXR-r14 = '2'.

      CLEAR vl_data.
      vl_data = hlp_duedate.
      add 1 to vl_data.
      CONCATENATE vl_data+6(2) vl_data+4(2) vl_data+0(4) INTO ZFI_J_1BDMEXR-r15.

*      ZFI_J_1BDMEXR-r17 =
*      ZFI_J_1BDMEXR-r18 =
*      ZFI_J_1BDMEXR-r19 =
*      ZFI_J_1BDMEXR-r20 =
*      ZFI_J_1BDMEXR-r21 =
*      ZFI_J_1BDMEXR-r22 =
*      ZFI_J_1BDMEXR-r23 =
*      ZFI_J_1BDMEXR-r24 =
*      ZFI_J_1BDMEXR-r25 =
*      ZFI_J_1BDMEXR-r26 =
*      ZFI_J_1BDMEXR-r27 =
*      ZFI_J_1BDMEXR-r28 =
*      ZFI_J_1BDMEXR-r29 =
    ENDIF.
***  ENDIF.
ENDENHANCEMENT.
