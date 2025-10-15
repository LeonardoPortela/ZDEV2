*----------------------------------------------------------------------*
***INCLUDE LZLES_REPORTF06.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_ADT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OPCAO  text
*----------------------------------------------------------------------*
FORM selecionar_dados_adt USING p_opcao.

  CONSTANTS set_departamentos TYPE c LENGTH 17 VALUE 'MAGGI_ZLES0079_DP'.

  TYPES: BEGIN OF ty_contas,
           hkont TYPE bsis-hkont,
         END OF ty_contas.

  TYPES: BEGIN OF ty_setleaf,
           setname     TYPE setleaf-setname,
           valfrom     TYPE setleaf-valfrom,
           valfrom_aux TYPE zfit0045-dep_resp,
         END OF ty_setleaf.

  DATA: t_conta   TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
        tg_contas TYPE TABLE OF ty_contas WITH HEADER LINE.

  DATA: lt_setleaf TYPE TABLE OF ty_setleaf,
        lw_setleaf TYPE ty_setleaf.

  DATA: lt_zfit0045 TYPE TABLE OF zfit0045,
        lw_zfit0045 TYPE zfit0045.

  DATA: lt_ekko TYPE TABLE OF ekko,
        lw_ekko TYPE ekko.

  DATA: lt_lfa1 TYPE TABLE OF lfa1,
        lw_lfa1 TYPE lfa1.

  DATA: v_lcto_estorno TYPE c.
  DATA: v_continue   TYPE c,
        v_valida     TYPE c,
        vl_dif_banco TYPE bsak-dmbtr,
        vl_vlr_comp  TYPE bsak-dmbtr.


  DATA: gt_bsis_jd   TYPE TABLE OF bsis WITH HEADER LINE,
        gt_bsak_jd   TYPE TABLE OF ty_bsak WITH HEADER LINE,
        gt_bsak_comp TYPE TABLE OF ty_bsak WITH HEADER LINE.

  DATA: var_tabix TYPE sy-tabix.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv.

  FIELD-SYMBOLS: <fs_setleaf> TYPE ty_setleaf,
                 <fs_zglt035> TYPE ty_zglt035.


  REFRESH: lt_setleaf[], lt_zfit0045[], lt_ekko[], gt_bsak[], gt_bsak_adt_dev[].
  CLEAR: lw_setleaf, lw_zfit0045, lw_ekko, t_conta[], tg_contas[], gt_bsis_jd[], gt_bsak_jd[], gt_bsak_comp[].

  CREATE OBJECT obj_zcl_util_sd.

  "Selecionar todos os departamentos cadastrados no SET.
  SELECT setname valfrom FROM setleaf
    INTO TABLE lt_setleaf
 WHERE setname EQ set_departamentos.

  IF ( sy-subrc EQ 0 ).

    LOOP AT lt_setleaf ASSIGNING <fs_setleaf>.
      <fs_setleaf>-valfrom_aux = <fs_setleaf>-valfrom.
    ENDLOOP.
    UNASSIGN <fs_setleaf>.

    SELECT * FROM zfit0045
      INTO TABLE lt_zfit0045
      FOR ALL ENTRIES IN lt_setleaf
    WHERE dep_resp EQ lt_setleaf-valfrom_aux
      AND bukrs    IN it_bukrs.

    CHECK lt_zfit0045[] IS NOT INITIAL.

    CASE p_opcao.
      WHEN 'R_AD_CX'.

        SELECT * FROM ekko
          INTO TABLE lt_ekko
          FOR ALL ENTRIES IN lt_zfit0045
        WHERE ebeln EQ lt_zfit0045-ebeln
          AND bsart EQ 'NB'.
        "AND BSART IN ( 'NB', 'ZDEF', 'ZFTE', 'ZSEM' ).

      WHEN 'R_AD_PI'.

        SELECT * FROM ekko
          INTO TABLE lt_ekko
          FOR ALL ENTRIES IN lt_zfit0045
        WHERE ebeln EQ lt_zfit0045-ebeln
          "AND BSART EQ 'NB'.
          AND bsart IN ( 'ZDEF', 'ZFTE', 'ZSEM' ).

      WHEN 'R_EV_PRT'.

*-CS2022000256 - 24.03.2022 - JT - inicio
*        IF R_LIFNR1[] IS INITIAL.
*          RETURN.
*        ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

        SELECT * FROM ekko
          INTO TABLE lt_ekko
          FOR ALL ENTRIES IN lt_zfit0045
        WHERE ebeln EQ lt_zfit0045-ebeln
          AND lifnr IN r_lifnr1.

      WHEN 'R_AQ_PA'.

        IF r_lifnr[] IS INITIAL.
          RETURN.
        ENDIF.

        SELECT * FROM ekko
          INTO TABLE lt_ekko
          FOR ALL ENTRIES IN lt_zfit0045
        WHERE ebeln EQ lt_zfit0045-ebeln
          AND lifnr IN r_lifnr.

    ENDCASE.


    "Selecionar frete terceiro/proprio.
    "SELECT BUKRS AUGDT BLART SHKZG AUGBL BELNR GJAHR DMBTR DMBE2 MONAT XBLNR LIFNR ZFBDT ZBD1T SGTXT BUDAT UMSKS EBELN GSBER
    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr
           a~dmbe2 a~monat a~xblnr a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat
           a~umsks a~ebeln a~gsber a~auggj INTO CORRESPONDING FIELDS OF TABLE gt_bsak
      FROM bsak AS a
      INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                              a~augbl = b~belnr AND
                              a~gjahr = b~gjahr
      FOR ALL ENTRIES IN lt_zfit0045
    WHERE a~bukrs EQ lt_zfit0045-bukrs
      AND a~belnr EQ lt_zfit0045-belnr
      AND b~budat IN it_augdt.
    "AND A~AUGDT IN IT_AUGDT.

    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr
          a~dmbe2 a~monat a~xblnr a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat
          a~umsks a~ebeln a~gsber a~auggj INTO CORRESPONDING FIELDS OF TABLE gt_bsak_aux02
     FROM bsak AS a
     INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                             a~augbl = b~belnr AND
                             a~auggj = b~gjahr
     FOR ALL ENTRIES IN lt_zfit0045
   WHERE a~bukrs EQ lt_zfit0045-bukrs
     AND a~belnr EQ lt_zfit0045-belnr
     AND b~budat IN it_augdt.

    LOOP AT gt_bsak_aux02.
      IF gt_bsak_aux02-auggj EQ gt_bsak_aux02-gjahr.
        DELETE gt_bsak_aux02.
      ENDIF.
    ENDLOOP.

    LOOP AT gt_bsak_aux02 INTO DATA(lwa_bsak_aux02).
      APPEND lwa_bsak_aux02 TO gt_bsak.
    ENDLOOP.

*-CS2022000256 - 24.03.2022 - JT - inicio
    IF p_opcao = 'R_EV_PRT'.
      LOOP AT gt_bsak            INTO gw_bsak.
        MOVE-CORRESPONDING gw_bsak TO w_info_port.
        APPEND w_info_port         TO t_info_port.
      ENDLOOP.

      SORT t_info_port BY bukrs belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM t_info_port
                            COMPARING bukrs belnr gjahr.

      PERFORM f_filtrar_portuario.
    ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

    "Delete Registros com Doc. Comp. com iniciais diferente de 01,15,20
    LOOP AT gt_bsak INTO gw_bsak.
      CLEAR: var_tabix.
      var_tabix = sy-tabix.
      DATA(_delete) = ''.
      IF ( gw_bsak-augbl(2) NE '20' ) AND
         ( gw_bsak-augbl(2) NE '15' ) AND
         ( gw_bsak-augbl(2) NE '01' ).
        _delete = 'X'.
*      DELETE gt_bsak INDEX var_tabix.
      ENDIF.

*-CS2022000256 - 24.03.2022 - JT - inicio
      DATA(_ok) = ''.
      PERFORM f_check_portuario USING gw_bsak-bukrs gw_bsak-belnr gw_bsak-gjahr
                                      gw_bsak-lifnr gw_bsak-ebeln
                                      p_opcao
                             CHANGING gw_bsak-kostl gw_bsak-prctr gw_bsak-matnr
                                      gw_bsak-stcd1
                                     _ok.
      IF _ok IS INITIAL.
        _delete = 'X'.
      ELSE.
        MODIFY gt_bsak FROM gw_bsak INDEX var_tabix TRANSPORTING kostl prctr matnr stcd1.
      ENDIF.
      IF _delete IS NOT INITIAL.
        DELETE gt_bsak INDEX var_tabix.
      ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim
      CLEAR gw_bsak.
    ENDLOOP.

    "Verificar se tem contrapartida.
    IF NOT ( gt_bsak[] IS INITIAL ).

*      SELECT BUKRS AUGDT BLART SHKZG AUGBL BELNR GJAHR DMBTR DMBE2 MONAT XBLNR LIFNR ZFBDT ZBD1T SGTXT BUDAT UMSKS EBELN GSBER
*        FROM BSAK
*        INTO TABLE GT_BSAK_AUX
*        FOR ALL ENTRIES IN GT_BSAK
*      WHERE BUKRS EQ GT_BSAK-BUKRS
*        AND BELNR EQ GT_BSAK-AUGBL.

      IF p_opcao = 'R_AD_PI'  OR
         p_opcao = 'R_EV_PRT' OR
         p_opcao = 'R_AQ_PA'.

        "Devoluções de Adiantamento
        SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr
               a~dmbe2 a~monat a~xblnr a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat
               a~umsks a~ebeln a~gsber INTO TABLE gt_bsak_adt_dev
          FROM bsak AS a  INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                                                  a~augbl = b~belnr AND
                                                  a~auggj = b~gjahr
           FOR ALL ENTRIES IN gt_bsak
         WHERE a~bukrs EQ gt_bsak-bukrs
           AND a~umsks EQ 'A'
           AND a~gjahr EQ gt_bsak-augdt(4)
           AND a~belnr EQ gt_bsak-augbl
           AND b~budat IN it_augdt
           AND a~belnr NE a~augbl.

        LOOP AT gt_bsak_adt_dev INTO gw_bsak_adt_dev.
          CLEAR gw_bsak.
          MOVE-CORRESPONDING gw_bsak_adt_dev TO gw_bsak.
          APPEND gw_bsak TO gt_bsak.
        ENDLOOP.

        CALL FUNCTION 'G_SET_GET_ALL_VALUES'
          EXPORTING
            class         = '0000'
            setnr         = 'MAGGI_HEDGE_64_JROS'
          TABLES
            set_values    = t_conta
          EXCEPTIONS
            set_not_found = 1
            OTHERS        = 2.

        SORT t_conta BY from.

        LOOP AT t_conta.
          IF ( t_conta-from IS NOT INITIAL ).
            tg_contas-hkont = t_conta-from(10).
            APPEND tg_contas.
          ENDIF.
        ENDLOOP.

      ENDIF.


      SELECT * FROM bsis
        INTO TABLE gt_bsis
        FOR ALL ENTRIES IN gt_bsak
      WHERE bukrs EQ gt_bsak-bukrs
        AND belnr EQ gt_bsak-augbl
        AND gjahr EQ gt_bsak-gjahr.

      SELECT * FROM bsis
       APPENDING TABLE gt_bsis
       FOR ALL ENTRIES IN gt_bsak
     WHERE bukrs EQ gt_bsak-bukrs
       AND belnr EQ gt_bsak-augbl
       AND gjahr EQ gt_bsak-auggj.

      SORT gt_bsis BY bukrs hkont augdt augbl zuonr gjahr belnr buzei.
      DELETE ADJACENT DUPLICATES FROM gt_bsis COMPARING bukrs hkont augdt augbl zuonr gjahr belnr buzei.


      IF ( gt_bsis[] IS NOT INITIAL ).

        SELECT * FROM ska1       "#EC CI_DB_OPERATION_OK[2431747]
          INTO TABLE gt_ska1     "#EC CI_DB_OPERATION_OK[2389136]
          FOR ALL ENTRIES IN gt_bsis
        WHERE saknr EQ gt_bsis-hkont
          AND ktopl EQ '0050'
          AND ktoks EQ 'YB04'.

        IF ( sy-subrc EQ 0 ).
          SELECT * FROM skat
            INTO TABLE gt_skat
            FOR ALL ENTRIES IN gt_ska1
          WHERE saknr EQ gt_ska1-saknr
            AND spras EQ 'PT'.
        ENDIF.

      ENDIF.

    ENDIF.

    IF p_opcao EQ 'R_AD_CX'.

      "Lançamentos Manuais - Cabeçalho
      SELECT bukrs tp_lcto budat doc_lcto
         FROM zglt035
        INTO TABLE gt_zglt035
      WHERE bukrs   IN it_bukrs
        AND tp_lcto EQ '837'.

    ENDIF.

    IF ( gt_zglt035[] IS NOT INITIAL ).

      LOOP AT gt_zglt035 ASSIGNING <fs_zglt035>. "Ajustar o campo objkey.
        CONCATENATE 'ZGL17' <fs_zglt035>-doc_lcto <fs_zglt035>-budat(4) INTO <fs_zglt035>-obj_key.
      ENDLOOP.
      UNASSIGN <fs_zglt035>.

      SELECT * FROM zib_contabil_chv
        INTO TABLE gt_zib_contabil_chv
        FOR ALL ENTRIES IN gt_zglt035
      WHERE obj_key EQ gt_zglt035-obj_key.

      "Selecionar frete terceiro/proprio.
      SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr
             a~dmbe2 a~monat a~xblnr a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat
             a~umsks a~ebeln a~gsber APPENDING TABLE gt_bsak
        FROM bsak AS a
        INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                                a~augbl = b~belnr AND
                                a~gjahr = b~gjahr
        FOR ALL ENTRIES IN gt_zib_contabil_chv
      WHERE a~bukrs EQ gt_zib_contabil_chv-bukrs
        AND a~belnr EQ gt_zib_contabil_chv-belnr
        AND b~budat IN it_augdt.
      "AND AUGDT IN IT_AUGDT.

      "Delete Registros com Doc. Comp. com iniciais diferente de 01,15,20
      LOOP AT gt_bsak INTO gw_bsak.
        CLEAR: var_tabix.
        var_tabix = sy-tabix.
        IF ( gw_bsak-augbl(2) NE '20' ) AND
           ( gw_bsak-augbl(2) NE '15' ) AND
           ( gw_bsak-augbl(2) NE '01' ).
          DELETE gt_bsak INDEX var_tabix.
        ENDIF.
        CLEAR gw_bsak.
      ENDLOOP.

    ENDIF.

    IF NOT ( gt_bsak[] IS INITIAL ).

      SELECT * FROM lfa1
        INTO TABLE lt_lfa1
        FOR ALL ENTRIES IN gt_bsak
      WHERE lifnr EQ gt_bsak-lifnr.

      "Cabeçalho do documento contábil
      "SELECT AWKEY BKTXT BUKRS BELNR GJAHR BLART STBLG TCODE BUDAT
      SELECT *
        FROM bkpf INTO CORRESPONDING FIELDS OF TABLE gt_bkpf
        FOR ALL ENTRIES IN gt_bsak
       WHERE bukrs EQ gt_bsak-bukrs
         AND belnr EQ gt_bsak-augbl
         AND gjahr EQ gt_bsak-gjahr.

      SELECT *
        FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE gt_bkpf
        FOR ALL ENTRIES IN gt_bsak
       WHERE bukrs EQ gt_bsak-bukrs
         AND belnr EQ gt_bsak-augbl
         AND gjahr EQ gt_bsak-auggj.

      SORT gt_bkpf BY bukrs belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM gt_bkpf COMPARING bukrs belnr gjahr.

    ENDIF.

    "Check Contas Banco BSIS.
    LOOP AT gt_bsis INTO gw_bsis.
      CLEAR: var_tabix.
      var_tabix = sy-tabix.

      "Carregar Contas Juros/Descontos Adiantamentos
      READ TABLE tg_contas WITH KEY hkont = gw_bsis-hkont.
      IF sy-subrc = 0.
        CLEAR: gt_bsis_jd.
        MOVE-CORRESPONDING gw_bsis TO gt_bsis_jd.
        APPEND gt_bsis_jd.
      ENDIF.

      READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
      IF sy-subrc NE 0.
        DELETE gt_bsis INDEX var_tabix.
      ENDIF.
    ENDLOOP.
    "Fim Contas Banco BSIS

    IF p_opcao = 'R_AD_PI'  OR
       p_opcao = 'R_EV_PRT' OR
       p_opcao = 'R_AQ_PA'.

      gt_bsak_comp[] = gt_bsak[].
      gt_bsak_jd[]   = gt_bsak[].
      SORT gt_bsak_jd BY bukrs augbl.
      DELETE ADJACENT DUPLICATES FROM gt_bsak_jd COMPARING bukrs augbl.
      SORT gt_bsak_jd BY bukrs belnr augbl.
    ENDIF.

    IF NOT ( gt_bsak[] IS INITIAL ).

      LOOP AT lt_zfit0045 INTO lw_zfit0045.

        READ TABLE lt_ekko INTO lw_ekko WITH KEY ebeln = lw_zfit0045-ebeln.
        IF ( sy-subrc NE 0 ).
          CONTINUE.
        ENDIF.

        gw_saida-bsart = lw_ekko-bsart.
        gw_saida-ebeln = lw_zfit0045-ebeln.
        gw_saida-belnr = lw_zfit0045-belnr.

        READ TABLE gt_bsak INTO gw_bsak WITH KEY bukrs = lw_zfit0045-bukrs
                                                 belnr = lw_zfit0045-belnr.

        IF ( sy-subrc NE 0 ).
          CONTINUE.
        ENDIF.

        READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                                 belnr = gw_bsak-augbl
                                                 gjahr = gw_bsak-gjahr.

        IF sy-subrc NE 0.
          READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                                   belnr = gw_bsak-augbl
                                                   gjahr = gw_bsak-augdt(4).

          CHECK sy-subrc EQ 0.
        ENDIF.

        IF ( gw_bkpf-tcode EQ 'FB08').
          CONTINUE.
        ENDIF.

        CASE p_opcao.
          WHEN 'R_EV_PRT'.
            gw_saida-tipo  = 'EA'.
            gw_saida-bktxt = 'Adiantamento'.
          WHEN 'R_AD_PI'.
            gw_saida-tipo = 'IA'.
            gw_saida-bktxt = 'Adiantamento'.
          WHEN 'R_AD_CX'.
            gw_saida-tipo = 'FA'.
          WHEN 'R_AQ_PA'.
            gw_saida-tipo = 'PA'.
            gw_saida-bktxt = 'Adiantamento'.
        ENDCASE.

        gw_saida-bukrs = lw_zfit0045-bukrs.
        gw_saida-augbl = gw_bsak-augbl.
        gw_saida-augdt = gw_bkpf-budat.
        gw_saida-budat = gw_bsak-budat.
        gw_saida-gjahr = gw_bsak-gjahr.
        gw_saida-zfbdt = gw_bsak-zfbdt + gw_bsak-zbd1t.
*-CS2022000256 - 24.03.2022 - JT - inicio
        gw_saida-kostl = gw_bsak-kostl.
        gw_saida-prctr = gw_bsak-prctr.
        gw_saida-matnr = gw_bsak-matnr.
        gw_saida-stcd1 = gw_bsak-stcd1.
*-CS2022000256 - 24.03.2022 - JT - fim

        CLEAR: gw_bkpf_aux.
        SELECT SINGLE *
          FROM bkpf INTO gw_bkpf_aux
         WHERE bukrs = gw_bsak-bukrs
           AND belnr = gw_bsak-belnr
           AND gjahr = gw_bsak-gjahr.

        IF ( gw_bkpf_aux-waers NE 'BRL').
          CONTINUE.
        ENDIF.

        CLEAR: v_lcto_estorno.
        IF ( NOT gw_bkpf_aux-stgrd IS INITIAL ).
          v_lcto_estorno = 'X' .
        ENDIF.

        IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Gambeta, e não me pergunte porque.

          "        IF ( GW_SAIDA-AUGDT EQ '20150915') OR ( GW_SAIDA-AUGDT EQ '20150916').
          "          GW_SAIDA-DMBTR = GW_BSAK-DMBTR.
          "          GW_SAIDA-DMBE2 = GW_BSAK-DMBTR / CS_TAX_FIX.
          "       ELSE.
          gw_saida-dmbtr = gw_bsak-dmbtr.
          gw_saida-dmbe2 = gw_bsak-dmbtr / cs_tax_fix.

          "       ENDIF.

        ELSE.

          "GW_SAIDA-DMBTR = GW_BSAK-DMBTR.


*          READ TABLE GT_BSAK_AUX INTO GW_BSAK_AUX WITH KEY BUKRS = GW_BSAK-BUKRS
*                                                           BELNR = GW_BSAK-AUGBL.
*
*          IF ( SY-SUBRC EQ 0 ).

          READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsak-bukrs
                                                   belnr = gw_bsak-augbl
                                                   gjahr = gw_bsak-gjahr.
          IF ( sy-subrc NE 0 ).
            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsak-bukrs
                                                     belnr = gw_bsak-augbl
                                                     gjahr = gw_bsak-auggj.
            IF ( sy-subrc NE 0 ).
              CONTINUE.
            ENDIF.
          ENDIF.

          READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont
                                                   ktopl = '0050'
                                                   ktoks = 'YB04'.

          CHECK sy-subrc = 0.

          IF ( p_opcao NE 'R_AD_PI'  ) AND
             ( p_opcao NE 'R_EV_PRT' ) AND
             ( p_opcao NE 'R_AQ_PA'  ).

            gw_saida-dmbe2 = gw_bsis-dmbe2.
            gw_saida-dmbtr = gw_bsis-dmbtr.

          ELSE.
            IF p_opcao = 'R_AD_PI'  OR
               p_opcao = 'R_EV_PRT' OR
               p_opcao = 'R_AQ_PA'.

              CLEAR: gw_saida-dmbtr, gw_saida-dmbe2.

              LOOP AT gt_bsak INTO gw_bsak WHERE bukrs = lw_zfit0045-bukrs
                                             AND belnr = lw_zfit0045-belnr.
                ADD gw_bsak-dmbtr TO gw_saida-dmbtr.
                ADD gw_bsak-dmbe2 TO gw_saida-dmbe2.
              ENDLOOP.

              "Pegar Valor Compensação
              CLEAR: vl_vlr_comp.
              LOOP AT gt_bsak_comp WHERE bukrs = gw_bsak-bukrs
                                     AND augbl = gw_bsak-augbl .
                ADD gt_bsak_comp-dmbtr TO vl_vlr_comp.
              ENDLOOP.

              "Se for primeiro Lançamento da compensação
              READ TABLE gt_bsak_jd WITH KEY bukrs = gw_bsak-bukrs
                                             belnr = gw_bsak-belnr
                                             augbl = gw_bsak-augbl BINARY SEARCH.
              IF ( sy-subrc = 0 ) AND ( vl_vlr_comp IS NOT INITIAL ).
                "Check se Compensação do banco possui juros ou descontos
                READ TABLE gt_bsis_jd WITH KEY bukrs = gw_bsak-bukrs
                                               belnr = gw_bsak-augbl
                                               gjahr = gw_bsak-gjahr.

                IF sy-subrc = 0.
                  IF gt_bsis_jd-shkzg = 'H'. "Adicionar Valor na partida de Banco para calcular diferença na sequencia
                    ADD gt_bsis_jd-dmbtr TO gw_bsis-dmbtr.
                  ELSE.
                    SUBTRACT gt_bsis_jd-dmbtr FROM gw_bsis-dmbtr.
                  ENDIF.
                ENDIF.

                IF vl_vlr_comp NE gw_bsis-dmbtr.
                  CLEAR: vl_dif_banco.
                  vl_dif_banco   = gw_bsis-dmbtr - vl_vlr_comp.
                  IF vl_dif_banco NE 0.
                    gw_saida-dmbtr = gw_saida-dmbtr + vl_dif_banco.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              gw_saida-dmbtr = gw_bsak-dmbtr.
              gw_saida-dmbe2 = gw_bsak-dmbe2.
            ENDIF.
          ENDIF.

          READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
          IF sy-subrc = 0.
            CONCATENATE gw_ska1-saknr '-' gw_skat-txt20 INTO gw_saida-banco_liq.
          ENDIF.

          "Busca Taxa Data Compensação
          CLEAR: vl_tx_cambio.
          MOVE gw_saida-augdt TO vl_gdatu.

          obj_zcl_util_sd->set_kurst('B').
          obj_zcl_util_sd->set_waerk('USD').
          obj_zcl_util_sd->set_tcurr('BRL').
          obj_zcl_util_sd->set_data( vl_gdatu ).

          vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

          IF vl_tx_cambio > 0.
            IF gw_bkpf_aux-waers EQ 'BRL'.
              gw_saida-dmbe2 = gw_saida-dmbtr / vl_tx_cambio.
            ELSE.
              gw_saida-dmbtr = gw_saida-dmbe2 * vl_tx_cambio.
            ENDIF.
            gw_saida-tx_camb = vl_tx_cambio.
          ENDIF.

        ENDIF.


        gw_saida-xblnr = gw_bsak-xblnr.
        gw_saida-sgtxt = gw_bsak-sgtxt.

        READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bsak-lifnr.
        gw_saida-lifnr = lw_lfa1-lifnr.
        gw_saida-name1 = lw_lfa1-name1.

        PERFORM f_valida_lcto USING gw_saida p_opcao
                           CHANGING v_valida.

        IF v_valida IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND gw_saida TO gt_saida.

        "Verifica se deve lançar estorno
        PERFORM f_check_estorno USING gw_saida.
*        IF ( NOT V_LCTO_ESTORNO IS INITIAL ).
*          GW_SAIDA-DMBTR = GW_SAIDA-DMBTR * -1.
*          GW_SAIDA-DMBE2 = GW_SAIDA-DMBE2 * -1.
*          GW_SAIDA-XBLNR = 'ESTORNO'.
*          APPEND GW_SAIDA TO GT_SAIDA.
*        ENDIF.

        CLEAR: lw_zfit0045, lw_ekko, gw_bsak, lw_lfa1, gw_saida, gw_bsis.

      ENDLOOP.

      "Devoluções de Adiantamento ----------------------------------------------------*
      IF p_opcao = 'R_AD_PI'  OR
         p_opcao = 'R_EV_PRT' OR
         p_opcao = 'R_AQ_PA'.

        LOOP AT lt_zfit0045 INTO lw_zfit0045.

          READ TABLE lt_ekko INTO lw_ekko WITH KEY ebeln = lw_zfit0045-ebeln.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          gw_saida-bsart = lw_ekko-bsart.
          gw_saida-ebeln = lw_zfit0045-ebeln.

          READ TABLE gt_bsak INTO gw_bsak WITH KEY bukrs = lw_zfit0045-bukrs
                                                   belnr = lw_zfit0045-belnr.
          IF ( sy-subrc NE 0 ).
            CONTINUE.
          ENDIF.

          CLEAR: gw_bsak_adt_dev.
          READ TABLE gt_bsak_adt_dev INTO gw_bsak_adt_dev WITH KEY bukrs = gw_bsak-bukrs
                                                                   gjahr = gw_bsak-augdt(4)
                                                                   belnr = gw_bsak-augbl.
          CHECK sy-subrc = 0.

          CLEAR: gw_bsak.
          MOVE-CORRESPONDING gw_bsak_adt_dev TO gw_bsak.

          READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                                   belnr = gw_bsak-augbl
                                                   gjahr = gw_bsak-gjahr.


          IF ( gw_bkpf-tcode EQ 'FB08').
            CONTINUE.
          ENDIF.

          CASE p_opcao.
            WHEN 'R_EV_PRT'.
              gw_saida-tipo  = 'EA'.
              gw_saida-bktxt = 'Adiantamento'.
            WHEN 'R_AD_PI'.
              gw_saida-tipo = 'IA'.
              gw_saida-bktxt = 'Adiantamento'.
            WHEN 'R_AD_CX'.
              gw_saida-tipo = 'FA'.
            WHEN 'R_AQ_PA'.
              gw_saida-tipo  = 'PA'.
              gw_saida-bktxt = 'Adiantamento'.
          ENDCASE.

          gw_saida-bukrs = lw_zfit0045-bukrs.
          gw_saida-belnr = gw_bsak-belnr.
          gw_saida-augbl = gw_bsak-augbl.
          gw_saida-augdt = gw_bkpf-budat.
          gw_saida-budat = gw_bsak-budat.
          gw_saida-gjahr = gw_bsak-gjahr.
          gw_saida-zfbdt = gw_bsak-zfbdt + gw_bsak-zbd1t.

          CLEAR: gw_bkpf_aux.
          SELECT SINGLE *
            FROM bkpf INTO gw_bkpf_aux
           WHERE bukrs = gw_bsak-bukrs
             AND belnr = gw_bsak-belnr
             AND gjahr = gw_bsak-gjahr.

          IF ( gw_bkpf_aux-waers NE 'BRL').
            CONTINUE.
          ENDIF.

          IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Gambeta, e não me pergunte porque.
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = gw_bsak-dmbtr / cs_tax_fix.
          ELSE.
            READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsak-bukrs
                                                     belnr = gw_bsak-augbl
                                                     gjahr = gw_bsak-augdt(4).
            IF ( sy-subrc NE 0 ).
              CONTINUE.
            ENDIF.

            READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont
                                                     ktopl = '0050'
                                                     ktoks = 'YB04'.


            CHECK ( sy-subrc EQ 0 ).

            LOOP AT gt_bsak INTO gw_bsak WHERE bukrs = lw_zfit0045-bukrs
                                           AND belnr = lw_zfit0045-belnr.
              ADD gw_bsak-dmbtr TO gw_saida-dmbtr.
              ADD gw_bsak-dmbe2 TO gw_saida-dmbe2.
            ENDLOOP.

            "Pegar Valor Compensação
            CLEAR: vl_vlr_comp.
            LOOP AT gt_bsak_comp WHERE bukrs = gw_bsak-bukrs
                                   AND augbl = gw_bsak-augbl .
              ADD gt_bsak_comp-dmbtr TO vl_vlr_comp.
            ENDLOOP.

            "Se for primeiro Lançamento da compensação
            READ TABLE gt_bsak_jd WITH KEY bukrs = gw_bsak-bukrs
                                           belnr = gw_bsak-belnr
                                           augbl = gw_bsak-augbl BINARY SEARCH.

            IF ( sy-subrc = 0 ) AND ( vl_vlr_comp IS NOT INITIAL ).
              "Check se Compensação do banco possui juros ou descontos
              READ TABLE gt_bsis_jd WITH KEY bukrs = gw_bsak-bukrs
                                             belnr = gw_bsak-augbl
                                             gjahr = gw_bsak-gjahr.

              IF sy-subrc = 0.
                IF gt_bsis_jd-shkzg = 'H'. "Adicionar Valor na partida de Banco para calcular diferença na sequencia
                  ADD gt_bsis_jd-dmbtr TO gw_bsis-dmbtr.
                ELSE.
                  SUBTRACT gt_bsis_jd-dmbtr FROM gw_bsis-dmbtr.
                ENDIF.
              ENDIF.

              IF vl_vlr_comp NE gw_bsis-dmbtr.
                CLEAR: vl_dif_banco.
                vl_dif_banco   = gw_bsis-dmbtr - vl_vlr_comp.
                IF vl_dif_banco NE 0.
                  gw_saida-dmbtr = gw_saida-dmbtr + vl_dif_banco.
                ENDIF.
              ENDIF.
            ENDIF.

            READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
            IF sy-subrc = 0.
              CONCATENATE gw_ska1-saknr '-' gw_skat-txt20 INTO gw_saida-banco_liq.
            ENDIF.

            "Busca Taxa Data Compensação
            CLEAR: vl_tx_cambio.
            MOVE gw_saida-augdt TO vl_gdatu.

            obj_zcl_util_sd->set_kurst('B').
            obj_zcl_util_sd->set_waerk('USD').
            obj_zcl_util_sd->set_tcurr('BRL').
            obj_zcl_util_sd->set_data( vl_gdatu ).

            vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

            IF vl_tx_cambio > 0.

              IF gw_bkpf_aux-waers EQ 'BRL'.
                gw_saida-dmbe2 = gw_saida-dmbtr / vl_tx_cambio.
              ELSE.
                gw_saida-dmbtr = gw_saida-dmbe2 * vl_tx_cambio.
              ENDIF.

              gw_saida-tx_camb = vl_tx_cambio.
            ENDIF.

          ENDIF.

          IF gw_bsak-shkzg = 'S'.
            gw_saida-dmbtr = gw_saida-dmbtr * -1.
            gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
          ENDIF.

          gw_saida-xblnr = gw_bsak-xblnr.
          gw_saida-sgtxt = gw_bsak-sgtxt.

          READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bsak-lifnr.
          gw_saida-lifnr = lw_lfa1-lifnr.
          gw_saida-name1 = lw_lfa1-name1.

          PERFORM f_valida_lcto USING gw_saida p_opcao
                             CHANGING v_valida.

          IF v_valida IS INITIAL.
            CONTINUE.
          ENDIF.

          APPEND gw_saida TO gt_saida.

          "Verifica se deve lançar estorno
          PERFORM f_check_estorno USING gw_saida.

          CLEAR: lw_zfit0045, lw_ekko, gw_bsak, lw_lfa1, gw_saida, gw_bsis.

        ENDLOOP.
      ENDIF. "IF P_OPCAO = 'R_AD_PI'.
      "Fim Devoluções Adiantamento ---------------------------------------------------*

    ENDIF.

    IF ( NOT ( gt_zglt035[] IS INITIAL ) ) AND
       ( NOT ( gt_bsak[]    IS INITIAL ) ).

      LOOP AT gt_zglt035 INTO gw_zglt035.

        READ TABLE gt_zib_contabil_chv INTO gw_zib_contabil_chv WITH KEY obj_key = gw_zglt035-obj_key.
        READ TABLE gt_bsak INTO gw_bsak WITH KEY bukrs = gw_zib_contabil_chv-bukrs
                                                 belnr = gw_zib_contabil_chv-belnr.

        IF ( sy-subrc NE 0 ).
          CONTINUE.
        ENDIF.

        READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                                 belnr = gw_bsak-augbl
                                                 gjahr = gw_bsak-gjahr.
        IF ( sy-subrc NE 0 ).
          CONTINUE.
        ENDIF.

        PERFORM f_check_zgl047 USING gw_zglt035-bukrs
                                     gw_zglt035-doc_lcto
                            CHANGING v_continue.
        IF v_continue IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        CASE p_opcao.
          WHEN 'R_EV_PRT'.
            gw_saida-tipo = 'EA'.
          WHEN 'R_AD_PI'.
            gw_saida-tipo = 'IA'.
          WHEN 'R_AD_CX'.
            gw_saida-tipo = 'FA'.
          WHEN 'R_AQ_PA'.
            gw_saida-tipo = 'PA'.
        ENDCASE.

        gw_saida-bukrs = gw_zglt035-bukrs.
        gw_saida-belnr = gw_zib_contabil_chv-belnr.
        gw_saida-augbl = gw_bsak-augbl.
        gw_saida-augdt = gw_bkpf-budat.
        gw_saida-budat = gw_bsak-budat.
        gw_saida-zfbdt = gw_bsak-zfbdt + gw_bsak-zbd1t.
        gw_saida-xblnr = gw_bsak-xblnr.
        gw_saida-sgtxt = gw_bsak-sgtxt.
        gw_saida-gjahr = gw_bsak-gjahr.

        CLEAR: gw_bkpf_aux.
        SELECT SINGLE *
          FROM bkpf INTO gw_bkpf_aux
         WHERE bukrs = gw_bsak-bukrs
           AND belnr = gw_bsak-belnr
           AND gjahr = gw_bsak-gjahr.

        IF ( gw_bkpf_aux-waers NE 'BRL').
          CONTINUE.
        ENDIF.

        CLEAR: v_lcto_estorno.
        IF ( NOT gw_bkpf_aux-stgrd IS INITIAL ).
          v_lcto_estorno = 'X' .
        ENDIF.

        IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Gambeta, e não me pergunte porque.

          IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = gw_bsak-dmbtr / cs_tax_fix.
          ENDIF.

        ELSE.
          gw_saida-dmbtr = gw_bsak-dmbtr.

          READ TABLE gt_bsak_aux INTO gw_bsak_aux WITH KEY bukrs = gw_bsak-bukrs
                                                           belnr = gw_bsak-augbl.

          IF ( sy-subrc EQ 0 ).
            gw_saida-dmbe2 = gw_bsak_aux-dmbe2.
          ELSE.
            gw_saida-dmbe2 = gw_bsak-dmbe2.
          ENDIF.

          "Busca Taxa Data Compensação
          CLEAR: vl_tx_cambio.
          MOVE gw_saida-augdt TO vl_gdatu.

          obj_zcl_util_sd->set_kurst('B').
          obj_zcl_util_sd->set_waerk('USD').
          obj_zcl_util_sd->set_tcurr('BRL').
          obj_zcl_util_sd->set_data( vl_gdatu ).

          vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

          IF vl_tx_cambio > 0.
            IF gw_bkpf_aux-waers EQ 'BRL'.
              gw_saida-dmbe2 = gw_saida-dmbtr / vl_tx_cambio.
            ELSE.
              gw_saida-dmbtr = gw_saida-dmbe2 * vl_tx_cambio.
            ENDIF.
            gw_saida-tx_camb = vl_tx_cambio.
          ENDIF.

        ENDIF.

        READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bsak-lifnr.
        gw_saida-lifnr = lw_lfa1-lifnr.
        gw_saida-name1 = lw_lfa1-name1.

        PERFORM f_valida_lcto USING gw_saida p_opcao
                           CHANGING v_valida.

        IF v_valida IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND gw_saida TO gt_saida.

        "Verifica se deve lançar estorno
        PERFORM f_check_estorno USING gw_saida.
*        IF ( NOT V_LCTO_ESTORNO IS INITIAL ).
*          GW_SAIDA-DMBTR = GW_SAIDA-DMBTR * -1.
*          GW_SAIDA-DMBE2 = GW_SAIDA-DMBE2 * -1.
*          GW_SAIDA-XBLNR = 'ESTORNO'.
*          APPEND GW_SAIDA TO GT_SAIDA.
*        ENDIF.

        CLEAR: gw_saida,gw_zglt035, gw_zib_contabil_chv, gw_bsak, gw_bsak_aux, gw_bkpf.
      ENDLOOP.
    ENDIF.

  ELSE.
    MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Não existe departamento no SET MAGGI_ZLES0079_DP'.
    RETURN.
  ENDIF.


ENDFORM. "Fim SELECIONAR_DADOS_ADT.

FORM selecionar_dados_juros USING p_opcao.

  DATA: lt_ekko TYPE TABLE OF ekko,
        lw_ekko TYPE ekko.

  DATA: lt_lfa1 TYPE TABLE OF lfa1,
        lw_lfa1 TYPE lfa1,
        gt_bkpf TYPE TABLE OF ty_bkpf WITH HEADER LINE,
        gt_ekbe TYPE TABLE OF ty_ekbe WITH HEADER LINE.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv,
        v_valida     TYPE c,
        var_tabix    TYPE sy-tabix,
        vl_min_data  TYPE date VALUE '20160323'.

  TYPES: BEGIN OF ty_contas,
           hkont TYPE bsis-hkont,
         END OF ty_contas.

  DATA: t_conta   TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
        tg_contas TYPE TABLE OF ty_contas WITH HEADER LINE.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_HEDGE_64_JROS'
    TABLES
      set_values    = t_conta
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  SORT t_conta BY from.

  CLEAR: tg_contas[].

  LOOP AT t_conta.
    IF ( t_conta-from IS NOT INITIAL ).
      tg_contas-hkont = t_conta-from(10).
      APPEND tg_contas.
    ENDIF.
  ENDLOOP.

  CHECK tg_contas[] IS NOT INITIAL.

  CREATE OBJECT obj_zcl_util_sd.

  "Contabilidade financ.: índice secundário p/contas do Razão
  SELECT *
    FROM bsis INTO TABLE gt_bsis
     FOR ALL ENTRIES IN tg_contas
   WHERE bukrs IN it_bukrs
     AND hkont EQ tg_contas-hkont
     AND budat >= vl_min_data
     AND budat IN it_augdt
     AND waers IN r_waers.

  CHECK gt_bsis[] IS NOT INITIAL.

  CASE p_opcao.
    WHEN 'R_AD_PI'  OR
         'R_EV_PRT' OR
         'R_AQ_PA'.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE gt_bsak
        FROM bsak AS a FOR ALL ENTRIES IN gt_bsis
       WHERE a~bukrs EQ gt_bsis-bukrs
         AND a~lifnr IN r_lifnr
         AND a~augbl EQ gt_bsis-belnr
         AND a~auggj EQ gt_bsis-gjahr
         AND a~belnr NE a~augbl
         AND a~waers IN r_waers.

  ENDCASE.

*-CS2022000256 - 24.03.2022 - JT - inicio
  FREE: t_info_port.

  IF p_opcao = 'R_EV_PRT'.
    LOOP AT gt_bsak            INTO gw_bsak.
      MOVE-CORRESPONDING gw_bsak TO w_info_port.
      APPEND w_info_port         TO t_info_port.
    ENDLOOP.

    SORT t_info_port BY bukrs belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM t_info_port
                          COMPARING bukrs belnr gjahr.

    PERFORM f_filtrar_portuario.
  ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

  LOOP AT gt_bsak INTO gw_bsak.
    CLEAR: var_tabix.
    var_tabix = sy-tabix.
    DATA(_delete) = ''.

    DATA(_ok) = ''.

*-CS2022000256 - 24.03.2022 - JT - inicio
*   PERFORM f_check_elevacao USING gw_bsak
*                                  p_opcao
*                         CHANGING _ok.
    PERFORM f_check_portuario USING gw_bsak-bukrs gw_bsak-belnr gw_bsak-gjahr
                                    gw_bsak-lifnr gw_bsak-ebeln
                                    p_opcao
                           CHANGING gw_bsak-kostl gw_bsak-prctr gw_bsak-matnr
                                    gw_bsak-stcd1
                                   _ok.
*-CS2022000256 - 24.03.2022 - JT - fim

    IF _ok IS INITIAL.
      _delete = 'X'.
    ENDIF.

    IF _delete IS NOT INITIAL.
      DELETE gt_bsak INDEX var_tabix.
    ELSE.
      MODIFY gt_bsak FROM gw_bsak INDEX var_tabix TRANSPORTING kostl prctr matnr stcd1.
    ENDIF.
    CLEAR gw_bsak.
  ENDLOOP.

  CHECK gt_bsak[] IS NOT INITIAL.

  SELECT * FROM ska1           "#EC CI_DB_OPERATION_OK[2431747]
    INTO TABLE gt_ska1         "#EC CI_DB_OPERATION_OK[2389136]
    FOR ALL ENTRIES IN gt_bsis
  WHERE saknr EQ gt_bsis-hkont
    AND ktopl	EQ '0050'.

  CHECK NOT gt_ska1[] IS INITIAL.

  SELECT * FROM skat
    INTO TABLE gt_skat
    FOR ALL ENTRIES IN gt_ska1
  WHERE saknr EQ gt_ska1-saknr
    AND ktopl	EQ '0050'
    AND spras EQ sy-langu.

  SELECT * FROM ekko
    INTO TABLE gt_ekko
    FOR ALL ENTRIES IN gt_bsak
  WHERE ebeln EQ gt_bsak-ebeln
    AND bsart IN r_bsart
    AND waers IN r_waers.

  SELECT * FROM lfa1
    INTO TABLE lt_lfa1
    FOR ALL ENTRIES IN gt_bsak
  WHERE lifnr EQ gt_bsak-lifnr.

  "Cabeçalho do documento contábil
  SELECT *
    FROM bkpf INTO CORRESPONDING FIELDS OF TABLE gt_bkpf
    FOR ALL ENTRIES IN gt_bsak
   WHERE bukrs EQ gt_bsak-bukrs
     AND belnr EQ gt_bsak-augbl
     AND gjahr EQ gt_bsak-augdt(4)
     AND waers IN r_waers.

  SELECT *
    FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE gt_bkpf
    FOR ALL ENTRIES IN gt_bsak
   WHERE bukrs EQ gt_bsak-bukrs
     AND belnr EQ gt_bsak-belnr
     AND gjahr EQ gt_bsak-gjahr
     AND waers IN r_waers.

  CHECK gt_bkpf[] IS NOT INITIAL.

  LOOP AT gt_bkpf.
    CHECK strlen( gt_bkpf-awkey ) = 14.

    gt_bkpf-re_belnr = gt_bkpf-awkey(10).
    gt_bkpf-re_gjahr = gt_bkpf-awkey+10(4).

    MODIFY gt_bkpf.
  ENDLOOP.

  SELECT *
    FROM ekbe INTO CORRESPONDING FIELDS OF TABLE gt_ekbe
     FOR ALL ENTRIES IN gt_bkpf
   WHERE gjahr = gt_bkpf-re_gjahr
     AND belnr = gt_bkpf-re_belnr.

  IF gt_ekbe[] IS NOT INITIAL.
    SELECT *
      FROM ekko APPENDING TABLE gt_ekko
       FOR ALL ENTRIES IN gt_ekbe
     WHERE ebeln EQ gt_ekbe-ebeln
       AND bsart IN r_bsart
       AND waers IN r_waers.
  ENDIF.

  LOOP AT gt_bsis INTO gw_bsis.

    CLEAR: lw_lfa1, gw_ekko, gw_bsak , gw_ska1, gw_skat, gw_saida, v_valida.

    READ TABLE gt_bsak INTO gw_bsak WITH KEY bukrs    = gw_bsis-bukrs
                                             augbl    = gw_bsis-belnr
                                             augdt(4) = gw_bsis-gjahr.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_ekko INTO gw_ekko WITH KEY ebeln = gw_bsak-ebeln.
    IF ( sy-subrc NE 0 ).
      READ TABLE gt_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                  belnr = gw_bsak-belnr
                                  gjahr = gw_bsak-gjahr.

      CHECK ( sy-subrc = 0 ) AND ( gt_bkpf-re_belnr IS NOT INITIAL ) .

      READ TABLE gt_ekbe WITH KEY gjahr = gt_bkpf-re_gjahr
                                  belnr = gt_bkpf-re_belnr.
      CHECK sy-subrc = 0.

      READ TABLE gt_ekko INTO gw_ekko WITH KEY ebeln = gt_ekbe-ebeln.

      CHECK sy-subrc = 0.

    ENDIF.

    gw_saida-bsart = gw_ekko-bsart.
    gw_saida-ebeln = gw_ekko-ebeln.
    gw_saida-bukrs = gw_bsis-bukrs.
    gw_saida-gsber = gw_bsis-gsber.
    gw_saida-bktxt = 'Fatura'.
    gw_saida-belnr = gw_bsis-belnr.
    gw_saida-augbl = gw_bsis-belnr.
    gw_saida-budat = gw_bsis-budat.
    gw_saida-augdt = gw_bsis-budat.
    gw_saida-gjahr = gw_bsis-gjahr.

*-CS2022000256 - 24.03.2022 - JT - inicio
    gw_saida-kostl = gw_bsak-kostl.
    gw_saida-prctr = gw_bsak-prctr.
    gw_saida-matnr = gw_bsak-matnr.
    gw_saida-stcd1 = gw_bsak-stcd1.
*-CS2022000256 - 24.03.2022 - JT - fim

    "GW_SAIDA-ZFBDT = GW_BSIS-ZFBDT + GW_BSIS-ZBD1T.

    IF ( gw_bsis-dmbe2 > gw_bsis-dmbtr ). "Gambeta, e não me pergunte porque.
      gw_saida-dmbtr = gw_bsis-dmbtr.
      gw_saida-dmbe2 = gw_bsis-dmbtr / cs_tax_fix.
    ELSE.
      gw_saida-dmbe2 = gw_bsis-dmbe2.
      gw_saida-dmbtr = gw_bsis-dmbtr.
    ENDIF.

    READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.

    CHECK sy-subrc = 0.

    READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
    IF sy-subrc = 0.
      CONCATENATE gw_ska1-saknr '-' gw_skat-txt20 INTO gw_saida-banco_liq.
    ENDIF.

    SELECT SINGLE *
      FROM bkpf INTO @DATA(_wl_bkpf)
     WHERE bukrs EQ @gw_bsis-bukrs
       AND belnr EQ @gw_bsis-belnr.

    CHECK sy-subrc EQ 0.

    "Busca Taxa Data Compensação
    CLEAR: vl_tx_cambio.
    MOVE gw_saida-augdt TO vl_gdatu.

    obj_zcl_util_sd->set_kurst('B').
    obj_zcl_util_sd->set_waerk('USD').
    obj_zcl_util_sd->set_tcurr('BRL').
    obj_zcl_util_sd->set_data( vl_gdatu ).

    vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

    IF vl_tx_cambio > 0.

      IF _wl_bkpf-waers EQ 'BRL'.
        gw_saida-dmbe2   = gw_saida-dmbtr / vl_tx_cambio.
      ELSE.
        gw_saida-dmbtr   = gw_saida-dmbe2 * vl_tx_cambio.
      ENDIF.

      gw_saida-tx_camb = vl_tx_cambio.
    ENDIF.

    IF gw_bsis-shkzg = 'S'.
      gw_saida-dmbtr = gw_saida-dmbtr * -1.
      gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
    ENDIF.

    gw_saida-xblnr = gw_bsis-xblnr.
    gw_saida-sgtxt = gw_bsis-sgtxt.

    READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bsak-lifnr.
    IF sy-subrc = 0.
      gw_saida-lifnr = lw_lfa1-lifnr.
      gw_saida-name1 = lw_lfa1-name1.
    ENDIF.

    CASE p_opcao.
      WHEN 'R_AD_PI'.
        gw_saida-tipo = 'IP'.
      WHEN 'R_EV_PRT'.
        gw_saida-tipo = 'EP'.
      WHEN 'R_AQ_PA'.
        gw_saida-tipo = 'PA'.
    ENDCASE.

    PERFORM f_valida_lcto USING gw_saida p_opcao
                       CHANGING v_valida.

    IF v_valida IS INITIAL.
      CONTINUE.
    ENDIF.

    APPEND gw_saida TO gt_saida.

    "Verifica se deve lançar estorno
    PERFORM f_check_estorno USING gw_saida.

  ENDLOOP.



ENDFORM.

************************************************************************************************
* filtrar parametrizacao portuario
************************************************************************************************
FORM filtrar_portuario USING p_opcao.

*  DATA: t_saida_aux1 TYPE TABLE OF ty_saida,
*        t_saida_aux2 TYPE TABLE OF ty_saida,
*        t_saida_aux3 TYPE TABLE OF ty_saida.
*
*  CHECK gt_saida[] is NOT INITIAL.
*
*  t_saida_aux1[] = gt_saida[].
*  SORT t_saida_aux1 BY bukrs belnr gjahr.
*  DELETE ADJACENT DUPLICATES FROM t_saida_aux1
*                        COMPARING bukrs belnr gjahr.
*
*  SELECT bukrs, belnr, gjahr, waers
*    FROM bkpf
*    INTO TABLE @DATA(t_bkpf)
*     FOR ALL ENTRIES IN @t_saida_aux1
*   WHERE bukrs = @t_saida_aux1-bukrs
*     AND belnr = @t_saida_aux1-belnr
*     AND gjahr = @t_saida_aux1-gjahr.
*
*  SELECT bukrs, belnr, gjahr, buzei, kostl, prctr
*    FROM bseg
*    INTO TABLE @DATA(t_bseg)
*     FOR ALL ENTRIES IN @t_saida_aux1
*   WHERE bukrs = @t_saida_aux1-bukrs
*     AND belnr = @t_saida_aux1-belnr
*     AND gjahr = @t_saida_aux1-gjahr.
*
*  DELETE t_bseg WHERE kostl IS INITIAL
*                  AND prctr IS INITIAL.
*
*  t_saida_aux2[] = gt_saida[].
*  SORT t_saida_aux2 BY lifnr.
*  DELETE ADJACENT DUPLICATES FROM t_saida_aux2
*                        COMPARING lifnr.
*
*  SELECT lifnr, stcd1
*    FROM lfa1
*    INTO TABLE @DATA(t_lfa1)
*     FOR ALL ENTRIES IN @t_saida_aux2
*   WHERE lifnr = @t_saida_aux2-lifnr.
*
*  t_saida_aux3[] = gt_saida[].
*  SORT t_saida_aux3 BY ebeln.
*  DELETE ADJACENT DUPLICATES FROM t_saida_aux3
*                        COMPARING ebeln.
*
*  SELECT ebeln, ebelp, matnr
*    FROM ekpo
*    INTO TABLE @DATA(t_ekpo)
*     FOR ALL ENTRIES IN @t_saida_aux3
*   WHERE ebeln = @t_saida_aux3-ebeln.
*
*  SORT t_bkpf BY bukrs belnr gjahr.
*  SORT t_bseg BY bukrs belnr gjahr.
*  SORT t_lfa1 BY lifnr.
*  SORT t_ekpo BY ebeln ebelp.
*
*  LOOP AT gt_saida INTO DATA(gl_saida).
*
*    DATA(l_tabix) = sy-tabix.
*    DATA(l_erro)  = abap_true.
*    DATA(l_flag)  = abap_false.
*
*    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = gl_saida-bukrs
*                                                 belnr = gl_saida-belnr
*                                                 gjahr = gl_saida-gjahr
*                                        BINARY SEARCH.
*    CHECK sy-subrc = 0.
*
*    IF w_bkpf-waers = 'USD'.
*      READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY lifnr = gl_saida-lifnr
*                                          BINARY SEARCH.
*    ENDIF.
*
*    LOOP AT t_bseg INTO DATA(w_bseg) WHERE bukrs = gl_saida-bukrs
*                                       AND belnr = gl_saida-belnr
*                                       AND gjahr = gl_saida-gjahr.
*      l_flag = abap_true.
*
*      IF w_bseg-kostl IS NOT INITIAL.
*        READ TABLE t_zlest0217 INTO w_zlest0217 WITH KEY kostl = w_bseg-kostl.
*        IF sy-subrc IS INITIAL.
*          l_erro = abap_false.
*          EXIT.
*        ENDIF.
*      ENDIF.
*
*      IF w_bseg-prctr IS NOT INITIAL.
*        READ TABLE t_zlest0217 INTO w_zlest0217 WITH KEY prctr = w_bseg-prctr.
*        IF sy-subrc IS INITIAL.
*          l_erro = abap_false.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF l_flag = abap_false.
*      DELETE gt_saida INDEX l_tabix.
*      CONTINUE.
*    ENDIF.
*
*    IF l_erro = abap_true.
*      DELETE gt_saida INDEX l_tabix.
*      CONTINUE.
*    ENDIF.
*  ENDLOOP.
*
ENDFORM.

************************************************************************************************
************************************************************************************************
