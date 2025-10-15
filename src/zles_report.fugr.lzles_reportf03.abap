*----------------------------------------------------------------------*
***INCLUDE LZLES_REPORTF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_ADT_INSUMOS
*&---------------------------------------------------------------------*
FORM selecionar_dados_adt_n1 USING p_opcao.

  TYPES: BEGIN OF ty_contas,
           hkont TYPE bsis-hkont,
         END OF ty_contas.

*---------------------------------------------------*
*  Seleção Adiatamento
*---------------------------------------------------*
  IF ( p_opcao NE 'R_AQUAV' ) AND
     ( p_opcao NE 'R_AQ_PA' ).
    PERFORM selecionar_dados_adt USING p_opcao.
  ENDIF.
*---------------------------------------------------*
*  Seleção Faturas
*---------------------------------------------------*

  DATA: gt_bsak_aux        TYPE TABLE OF ty_bsak,
        gw_bsak_aux        TYPE ty_bsak,
        gt_tvtkt           TYPE TABLE OF tvtkt WITH HEADER LINE,
        gt_vttk            TYPE TABLE OF vttk WITH HEADER LINE,
        gt_zlest0032       TYPE TABLE OF zlest0032 WITH HEADER LINE,
        gt_bsak_ag_ft      TYPE TABLE OF ty_bsak WITH HEADER LINE,
        gt_bkpf            TYPE TABLE OF ty_bkpf WITH HEADER LINE,
        gt_ekbe            TYPE TABLE OF ty_ekbe WITH HEADER LINE,
        gt_bsis_jd         TYPE TABLE OF bsis WITH HEADER LINE,
        gt_bsak_jd         TYPE TABLE OF ty_bsak WITH HEADER LINE,
        gt_bsak_comp       TYPE TABLE OF ty_bsak WITH HEADER LINE,

        lit_zmmt0037       TYPE TABLE OF zmmt0037,
        lit_zmmt0035       TYPE TABLE OF zmmt0035,
        lit_ekko_principal TYPE TABLE OF ekko,
        lwa_ekko_principal TYPE ekko,
        lwa_ekko_origem    TYPE ekko,
        lit_ekko_origem    TYPE TABLE OF ekko,

        t_conta            TYPE STANDARD TABLE OF rgsb4 WITH HEADER LINE,
        tg_contas          TYPE TABLE OF ty_contas WITH HEADER LINE.

  DATA: lt_lfa1 TYPE TABLE OF lfa1,
        lw_lfa1 TYPE lfa1.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv,
        v_valida     TYPE c,
        vl_dif_banco TYPE bsak-dmbtr,
        vl_vlr_comp  TYPE bsak-dmbtr,
        var_tabix    TYPE sy-tabix.

  CLEAR: t_conta[], tg_contas[], gt_bsis_jd[], gt_bsak_jd[], gt_bsak_comp[], gt_bsik[], gt_bsak[],
         gt_bsak_ag_ft[], gt_zlest0032[], gt_tvtkt[], gt_vttk[], lit_zmmt0037[], lit_zmmt0035[], lit_ekko_origem[], lit_ekko_principal[].

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

  CREATE OBJECT obj_zcl_util_sd.

  SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr a~dmbe2 a~monat a~xblnr
         a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat a~umsks a~ebeln a~gsber a~waers
    INTO CORRESPONDING FIELDS OF TABLE gt_bsak
    FROM bsak AS a INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                                            a~augbl = b~belnr AND
                                            a~auggj = b~gjahr
    WHERE a~bukrs IN it_bukrs
      AND a~lifnr IN r_lifnr
      AND a~auggj IN r_auggj
      AND a~blart IN r_blart
      AND a~waers IN r_waers
      AND a~belnr NE a~augbl
      AND b~budat IN it_augdt.

  "Seleção feita no perform SELECIONAR_DADOS
  IF p_opcao EQ 'R_AQUAV'.
    gt_bsak_aux[] = gt_bsak[].

    LOOP AT gt_bsak_aux INTO DATA(wl_bsak_aux) WHERE ( blart EQ 'ME' ) OR
                                                     ( blart EQ 'FT' ).

      READ TABLE gt_saida INTO DATA(wl_saida_aux) WITH KEY bukrs = wl_bsak_aux-bukrs
                                                           belnr = wl_bsak_aux-belnr
                                                           augbl = wl_bsak_aux-augbl.
      IF sy-subrc EQ 0.
        DELETE gt_bsak WHERE bukrs = wl_bsak_aux-bukrs
                         AND belnr = wl_bsak_aux-belnr
                         AND augbl = wl_bsak_aux-augbl.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ( p_opcao EQ 'R_AQUAV' ) OR
     ( p_opcao EQ 'R_AQ_PA' ).

    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr a~dmbe2 a~monat a~xblnr
           a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat a~umsks a~ebeln a~gsber a~waers a~zuonr
       APPENDING CORRESPONDING FIELDS OF TABLE gt_bsak
       FROM bsak AS a INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                                              a~augbl = b~belnr AND
                                              a~auggj = b~gjahr
      WHERE a~bukrs IN it_bukrs
        AND a~lifnr IN r_lifnr
        AND a~auggj IN r_auggj
        AND a~blart EQ 'AG'    "Agrupamento
        AND a~waers IN r_waers
        AND a~belnr NE a~augbl
        AND b~budat IN it_augdt
        AND EXISTS ( SELECT belnr
                       FROM bsak AS c
                      WHERE c~bukrs EQ a~bukrs
                        AND c~augbl EQ a~belnr
                        AND c~augbl NE c~belnr
                        AND c~blart IN r_blart ).

    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr a~dmbe2 a~monat a~xblnr
           a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat a~umsks a~ebeln a~gsber a~waers a~zuonr
       APPENDING CORRESPONDING FIELDS OF TABLE gt_bsak_ag_ft
       FROM bsak AS a INNER JOIN bkpf AS b ON a~bukrs = b~bukrs AND
                                              a~augbl = b~belnr AND
                                              a~auggj = b~gjahr
      WHERE a~bukrs IN it_bukrs
        AND a~lifnr IN r_lifnr
        AND a~auggj IN r_auggj
        AND a~blart EQ 'AG'    "Agrupamento
        AND a~waers IN r_waers
        AND a~belnr NE a~augbl
        AND b~budat IN it_augdt
        AND EXISTS ( SELECT belnr
                       FROM bsak AS c
                      WHERE c~bukrs EQ a~bukrs
                        AND c~augbl EQ a~belnr
                        AND c~augbl NE c~belnr
                        AND c~blart EQ 'FT' ).


    LOOP AT gt_bsak_ag_ft ASSIGNING FIELD-SYMBOL(<fs_bsak_ag_ft>).
      CHECK <fs_bsak_ag_ft>-zuonr IS NOT INITIAL.
      <fs_bsak_ag_ft>-ebeln_fr = <fs_bsak_ag_ft>-zuonr(10).
    ENDLOOP.

    IF gt_bsak_ag_ft[] IS NOT INITIAL.
      SELECT *
        FROM zlest0032 INTO TABLE gt_zlest0032
         FOR ALL ENTRIES IN gt_bsak_ag_ft
       WHERE ebeln = gt_bsak_ag_ft-ebeln_fr.

      IF gt_zlest0032[] IS NOT INITIAL.

        SELECT *
          FROM vttk INTO TABLE gt_vttk
           FOR ALL ENTRIES IN gt_zlest0032
         WHERE tknum EQ gt_zlest0032-tknum.

        IF gt_vttk[] IS NOT INITIAL.
          SELECT *
            FROM tvtkt INTO TABLE gt_tvtkt
             FOR ALL ENTRIES IN gt_vttk
           WHERE spras = sy-langu
             AND shtyp = gt_vttk-shtyp.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

*-CS2022000256 - 24.03.2022 - JT - inicio
  IF ( p_opcao = 'R_EV_PRT' ). " AND ( r_lifnr2[] IS NOT INITIAL ).
*-CS2022000256 - 24.03.2022 - JT - fim
    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr a~dmbe2 a~monat a~xblnr
           a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat a~umsks a~ebeln a~gsber a~waers
      INTO CORRESPONDING FIELDS OF TABLE gt_bsik
      FROM bsik AS a
     WHERE a~bukrs IN it_bukrs
       AND a~lifnr IN r_lifnr2
       AND a~blart IN r_blart
       AND a~waers IN r_waers
       AND a~budat IN it_augdt.

    SELECT a~bukrs a~augdt a~blart a~shkzg a~augbl a~belnr a~gjahr a~dmbtr a~dmbe2 a~monat a~xblnr
           a~lifnr a~zfbdt a~zbd1t a~sgtxt a~budat a~umsks a~ebeln a~gsber a~waers
      APPENDING CORRESPONDING FIELDS OF TABLE gt_bsak
      FROM bsak AS a
     WHERE a~bukrs IN it_bukrs
       AND a~lifnr IN r_lifnr2
       AND a~blart IN r_blart
       AND a~waers IN r_waers
       AND a~belnr NE a~augbl
       AND a~budat IN it_augdt.

    SORT gt_bsak                                   BY bukrs lifnr augdt augbl gjahr belnr.
    DELETE ADJACENT DUPLICATES FROM gt_bsak COMPARING bukrs lifnr augdt augbl gjahr belnr.

  ENDIF.

*-CS2022000256 - 24.03.2022 - JT - inicio
  FREE: t_info_port.

  IF p_opcao = 'R_EV_PRT'.
    LOOP AT gt_bsak            INTO gw_bsak.
      MOVE-CORRESPONDING gw_bsak TO w_info_port.
      APPEND w_info_port         TO t_info_port.
    ENDLOOP.
    LOOP AT gt_bsik            INTO gw_bsik.
      MOVE-CORRESPONDING gw_bsik TO w_info_port.
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
    ENDIF.

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
    ELSE.
      MODIFY gt_bsak FROM gw_bsak INDEX var_tabix TRANSPORTING kostl prctr matnr stcd1.
    ENDIF.

    IF _delete IS NOT INITIAL.
      DELETE gt_bsak INDEX var_tabix.
    ENDIF.
    CLEAR gw_bsak.
  ENDLOOP.

*-CS2022000256 - 24.03.2022 - JT - inicio
  LOOP AT gt_bsik INTO gw_bsik.
    CLEAR: var_tabix.
    var_tabix = sy-tabix.
    _delete = ''.
    _ok     = ''.

    PERFORM f_check_portuario USING gw_bsik-bukrs gw_bsik-belnr gw_bsik-gjahr
                                    gw_bsik-lifnr gw_bsik-ebeln
                                    p_opcao
                           CHANGING gw_bsik-kostl gw_bsik-prctr gw_bsik-matnr
                                    gw_bsik-stcd1
                                   _ok.
    IF _ok IS INITIAL.
      _delete = 'X'.
    ENDIF.

    IF _delete IS NOT INITIAL.
      DELETE gt_bsik INDEX var_tabix.
    ELSE.
      MODIFY gt_bsik FROM gw_bsik INDEX var_tabix TRANSPORTING kostl prctr matnr stcd1.
    ENDIF.
    CLEAR gw_bsik.
  ENDLOOP.
*-CS2022000256 - 24.03.2022 - JT - fim

  IF ( gt_bsak[] IS NOT INITIAL ).

    SELECT * FROM bsis
      INTO TABLE gt_bsis
      FOR ALL ENTRIES IN gt_bsak
    WHERE bukrs EQ gt_bsak-bukrs
      AND belnr EQ gt_bsak-augbl
      AND gjahr EQ gt_bsak-augdt(4).

  ENDIF.

  IF ( p_opcao = 'R_EV_PRT' OR
       p_opcao = 'R_AQ_PA'  ) AND ( gt_bsik[] IS NOT INITIAL ) .
    SELECT *
      FROM bsis INTO TABLE gt_bsis_aux
       FOR ALL ENTRIES IN gt_bsik
     WHERE bukrs EQ gt_bsik-bukrs
       AND belnr EQ gt_bsik-belnr
       AND gjahr EQ gt_bsik-gjahr.

    LOOP AT gt_bsis_aux.
      IF ( gt_bsis_aux-bschl EQ '40' ) OR
         ( gt_bsis_aux-bschl EQ '50' ).
        APPEND gt_bsis_aux TO gt_bsis.
      ENDIF.
    ENDLOOP.
  ENDIF.

  CHECK NOT gt_bsis[] IS INITIAL.
  sort gt_bsis by bukrs belnr gjahr hkont. "ALRS BUG ordenação

  SELECT * FROM ska1     "#EC CI_DB_OPERATION_OK[2431747]
    INTO TABLE gt_ska1   "#EC CI_DB_OPERATION_OK[2389136]
    FOR ALL ENTRIES IN gt_bsis
  WHERE saknr EQ gt_bsis-hkont
    AND ktopl EQ '0050'.
  "AND KTOKS EQ 'YB04'.

  CHECK NOT gt_ska1[] IS INITIAL.

  SELECT * FROM skat
    INTO TABLE gt_skat
    FOR ALL ENTRIES IN gt_ska1
  WHERE saknr EQ gt_ska1-saknr
    AND spras EQ 'PT'.

  IF gt_bsak[] IS NOT INITIAL.

    SELECT * FROM ekko
      INTO TABLE gt_ekko
      FOR ALL ENTRIES IN gt_bsak
    WHERE ebeln EQ gt_bsak-ebeln
      AND bsart IN r_bsart
      AND waers IN r_waers.

    SELECT *
      FROM zmmt0037 INTO TABLE lit_zmmt0037
      FOR ALL ENTRIES IN gt_bsak
     WHERE ebeln EQ gt_bsak-ebeln.

    IF lit_zmmt0037[] IS NOT INITIAL.
      SELECT *
        FROM zmmt0035 INTO TABLE lit_zmmt0035
        FOR ALL ENTRIES IN lit_zmmt0037
       WHERE nro_sol_cp EQ lit_zmmt0037-nro_sol_cp.

      IF lit_zmmt0035[] IS NOT INITIAL.
        SELECT *
          FROM ekko INTO TABLE lit_ekko_origem
          FOR ALL ENTRIES IN lit_zmmt0035
         WHERE ebeln EQ lit_zmmt0035-ebeln.
      ENDIF.

      SELECT *
        FROM ekko INTO TABLE lit_ekko_principal
         FOR ALL ENTRIES IN gt_bsak
       WHERE ebeln EQ gt_bsak-ebeln
         AND waers IN r_waers.
    ENDIF.

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

  ENDIF.

  IF gt_bsik[] IS NOT INITIAL.
    SELECT *
      FROM ekko APPENDING TABLE gt_ekko
       FOR ALL ENTRIES IN gt_bsik
     WHERE ebeln EQ gt_bsik-ebeln
       AND bsart IN r_bsart
       AND waers IN r_waers.

    SELECT *
      FROM lfa1 APPENDING TABLE lt_lfa1
       FOR ALL ENTRIES IN gt_bsik
     WHERE lifnr EQ gt_bsik-lifnr.

    SELECT *
     FROM bkpf APPENDING CORRESPONDING FIELDS OF TABLE gt_bkpf
      FOR ALL ENTRIES IN gt_bsik
    WHERE bukrs EQ gt_bsik-bukrs
      AND belnr EQ gt_bsik-belnr
      AND gjahr EQ gt_bsik-gjahr
      AND waers IN r_waers.
  ENDIF.

  LOOP AT gt_bkpf.
    CHECK strlen( gt_bkpf-awkey ) = 14.

    gt_bkpf-re_belnr = gt_bkpf-awkey(10).
    gt_bkpf-re_gjahr = gt_bkpf-awkey+10(4).

    MODIFY gt_bkpf.
  ENDLOOP.

  CHECK gt_bkpf[] IS NOT INITIAL.

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

  gt_bsak_comp[] = gt_bsak[].
  gt_bsak_jd[] = gt_bsak[].
  SORT gt_bsak_jd BY bukrs augbl.
  DELETE ADJACENT DUPLICATES FROM gt_bsak_jd COMPARING bukrs augbl.
  SORT gt_bsak_jd BY bukrs belnr augbl.

  LOOP AT gt_bsak INTO gw_bsak.

    CLEAR: lw_lfa1, gw_ekko, gw_bsis, gw_ska1, gw_skat, gw_saida, v_valida, lwa_ekko_principal, lwa_ekko_origem.

    READ TABLE gt_ekko INTO gw_ekko WITH KEY ebeln = gw_bsak-ebeln.

    "Mapeamento Pedidos Filho Tipo -> ZSON   Inicio
    IF sy-subrc NE 0.
      READ TABLE lit_ekko_principal INTO lwa_ekko_principal WITH KEY ebeln = gw_bsak-ebeln.
      IF ( sy-subrc EQ 0 ) AND ( lwa_ekko_principal-bsart = 'ZSON' ).
        READ TABLE lit_zmmt0037 INTO DATA(lwa_zmmt0037) WITH KEY ebeln = gw_bsak-ebeln.
        IF sy-subrc EQ 0.
          READ TABLE lit_zmmt0035 INTO DATA(lwa_zmmt0035) WITH KEY nro_sol_cp = lwa_zmmt0037-nro_sol_cp.
          IF sy-subrc EQ 0.
            READ TABLE lit_ekko_origem INTO lwa_ekko_origem WITH KEY ebeln = lwa_zmmt0035-ebeln.
            IF sy-subrc EQ 0.
              MOVE-CORRESPONDING lwa_ekko_principal TO gw_ekko.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        sy-subrc = 4.
      ENDIF.
    ENDIF.
    "Mapeamento Pedidos Filho Tipo -> ZSON - Fim

    IF ( sy-subrc NE 0          ) AND
       ( p_opcao  NE 'R_AQ_PA'  ) AND
       ( p_opcao  NE 'R_AQUAV'  ) AND
       ( p_opcao  NE 'R_EV_PRT' ).

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

    READ TABLE gt_bsak_ag_ft WITH KEY bukrs = gw_bsak-bukrs
                                      belnr = gw_bsak-belnr
                                      gjahr = gw_bsak-gjahr.
    IF sy-subrc EQ 0.
      gw_saida-blart = 'FT'.
      READ TABLE gt_zlest0032 WITH KEY ebeln = <fs_bsak_ag_ft>-ebeln_fr.
      IF ( sy-subrc EQ 0 ) AND ( gt_zlest0032-tknum IS NOT INITIAL ) AND ( <fs_bsak_ag_ft>-ebeln_fr IS NOT INITIAL ).
        READ TABLE gt_vttk WITH KEY tknum = gt_zlest0032-tknum.
        IF sy-subrc EQ 0.
          READ TABLE gt_tvtkt WITH KEY shtyp = gt_vttk-shtyp.
          IF sy-subrc EQ 0.
            gw_saida-bezei = gt_tvtkt-shtyp && '-' && gt_tvtkt-bezei.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    gw_saida-bsart        = gw_ekko-bsart.
    gw_saida-ebeln        = gw_ekko-ebeln.
    gw_saida-ebeln_origem = lwa_ekko_origem-ebeln.
    gw_saida-bsart_origem = lwa_ekko_origem-bsart.

    gw_saida-bukrs = gw_bsak-bukrs.
    gw_saida-gsber = gw_bsak-gsber.
    gw_saida-bktxt = 'Fatura'.
    gw_saida-belnr = gw_bsak-belnr.
    gw_saida-augbl = gw_bsak-augbl.
    gw_saida-budat = gw_bsak-budat.
*-CS2022000256 - 24.03.2022 - JT - inicio
    gw_saida-kostl = gw_bsak-kostl.
    gw_saida-prctr = gw_bsak-prctr.
    gw_saida-matnr = gw_bsak-matnr.
    gw_saida-stcd1 = gw_bsak-stcd1.
*-CS2022000256 - 24.03.2022 - JT - fim

    READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                             belnr = gw_bsak-augbl
                                             gjahr = gw_bsak-augdt(4).

    IF ( sy-subrc NE 0 ).
      CONTINUE.
    ENDIF.

    gw_saida-augdt = gw_bkpf-budat.
    gw_saida-gjahr = gw_bsak-gjahr.
    gw_saida-zfbdt = gw_bsak-zfbdt + gw_bsak-zbd1t.

    IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Gambeta, e não me pergunte porque.
      gw_saida-dmbtr = gw_bsak-dmbtr.
      gw_saida-dmbe2 = gw_bsak-dmbtr / cs_tax_fix.

      IF gw_bsak-shkzg = 'S'.
        gw_saida-dmbtr = gw_saida-dmbtr * -1.
        gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
      ENDIF.

    ELSE.

      READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsak-bukrs
                                               belnr = gw_bsak-augbl
                                               gjahr = gw_bsak-augdt(4).
      CHECK ( sy-subrc EQ 0 ).

      READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont
                                               ktopl = '0050'
                                               ktoks = 'YB04'.
      CHECK sy-subrc = 0.

      gw_saida-dmbe2 = gw_bsak-dmbe2.
      gw_saida-dmbtr = gw_bsak-dmbtr.

      IF gw_bsak-shkzg = 'S'.
        gw_saida-dmbtr = gw_saida-dmbtr * -1.
        gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
      ENDIF.

      "Pegar Valor Compensação
      CLEAR: vl_vlr_comp.
      LOOP AT gt_bsak_comp WHERE bukrs = gw_bsak-bukrs
                             AND augbl = gw_bsak-augbl .
        IF gt_bsak_comp-shkzg = 'H'.
          ADD gt_bsak_comp-dmbtr TO vl_vlr_comp.
        ELSE.
          SUBTRACT gt_bsak_comp-dmbtr FROM vl_vlr_comp.
        ENDIF.
      ENDLOOP.

      "Se for primeiro Lançamento da compensação
      READ TABLE gt_bsak_jd WITH KEY bukrs = gw_bsak-bukrs
                                     belnr = gw_bsak-belnr
                                     augbl = gw_bsak-augbl BINARY SEARCH.

      IF ( sy-subrc = 0 ) AND ( vl_vlr_comp IS NOT INITIAL ).
        "Check se Compensação do banco possui juros ou descontos
*        LOOP AT GT_BSIS_JD WHERE BUKRS = GW_BSAK-BUKRS
*                             AND BELNR = GW_BSAK-AUGBL
*                             AND GJAHR = GW_BSAK-GJAHR.
*
*          IF GT_BSIS_JD-SHKZG = 'H'. "Adicionar Valor na partida de Banco para calcular diferença na sequencia
*            ADD GT_BSIS_JD-DMBTR TO GW_BSIS-DMBTR.
*          ELSE.
*            SUBTRACT GT_BSIS_JD-DMBTR FROM GW_BSIS-DMBTR.
*          ENDIF.
*        ENDLOOP.

        IF gw_bsis-shkzg = 'S'.
          gw_bsis-dmbtr = gw_bsis-dmbtr * -1.
          gw_bsis-dmbe2 = gw_bsis-dmbe2 * -1.
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

        IF gw_bkpf-waers = 'BRL'.
          gw_saida-dmbe2   = gw_saida-dmbtr / vl_tx_cambio.
        ELSE.
          gw_saida-dmbtr   = gw_saida-dmbe2 * vl_tx_cambio.
        ENDIF.

        gw_saida-tx_camb = vl_tx_cambio.
      ENDIF.

    ENDIF.


    gw_saida-xblnr = gw_bsak-xblnr.
    gw_saida-sgtxt = gw_bsak-sgtxt.

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
      WHEN 'R_AQUAV'.
        gw_saida-tipo = 'AQ'.
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

  LOOP AT gt_bsik INTO gw_bsik.

    CLEAR: lw_lfa1, gw_ekko, gw_bsis, gw_ska1, gw_skat, gw_saida, v_valida.

    READ TABLE gt_ekko INTO gw_ekko WITH KEY ebeln = gw_bsik-ebeln.

    IF ( sy-subrc NE 0 ) AND ( p_opcao NE 'R_AQ_PA' ) AND ( p_opcao NE 'R_EV_PRT' ).

      READ TABLE gt_bkpf WITH KEY bukrs = gw_bsik-bukrs
                                  belnr = gw_bsik-belnr
                                  gjahr = gw_bsik-gjahr.

      IF ( sy-subrc = 0 ) AND ( gt_bkpf-re_belnr IS NOT INITIAL ) .

        READ TABLE gt_ekbe WITH KEY gjahr = gt_bkpf-re_gjahr
                                    belnr = gt_bkpf-re_belnr.

        IF ( sy-subrc = 0 ) AND ( gt_ekbe-ebeln IS NOT INITIAL ).

          READ TABLE gt_ekko INTO gw_ekko WITH KEY ebeln = gt_ekbe-ebeln.

          CHECK sy-subrc = 0.
        ENDIF.
      ENDIF.
    ENDIF.

    gw_saida-bsart = gw_ekko-bsart.
    gw_saida-ebeln = gw_ekko-ebeln.
    gw_saida-bukrs = gw_bsik-bukrs.
    gw_saida-gsber = gw_bsik-gsber.
    gw_saida-bktxt = 'Fatura'.
    gw_saida-belnr = gw_bsik-belnr.
    gw_saida-augbl = gw_bsik-belnr.
    gw_saida-budat = gw_bsik-budat.
*-CS2022000256 - 24.03.2022 - JT - inicio
    gw_saida-kostl = gw_bsik-kostl.
    gw_saida-prctr = gw_bsik-prctr.
    gw_saida-matnr = gw_bsik-matnr.
    gw_saida-stcd1 = gw_bsik-stcd1.
*-CS2022000256 - 24.03.2022 - JT - fim

    READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsik-bukrs
                                             belnr = gw_bsik-belnr
                                             gjahr = gw_bsik-gjahr.

    IF ( sy-subrc NE 0 ) OR ( gw_bkpf-stblg IS NOT INITIAL ).
      CONTINUE.
    ENDIF.

    gw_saida-augdt = gw_bkpf-budat.
    gw_saida-gjahr = gw_bsik-gjahr.
    gw_saida-zfbdt = gw_bsik-zfbdt + gw_bsik-zbd1t.

    IF ( gw_bsik-dmbe2 > gw_bsik-dmbtr ). "Gambeta, e não me pergunte porque.
      gw_saida-dmbtr = gw_bsik-dmbtr.
      gw_saida-dmbe2 = gw_bsik-dmbtr / cs_tax_fix.

      IF gw_bsik-shkzg = 'S'.
        gw_saida-dmbtr = gw_saida-dmbtr * -1.
        gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
      ENDIF.

    ELSE.

      READ TABLE gt_bsis INTO gw_bsis WITH KEY bukrs = gw_bsik-bukrs
                                               belnr = gw_bsik-belnr
                                               gjahr = gw_bsik-gjahr.
      CHECK ( sy-subrc EQ 0 ).

      READ TABLE gt_ska1 INTO gw_ska1 WITH KEY saknr = gw_bsis-hkont.
      "KTOPL = '0050'.
      "KTOKS = 'YB04'.
      CHECK sy-subrc = 0.

      gw_saida-dmbe2 = gw_bsik-dmbe2.
      gw_saida-dmbtr = gw_bsik-dmbtr.

      IF gw_bsik-shkzg = 'S'.
        gw_saida-dmbtr = gw_saida-dmbtr * -1.
        gw_saida-dmbe2 = gw_saida-dmbe2 * -1.
      ENDIF.

      READ TABLE gt_skat INTO gw_skat WITH KEY saknr = gw_ska1-saknr.
      IF sy-subrc = 0.
        "CONCATENATE GW_SKA1-SAKNR '-' GW_SKAT-TXT20 INTO GW_SAIDA-BANCO_LIQ.
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
        IF gw_bkpf-waers = 'BRL'.
          gw_saida-dmbe2   = gw_saida-dmbtr / vl_tx_cambio.
        ELSE.
          gw_saida-dmbtr   = gw_saida-dmbe2 * vl_tx_cambio.
        ENDIF.
        gw_saida-tx_camb = vl_tx_cambio.
      ENDIF.
    ENDIF.

    gw_saida-xblnr = gw_bsik-xblnr.
    gw_saida-sgtxt = gw_bsik-sgtxt.

    READ TABLE lt_lfa1 INTO lw_lfa1 WITH KEY lifnr = gw_bsik-lifnr.
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


ENDFORM.                    " SELECIONAR_DADOS_ADT_INSUMOS

FORM f_check_elevacao  USING p_bsak TYPE ty_bsak
                             p_opcao
                    CHANGING p_ok.

  p_ok = 'X'.

  CHECK p_opcao = 'R_EV_PRT'.

*-CS2022000256 - 24.03.2022 - JT - inicio
*  READ TABLE tg_setleaf_eleva WITH KEY valfrom = p_bsak-lifnr.
*  IF ( sy-subrc NE 0 ).
*    CLEAR: p_ok.
*  ELSE.
*    CASE tg_setleaf_eleva-descript(1).
*      WHEN abap_true.
*
*      WHEN abap_false.
*        IF p_bsak-waers NE 'BRL'.
*          CLEAR: p_ok.
*        ENDIF.
*    ENDCASE.
*  ENDIF.
*-CS2022000256 - 24.03.2022 - JT - fim

ENDFORM.

*-CS2022000256 - 24.03.2022 - JT - inicio
FORM f_filtrar_portuario.

  CHECK t_info_port[] IS NOT INITIAL.

  SELECT bukrs belnr gjahr waers awkey
    FROM bkpf
    INTO TABLE t_bkpf_port
     FOR ALL ENTRIES IN t_info_port
   WHERE bukrs = t_info_port-bukrs
     AND belnr = t_info_port-belnr
     AND gjahr = t_info_port-gjahr.

  LOOP AT t_bkpf_port  INTO w_bkpf_port.
    w_bkpf_port-re_belnr  = w_bkpf_port-awkey(10).
    w_bkpf_port-re_gjahr  = w_bkpf_port-awkey+10(4).
    MODIFY t_bkpf_port FROM w_bkpf_port.
  ENDLOOP.

  SELECT belnr gjahr ebeln
    FROM ekbe
    INTO TABLE t_ekbe_port
     FOR ALL ENTRIES IN t_bkpf_port
   WHERE belnr = t_bkpf_port-re_belnr
     AND gjahr = t_bkpf_port-re_gjahr.

*------------------------------------
  DATA ETL906C2R4431 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L906C2R6829 TYPE FAGL_T_FIELD.
LT_FIELDS_L906C2R6829 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BUZEI' )
 ( LINE = 'KOSTL' )
 ( LINE = 'PRCTR' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = T_INFO_PORT
              I_WHERE_CLAUSE = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
              IT_FIELDLIST = LT_FIELDS_L906C2R6829
    IMPORTING ET_BSEG = ETL906C2R4431
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL906C2R4431 ) > 0.
  MOVE-CORRESPONDING ETL906C2R4431 TO T_BSEG_PORT.
  SY-DBCNT = LINES( ETL906C2R4431 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.


  DELETE t_bseg_port WHERE kostl IS INITIAL
                       AND prctr IS INITIAL.

  FREE: t_bseg_port_kostl, t_bseg_port_prctr.

  t_bseg_port_aux[] = t_bseg_port[].
  DELETE t_bseg_port_aux WHERE kostl IS INITIAL.
  SORT t_bseg_port_aux BY bukrs belnr gjahr kostl.
  DELETE ADJACENT DUPLICATES FROM t_bseg_port_aux
                        COMPARING bukrs belnr gjahr kostl.
  LOOP AT t_bseg_port_aux INTO w_bseg_port_aux.
    MOVE-CORRESPONDING w_bseg_port_aux TO w_bseg_port_kostl.
    APPEND w_bseg_port_kostl TO t_bseg_port_kostl.
  ENDLOOP.

  t_bseg_port_aux[] = t_bseg_port[].
  DELETE t_bseg_port_aux WHERE prctr IS INITIAL.
  SORT t_bseg_port_aux BY bukrs belnr gjahr prctr.
  DELETE ADJACENT DUPLICATES FROM t_bseg_port_aux
                        COMPARING bukrs belnr gjahr prctr.
  LOOP AT t_bseg_port_aux INTO w_bseg_port_aux.
    MOVE-CORRESPONDING w_bseg_port_aux TO w_bseg_port_prctr.
    APPEND w_bseg_port_prctr TO t_bseg_port_prctr.
  ENDLOOP.
*------------------------------------

  t_info_port_aux[] = t_info_port[].
  SORT t_info_port_aux BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_info_port_aux
                     COMPARING lifnr.

  SELECT lifnr stcd1
    FROM lfa1
    INTO TABLE t_lfa1_port
     FOR ALL ENTRIES IN t_info_port_aux
   WHERE lifnr = t_info_port_aux-lifnr.

  t_info_port_aux[] = t_info_port[].
  SORT t_info_port_aux BY ebeln.
  DELETE ADJACENT DUPLICATES FROM t_info_port_aux
                     COMPARING ebeln.

  SELECT ebeln ebelp matnr
    FROM ekpo
    INTO TABLE t_ekpo_port1
     FOR ALL ENTRIES IN t_info_port_aux
   WHERE ebeln = t_info_port_aux-ebeln.

  IF t_ekbe_port[] IS NOT INITIAL.
    SELECT ebeln ebelp matnr
      FROM ekpo
      APPENDING TABLE t_ekpo_port1
       FOR ALL ENTRIES IN t_ekbe_port
     WHERE ebeln = t_ekbe_port-ebeln.
  ENDIF.

  SORT t_ekpo_port1 BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM t_ekpo_port1
                        COMPARING ebeln ebelp.
  t_ekpo_port[] = t_ekpo_port1[].

  SORT t_bkpf_port BY bukrs belnr gjahr.
  SORT t_lfa1_port BY lifnr.

  SORT t_ekbe_port BY belnr gjahr.
  DELETE ADJACENT DUPLICATES FROM t_ekbe_port
                        COMPARING belnr gjahr.

ENDFORM.
*-CS2022000256 - 24.03.2022 - JT - fim

*-CS2022000256 - 24.03.2022 - JT - inicio
FORM f_check_portuario  USING p_bukrs
                              p_belnr
                              p_gjahr
                              p_lifnr
                              p_ebeln
                              p_opcao
                     CHANGING p_kostl
                              p_prctr
                              p_matnr
                              p_stcd1
                              p_ok.

  IF p_opcao NE 'R_EV_PRT'.
    p_ok = abap_true.
    EXIT.
  ENDIF.

  FREE: p_kostl, p_prctr, p_matnr, p_stcd1, p_ok.

  READ TABLE t_bkpf_port INTO w_bkpf_port WITH KEY bukrs = p_bukrs
                                                   belnr = p_belnr
                                                   gjahr = p_gjahr
                                          BINARY SEARCH.
  IF sy-subrc <> 0.
    p_ok = abap_false.
    EXIT.
  ENDIF.

*-------------------------------
* moeda USD
*-------------------------------
  IF w_bkpf_port-waers = 'USD'.
    READ TABLE t_lfa1_port INTO w_lfa1_port WITH KEY lifnr = p_lifnr
                                            BINARY SEARCH.
    READ TABLE t_zlest0218 INTO w_zlest0218 WITH KEY stcd1(8) = w_lfa1_port-stcd1(8).
    IF sy-subrc = 0.
      LOOP AT t_bseg_port_prctr INTO w_bseg_port_prctr WHERE bukrs  = p_bukrs
                                                         AND belnr  = p_belnr
                                                         AND gjahr  = p_gjahr.
        p_prctr = w_bseg_port_prctr-prctr.
        EXIT.
      ENDLOOP.
      LOOP AT t_bseg_port_kostl INTO w_bseg_port_kostl WHERE bukrs  = p_bukrs
                                                         AND belnr  = p_belnr
                                                         AND gjahr  = p_gjahr.
        p_kostl = w_bseg_port_kostl-kostl.
        EXIT.
      ENDLOOP.
      p_stcd1 = w_lfa1_port-stcd1. "w_zlest0218-stcd1.
      p_ok    = abap_true.
    ELSE.
      p_ok    = abap_false.
    ENDIF.
    EXIT.
  ENDIF.

  DATA(l_erro)  = abap_true.

*-------------------------------
* moeda BRL
*-------------------------------
  LOOP AT t_bseg_port_prctr INTO w_bseg_port_prctr WHERE bukrs  = p_bukrs
                                                     AND belnr  = p_belnr
                                                     AND gjahr  = p_gjahr
                                                     AND prctr IN r_0217_prctr[].
    READ TABLE t_zlest0217 INTO w_zlest0217 WITH KEY prctr = w_bseg_port_prctr-prctr.
    p_prctr = w_bseg_port_prctr-prctr.
    p_matnr = w_zlest0217-matnr.
    l_erro  = abap_false.
    p_ok    = abap_true.
    EXIT.
  ENDLOOP.

  LOOP AT t_bseg_port_kostl INTO w_bseg_port_kostl WHERE bukrs  = p_bukrs
                                                     AND belnr  = p_belnr
                                                     AND gjahr  = p_gjahr
                                                     AND kostl IN r_0217_kostl[].
    p_kostl = w_bseg_port_kostl-kostl.
    l_erro  = abap_false.
    EXIT.
  ENDLOOP.

  IF l_erro = abap_true.
    p_ok = abap_false.
    EXIT.
  ENDIF.

*----------------------
* centro custo + material
*----------------------
  IF p_kostl IS NOT INITIAL.
    LOOP AT t_ekpo_port INTO w_ekpo_port WHERE ebeln = p_ebeln.
      READ TABLE t_zlest0217 INTO w_zlest0217 WITH KEY kostl      = p_kostl
                                                       matnr_serv = w_ekpo_port-matnr.
      IF sy-subrc IS INITIAL.
        p_matnr = w_zlest0217-matnr.
        p_ok    = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF sy-subrc <> 0.
      READ TABLE t_ekbe_port INTO w_ekbe_port WITH KEY belnr = w_bkpf_port-belnr
                                                       gjahr = w_bkpf_port-gjahr
                                              BINARY SEARCH.
      CHECK sy-subrc = 0.

      LOOP AT t_ekpo_port INTO w_ekpo_port WHERE ebeln = w_ekbe_port-ebeln.
        READ TABLE t_zlest0217 INTO w_zlest0217 WITH KEY kostl      = p_kostl
                                                         matnr_serv = w_ekpo_port-matnr.
        IF sy-subrc IS INITIAL.
          p_matnr = w_zlest0217-matnr.
          p_ok    = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*-CS2022000256 - 24.03.2022 - JT - fim
