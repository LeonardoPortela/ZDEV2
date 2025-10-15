*----------------------------------------------------------------------*
***INCLUDE LZLES_REPORTF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OPCAO  text
*----------------------------------------------------------------------*
FORM selecionar_dados  USING p_opcao
                             p_not_msg TYPE c.

  DATA: t_lfa1_zfic   TYPE TABLE OF lfa1 WITH HEADER LINE,
        t_branch_zfic TYPE TABLE OF j_1bbranch WITH HEADER LINE.

  DATA: var_tabix TYPE sy-tabix,
        v_valida  TYPE c.


  TYPES: BEGIN OF ty_text,
           descricao TYPE bktxt,
         END OF ty_text,

         BEGIN OF ty_lfa1,
           lifnr TYPE lfa1-lifnr,
         END OF ty_lfa1.

  DATA: lt_text TYPE TABLE OF ty_text,
        lw_text TYPE ty_text.

  DATA: lt_forn TYPE TABLE OF ty_lfa1,
        lw_forn TYPE ty_lfa1.

  lw_text-descricao = 'PEDAGIO-FRETE'.
  APPEND lw_text TO lt_text.
  CLEAR: lw_text.

  lw_text-descricao = 'VALE PEDÁGIO'.
  APPEND lw_text TO lt_text.
  CLEAR: lw_text.


  lw_forn-lifnr = '0000001006'.
  APPEND lw_forn TO lt_forn.
  CLEAR: lw_forn.


  FIELD-SYMBOLS: <fs_bsak>      TYPE ty_bsak,
                 <fs_zlest0032> TYPE ty_zlest0032,
                 <fs_j_1bnfdoc> TYPE ty_j_1bnfdoc,
                 <fs_bkpf>      TYPE ty_bkpf.

  CLEAR: gt_zcte_ciot[], gt_zib_cte_dist_n55[], gt_zib_cte_dist_n01[].

  "ALRS
  "Selecionar frete terceiro/proprio.
  SELECT  bsak~bukrs bsak~augdt bsak~blart bsak~shkzg bsak~augbl
          bsak~belnr bsak~gjahr bsak~dmbtr bsak~dmbe2 bsak~monat
          bsak~xblnr bsak~lifnr bsak~zfbdt bsak~zbd1t bsak~sgtxt
          bsak~budat bsak~umsks bsak~ebeln bsak~gsber bsak~zuonr
    FROM bsak
    INNER JOIN bkpf
    ON  bkpf~bukrs EQ bsak~bukrs
    AND bkpf~belnr EQ bsak~belnr
    AND bkpf~gjahr EQ bsak~gjahr
    AND bkpf~waers EQ 'BRL'
    INTO TABLE gt_bsak
  WHERE bsak~bukrs IN it_bukrs
    AND bsak~augdt IN it_augdt
    AND bsak~blart IN ('ME','FT')
    AND bsak~shkzg EQ 'H'.

  IF ( gt_bsak[] IS INITIAL ).
    IF p_not_msg EQ abap_false.
      MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Sem movimentação.'.
    ENDIF.
    RETURN.
  ENDIF.

  IF ( p_opcao EQ 'PAG' ).

    "Deletar todoo o documento selecionado na BSAK
    "quando o AUGBL for diferente de 2 e o radio button estiver marcado como pagamento.
    DELETE gt_bsak WHERE augbl(1) NE '2'.


    DELETE gt_bsak WHERE lifnr = '0000004102'. "Tefron
    DELETE gt_bsak WHERE lifnr = '0000001003'. "Hermasa
    DELETE gt_bsak WHERE lifnr = '0000001013'. "Hermasa
    DELETE gt_bsak WHERE lifnr = '0000009402'. "Hermasa

    CLEAR: t_lfa1_zfic[],t_branch_zfic[].

    IF gt_bsak[] IS NOT INITIAL.
      SELECT *
        FROM lfa1 INTO TABLE t_lfa1_zfic
         FOR ALL ENTRIES IN gt_bsak
       WHERE lifnr EQ gt_bsak-lifnr.

      DELETE t_lfa1_zfic WHERE ktokk NE 'ZFIC'.

      IF t_lfa1_zfic[] IS NOT INITIAL.
        SELECT *
          FROM j_1bbranch INTO TABLE t_branch_zfic
           FOR ALL ENTRIES IN t_lfa1_zfic
         WHERE branch EQ t_lfa1_zfic-lifnr+6(4).
      ENDIF.
    ENDIF.

    DATA(gt_bsak_aux) = gt_bsak[].
    LOOP AT gt_bsak_aux INTO DATA(wl_bsak_aux).

      READ TABLE t_lfa1_zfic WITH KEY lifnr = wl_bsak_aux-lifnr.
      CHECK sy-subrc EQ 0.

      READ TABLE t_branch_zfic WITH KEY branch = t_lfa1_zfic-lifnr+6(4).
      CHECK sy-subrc EQ 0.

      IF ( t_branch_zfic-bukrs EQ '0010' ) OR ( t_branch_zfic-bukrs EQ '0039' AND wl_bsak_aux-augdt < '20200625'  ).
        DELETE gt_bsak WHERE bukrs EQ wl_bsak_aux-bukrs
                         AND belnr EQ wl_bsak_aux-belnr
                         AND gjahr EQ wl_bsak_aux-gjahr.
      ENDIF.

    ENDLOOP.


    IF ( gt_bsak[] IS INITIAL ).
      MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Sem movimentação para a condição de pagamento.'.
      RETURN.
    ENDIF.

  ELSEIF ( p_opcao EQ 'COM' ). "Documentos compensados.

    "Deletar todoo o documento selecionado na BSAK
    "quando o AUGBL igual a 2 e o radio button estiver marcado como compensação.
    DELETE gt_bsak WHERE augbl(1) EQ '2'.

    IF ( gt_bsak[] IS INITIAL ).
      MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Sem movimentação para a condição de compensação.'.
      RETURN.
    ENDIF.

  ELSEIF ( p_opcao EQ 'PED' ).

    "Deletar todoo o documento selecionado na BSAK
    "quando os sete primeiros caracteres do campo SGTXT for diferente de pedágio.
    "infelizmente este delete é ridiculo, mas é preciso.
    DELETE gt_bsak WHERE sgtxt(7) NE 'Pedágio'
                     AND sgtxt(7) NE 'PEDAGIO'
                     AND sgtxt(7) NE 'Pedagio'
                     AND sgtxt(7) NE 'pedagio'.

    IF ( gt_bsak[] IS INITIAL ).
      MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Sem movimentação para a condição de compensação.'.
      RETURN.
    ENDIF.
  ELSEIF ( p_opcao EQ 'R_AQUAV' ).
    "Deletar todoo o documento selecionado na BSAK
    "quando o AUGBL for diferente de 2 e o radio button estiver marcado como pagamento.
    DELETE gt_bsak WHERE augbl(1) NE '2' AND
                         augbl(2) NE '15'.

    IF ( gt_bsak[] IS INITIAL ).
      IF p_not_msg EQ abap_false.
        MESSAGE s899(fi) DISPLAY LIKE 'S' WITH 'Sem movimentação para a condição de pagamento.'.
      ENDIF.
      RETURN.
    ENDIF.
  ENDIF.


  LOOP AT gt_bsak ASSIGNING <fs_bsak>.
    <fs_bsak>-nm_lote = <fs_bsak>-xblnr.
  ENDLOOP.
  UNASSIGN <fs_bsak>.

  "Cabeçalho do documento contábil
  SORT gt_bsak BY bukrs belnr gjahr blart.


  "SD-WPP-CS1127595 - DUMP ZLES0079 - Fim
*  SELECT AWKEY BKTXT BUKRS BELNR GJAHR BLART STBLG
*    FROM BKPF
*   INTO TABLE GT_BKPF
*    FOR ALL ENTRIES IN GT_BSAK
*   WHERE BUKRS EQ GT_BSAK-BUKRS
*     AND BELNR EQ GT_BSAK-BELNR
*     AND GJAHR EQ GT_BSAK-GJAHR
*     AND BLART EQ GT_BSAK-BLART
*     AND STBLG EQ SPACE.

  SELECT awkey bktxt bukrs belnr gjahr blart stblg
    FROM bkpf
   INTO TABLE gt_bkpf
    FOR ALL ENTRIES IN gt_bsak
   WHERE bukrs EQ gt_bsak-bukrs
     AND belnr EQ gt_bsak-belnr
     AND gjahr EQ gt_bsak-gjahr
     AND blart EQ gt_bsak-blart
     AND stblg EQ space.



  CHECK NOT gt_bkpf[] IS INITIAL.

  LOOP AT gt_bkpf ASSIGNING <fs_bkpf>.
    <fs_bkpf>-re_belnr = <fs_bkpf>-awkey(10).
    <fs_bkpf>-re_gjahr = <fs_bkpf>-awkey+10(4).
  ENDLOOP.
  UNASSIGN <fs_bkpf>.


  "Cabeçalho doc.da fatura recebida
  SELECT * FROM rbkp
    INTO TABLE gt_rbkp
    FOR ALL ENTRIES IN gt_bsak
  WHERE belnr EQ gt_bsak-belnr(10)
    AND gjahr EQ gt_bsak-gjahr
    AND stblg NE space.

  "Tabela de Lotes Importados - Itens
  SELECT * FROM zpfe_lote_item
    INTO TABLE gt_zpfe_lote_item
    FOR ALL ENTRIES IN gt_bsak
  WHERE nm_lote EQ gt_bsak-nm_lote.

  "Buscar informações do documento proprio ME
  IF ( sy-subrc EQ 0 ).

    SELECT belnr tknum add03 fknum docnum
      FROM zlest0032
      INTO TABLE gt_zlest0032
      FOR ALL ENTRIES IN gt_zpfe_lote_item
    WHERE tknum EQ gt_zpfe_lote_item-tknum.

    IF ( sy-subrc EQ 0 ).

      LOOP AT gt_zlest0032 ASSIGNING <fs_zlest0032>.
        <fs_zlest0032>-refkey = <fs_zlest0032>-belnr.
      ENDLOOP.
      UNASSIGN <fs_zlest0032>.

      "Partidas individuais da nota fiscal
      SELECT * FROM j_1bnflin
        INTO TABLE gt_j_1bnflin
        FOR ALL ENTRIES IN gt_zlest0032
      WHERE refkey EQ gt_zlest0032-refkey
        AND reftyp EQ 'LI'.

      CHECK NOT gt_j_1bnflin[] IS INITIAL.

      "Cabeçalho da nota fiscal
      SELECT * FROM j_1bnfdoc
        INTO TABLE gt_j_1bnfdoc
        FOR ALL ENTRIES IN gt_j_1bnflin
      WHERE docnum EQ gt_j_1bnflin-docnum.


    ENDIF.

    SELECT * FROM vfkp
      INTO TABLE gt_vfkp
      FOR ALL ENTRIES IN gt_zpfe_lote_item
    WHERE rebel EQ gt_zpfe_lote_item-tknum.

    SELECT * FROM vttk
      INTO TABLE gt_vttk
      FOR ALL ENTRIES IN gt_zpfe_lote_item
    WHERE tknum EQ gt_zpfe_lote_item-tknum.

    SELECT * FROM vttp
      INTO TABLE gt_vttp
      FOR ALL ENTRIES IN gt_zpfe_lote_item
    WHERE tknum EQ gt_zpfe_lote_item-tknum.

    SELECT * FROM lips
      INTO TABLE gt_lips
      FOR ALL ENTRIES IN gt_vttp
    WHERE vbeln EQ gt_vttp-vbeln
      AND posnr EQ '000010'.

    SORT gt_vttp BY vbeln.
    CLEAR: gw_vttp,gw_lips.
*---> 05/07/2023 - Migração S4 - DL
    SORT gt_lips BY vbeln.
*<--- 05/07/2023 - Migração S4 - DL
    LOOP AT gt_vttp INTO gw_vttp .


      READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln BINARY SEARCH.

      IF ( sy-subrc NE 0 ).

        SELECT * FROM lips
          APPENDING TABLE gt_lips
        WHERE vbeln EQ gw_vttp-vbeln
        AND posnr EQ '000020'.

      ENDIF.
    ENDLOOP.


    SELECT * FROM mara
      INTO TABLE gt_mara
      FOR ALL ENTRIES IN gt_lips
    WHERE matnr EQ gt_lips-matnr.

    SELECT * FROM t023
      INTO TABLE gt_t023
      FOR ALL ENTRIES IN gt_mara
    WHERE matkl EQ gt_mara-matkl.

    SELECT * FROM t023t
      INTO TABLE gt_t023t
      FOR ALL ENTRIES IN gt_t023
    WHERE matkl EQ gt_t023-matkl
      AND spras EQ sy-langu.

    SELECT * FROM makt
      INTO TABLE gt_makt
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr EQ gt_mara-matnr
      AND spras EQ sy-langu.


  ENDIF.

  SELECT * FROM zlest0034
    INTO TABLE gt_zlest0034
    FOR ALL ENTRIES IN gt_bkpf
  WHERE re_belnr EQ gt_bkpf-re_belnr
    AND re_gjahr EQ gt_bkpf-re_gjahr
    AND bukrs    EQ gt_bkpf-bukrs.

  LOOP AT gt_zlest0034 INTO gw_zlest0034.
    IF gw_zlest0034-zvlr_liq_pagar = 0.
*---> 13/06/2023 - Migração S4 - JS
*        GW_ZLEST0034-ZVLR_LIQ_PAGAR = GW_ZLEST0034-DMBTR.
      gw_zlest0034-zvlr_liq_pagar = CONV #( gw_zlest0034-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS

      MODIFY gt_zlest0034 FROM gw_zlest0034 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  IF ( gt_zlest0034[] IS NOT INITIAL ).

    "Partidas individuais da nota fiscal
    SELECT * FROM j_1bnflin
      APPENDING TABLE gt_j_1bnflin
      FOR ALL ENTRIES IN gt_zlest0034
    WHERE docnum EQ gt_zlest0034-en_docnum.

    "Cabeçalho da nota fiscal
    IF gt_j_1bnflin[] IS NOT INITIAL.
      SELECT * FROM j_1bnfdoc
        APPENDING TABLE gt_j_1bnfdoc
        FOR ALL ENTRIES IN gt_j_1bnflin
      WHERE docnum EQ gt_j_1bnflin-docnum.
    ENDIF.

    SELECT * FROM vttk
      APPENDING TABLE gt_vttk
      FOR ALL ENTRIES IN gt_zlest0034
    WHERE tknum EQ gt_zlest0034-tknum.

    "17.01.2017
    IF gt_zlest0034[] IS NOT INITIAL.

      SELECT * FROM vttp
        APPENDING TABLE gt_vttp
        FOR ALL ENTRIES IN gt_zlest0034
      WHERE tknum EQ gt_zlest0034-tknum.

      IF gt_vttp[] IS NOT INITIAL.

        SELECT * FROM lips
          APPENDING TABLE gt_lips
          FOR ALL ENTRIES IN gt_vttp
        WHERE vbeln EQ gt_vttp-vbeln
          AND posnr EQ '000010'.


        SORT gt_vttp BY vbeln.
        CLEAR: gw_vttp,gw_lips.
*---> 05/07/2023 - Migração S4 - DL
        SORT gt_lips BY vbeln.
*<--- 05/07/2023 - Migração S4 - DL
        LOOP AT gt_vttp INTO gw_vttp .
          READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln BINARY SEARCH.

          IF ( sy-subrc NE 0 ).
            SELECT * FROM lips
              APPENDING TABLE gt_lips
            WHERE vbeln EQ gw_vttp-vbeln
            AND posnr EQ '000020'.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.
    "Fim


    SELECT * FROM mara
      APPENDING TABLE gt_mara
      FOR ALL ENTRIES IN gt_zlest0034
    WHERE matnr EQ gt_zlest0034-matnr.

    SELECT * FROM t023
      APPENDING TABLE gt_t023
      FOR ALL ENTRIES IN gt_mara
    WHERE matkl EQ gt_mara-matkl.

    SELECT * FROM t023t
      APPENDING TABLE gt_t023t
      FOR ALL ENTRIES IN gt_t023
    WHERE matkl EQ gt_t023-matkl
      AND spras EQ sy-langu.

    SELECT * FROM makt
      APPENDING TABLE gt_makt
      FOR ALL ENTRIES IN gt_zlest0034
    WHERE matnr EQ gt_zlest0034-matnr
      AND spras EQ sy-langu.


  ENDIF.

  SELECT *
    FROM zlest0141 INTO TABLE gt_zlest0141
     FOR ALL ENTRIES IN gt_bsak
   WHERE bukrs EQ gt_bsak-bukrs
     AND belnr EQ gt_bsak-belnr.

  "Complemento.
  SELECT * FROM zlest0042
   INTO TABLE gt_zlest0042
   FOR ALL ENTRIES IN gt_bsak
 WHERE comp_belnr EQ gt_bsak-belnr
   AND COMP_GJAHR  =  gt_bsak-gjahr.

  IF NOT ( gt_zlest0042[] IS INITIAL ).

    "Partidas individuais da nota fiscal
    SELECT * FROM j_1bnflin
      APPENDING TABLE gt_j_1bnflin
      FOR ALL ENTRIES IN gt_zlest0042
    WHERE docnum EQ gt_zlest0042-comp_docnum.

    SELECT * FROM j_1bnfdoc
      APPENDING TABLE gt_j_1bnfdoc
      FOR ALL ENTRIES IN gt_j_1bnflin
    WHERE docnum EQ gt_j_1bnflin-docnum.

    SELECT * FROM vttk
      APPENDING TABLE gt_vttk
      FOR ALL ENTRIES IN gt_zlest0042
    WHERE tknum EQ gt_zlest0042-tknum.

    SELECT * FROM vttp
      APPENDING TABLE gt_vttp
      FOR ALL ENTRIES IN gt_zlest0042
    WHERE tknum EQ gt_zlest0042-tknum.

    SELECT * FROM lips
      APPENDING TABLE gt_lips
      FOR ALL ENTRIES IN gt_vttp
    WHERE vbeln EQ gt_vttp-vbeln
      AND posnr EQ '000010'.

    SORT gt_vttp BY vbeln.
    CLEAR: gw_vttp,gw_lips.
*---> 05/07/2023 - Migração S4 - DL
    SORT gt_lips BY vbeln.
*<--- 05/07/2023 - Migração S4 - DL
    LOOP AT gt_vttp INTO gw_vttp .


      READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln BINARY SEARCH.

      IF ( sy-subrc NE 0 ).

        SELECT * FROM lips
          APPENDING TABLE gt_lips
        WHERE vbeln EQ gw_vttp-vbeln
        AND posnr EQ '000020'.

      ENDIF.
    ENDLOOP.



    SELECT * FROM mara
      APPENDING TABLE gt_mara
      FOR ALL ENTRIES IN gt_lips
    WHERE matnr EQ gt_lips-matnr.

    SELECT * FROM t023
      APPENDING TABLE gt_t023
      FOR ALL ENTRIES IN gt_mara
    WHERE matkl EQ gt_mara-matkl.

    SELECT * FROM t023t
      APPENDING TABLE gt_t023t
      FOR ALL ENTRIES IN gt_t023
    WHERE matkl EQ gt_t023-matkl
      AND spras EQ sy-langu.

    SELECT * FROM makt
      APPENDING TABLE gt_makt
      FOR ALL ENTRIES IN gt_mara
    WHERE matnr EQ gt_mara-matnr
      AND spras EQ sy-langu.

  ENDIF.


  IF NOT ( gt_vttk[] IS INITIAL ).

    SELECT * FROM t173
      INTO TABLE gt_t173
      FOR ALL ENTRIES IN gt_vttk
    WHERE vsart EQ gt_vttk-vsart.

    SELECT * FROM t173t
      INTO TABLE gt_t173t
      FOR ALL ENTRIES IN gt_t173
    WHERE vsart EQ gt_t173-vsart
      AND spras EQ 'PT'.

    SELECT *
      FROM zcte_ciot INTO TABLE gt_zcte_ciot
       FOR ALL ENTRIES IN gt_vttk
     WHERE tknum = gt_vttk-tknum.

    SELECT *
      FROM zib_cte_dist_n55 INTO TABLE gt_zib_cte_dist_n55
       FOR ALL ENTRIES IN gt_vttk
     WHERE tknum = gt_vttk-tknum.

    SELECT *
      FROM zib_cte_dist_n01 INTO TABLE gt_zib_cte_dist_n01
       FOR ALL ENTRIES IN gt_vttk
     WHERE tknum = gt_vttk-tknum.

  ENDIF.

  "ME
  LOOP AT gt_zpfe_lote_item INTO gw_zpfe_lote_item.

    CLEAR: gw_t023,gw_t023t,gw_vttk, gw_vttp, gw_lips ,gw_saida, gw_bsak, gw_bkpf, gw_zlest0032, gw_j_1bnflin, gw_vttk, gw_mara, gw_makt, gw_vttp, gw_vfkp, gw_j_1bnfdoc, lw_text.

    CASE p_opcao.
      WHEN: 'PAG'.
        gw_saida-tipo = 'FP'.
      WHEN  'R_AQUAV'.
        gw_saida-tipo = 'AQ'.
    ENDCASE.

    READ TABLE gt_bsak INTO gw_bsak WITH KEY nm_lote = gw_zpfe_lote_item-nm_lote.

    IF ( sy-subrc EQ 0 ).

      READ TABLE gt_rbkp INTO gw_rbkp WITH KEY belnr = gw_bsak-belnr(10)
                                               gjahr = gw_bsak-gjahr
                                               stblg = space.
      IF ( sy-subrc EQ 0 ).
        CLEAR: gw_zpfe_lote_item, gw_bsak, gw_rbkp.
        CONTINUE.
      ELSE.

        gw_saida-bukrs       = gw_bsak-bukrs.
        gw_saida-augdt       = gw_bsak-augdt.
        gw_saida-monat       = gw_bsak-augdt+4(2).
        gw_saida-gjahr       = gw_bsak-gjahr.
        gw_saida-augbl       = gw_bsak-augbl.
        gw_saida-belnr       = gw_bsak-belnr.
        gw_saida-blart       = gw_bsak-blart.
        gw_saida-lifnr       = gw_bsak-lifnr.

        READ TABLE lt_forn INTO lw_forn WITH KEY lifnr = gw_saida-lifnr.
        IF ( sy-subrc EQ 0 ).
          CONTINUE.
        ENDIF.


        gw_saida-doc_fiscal  = gw_bsak-xblnr.
        gw_saida-zfbdt       = gw_bsak-zfbdt + gw_bsak-zbd1t.


        READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                                 belnr = gw_bsak-belnr
                                                 gjahr = gw_bsak-gjahr
                                                 blart = gw_bsak-blart.


        READ TABLE lt_text INTO lw_text WITH KEY descricao = gw_bkpf-bktxt.
        IF ( sy-subrc EQ 0 ).
          CONTINUE.
        ENDIF.

        gw_saida-bktxt       = gw_bkpf-bktxt.


        READ TABLE gt_zlest0032 INTO gw_zlest0032 WITH KEY tknum  = gw_zpfe_lote_item-tknum.

        gw_saida-add03 = 'Proprio'.
        gw_saida-miro  = gw_zlest0032-belnr.
        READ TABLE gt_j_1bnflin INTO gw_j_1bnflin WITH KEY refkey = gw_zlest0032-refkey
                                                           reftyp = 'LI'.

        gw_saida-docnum = gw_j_1bnflin-docnum.
        gw_saida-tknum  = gw_zpfe_lote_item-tknum.

        READ TABLE gt_j_1bnfdoc INTO gw_j_1bnfdoc WITH KEY docnum = gw_j_1bnflin-docnum.

        CASE gw_j_1bnfdoc-nfe.
          WHEN: 'X'.
            gw_saida-xblnr = gw_j_1bnfdoc-nfenum.
          WHEN OTHERS.
            gw_saida-xblnr = gw_j_1bnfdoc-nfnum.
        ENDCASE.


        READ TABLE gt_vfkp INTO gw_vfkp WITH KEY rebel = gw_zpfe_lote_item-tknum.

        gw_saida-fknum = gw_vfkp-fknum.

        READ TABLE gt_vttk INTO gw_vttk WITH KEY tknum = gw_zpfe_lote_item-tknum.
        gw_saida-modal = gw_vttk-vsart.

        PERFORM f_valida_lcto_aqua USING p_opcao
                                         gw_vttk
                                         gw_saida
                                CHANGING v_valida.
        CHECK v_valida EQ abap_true.

        READ TABLE gt_vttp INTO gw_vttp WITH KEY tknum = gw_zpfe_lote_item-tknum.
        IF ( sy-subrc EQ 0 ).
          READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln.
          "POSNR = '000010'.CSB
          gw_saida-matnr = gw_lips-matnr.
          gw_saida-gewei = gw_lips-gewei.
          gw_saida-charg = gw_lips-charg.

          READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_lips-matnr.
          gw_saida-matkl = gw_mara-matkl.

          READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
          gw_saida-maktx = gw_makt-maktx.

          READ TABLE gt_t023  INTO gw_t023  WITH KEY matkl = gw_mara-matkl.
          READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = gw_t023-matkl.
          gw_saida-wgbez = gw_t023t-wgbez.

        ENDIF.

        CASE gw_zpfe_lote_item-chvid.
          WHEN: '2'. "Saldo de Frete
            gw_saida-peso         = gw_zpfe_lote_item-peso_chegada.
            gw_saida-zpeso_origem = gw_zpfe_lote_item-peso_origem.
          WHEN: '30'.	"Quebra
            gw_saida-vlr_quebra   = gw_zpfe_lote_item-vl_transacao.
          WHEN: '31'.	"Perda
            gw_saida-vlr_perda    = gw_zpfe_lote_item-vl_transacao.
        ENDCASE.

        gw_saida-vlr_quebra_perda = gw_saida-vlr_quebra + gw_saida-vlr_perda.

        READ TABLE gt_zcte_ciot WITH KEY tknum = gw_zpfe_lote_item-tknum.
        IF ( sy-subrc EQ 0 ) AND ( gw_zpfe_lote_item-tknum IS NOT INITIAL ).
          gw_saida-vlr_cte         = gt_zcte_ciot-vlr_frete.

          CASE gt_zcte_ciot-unid_vlr_frete.
            WHEN 'KG'.
              gw_saida-vlr_tarifa_pgto = gw_saida-vlr_tarifa_pgto * 1000. "Converter Ton.
            WHEN OTHERS.
              gw_saida-vlr_tarifa_pgto = gt_zcte_ciot-vlr_unit_frete.
          ENDCASE.
        ENDIF.

        gw_saida-chvid        = gw_zpfe_lote_item-chvid.

        gw_saida-vl_pago_lote = gw_zpfe_lote_item-vl_pago_lote.

        IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Gambeta, não me pergunte porque.

          IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ) .
          ENDIF.

        ELSE.
          gw_saida-dmbe2       = ( gw_zpfe_lote_item-vl_pago_lote  / ( gw_bsak-dmbtr / gw_bsak-dmbe2 ) ).

          "Busca Valor Dolar pela BKPF
          CLEAR: gw_bkpf_aux.
          SELECT SINGLE *
            FROM bkpf INTO gw_bkpf_aux
           WHERE bukrs = gw_bsak-bukrs
             AND belnr = gw_bsak-augbl
             AND gjahr = gw_bsak-gjahr.

          IF sy-cprog <> 'ZLESR0026'.
            gw_saida-vl_pago_lote = gw_zpfe_lote_item-vl_pago_lote.

            IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-vl_pago_lote NE 0 ).
              gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

              gw_saida-dmbe2 = gw_saida-vl_pago_lote / gw_bkpf_aux-kurs2.
            ENDIF.

          ELSE.

            gw_saida-vl_pago_lote = gw_zpfe_lote_item-vl_pago_lote.

            gw_saida-dmbtr =  gw_zpfe_lote_item-vl_pago_lote.

            IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-dmbtr NE 0 ).

              gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

              gw_saida-dmbe2 = gw_saida-dmbtr / gw_bkpf_aux-kurs2.

            ENDIF.

          ENDIF.

          PERFORM get_vlr_dolar USING '01' p_opcao
                             CHANGING gw_saida.

          "GW_SAIDA-VL_PAGO_LOTE = GW_ZPFE_LOTE_ITEM-VL_PAGO_LOTE. "CSB

        ENDIF.

        "GW_SAIDA-DMBE2        = ( GW_ZPFE_LOTE_ITEM-VL_PAGO_LOTE / ( GW_BSAK-DMBTR / GW_BSAK-DMBE2 ) ).


        "Modificação para colocar o grupo de material do fertilizante para quando for PAGAMENTO A POSTO.
        CASE gw_saida-bktxt.
          WHEN: 'Pagamento a posto'.
            READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = '700150'.
            gw_saida-wgbez = gw_t023t-wgbez.
            gw_saida-matkl = gw_t023t-matkl.
        ENDCASE.

        PERFORM f_valida_lcto USING gw_saida p_opcao
                           CHANGING v_valida.

        IF v_valida IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND gw_saida TO gt_saida.

        "Verifica se deve gerar Estorno.
        PERFORM f_check_estorno USING gw_saida.



      ENDIF.

    ENDIF.
  ENDLOOP.

  "FT
  LOOP AT gt_zlest0034 INTO gw_zlest0034.

    CLEAR: gw_t023,gw_t023t,gw_vttk, gw_vttp, gw_lips ,gw_zlest0042, gw_bsak, gw_bkpf, gw_rbkp, gw_j_1bnfdoc, gw_j_1bnflin, gw_vttk, gw_mara, gw_makt, gw_saida, lw_text.

    CASE p_opcao.
      WHEN: 'PAG'.
        gw_saida-tipo = 'FP'.
      WHEN  'R_AQUAV'.
        gw_saida-tipo = 'AQ'.
    ENDCASE.

    CLEAR: var_belnr_gjahr.
    CONCATENATE gw_zlest0034-re_belnr(10) gw_zlest0034-re_gjahr INTO var_belnr_gjahr.
    READ TABLE gt_bkpf INTO gw_bkpf WITH KEY awkey = var_belnr_gjahr.

    READ TABLE gt_bsak INTO gw_bsak WITH KEY belnr = gw_bkpf-belnr.

    IF ( sy-subrc EQ 0 ).
      READ TABLE gt_rbkp INTO gw_rbkp WITH KEY belnr = gw_bsak-belnr(10)
                                               gjahr = gw_bsak-gjahr
                                               stblg = space.
      IF ( sy-subrc EQ 0 ).
        CLEAR: gw_zpfe_lote_item, gw_bsak, gw_rbkp.
        CONTINUE.
      ELSE.

        gw_saida-bukrs       = gw_bsak-bukrs.
        gw_saida-augdt       = gw_bsak-augdt.
        gw_saida-monat       = gw_bsak-augdt+4(2).
        gw_saida-gjahr       = gw_bsak-gjahr.
        gw_saida-augbl       = gw_bsak-augbl.
        gw_saida-belnr       = gw_bsak-belnr.
        gw_saida-blart       = gw_bsak-blart.
        gw_saida-lifnr       = gw_bsak-lifnr.

        READ TABLE lt_forn INTO lw_forn WITH KEY lifnr = gw_saida-lifnr.
        IF ( sy-subrc EQ 0 ).
          CONTINUE.
        ENDIF.

        gw_saida-doc_fiscal  = gw_bsak-xblnr.
        gw_saida-zfbdt       = gw_bsak-zfbdt + gw_bsak-zbd1t.

*---> 13/06/2023 - Migração S4 - JS
*         GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.


        gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS



        IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Regra para o dia 15092015 (Problema com a taxa do dólar no sistema)"

          IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').

            IF sy-cprog <> 'ZLESR0026'.
*---> 13/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
              gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS
              gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).


            ELSE.
*---> 13/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
              gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS
              gw_saida-dmbtr = gw_bsak-dmbtr.
              gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).


            ENDIF.
* CSB
*            GW_SAIDA-DMBTR = GW_BSAK-DMBTR.
*            GW_SAIDA-DMBE2 = (  GW_BSAK-DMBTR / CS_TAX_FIX ).
*            GW_SAIDA-VL_PAGO_LOTE = GW_SAIDA-DMBTR.

          ENDIF.

        ELSE.
          gw_saida-dmbe2       = ( gw_zlest0034-zvlr_liq_pagar / ( gw_bsak-dmbtr / gw_bsak-dmbe2 ) ).
        ENDIF.

        gw_saida-add03       = 'Terceiro'.
        gw_saida-miro        = gw_bkpf-awkey.

        READ TABLE lt_text INTO lw_text WITH KEY descricao = gw_bkpf-bktxt.
        IF ( sy-subrc EQ 0 ).
          CONTINUE.
        ENDIF.
        gw_saida-bktxt       = gw_bkpf-bktxt.

        gw_saida-tknum        = gw_zlest0034-tknum.
        gw_saida-fknum        = gw_zlest0034-fknum.
        gw_saida-peso         = gw_zlest0034-zpeso_destino.
        gw_saida-zpeso_origem = gw_zlest0034-zpeso_origem.

        IF ( gw_saida-tknum IS NOT INITIAL ).
          READ TABLE gt_zib_cte_dist_n55 WITH KEY tknum = gw_saida-tknum.
          IF ( sy-subrc EQ 0 ).
            LOOP AT gt_zib_cte_dist_n55 WHERE tknum = gw_saida-tknum
                                          AND zvlr_liq_pagar NE 0.
              gw_saida-vlr_cte           = gt_zib_cte_dist_n55-zvlr_frete.
              gw_saida-vlr_quebra        = gt_zib_cte_dist_n55-zvlr_quebra.
              gw_saida-vlr_perda         = gt_zib_cte_dist_n55-zvlr_perda.
              gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
              gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n55-zvlr_kg_transp * 1000.  "Converte Ton
              EXIT.
            ENDLOOP.
          ELSE.
            READ TABLE gt_zib_cte_dist_n01 WITH KEY tknum = gw_saida-tknum.
            IF sy-subrc EQ 0.
              LOOP AT gt_zib_cte_dist_n01 WHERE tknum = gw_saida-tknum
                                            AND zvlr_liq_pagar NE 0.
                gw_saida-vlr_cte           = gt_zib_cte_dist_n01-zvlr_frete.
                gw_saida-vlr_quebra        = gt_zib_cte_dist_n01-zvlr_quebra.
                gw_saida-vlr_perda         = gt_zib_cte_dist_n01-zvlr_perda.
                gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
                gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n01-zvlr_kg_transp * 1000. "Converte Ton
                EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE gt_j_1bnflin INTO gw_j_1bnflin WITH KEY docnum = gw_zlest0034-en_docnum.

        IF ( sy-subrc EQ 0 ).
          READ TABLE gt_j_1bnfdoc INTO gw_j_1bnfdoc WITH KEY docnum = gw_j_1bnflin-docnum.
          gw_saida-docnum = gw_j_1bnfdoc-docnum.
        ENDIF.

        READ TABLE gt_vttk INTO gw_vttk WITH KEY tknum = gw_zlest0034-tknum.
        gw_saida-modal = gw_vttk-vsart.

        PERFORM f_valida_lcto_aqua USING p_opcao
                                         gw_vttk
                                         gw_saida
                                CHANGING v_valida.
        CHECK v_valida EQ abap_true.

        READ TABLE gt_vttp INTO gw_vttp WITH KEY tknum = gw_zlest0034-tknum.
        IF sy-subrc = 0.
          READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln.
          IF sy-subrc = 0.
            gw_saida-charg = gw_lips-charg.
          ENDIF.
        ENDIF.

        READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_zlest0034-matnr.
        gw_saida-matkl = gw_mara-matkl.
        gw_saida-matnr = gw_zlest0034-matnr.

        READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_zlest0034-matnr.
        gw_saida-maktx = gw_makt-maktx.

        gw_saida-gewei = gw_zlest0034-gewei.

        IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Regra para o dia 15092015 (Problema com a taxa do dólar no sistema)"

          IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').


            IF sy-cprog <> 'ZLESR0026'.

              gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 13/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
              gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS


            ELSE.
              gw_saida-dmbtr = gw_bsak-dmbtr.
              gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 13/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
              gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS





            ENDIF.
*CSB
*            GW_SAIDA-DMBTR = GW_BSAK-DMBTR.
*            GW_SAIDA-DMBE2 = (  GW_BSAK-DMBTR / CS_TAX_FIX ).
*            GW_SAIDA-VL_PAGO_LOTE = GW_SAIDA-DMBTR.

          ENDIF.
        ELSE.
          "Busca Valor Dolar pela BKPF
          CLEAR: gw_bkpf_aux.
          SELECT SINGLE *
            FROM bkpf INTO gw_bkpf_aux
           WHERE bukrs = gw_bsak-bukrs
             AND belnr = gw_bsak-augbl
             AND gjahr = gw_bsak-gjahr.

          IF sy-cprog <> 'ZLESR0026'.
            gw_saida-vl_pago_lote  = gw_zlest0034-zvlr_liq_pagar.

            IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-vl_pago_lote NE 0 ).

              gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

              gw_saida-dmbe2 = gw_saida-vl_pago_lote / gw_bkpf_aux-kurs2.

            ENDIF.

          ELSE.
            gw_saida-vl_pago_lote  = gw_zlest0034-zvlr_liq_pagar.

            gw_saida-dmbtr = gw_zlest0034-zvlr_liq_pagar.

            IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-dmbtr NE 0 ).

              gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

              gw_saida-dmbe2 = gw_saida-dmbtr / gw_bkpf_aux-kurs2.

            ENDIF.

          ENDIF.

          PERFORM get_vlr_dolar USING '02' p_opcao
                             CHANGING gw_saida.

        ENDIF.

        "CSB
        "GW_SAIDA-VL_PAGO_LOTE  = GW_ZLEST0034-ZVLR_LIQ_PAGAR.


        READ TABLE gt_t023  INTO gw_t023  WITH KEY matkl = gw_mara-matkl.
        READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = gw_t023-matkl.
        gw_saida-wgbez = gw_t023t-wgbez.

      ENDIF.

      "Modificação para colocar o grupo de material do fertilizante para quando for PAGAMENTO A POSTO.
      CASE gw_saida-bktxt.
        WHEN: 'Pagamento a posto'.
          READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = '700150'.
          gw_saida-wgbez = gw_t023t-wgbez.
          gw_saida-matkl = gw_t023t-matkl.
      ENDCASE.

      PERFORM f_valida_lcto USING gw_saida p_opcao
                         CHANGING v_valida.

      IF v_valida IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND gw_saida TO gt_saida.

      "Verifica se deve gerar Estorno.
      PERFORM f_check_estorno USING gw_saida.


    ENDIF.
  ENDLOOP.

  "Complemento.
  LOOP AT gt_zlest0042 INTO gw_zlest0042.

    CLEAR: gw_t023,gw_t023t,gw_vttk, gw_vttp, gw_lips ,gw_saida, gw_zlest0034, gw_bsak, gw_bkpf, gw_rbkp, gw_j_1bnfdoc, gw_j_1bnflin, gw_vttk, gw_mara, gw_makt, gw_saida.

    CASE p_opcao.
      WHEN: 'PAG'.
        gw_saida-tipo = 'FP'.
      WHEN  'R_AQUAV'.
        gw_saida-tipo = 'AQ'.
    ENDCASE.

    READ TABLE gt_bsak INTO gw_bsak WITH KEY belnr = gw_zlest0042-comp_belnr.

    READ TABLE gt_rbkp INTO gw_rbkp WITH KEY belnr = gw_bsak-belnr(10)
                                             gjahr = gw_bsak-gjahr
                                             stblg = space.
    IF ( sy-subrc EQ 0 ).
      CLEAR: gw_zlest0042, gw_zlest0034, gw_bkpf, gw_bsak, gw_rbkp.
      CONTINUE.
    ELSE.

      gw_saida-bukrs       = gw_bsak-bukrs.
      gw_saida-augdt       = gw_bsak-augdt.
      gw_saida-monat       = gw_bsak-augdt+4(2).
      gw_saida-gjahr       = gw_bsak-gjahr.
      gw_saida-augbl       = gw_bsak-augbl.
      gw_saida-belnr       = gw_bsak-belnr.
      gw_saida-blart       = gw_bsak-blart.
      gw_saida-lifnr       = gw_bsak-lifnr.

      READ TABLE lt_forn INTO lw_forn WITH KEY lifnr = gw_saida-lifnr.
      IF ( sy-subrc EQ 0 ).
        CONTINUE.
      ENDIF.

      gw_saida-doc_fiscal  = gw_bsak-xblnr.
      gw_saida-zfbdt       = gw_bsak-zfbdt + gw_bsak-zbd1t.
*---> 13/06/2023 - Migração S4 - JS
*      GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
      gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 13/06/2023 - Migração S4 - JS





      gw_saida-vl_pago_lote = gw_bsak-dmbtr.


      IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Regra para o dia 15092015 (Problema com a taxa do dólar no sistema)"
        IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').







          IF sy-cprog <> 'ZLESR0026'.
            gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS

          ELSE.
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS

          ENDIF.

*CSB
*          GW_SAIDA-DMBTR = GW_BSAK-DMBTR.
*          GW_SAIDA-DMBE2 = ( GW_BSAK-DMBTR / CS_TAX_FIX ).
*          GW_SAIDA-VL_PAGO_LOTE = GW_SAIDA-DMBTR.


        ENDIF.
      ELSE.
        gw_saida-dmbe2       = ( gw_zlest0042-comp_valor / ( gw_bsak-dmbtr / gw_bsak-dmbe2 ) ).
      ENDIF.

      gw_saida-add03       = 'Terceiro'.
      gw_saida-miro        = gw_bkpf-awkey.
      gw_saida-bktxt       = 'Complemento'.

      gw_saida-tknum        = gw_zlest0042-tknum.
      gw_saida-fknum        = gw_zlest0034-fknum.

      IF ( gw_saida-tknum IS NOT INITIAL ).
        READ TABLE gt_zib_cte_dist_n55 WITH KEY tknum = gw_saida-tknum.
        IF ( sy-subrc EQ 0 ).
          LOOP AT gt_zib_cte_dist_n55 WHERE tknum = gw_saida-tknum
                                        AND zvlr_liq_pagar NE 0.
            gw_saida-vlr_cte           = gt_zib_cte_dist_n55-zvlr_frete.
            gw_saida-vlr_quebra        = gt_zib_cte_dist_n55-zvlr_quebra.
            gw_saida-vlr_perda         = gt_zib_cte_dist_n55-zvlr_perda.
            gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
            gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n55-zvlr_kg_transp * 1000.  "Converte Ton
            EXIT.
          ENDLOOP.
        ELSE.
          READ TABLE gt_zib_cte_dist_n01 WITH KEY tknum = gw_saida-tknum.
          IF sy-subrc EQ 0.
            LOOP AT gt_zib_cte_dist_n01 WHERE tknum = gw_saida-tknum
                                          AND zvlr_liq_pagar NE 0.
              gw_saida-vlr_cte           = gt_zib_cte_dist_n01-zvlr_frete.
              gw_saida-vlr_quebra        = gt_zib_cte_dist_n01-zvlr_quebra.
              gw_saida-vlr_perda         = gt_zib_cte_dist_n01-zvlr_perda.
              gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
              gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n01-zvlr_kg_transp * 1000. "Converte Ton
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE gt_j_1bnflin INTO gw_j_1bnflin WITH KEY docnum = gw_zlest0042-comp_docnum.

      IF ( sy-subrc EQ 0 ).
        READ TABLE gt_j_1bnfdoc INTO gw_j_1bnfdoc WITH KEY docnum = gw_j_1bnflin-docnum.
        gw_saida-docnum = gw_j_1bnfdoc-docnum.
      ENDIF.

      READ TABLE gt_vttk INTO gw_vttk WITH KEY tknum = gw_zlest0042-tknum.
      gw_saida-modal = gw_vttk-vsart.

      PERFORM f_valida_lcto_aqua USING p_opcao
                                       gw_vttk
                                       gw_saida
                              CHANGING v_valida.
      CHECK v_valida EQ abap_true.

      READ TABLE gt_vttp INTO gw_vttp WITH KEY tknum = gw_zlest0042-tknum.

      READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln.
      "POSNR = '000010'.CSB

      IF sy-subrc = 0.
        gw_saida-charg = gw_lips-charg.
      ENDIF.

      READ TABLE gt_mara INTO gw_mara WITH KEY matnr = gw_lips-matnr.
      gw_saida-matkl = gw_mara-matkl.
      gw_saida-matnr = gw_mara-matnr.

      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_mara-matnr.
      gw_saida-maktx = gw_makt-maktx.

      "      GW_SAIDA-GEWEI = GW_ZLEST0034-GEWEI.

      IF ( gw_bsak-dmbe2 > gw_bsak-dmbtr ). "Regra para o dia 15092015 (Problema com a taxa do dólar no sistema)"
        IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').

          IF sy-cprog <> 'ZLESR0026'.
            gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS


          ELSE.
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = ( gw_bsak-dmbtr / cs_tax_fix ).
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS

          ENDIF.
* CSB
*          GW_SAIDA-DMBTR = GW_BSAK-DMBTR.
*          GW_SAIDA-DMBE2 = ( GW_BSAK-DMBTR / CS_TAX_FIX ).
*          GW_SAIDA-VL_PAGO_LOTE = GW_SAIDA-DMBTR.

        ENDIF.
      ELSE.

        "Busca Valor Dolar pela BKPF
        CLEAR: gw_bkpf_aux.
        SELECT SINGLE *
          FROM bkpf INTO gw_bkpf_aux
         WHERE bukrs = gw_bsak-bukrs
           AND belnr = gw_bsak-augbl
           AND gjahr = gw_bsak-gjahr.

        IF sy-cprog <> 'ZLESR0026'.
*---> 13/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE  = GW_ZLEST0042-COMP_VALOR.
          gw_saida-vl_pago_lote = CONV #( gw_zlest0042-comp_valor ).
*<--- 13/06/2023 - Migração S4 - JS





          IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-vl_pago_lote NE 0 ).

            gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

            gw_saida-dmbe2 = gw_saida-vl_pago_lote / gw_bkpf_aux-kurs2.

          ENDIF.
        ELSE.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE  = GW_ZLEST0042-COMP_VALOR.
          gw_saida-vl_pago_lote = CONV #( gw_zlest0042-comp_valor ).
*<--- 14/06/2023 - Migração S4 - JS
          gw_saida-dmbtr = gw_zlest0042-comp_valor.


          IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-dmbtr NE 0 ).

            gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

            gw_saida-dmbe2 = gw_saida-dmbtr / gw_bkpf_aux-kurs2.

          ENDIF.

        ENDIF.

        PERFORM get_vlr_dolar USING '03' p_opcao
                           CHANGING gw_saida.

        "CSB
        "GW_SAIDA-VL_PAGO_LOTE  = GW_ZLEST0042-COMP_VALOR.

      ENDIF.

      READ TABLE gt_t023  INTO gw_t023  WITH KEY matkl = gw_mara-matkl.
      READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = gw_t023-matkl.
      gw_saida-wgbez = gw_t023t-wgbez.

      PERFORM f_valida_lcto USING gw_saida p_opcao
                         CHANGING v_valida.

      IF v_valida IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND gw_saida TO gt_saida.

      "Verifica se deve gerar Estorno.
      PERFORM f_check_estorno USING gw_saida.



    ENDIF.
  ENDLOOP.

  "Seleção Especifica para o Pedágio.
  "PED = PEDIDO
  IF ( p_opcao EQ 'PED' ).

    REFRESH: gt_zlest0032[], gt_vttk[], gt_vttp[].
    REFRESH: gt_lips[], gt_mara[], gt_mara[], gt_t023[], gt_t023t[], gt_makt[].

    SELECT belnr tknum add03
      FROM zlest0032
      INTO TABLE gt_zlest0032
      FOR ALL ENTRIES IN gt_bsak
    WHERE belnr EQ gt_bsak-xblnr(10).

    IF ( sy-subrc EQ 0 ).

      SELECT * FROM vttk
        APPENDING TABLE gt_vttk
        FOR ALL ENTRIES IN gt_zlest0032
      WHERE tknum EQ gt_zlest0032-tknum.

      SELECT * FROM vttp
        APPENDING TABLE gt_vttp
        FOR ALL ENTRIES IN gt_vttk
      WHERE tknum EQ gt_vttk-tknum.

      SELECT * FROM lips
        APPENDING TABLE gt_lips
        FOR ALL ENTRIES IN gt_vttp
      WHERE vbeln EQ gt_vttp-vbeln
        AND posnr EQ '000010'.

      SORT gt_vttp BY vbeln.
      CLEAR: gw_vttp,gw_lips.
*---> 05/07/2023 - Migração S4 - DL
      SORT gt_lips BY vbeln.
*<--- 05/07/2023 - Migração S4 - DL
      LOOP AT gt_vttp INTO gw_vttp .


        READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln BINARY SEARCH.

        IF ( sy-subrc NE 0 ).

          SELECT * FROM lips
            APPENDING TABLE gt_lips
          WHERE vbeln EQ gw_vttp-vbeln
          AND posnr EQ '000020'.

        ENDIF.
      ENDLOOP.

      SELECT * FROM mara
        APPENDING TABLE gt_mara
        FOR ALL ENTRIES IN gt_lips
      WHERE matnr EQ gt_lips-matnr.

      SELECT * FROM t023
        APPENDING TABLE gt_t023
        FOR ALL ENTRIES IN gt_mara
      WHERE matkl EQ gt_mara-matkl.

      SELECT * FROM t023t
        APPENDING TABLE gt_t023t
        FOR ALL ENTRIES IN gt_t023
      WHERE matkl EQ gt_t023-matkl
        AND spras EQ sy-langu.

      SELECT * FROM makt
        APPENDING TABLE gt_makt
        FOR ALL ENTRIES IN gt_mara
      WHERE matnr EQ gt_mara-matnr
        AND spras EQ sy-langu.


    ENDIF.
  ENDIF.

  "Separação dos documentos "Pagamentos a Postos" e "Seguro-frete".
  LOOP AT gt_bsak INTO gw_bsak.

    CLEAR: gw_saida, gw_bkpf, gw_zpfe_lote_item, gw_zlest0034, gw_zlest0042, gw_vttk, gw_vttp,
           gw_lips, gw_makt, gw_t023, gw_t023t, lw_text, lw_forn, gw_zlest0032, gw_zlest0141.

    CASE p_opcao.
      WHEN: 'PAG'.
        gw_saida-tipo = 'FP'.
      WHEN  'R_AQUAV'.
        gw_saida-tipo = 'AQ'.
    ENDCASE.

    var_tabix = sy-tabix.
    READ TABLE gt_rbkp INTO gw_rbkp WITH KEY belnr = gw_bsak-belnr(10)
                                             gjahr = gw_bsak-gjahr
                                             stblg = space.

    READ TABLE gt_bkpf INTO gw_bkpf WITH KEY bukrs = gw_bsak-bukrs
                                             belnr = gw_bsak-belnr
                                             gjahr = gw_bsak-gjahr
                                             blart = gw_bsak-blart
                                             stblg = space.
    IF ( sy-subrc NE 0 ).
      CLEAR: gw_bsak, gw_rbkp, gw_bkpf.
      CONTINUE.
    ELSE.

      READ TABLE gt_zpfe_lote_item INTO gw_zpfe_lote_item WITH KEY nm_lote = gw_bsak-nm_lote.
      IF ( sy-subrc EQ 0 ).
        DELETE gt_bsak INDEX var_tabix.
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zpfe_lote_item.
        CONTINUE.
      ENDIF.

      READ TABLE gt_zlest0034 INTO gw_zlest0034 WITH KEY re_belnr = gw_bkpf-re_belnr
                                                         re_gjahr = gw_bkpf-re_gjahr
                                                         bukrs    = gw_bkpf-bukrs.
      IF ( sy-subrc EQ 0 ).
        DELETE gt_bsak INDEX var_tabix.
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zlest0034.
        CONTINUE.
      ENDIF.

      READ TABLE gt_zlest0042 INTO gw_zlest0042 WITH KEY comp_belnr = gw_bsak-belnr.
      IF ( sy-subrc EQ 0 ).
        DELETE gt_bsak INDEX var_tabix.
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zlest0042.
        CONTINUE.
      ENDIF.

      READ TABLE gt_zlest0032 INTO gw_zlest0032 WITH KEY belnr = gw_bsak-xblnr(10).
      IF ( gw_zlest0032-belnr IS INITIAL ).
        CLEAR: gw_zlest0032.
      ENDIF.

      READ TABLE gt_zlest0141 INTO gw_zlest0141 WITH KEY bukrs = gw_bsak-bukrs
                                                         belnr = gw_bsak-belnr.
      IF ( sy-subrc EQ 0 ).
        DELETE gt_bsak INDEX var_tabix.
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zlest0042, gw_zlest0141.
        CONTINUE.
      ENDIF.

      gw_saida-bukrs  = gw_bsak-bukrs.
      gw_saida-augdt  = gw_bsak-augdt.
      gw_saida-tknum  = gw_zlest0032-tknum.
      gw_saida-fknum  = gw_zlest0032-fknum.
      gw_saida-docnum = gw_zlest0032-docnum.

      IF ( gw_saida-tknum IS NOT INITIAL ).
        READ TABLE gt_zib_cte_dist_n55 WITH KEY tknum = gw_saida-tknum.
        IF ( sy-subrc EQ 0 ).
          LOOP AT gt_zib_cte_dist_n55 WHERE tknum = gw_saida-tknum
                                        AND zvlr_liq_pagar NE 0.
            gw_saida-vlr_cte           = gt_zib_cte_dist_n55-zvlr_frete.
            gw_saida-vlr_quebra        = gt_zib_cte_dist_n55-zvlr_quebra.
            gw_saida-vlr_perda         = gt_zib_cte_dist_n55-zvlr_perda.
            gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
            gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n55-zvlr_kg_transp * 1000.  "Converte Ton
            EXIT.
          ENDLOOP.
        ELSE.
          READ TABLE gt_zib_cte_dist_n01 WITH KEY tknum = gw_saida-tknum.
          IF sy-subrc EQ 0.
            LOOP AT gt_zib_cte_dist_n01 WHERE tknum = gw_saida-tknum
                                          AND zvlr_liq_pagar NE 0.
              gw_saida-vlr_cte           = gt_zib_cte_dist_n01-zvlr_frete.
              gw_saida-vlr_quebra        = gt_zib_cte_dist_n01-zvlr_quebra.
              gw_saida-vlr_perda         = gt_zib_cte_dist_n01-zvlr_perda.
              gw_saida-vlr_quebra_perda  = gw_saida-vlr_quebra + gw_saida-vlr_perda.
              gw_saida-vlr_tarifa_pgto   = gt_zib_cte_dist_n01-zvlr_kg_transp * 1000. "Converte Ton
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.

      READ TABLE gt_vttk INTO gw_vttk WITH KEY tknum = gw_zlest0032-tknum.
      gw_saida-modal = gw_vttk-vsart.

      PERFORM f_valida_lcto_aqua USING p_opcao
                                       gw_vttk
                                       gw_saida
                              CHANGING v_valida.
      CHECK v_valida EQ abap_true.

      READ TABLE gt_vttp INTO gw_vttp WITH KEY tknum = gw_vttk-tknum.
      IF sy-subrc = 0.
        READ TABLE gt_lips INTO gw_lips WITH KEY vbeln = gw_vttp-vbeln.

        IF sy-subrc = 0.
          gw_saida-matnr = gw_lips-matnr.
          gw_saida-matkl = gw_lips-matkl.
          gw_saida-charg = gw_lips-charg.
        ENDIF.
      ENDIF.

      READ TABLE gt_makt INTO gw_makt WITH KEY matnr = gw_lips-matnr.
      gw_saida-maktx = gw_makt-maktx.

      READ TABLE gt_t023 INTO gw_t023 WITH KEY matkl = gw_lips-matkl.
      IF sy-subrc = 0.
        READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = gw_t023-matkl.
        gw_saida-matkl = gw_t023t-matkl.
        gw_saida-wgbez = gw_t023t-wgbez.
      ENDIF.

      gw_saida-sgtxt        = gw_bsak-sgtxt.
      gw_saida-bukrs        = gw_bsak-bukrs.
      gw_saida-augdt        = gw_bsak-augdt.
      gw_saida-monat        = gw_bsak-augdt+4(2).
      gw_saida-gjahr        = gw_bsak-gjahr.
      gw_saida-augbl        = gw_bsak-augbl.
      gw_saida-belnr        = gw_bsak-belnr.
      gw_saida-blart        = gw_bsak-blart.
      gw_saida-lifnr        = gw_bsak-lifnr.

      READ TABLE lt_forn INTO lw_forn WITH KEY lifnr = gw_saida-lifnr.
      IF ( sy-subrc EQ 0 ).
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zlest0042, gw_zlest0032, gw_vttk, gw_vttp, gw_lips, gw_makt, gw_t023, gw_t023t, lw_forn.
        CONTINUE.
      ENDIF.


      gw_saida-doc_fiscal   = gw_bsak-xblnr.

      "GW_SAIDA-DMBE2        = GW_BSAK-DMBE2.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
      gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS
      IF (  gw_bsak-dmbtr > gw_bsak-dmbe2 ).

        IF ( gw_saida-augdt EQ '20150915') OR ( gw_saida-augdt EQ '20150916').
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
          gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS
          gw_saida-dmbe2 = gw_bsak-dmbtr / cs_tax_fix.



        ELSE.

          IF sy-cprog <> 'ZLESR0026'.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS
            gw_saida-dmbe2 = gw_bsak-dmbe2.


          ELSE.
            gw_saida-dmbtr = gw_bsak-dmbtr.
            gw_saida-dmbe2 = gw_bsak-dmbe2.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
            gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS


          ENDIF.

          PERFORM get_vlr_dolar USING '04' p_opcao
                             CHANGING gw_saida.

          "CSB
          "GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
          "GW_SAIDA-DMBE2 = GW_BSAK-DMBE2.

        ENDIF.
      ELSE.

        "Busca Valor Dolar pela BKPF
        CLEAR: gw_bkpf_aux.
        SELECT SINGLE *
          FROM bkpf INTO gw_bkpf_aux
         WHERE bukrs = gw_bsak-bukrs
           AND belnr = gw_bsak-augbl
           AND gjahr = gw_bsak-gjahr.

        IF sy-cprog <> 'ZLESR0026'.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
          gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS
          gw_saida-dmbe2 = gw_bsak-dmbe2.



          IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-vl_pago_lote NE 0 ).

            gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

            gw_saida-dmbe2 = gw_saida-vl_pago_lote / gw_bkpf_aux-kurs2.

          ENDIF.

        ELSE.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
          gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS
          gw_saida-dmbtr = gw_bsak-dmbtr.
          gw_saida-dmbe2 = gw_bsak-dmbe2.



          IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-dmbtr NE 0 ).

            gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

            gw_saida-dmbe2 = gw_saida-dmbtr / gw_bkpf_aux-kurs2.

          ENDIF.

        ENDIF.

        PERFORM get_vlr_dolar USING '04' p_opcao
                           CHANGING gw_saida.
        "CSB
        "GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
        "GW_SAIDA-DMBE2 = GW_BSAK-DMBE2.
      ENDIF.



      gw_saida-zfbdt        = gw_bsak-zfbdt + gw_bsak-zbd1t.

      CASE gw_bkpf-bktxt.
        WHEN: 'SEGURO-FRETE'.
          gw_saida-miro         = gw_bsak-xblnr(10).
        WHEN OTHERS.
          gw_saida-miro         = gw_bkpf-awkey.
      ENDCASE.

      READ TABLE lt_text INTO lw_text WITH KEY descricao = gw_bkpf-bktxt.
      IF ( sy-subrc EQ 0 ).
        CLEAR: gw_bsak, gw_rbkp, gw_bkpf, gw_zlest0042, gw_zlest0032, gw_vttk, gw_vttp, gw_lips, gw_makt, gw_t023, gw_t023t, lw_forn.
        CONTINUE.
      ENDIF.
      gw_saida-bktxt        = gw_bkpf-bktxt.


      "Modificação para colocar o grupo de material do fertilizante para quando for PAGAMENTO A POSTO.
      CASE gw_saida-bktxt.
        WHEN: 'Pagamento a posto'.
          READ TABLE gt_t023t INTO gw_t023t WITH KEY matkl = '700150'.
          gw_saida-wgbez = gw_t023t-wgbez.
          gw_saida-matkl = gw_t023t-matkl.
      ENDCASE.

      PERFORM f_valida_lcto USING gw_saida p_opcao
                         CHANGING v_valida.

      IF v_valida IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND gw_saida TO gt_saida.

      "Verifica se deve gerar Estorno.
      PERFORM f_check_estorno USING gw_saida.

    ENDIF.
  ENDLOOP.


  "*********************************************************************
  " A REGRA ABAIXO É REFERENTE AO FRETE HERMSA - INICIO
  "*********************************************************************

  REFRESH: gt_bsak[].
  CLEAR: gw_bsak, gw_saida.

  IF p_opcao NE 'R_AQUAV'. "Aquaviario

    SELECT  bukrs augdt blart shkzg augbl belnr gjahr dmbtr dmbe2 monat xblnr lifnr zfbdt zbd1t sgtxt budat umsks ebeln gsber zuonr
      FROM bsak
      INTO TABLE gt_bsak
    WHERE bukrs IN it_bukrs
      AND augdt IN it_augdt
      AND gjahr IN it_gjahr
      AND lifnr EQ '0000001006'
      AND zuonr EQ 'FRETE RODO INTERC'.

  ENDIF.

  LOOP AT gt_bsak INTO gw_bsak.

    CASE p_opcao.
      WHEN: 'PAG'.
        gw_saida-tipo = 'FP'.
    ENDCASE.

    gw_saida-bukrs         = gw_bsak-bukrs.
    gw_saida-monat         = gw_bsak-monat.
    gw_saida-gjahr         = gw_bsak-gjahr.
    gw_saida-augbl         = gw_bsak-augbl.
    gw_saida-belnr         = gw_bsak-belnr.
    gw_saida-blart         = gw_bsak-blart.
    gw_saida-lifnr         = gw_bsak-lifnr.
    gw_saida-bktxt         = gw_bsak-zuonr.
    gw_saida-sgtxt         = gw_bsak-sgtxt.
    gw_saida-augdt         = gw_bsak-augdt.
    gw_saida-zfbdt         = gw_bsak-augdt.
    gw_saida-dmbe2         = gw_bsak-dmbe2.

    "Busca Valor Dolar pela BKPF
    CLEAR: gw_bkpf_aux.
    SELECT SINGLE *
      FROM bkpf INTO gw_bkpf_aux
     WHERE bukrs = gw_bsak-bukrs
       AND belnr = gw_bsak-augbl
       AND gjahr = gw_bsak-gjahr.

*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
    gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS






    IF sy-cprog <> 'ZLESR0026'.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
      gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS





      IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-vl_pago_lote NE 0 ).

        gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

        gw_saida-dmbe2 = gw_saida-vl_pago_lote / gw_bkpf_aux-kurs2.

      ENDIF.
    ELSE.
*---> 14/06/2023 - Migração S4 - JS
*            GW_SAIDA-VL_PAGO_LOTE = GW_BSAK-DMBTR.
      gw_saida-vl_pago_lote = CONV #( gw_bsak-dmbtr ).
*<--- 14/06/2023 - Migração S4 - JS





      gw_saida-dmbtr = gw_bsak-dmbtr.

      IF ( gw_bkpf_aux-kurs2 NE 0 ) AND ( gw_saida-dmbtr NE 0 ).

        gw_bkpf_aux-kurs2 = abs( gw_bkpf_aux-kurs2 ).

        gw_saida-dmbe2 = gw_saida-dmbtr / gw_bkpf_aux-kurs2.

      ENDIF.
    ENDIF.

    PERFORM get_vlr_dolar USING '05' p_opcao
                       CHANGING gw_saida.
    "CSB
    "GW_SAIDA-VL_PAGO_LOTE  = GW_BSAK-DMBTR.

    PERFORM f_valida_lcto USING gw_saida p_opcao
                       CHANGING v_valida.

    IF v_valida IS INITIAL.
      CONTINUE.
    ENDIF.

    APPEND gw_saida TO gt_saida.

    "Verifica se deve gerar Estorno.
    PERFORM f_check_estorno USING gw_saida.

    CLEAR: gw_saida, gw_bsak.

  ENDLOOP.

  "*********************************************************************
  " A REGRA ABAIXO É REFERENTE AO FRETE HERMSA - FIM
  "*********************************************************************

ENDFORM.                    " SELECIONAR_DADOS

FORM get_vlr_dolar USING p_origem p_opcao
                CHANGING p_saida TYPE ty_saida.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

  DATA: vl_tx_cambio TYPE ukurs_curr,
        vl_gdatu     TYPE gdatu_inv.

  CHECK p_opcao EQ 'PAG'.

  CREATE OBJECT obj_zcl_util_sd.

  "Busca Taxa Data Compensação
  CLEAR: vl_tx_cambio.
  MOVE p_saida-augdt TO vl_gdatu.

  obj_zcl_util_sd->set_kurst('B').
  obj_zcl_util_sd->set_waerk('USD').
  obj_zcl_util_sd->set_tcurr('BRL').
  obj_zcl_util_sd->set_data( vl_gdatu ).

  vl_tx_cambio = abs( obj_zcl_util_sd->taxa_cambio( ) ).

  IF vl_tx_cambio > 0.
    IF p_saida-vl_pago_lote NE 0.
      p_saida-dmbe2 = p_saida-vl_pago_lote / vl_tx_cambio.
    ELSE.
      p_saida-dmbe2 = p_saida-dmbtr / vl_tx_cambio.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_check_estorno USING p_saida TYPE ty_saida.

  DATA: gw_bkpf_aux TYPE bkpf,
        vl_estorno  TYPE c.

  CLEAR: gw_bkpf_aux, vl_estorno.

  CHECK ( p_saida-bukrs IS NOT INITIAL ) AND
        ( p_saida-belnr IS NOT INITIAL ) AND
        ( p_saida-gjahr IS NOT INITIAL ).

  SELECT SINGLE *
    FROM bkpf INTO gw_bkpf_aux
   WHERE bukrs = p_saida-bukrs
     AND belnr = p_saida-belnr
     AND gjahr = p_saida-gjahr.

  IF ( sy-subrc = 0 ) AND ( gw_bkpf_aux-stgrd IS NOT INITIAL ).
    vl_estorno = 'X'.
  ELSEIF ( p_saida-augbl IS NOT INITIAL ) AND
         ( p_saida-augdt IS NOT INITIAL ).

    CLEAR: gw_bkpf_aux.
    SELECT SINGLE *
      FROM bkpf INTO gw_bkpf_aux
     WHERE bukrs = p_saida-bukrs
       AND belnr = p_saida-augbl
       AND gjahr = p_saida-augdt(4).

    IF ( sy-subrc = 0 ) AND ( gw_bkpf_aux-stgrd IS NOT INITIAL ).
      vl_estorno = 'X'.
    ENDIF.
  ENDIF.

  IF vl_estorno IS NOT INITIAL.
    p_saida-dmbtr = p_saida-dmbtr * -1.
    p_saida-dmbe2 = p_saida-dmbe2 * -1.
    p_saida-vl_pago_lote = p_saida-vl_pago_lote * -1.
    p_saida-xblnr = 'ESTORNO'.
    APPEND p_saida TO gt_saida.
  ENDIF.

ENDFORM.

FORM f_check_zgl047 USING p_bukrs     TYPE zglt035-bukrs
                          p_doc_lcto  TYPE zglt035-doc_lcto
                 CHANGING p_continue  TYPE c.

  DATA: vl_seq_tipo_ap TYPE zglt050-seq_tipo,
        vl_seq_tipo_fr TYPE zglt050-seq_tipo.

  CLEAR: p_continue, vl_seq_tipo_ap, vl_seq_tipo_fr.

  CHECK ( p_bukrs IS NOT INITIAL ) AND ( p_doc_lcto IS NOT INITIAL ).

  SELECT SINGLE b~seq_tipo INTO vl_seq_tipo_ap
    FROM zglt067 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
   WHERE b~bukrs    = p_bukrs
     AND a~doc_lcto = p_doc_lcto.

  CHECK sy-subrc = 0.

  "Se for lançamento da ZGLT047 e não for do tipo 8 - FRETES RODOVIÁRIOS, ignorar registro.

  vl_seq_tipo_fr = 8.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vl_seq_tipo_fr
    IMPORTING
      output = vl_seq_tipo_fr.

  IF vl_seq_tipo_ap NE vl_seq_tipo_fr.
    p_continue = 'X'.
  ENDIF.

ENDFORM.

FORM f_valida_lcto  USING  p_saida  TYPE ty_saida
                           p_opcao
                  CHANGING p_valida.

  DATA: gw_zlest0105    TYPE zlest0105,
        var_tipo_reg(2) TYPE c.

  CLEAR: p_valida, var_tipo_reg.

  CASE p_opcao.
    WHEN: 'R_AD_CX'.
      var_tipo_reg = 'FA'.
    WHEN: 'PAG'.
      var_tipo_reg = 'FP'.
  ENDCASE.

  CLEAR: gw_zlest0105.
  SELECT SINGLE *
    FROM zlest0105 INTO gw_zlest0105
   WHERE belnr          = p_saida-belnr
     AND augbl          = p_saida-augbl
     AND bukrs          = p_saida-bukrs
     AND docnum         = p_saida-docnum
     AND tknum          = p_saida-tknum
     AND chvid          = p_saida-chvid
     AND tipo           = p_saida-tipo
     AND estorno        = 'X'
     AND estorno_manual = 'M'. " Especifico para um caso(Não é rotina normal). Ajuste em 16.01.2017

  IF sy-subrc = 0.
    p_saida-dmbtr = p_saida-dmbtr * -1.
    p_saida-dmbe2 = p_saida-dmbe2 * -1.
    p_saida-vl_pago_lote = p_saida-vl_pago_lote * -1.
    p_saida-xblnr = 'ESTORNO'.
    APPEND p_saida TO gt_saida.
    RETURN.
  ENDIF.

  CLEAR: gw_zlest0105.
  SELECT SINGLE *
    FROM zlest0105 INTO gw_zlest0105
   WHERE belnr          = p_saida-belnr
     AND augbl          = p_saida-augbl
     AND bukrs          = p_saida-bukrs
     AND docnum         = p_saida-docnum
     AND tknum          = p_saida-tknum
     AND chvid          = p_saida-chvid
     AND tipo           = p_saida-tipo
     AND estorno        = 'X'
     AND estorno_manual = 'X'.

  IF sy-subrc = 0.
    RETURN.
  ENDIF.

  p_valida = 'X'.

ENDFORM.

FORM f_valida_lcto_aqua  USING  p_opcao
                                p_vttk  TYPE vttk
                                p_saida TYPE ty_saida
                       CHANGING p_valida.

  p_valida = abap_false.

  DATA(_lcto_aquav) = abap_false.

  IF p_saida-augdt >= '20200101'.
    IF ( ( p_vttk-shtyp EQ 'Z002' ) OR ( p_vttk-shtyp EQ 'Z027' ) ) AND ( p_saida-bukrs NE '0001' ).
      _lcto_aquav = abap_true.
    ENDIF.
  ELSE.
    IF ( ( p_vttk-shtyp EQ 'Z002' ) OR ( p_vttk-shtyp EQ 'Z027' ) ).
      _lcto_aquav = abap_true.
    ENDIF.
  ENDIF.

  CASE p_opcao.
    WHEN 'R_AQUAV'.
      IF _lcto_aquav EQ abap_false.
        RETURN.
      ENDIF.
    WHEN OTHERS.
      IF _lcto_aquav EQ abap_true.
        RETURN.
      ENDIF.
  ENDCASE.

  p_valida = abap_true.

ENDFORM.
