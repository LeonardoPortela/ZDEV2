FUNCTION z_inves_sysphera_report.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_AREA) TYPE  KOKRS
*"     VALUE(I_EMPRESA) TYPE  BUKRS
*"     VALUE(I_DT_INICIAL) TYPE  DATUM
*"     VALUE(I_DT_FINAL) TYPE  DATUM
*"  TABLES
*"      POSNR STRUCTURE  ZSDS_POSNR_RANGE OPTIONAL
*"      RESULTADO STRUCTURE  ZSYS_INVES OPTIONAL
*"      RESULTADO_TOT STRUCTURE  ZSYS_INVES2 OPTIONAL
*"----------------------------------------------------------------------

  REFRESH: resultado,
           resultado_tot.

  TYPES: BEGIN OF ty_res_ima,
           im_posnr TYPE ima_posnr,
           im_txt50 TYPE ima_txt50,
           im_bukrs TYPE ima_abukrs,
           im_kostl TYPE ima_vkostl,
           im_anln1 TYPE anln1,
           im_anln2 TYPE anln2,
           im_objnr TYPE aufnr,
           im_text  TYPE msgli.
  TYPES: END   OF ty_res_ima.

  DATA: w_resultado TYPE zsys_inves.
  DATA: w_res_ima   TYPE ty_res_ima.
  DATA: lv_objnr TYPE  onr00-objnr.

  DATA: lv_gjahr  TYPE imak-gjahr.

  DATA: lv_text  TYPE  sy-msgli,
        lv_ionra.

  DATA: l_tem_anep01 TYPE c,
        l_tem_anep41 TYPE c,
        l_tabix      TYPE sy-tabix.


****USER STORY 72406* / Anderson Oenning
  DATA: it_anlc TYPE TABLE OF anlc,
        it_ekkn TYPE TABLE OF ekkn,
        it_ekko TYPE TABLE OF ekko,
        it_rbkp TYPE TABLE OF rbkp,
        it_ekbe TYPE TABLE OF ekbe,
        it_lfa1 TYPE TABLE OF lfa1.


****USER STORY 72406*  / Anderson Oenning

  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

  RANGES: r_posnr   FOR imak-posnr.

  FREE: r_posnr.

  LOOP AT posnr              INTO DATA(w_posnr).
    MOVE-CORRESPONDING w_posnr TO r_posnr.
    APPEND r_posnr.
  ENDLOOP.

  REFRESH gt_imak.

  IF r_posnr[] IS INITIAL.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 02  INICIO - BG
*    lv_gjahr = i_dt_inicial(4) - 1.
*    SELECT posnr,
*           abukrs,
*           vkostl,
*           werks,
*           vkokrs,
*           vgsber
*      INTO TABLE @gt_imak
*      FROM imak
*      WHERE abukrs = @i_empresa
*        AND gjahr  = @lv_gjahr
*        AND posnr IN @r_posnr.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 02  FIM - BG
    lv_gjahr = i_dt_inicial(4) .
    SELECT posnr,
           abukrs,
           vkostl,
           werks,
           vkokrs,
           vgsber
       APPENDING TABLE @gt_imak
      FROM imak
      WHERE abukrs = @i_empresa
        AND gjahr  = @lv_gjahr
        AND posnr IN @r_posnr.
  ELSE.
    SELECT posnr,
          abukrs,
          vkostl,
          werks,
          vkokrs,
          vgsber
      APPENDING TABLE @gt_imak
     FROM imak
     WHERE abukrs = @i_empresa
       AND posnr IN @r_posnr.

  ENDIF.

  IF gt_imak[] IS NOT INITIAL.
    SELECT werks,
           name1 INTO TABLE @DATA(lt_t001w)
      FROM t001w
      FOR ALL ENTRIES IN @gt_imak
      WHERE werks = @gt_imak-vgsber. "@gt_imak-werks.
    SORT lt_t001w BY werks.

    SELECT posnr,
           txt50
       INTO TABLE @DATA(lt_imakt)
      FROM imakt
      FOR ALL ENTRIES IN @gt_imak
      WHERE posnr = @gt_imak-posnr
        AND spras = 'P'.
    SORT lt_imakt BY posnr.

    SELECT posnr,
           objnr
       INTO TABLE @gt_imakz
      FROM imakz
      FOR ALL ENTRIES IN @gt_imak
      WHERE posnr = @gt_imak-posnr.
    SORT gt_imakz BY posnr.

    IF gt_imakz[] IS NOT INITIAL.
      PERFORM zf_exec_kob1 USING i_dt_inicial
                                 i_dt_final
                                 ''.

      PERFORM zf_exec_kob2 USING i_dt_inicial
                                 i_dt_final
                                 '' .
    ENDIF.

    SELECT posnr,
           bukrs,
           anln1,
           anln2
       INTO TABLE @DATA(lt_imaka)
      FROM imaka
      FOR ALL ENTRIES IN @gt_imak
      WHERE posnr = @gt_imak-posnr.
    SORT lt_imaka BY posnr.

*    lv_gjahr = i_dt_inicial(4) - 1.
    SELECT  izwek,
            kostl,
            knttp,
            saknr,
            txt20,
            descr_item,
            bukrs,
            ano,
            fase,
            gsber,
            finalidade,
            solicitacao_invest,
            objetivo,
            menge,
            posnr,
            tx_usd,
            vlr_unitario,
            vlr_total,
            vl_usd
      INTO TABLE @DATA(lt_inv)
      FROM zim01_sol_ap_inv
      FOR ALL ENTRIES IN @gt_imak
      WHERE posnr = @gt_imak-posnr
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  INICIO - BG
        AND ano = @lv_gjahr.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  FIM - BG

*    lv_gjahr = i_dt_inicial(4).
*    SELECT  izwek,
*            kostl,
*            knttp,
*            saknr,
*            txt20,
*            descr_item,
*            bukrs,
*            ano,
*            fase,
*            gsber,
*            finalidade,
*            solicitacao_invest,
*            objetivo,
*            menge,
*            posnr,
*            tx_usd,
*            vlr_unitario,
*            vlr_total,
*            vl_usd
*       APPENDING TABLE @lt_inv
*      FROM zim01_sol_ap_inv
*      FOR ALL ENTRIES IN @gt_imak
*      WHERE posnr = @gt_imak-posnr
*        AND ano = @lv_gjahr.
*    SORT lt_inv BY posnr.


    IF lt_inv[] IS NOT INITIAL.
      SELECT kostl,
             ktext
         INTO TABLE @DATA(lt_cskt)
        FROM cskt
        FOR ALL ENTRIES IN @lt_inv
        WHERE kokrs = @i_area
          AND kostl = @lt_inv-kostl
          AND spras = @sy-langu.
      SORT lt_cskt BY kostl.
    ENDIF.

*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 01  INICIO - BG
    SELECT bukrs, anln1, anln2, gjahr,
                 lnran, afabe, zujhr, zucod,
                 belnr, bzdat, anbtr, bwasl
            INTO TABLE @DATA(lt_anep)
            FROM anep
            WHERE bukrs = @i_empresa "@lt_anla-bukrs
              AND bzdat BETWEEN @i_dt_inicial AND @i_dt_final
              AND ( afabe = '01' OR afabe = '41' ).

    SORT lt_anep BY bukrs
                    anln1
                    anln2
                    lnran
                    afabe
                    bwasl.

    DATA(lt_anep_aux) = lt_anep[].

    DELETE lt_anep_aux WHERE afabe = '01'.

    SORT lt_anep_aux BY bukrs anln1 anln2 lnran bwasl.

    IF lt_anep[] IS NOT INITIAL.
      SELECT posnr,
       bukrs,
       anln1,
       anln2
       APPENDING TABLE @DATA(lt_imaka_)
      FROM imaka
      FOR ALL ENTRIES IN @lt_anep
      WHERE bukrs = @lt_anep-bukrs
      AND   anln1 = @lt_anep-anln1
      AND   anln2 = @lt_anep-anln2.

      SORT lt_imaka_ BY bukrs anln1 anln2.

      SELECT posnr,
             txt50
       APPENDING TABLE @lt_imakt
       FROM imakt
       FOR ALL ENTRIES IN @lt_imaka_
       WHERE posnr = @lt_imaka_-posnr
       AND spras = 'P'.

      SORT lt_imakt BY posnr.

      SELECT bukrs,
             anln1,
             anln2,
             txt50,
             txa50
         INTO TABLE @DATA(lt_anla)
        FROM anla
        FOR ALL ENTRIES IN @lt_anep
        WHERE bukrs = @lt_anep-bukrs
          AND anln1 = @lt_anep-anln1
          AND anln2 = @lt_anep-anln2.


      SELECT bwasl,
             bwatxt INTO TABLE @DATA(lt_tabwt)
        FROM tabwt
        FOR ALL ENTRIES IN @lt_anep
        WHERE bwasl = @lt_anep-bwasl
          AND spras = @sy-langu.

* ---> S4 Migration - 16/06/2023 - JS
*      SELECT bukrs, belnr, gjahr, buzei, lifnr
*        INTO TABLE @DATA(lt_bseg)
*        FROM bseg
*         FOR ALL ENTRIES IN @lt_anep
*       WHERE bukrs = @lt_anep-bukrs
*         AND belnr = @lt_anep-belnr
*         AND gjahr = @lt_anep-bzdat(4).

  DATA lt_fields TYPE fagl_t_field.
  DATA lt_bseg   TYPE TABLE OF bseg.

  lt_fields = VALUE #( ( line = 'BUKRS' )
                       ( line = 'BELNR' )
                       ( line = 'GJAHR' )
                       ( line = 'BUZEI' )
                       ( line = 'LIFNR' ) ).

  CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING
      it_for_all_entries = lt_anep
*      i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-BZDAT(4)|
      i_where_clause     = |BUKRS = IT_FOR_ALL_ENTRIES-BUKRS AND BELNR = IT_FOR_ALL_ENTRIES-BELNR AND GJAHR = IT_FOR_ALL_ENTRIES-GJAHR|
      it_fieldlist       = lt_fields
    IMPORTING
      et_bseg            = lt_bseg
    EXCEPTIONS
      not_found          = 1.
* <--- S4 Migration - 16/06/2023 - JS

      DELETE lt_bseg WHERE lifnr IS INITIAL.

      SORT lt_tabwt BY bwasl.

      SORT lt_bseg  BY bukrs belnr gjahr.
      DELETE ADJACENT DUPLICATES FROM lt_bseg
                            COMPARING bukrs belnr gjahr.


      SORT lt_anla BY bukrs
                      anln1
                      anln2.

      IF lt_anla[] IS NOT INITIAL.

        SELECT bukrs,
               anln1,
               anln2,
               kostl
           INTO TABLE @DATA(lt_anlz)
         FROM anlz
         FOR ALL ENTRIES IN @lt_anla
         WHERE bukrs = @lt_anla-bukrs
           AND anln1 = @lt_anla-anln1
           AND anln2 = @lt_anla-anln2
           AND bdatu >= @i_dt_inicial.

        SORT lt_anlz BY bukrs
                        anln1
                        anln2.



      ENDIF.
***USER STORY 72406* / Anderson Oenning
      "Dados pedido.
      SELECT * FROM ekkn INTO TABLE it_ekkn
        FOR ALL ENTRIES IN lt_anla
        WHERE anln1 EQ lt_anla-anln1 AND anln2 EQ lt_anla-anln2.
      IF sy-subrc EQ 0.
        SELECT * FROM ekko INTO TABLE it_ekko FOR ALL ENTRIES IN it_ekkn WHERE ebeln EQ it_ekkn-ebeln.

        "Dados compra imobilizado.
        SELECT * FROM ekbe INTO TABLE it_ekbe FOR ALL ENTRIES IN it_ekkn WHERE ebeln EQ it_ekkn-ebeln AND ebelp EQ it_ekkn-ebelp AND vgabe EQ '2'.
        IF sy-subrc EQ 0.
          "Dados da nota entrada.
          SELECT * FROM rbkp INTO TABLE it_rbkp FOR ALL ENTRIES IN it_ekbe WHERE belnr EQ it_ekbe-belnr AND gjahr EQ it_ekbe-gjahr.
          IF sy-subrc EQ 0.
            "Dados do fornecedor.
            SELECT * FROM lfa1 INTO TABLE it_lfa1 FOR ALL ENTRIES IN it_rbkp WHERE lifnr EQ it_rbkp-lifnr.
          ENDIF.
        ENDIF.
      ENDIF.


      "Seleção das informações valor da aquisição.
      SELECT * FROM anlc INTO TABLE it_anlc
      FOR ALL ENTRIES IN lt_anla
      WHERE anln1 EQ lt_anla-anln1 AND anln2 EQ lt_anla-anln2 AND bukrs EQ lt_anla-bukrs AND afabe EQ '50'.
      IF it_anlc IS NOT INITIAL.
        SORT it_anlc DESCENDING BY anln1 gjahr.
      ENDIF.
***USER STORY 72406* / Anderson Oenning
    ENDIF.


*if lt_imaka[] is not INITIAL.
*    SELECT bukrs,
*           anln1,
*           anln2,
*           txt50,
*           txa50
*       INTO TABLE @DATA(lt_anla)
*      FROM anla
*      FOR ALL ENTRIES IN @lt_imaka
*      WHERE bukrs = @lt_imaka-bukrs
*        AND anln1 = @lt_imaka-anln1
*        AND anln2 = @lt_imaka-anln2.
*endif.
*    SORT lt_anla BY bukrs
*                    anln1
*                    anln2.

*    IF lt_anla[] IS NOT INITIAL.
*
*      SELECT bukrs,
*             anln1,
*             anln2,
*             kostl
*         INTO TABLE @DATA(lt_anlz)
*       FROM anlz
*       FOR ALL ENTRIES IN @lt_anla
*       WHERE bukrs = @lt_anla-bukrs
*         AND anln1 = @lt_anla-anln1
*         AND anln2 = @lt_anla-anln2
*         AND bdatu >= @i_dt_inicial.
*
*      SORT lt_anlz BY bukrs
*                      anln1
*                      anln2.
*
**      lv_gjahr = i_dt_inicial(4).
*      SELECT bukrs, anln1, anln2, gjahr,
*             lnran, afabe, zujhr, zucod,
*             belnr, bzdat, anbtr, bwasl
*        INTO TABLE @DATA(lt_anep)
*        FROM anep
*        FOR ALL ENTRIES IN @lt_anla
*        WHERE bukrs = @lt_anla-bukrs
*          AND anln1 = @lt_anla-anln1
*          AND anln2 = @lt_anla-anln2
**          AND gjahr = @lv_gjahr
*          AND bzdat BETWEEN @i_dt_inicial AND @i_dt_final
*          AND ( afabe = '01' OR afabe = '41' ).
*
*      SORT lt_anep BY bukrs
*                      anln1
*                      anln2
*                      lnran
*                      afabe
*                      bwasl.                    .
*
*      DATA(lt_anep_aux) = lt_anep[].
*
*      DELETE lt_anep_aux WHERE afabe = '01'.
*
*      SORT lt_anep_aux BY bukrs anln1 anln2 lnran bwasl.
*
*      IF lt_anep[] IS NOT INITIAL.
*        SELECT bwasl,
*               bwatxt INTO TABLE @DATA(lt_tabwt)
*          FROM tabwt
*          FOR ALL ENTRIES IN @lt_anep
*          WHERE bwasl = @lt_anep-bwasl
*            AND spras = @sy-langu.
*
*        SELECT bukrs, belnr, gjahr, buzei, lifnr
*          INTO TABLE @DATA(lt_bseg)
*          FROM bseg
*           FOR ALL ENTRIES IN @lt_anep
*         WHERE bukrs = @lt_anep-bukrs
*           AND belnr = @lt_anep-belnr
*           AND gjahr = @lt_anep-bzdat(4).
*
*        DELETE lt_bseg WHERE lifnr IS INITIAL.
*
*        SORT lt_tabwt BY bwasl.
*
*        SORT lt_bseg  BY bukrs belnr gjahr.
*        DELETE ADJACENT DUPLICATES FROM lt_bseg
*                              COMPARING bukrs belnr gjahr.
*      ENDIF.
*    ENDIF.



*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 01  FIM - BG



*------------------------------------------
*-- criar linhas IM
*------------------------------------------
    LOOP AT gt_imak INTO DATA(ls_imak) .
      CLEAR: w_resultado,
             l_tem_anep01,
             l_tem_anep41,
             lv_text.

      w_resultado-tcode     = 'ZIM04'.
      w_resultado-im_posnr  = ls_imak-posnr.
      READ TABLE lt_imakt INTO DATA(ls_imakt) WITH KEY posnr = ls_imak-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        w_resultado-im_txt50  = ls_imakt-txt50.
      ENDIF.
      w_resultado-im_bukrs  = ls_imak-abukrs.
      w_resultado-im_kostl  = ls_imak-vkostl.
      READ TABLE lt_imaka INTO DATA(ls_imaka) WITH KEY posnr = ls_imak-posnr BINARY SEARCH.
      IF sy-subrc = 0.
        w_resultado-im_anln1  = ls_imaka-anln1.
        w_resultado-im_anln2  = ls_imaka-anln2.
      ENDIF.

      MOVE-CORRESPONDING w_resultado TO w_res_ima.

*------------------
*---- investimentos
*------------------
      "Inicio Ajuste referente [IR077557] / Anderson Oenning
*      READ TABLE lt_inv INTO DATA(ls_inv) WITH KEY posnr = ls_imak-posnr. " BINARY SEARCH.
      LOOP AT lt_inv INTO DATA(ls_inv) WHERE posnr EQ ls_imak-posnr.
*        IF sy-subrc = 0.
* CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  INICIO - BG
        w_resultado-tcode     = 'ZIM04'.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  FIM - BG
        MOVE-CORRESPONDING w_res_ima TO w_resultado.
        w_resultado-zi_izwek  = ls_inv-izwek.
        w_resultado-zi_kostl  = ls_inv-kostl.
        w_resultado-zi_knttp  = ls_inv-knttp.
        w_resultado-zi_saknr  = ls_inv-saknr.
        w_resultado-zi_txt20  = ls_inv-txt20.
        w_resultado-zi_descr_item	= ls_inv-descr_item.
        w_resultado-zi_bukrs  = ls_inv-bukrs.
        w_resultado-zi_ano    = ls_inv-ano.
        w_resultado-zi_fase   = ls_inv-fase.
        w_resultado-zi_gsber  = ls_inv-gsber.
        w_resultado-zi_final  = ls_inv-finalidade.
        w_resultado-zi_sysphera	= ls_inv-solicitacao_invest.
        w_resultado-zi_objetivo	= ls_inv-objetivo.
        w_resultado-zi_menge   = ls_inv-menge.
        w_resultado-zi_posnr  = ls_inv-posnr.
        w_resultado-zi_tx_usd	= ls_inv-tx_usd.
        w_resultado-zi_vlr_unit	= ls_inv-vlr_unitario.
        w_resultado-zi_vlr_total  = ls_inv-vlr_total.
        w_resultado-zi_vlr_tot_us  = ls_inv-vl_usd.
*        w_resultado-zi_vlr_tot_us  = ls_inv-vlr_total / ls_inv-tx_usd.
        READ TABLE lt_cskt INTO DATA(ls_cskt) WITH KEY kostl = ls_inv-kostl BINARY SEARCH.
        IF sy-subrc = 0.
          w_resultado-zi_ktext = ls_cskt-ktext.
        ENDIF.
        READ TABLE lt_t001w INTO DATA(ls_t001w) WITH KEY werks = ls_imak-vgsber BINARY SEARCH.
        IF sy-subrc = 0.
          w_resultado-zi_name1 = ls_t001w-name1.
        ENDIF.
        APPEND w_resultado    TO resultado.
        CLEAR: w_resultado.
*        ENDIF.
      ENDLOOP.

      "Ordens
      LOOP AT gt_imakz INTO DATA(ls_imakz) WHERE posnr = ls_imak-posnr.
*------------------
*---- KOB1
*------------------
        LOOP AT gt_kob1 INTO gs_kob1 WHERE k1_aufnr = ls_imakz-objnr+2(12).
          MOVE-CORRESPONDING gs_kob1   TO w_resultado.
          MOVE-CORRESPONDING w_res_ima TO w_resultado.
          w_resultado-tcode             = 'KOB1'.
          w_resultado-im_objnr  = ls_imakz-objnr+2(12).
          IF ls_imakz-objnr IS NOT INITIAL.
            lv_objnr = ls_imakz-objnr.
            CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
              EXPORTING
                langu         = sy-langu
                objnr         = lv_objnr
              IMPORTING
*               e_ionra       = lv_ionra
                e_text        = lv_text
              EXCEPTIONS
                obart_invalid = 1
                OTHERS        = 2.

            w_resultado-im_text    = lv_text.
          ELSE.
            w_resultado-im_text = ''.
          ENDIF.
          MOVE lv_text                 TO w_resultado-k1_text_ima.
          MOVE ls_imakz-objnr+2(12)    TO w_resultado-k1_objnr_ima.
          APPEND w_resultado           TO resultado.
          CLEAR w_resultado.
        ENDLOOP.

*------------------
*---- KOB2
*------------------
        LOOP AT gt_kob2 INTO gs_kob2 WHERE k2_aufnr = ls_imakz-objnr+2(12).
          MOVE-CORRESPONDING gs_kob2   TO w_resultado.
          MOVE-CORRESPONDING w_res_ima TO w_resultado.
          w_resultado-tcode             = 'KOB2'.
          w_resultado-im_objnr  = ls_imakz-objnr+2(12).
          IF ls_imakz-objnr IS NOT INITIAL.
            lv_objnr = ls_imakz-objnr.
            CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
              EXPORTING
                langu         = sy-langu
                objnr         = lv_objnr
              IMPORTING
*               e_ionra       = lv_ionra
                e_text        = lv_text
              EXCEPTIONS
                obart_invalid = 1
                OTHERS        = 2.

            w_resultado-im_text    = lv_text.
          ELSE.
            w_resultado-im_text = ''.
          ENDIF.
          APPEND w_resultado           TO resultado.
          CLEAR w_resultado.
        ENDLOOP.
      ENDLOOP.

*------------------
*----AS03  - Imobilizado
*------------------


*COMO ESTAVA O LOOP

*      LOOP AT lt_imaka INTO ls_imaka WHERE posnr = ls_imak-posnr.
*        READ TABLE lt_anla INTO DATA(ls_anla) WITH KEY bukrs = ls_imaka-bukrs
*                                                       anln1 = ls_imaka-anln1
*                                                       anln2 = ls_imaka-anln2. " BINARY SEARCH.
*        IF sy-subrc = 0.
*          MOVE-CORRESPONDING w_res_ima TO w_resultado.
*          w_resultado-tcode        = 'AS03'.
*          w_resultado-as_anln1     = ls_anla-anln1.
*          w_resultado-as_anln2     = ls_anla-anln2.
*          w_resultado-as_anln1_ima = ls_imaka-anln1.
*          w_resultado-as_anln2_ima = ls_imaka-anln2.
*          w_resultado-as_txa50     = ls_anla-txt50.
*          w_resultado-as_txa50_c   = ls_anla-txa50.
*          READ TABLE lt_anlz INTO DATA(ls_anlz) WITH KEY bukrs = ls_anla-bukrs
*                                                         anln1 = ls_anla-anln1
*                                                         anln2 = ls_anla-anln2. " BINARY SEARCH.
*          IF sy-subrc = 0.
*            w_resultado-as_kostl = ls_anlz-kostl.
*          ENDIF.
*
**------------------
**----AW01N - Imobilizado
**------------------
*          LOOP AT lt_anep_aux INTO DATA(ls_anep_aux) WHERE bukrs = ls_anla-bukrs
*                                                       AND anln1 = ls_anla-anln1
*                                                       AND anln2 = ls_anla-anln2.
*            CLEAR: l_tem_anep01,
*                   l_tem_anep41.
*
*            READ TABLE lt_anep INTO DATA(ls_anep) WITH KEY bukrs = ls_anep_aux-bukrs
*                                                           anln1 = ls_anep_aux-anln1
*                                                           anln2 = ls_anep_aux-anln2
*                                                           lnran = ls_anep_aux-lnran
*                                                           afabe = '01' " BINARY SEARCH.
*                                                           bwasl = ls_anep_aux-bwasl.
*            IF sy-subrc = 0.
*              l_tem_anep01          = abap_true.
*              l_tabix               = sy-tabix.
*
*              READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_anep-bukrs
*                                                             belnr = ls_anep-belnr
*                                                             gjahr = ls_anep-bzdat(4)
*                                                    BINARY SEARCH.
*              IF sy-subrc = 0.
*                w_resultado-aw_lifnr = ls_bseg-lifnr.
*              ENDIF.
*
*              MOVE-CORRESPONDING w_res_ima TO w_resultado.
*              w_resultado-tcode        = 'AS03'.
*              w_resultado-as_anln1     = ls_anla-anln1.
*              w_resultado-as_anln2     = ls_anla-anln2.
*              w_resultado-as_anln1_ima = ls_imaka-anln1.
*              w_resultado-as_anln2_ima = ls_imaka-anln2.
*              w_resultado-as_txa50     = ls_anla-txt50.
*              w_resultado-as_txa50_c   = ls_anla-txa50.
*              w_resultado-as_kostl     = ls_anlz-kostl.
*              w_resultado-aw_bzdat     = ls_anep-bzdat.
*              w_resultado-aw_anbtrb    = ls_anep-anbtr.
*
*              DELETE lt_anep INDEX l_tabix.
*            ENDIF.
*
*            READ TABLE lt_anep INTO ls_anep WITH KEY bukrs = ls_anep_aux-bukrs
*                                                     anln1 = ls_anep_aux-anln1
*                                                     anln2 = ls_anep_aux-anln2
*                                                     lnran = ls_anep_aux-lnran
*                                                     afabe = '41'  " BINARY SEARCH.
*                                                     bwasl = ls_anep_aux-bwasl.
*            IF sy-subrc = 0.
*              l_tem_anep41          = abap_true.
*              l_tabix               = sy-tabix.
*
*              READ TABLE lt_bseg INTO ls_bseg  WITH KEY bukrs = ls_anep-bukrs
*                                                        belnr = ls_anep-belnr
*                                                        gjahr = ls_anep-bzdat(4)
*                                               BINARY SEARCH.
*              IF sy-subrc = 0.
*                w_resultado-aw_lifnr = ls_bseg-lifnr.
*              ENDIF.
*
*              MOVE-CORRESPONDING w_res_ima TO w_resultado.
*              w_resultado-tcode        = 'AS03'.
*              w_resultado-as_anln1     = ls_anla-anln1.
*              w_resultado-as_anln2     = ls_anla-anln2.
*              w_resultado-as_anln1_ima = ls_imaka-anln1.
*              w_resultado-as_anln2_ima = ls_imaka-anln2.
*              w_resultado-as_txa50     = ls_anla-txt50.
*              w_resultado-as_txa50_c   = ls_anla-txa50.
*              w_resultado-as_kostl     = ls_anlz-kostl.
*              w_resultado-aw_anbtru    = ls_anep-anbtr.
*              w_resultado-aw_bwasl     = ls_anep-bwasl.
*              READ TABLE lt_tabwt INTO DATA(ls_tabwt) WITH KEY bwasl = ls_anep-bwasl BINARY SEARCH.
*              IF sy-subrc = 0.
*                w_resultado-aw_bwatxt  = ls_tabwt-bwatxt.
*              ENDIF.
*
*              DELETE lt_anep INDEX l_tabix.
*            ENDIF.
*
*            IF l_tem_anep01 = abap_true OR
*               l_tem_anep41 = abap_true.
*              APPEND w_resultado    TO resultado.
*              CLEAR w_resultado.
*            ENDIF.
*          ENDLOOP.
*
*        ENDIF.
*      ENDLOOP.


*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 01  FIM 02 - BG

    ENDLOOP.

*cs2022000117 - ajustes na transação zim15  us 72901 - ajuste 01  inicio 02 - bg

    LOOP AT lt_anep_aux INTO DATA(ls_anep_aux).
      READ TABLE lt_imaka_ INTO ls_imaka WITH KEY bukrs = ls_anep_aux-bukrs
                                                     anln1 = ls_anep_aux-anln1
                                                     anln2 = ls_anep_aux-anln2 BINARY SEARCH.
      IF sy-subrc NE 0.
        CLEAR ls_imaka.
      ENDIF.
      READ TABLE lt_imakt INTO DATA(ls_imakt2) WITH KEY posnr = ls_imaka-posnr BINARY SEARCH.
      w_resultado-tcode        = 'AS03'.
      w_resultado-im_bukrs     = ls_anep_aux-bukrs.
      w_resultado-im_posnr     = ls_imaka-posnr.
      w_resultado-im_txt50     = ls_imakt2-txt50.
      w_resultado-as_anln1     = ls_anep_aux-anln1.
      w_resultado-as_anln2     = ls_anep_aux-anln2.
      w_resultado-as_anln1_ima = ls_anep_aux-anln1.
      w_resultado-as_anln2_ima = ls_anep_aux-anln2.
      READ TABLE lt_anla INTO DATA(ls_anla) WITH KEY bukrs = ls_anep_aux-bukrs
                                                   anln1 = ls_anep_aux-anln1
                                                   anln2 = ls_anep_aux-anln2. " BINARY SEARCH.

      IF sy-subrc = 0.
        w_resultado-as_txa50     = ls_anla-txt50.
        w_resultado-as_txa50_c   = ls_anla-txa50.
      ENDIF.

      READ TABLE lt_anlz INTO DATA(ls_anlz) WITH KEY bukrs = ls_anep_aux-bukrs
                                                     anln1 = ls_anep_aux-anln1
                                                     anln2 = ls_anep_aux-anln2. " BINARY SEARCH.
      IF sy-subrc = 0.
        w_resultado-as_kostl = ls_anlz-kostl.
      ENDIF.

*------------------
*----AW01N - Imobilizado
*------------------

      CLEAR: l_tem_anep01,
             l_tem_anep41.
*
      READ TABLE lt_anep INTO DATA(ls_anep) WITH KEY bukrs = ls_anep_aux-bukrs
                                                     anln1 = ls_anep_aux-anln1
                                                     anln2 = ls_anep_aux-anln2
                                                     lnran = ls_anep_aux-lnran
                                                     afabe = '01' " BINARY SEARCH.
                                                     bwasl = ls_anep_aux-bwasl.
      IF sy-subrc = 0.
        l_tem_anep01          = abap_true.
        l_tabix               = sy-tabix.

        READ TABLE lt_bseg INTO DATA(ls_bseg) WITH KEY bukrs = ls_anep-bukrs
                                                       belnr = ls_anep-belnr
                                                       gjahr = ls_anep-bzdat(4)
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          w_resultado-aw_lifnr = ls_bseg-lifnr.
        ENDIF.

*          MOVE-CORRESPONDING w_res_ima TO w_resultado.
        w_resultado-tcode        = 'AS03'.
        w_resultado-as_anln1     = ls_anla-anln1.
        w_resultado-as_anln2     = ls_anla-anln2.
        w_resultado-as_anln1_ima = ls_imaka-anln1.
        w_resultado-as_anln2_ima = ls_imaka-anln2.
        w_resultado-as_txa50     = ls_anla-txt50.
        w_resultado-as_txa50_c   = ls_anla-txa50.
        w_resultado-as_kostl     = ls_anlz-kostl.
        w_resultado-aw_bzdat     = ls_anep-bzdat.
        w_resultado-aw_anbtrb    = ls_anep-anbtr.

        DELETE lt_anep INDEX l_tabix.
      ENDIF.
*
      READ TABLE lt_anep INTO ls_anep WITH KEY bukrs = ls_anep_aux-bukrs
                                               anln1 = ls_anep_aux-anln1
                                               anln2 = ls_anep_aux-anln2
                                               lnran = ls_anep_aux-lnran
                                               afabe = '41'  " BINARY SEARCH.
                                               bwasl = ls_anep_aux-bwasl.
      IF sy-subrc = 0.
        l_tem_anep41          = abap_true.
        l_tabix               = sy-tabix.

        READ TABLE lt_bseg INTO ls_bseg  WITH KEY bukrs = ls_anep-bukrs
                                                  belnr = ls_anep-belnr
                                                  gjahr = ls_anep-bzdat(4)
                                         BINARY SEARCH.
        IF sy-subrc = 0.
          w_resultado-aw_lifnr = ls_bseg-lifnr.
        ENDIF.

*          MOVE-CORRESPONDING w_res_ima TO w_resultado.
        w_resultado-tcode        = 'AS03'.
        w_resultado-as_anln1     = ls_anep_aux-anln1.
        w_resultado-as_anln2     = ls_anep_aux-anln2.
        w_resultado-as_anln1_ima = ls_anep_aux-anln1.
        w_resultado-as_anln2_ima = ls_anep_aux-anln2.
        w_resultado-as_txa50     = ls_anla-txt50.
        w_resultado-as_txa50_c   = ls_anla-txa50.
        w_resultado-as_kostl     = ls_anlz-kostl.
        w_resultado-aw_anbtru    = ls_anep-anbtr.
        w_resultado-aw_bwasl     = ls_anep-bwasl.
        READ TABLE lt_tabwt INTO DATA(ls_tabwt) WITH KEY bwasl = ls_anep-bwasl BINARY SEARCH.
        IF sy-subrc = 0.
          w_resultado-aw_bwatxt  = ls_tabwt-bwatxt.
        ENDIF.

        DELETE lt_anep INDEX l_tabix.
      ENDIF.

***USER STORY 72406* / Anderson Oenning
      READ TABLE it_anlc INTO DATA(ws_anlc) WITH KEY anln1 = ls_anla-anln1 anln2 = ls_anla-anln2 bukrs = ls_anla-bukrs.
      IF sy-subrc EQ 0.
        w_resultado-as_kansw = ws_anlc-kansw. "Valor do ativo atualizado
      ENDIF.

      "Valor compra.
      READ TABLE it_ekkn INTO DATA(ws_ekkn) WITH KEY anln1 = ls_anla-anln1 anln2 = ls_anla-anln2.
      IF sy-subrc EQ 0.
        READ TABLE it_ekbe INTO DATA(ws_ekbe) WITH KEY ebeln = ws_ekkn-ebeln ebelp = ws_ekkn-ebelp.
        IF sy-subrc EQ 0.
          READ TABLE it_rbkp INTO DATA(ws_rbkp) WITH KEY belnr = ws_ekbe-belnr gjahr = ws_ekbe-gjahr.
          IF sy-subrc EQ 0.
            w_resultado-as_rmwwr = ws_rbkp-rmwwr. "Valor da fatura.
            w_resultado-as_budat = ws_rbkp-budat. "Data da fatura.
            w_resultado-as_xblnr = ws_rbkp-xblnr. "Nº da nota fiscal.


            "Dados do fornecedor.
            READ TABLE it_lfa1 INTO DATA(ws_lfa1) WITH KEY lifnr = ws_rbkp-lifnr.
            IF sy-subrc EQ 0.
              w_resultado-as_name1 = ws_lfa1-name1. "Nome fornecedor
              w_resultado-as_stcd1 = ws_lfa1-stcd1. "CNPJ fornecedor
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE it_ekko INTO DATA(ws_ekko) WITH KEY ebeln = ws_ekkn-ebeln.
        IF sy-subrc EQ 0.
          w_resultado-as_bsart = ws_ekko-bsart. "Tipo de pedido
          w_resultado-as_ebeln = ws_ekko-ebeln. "Pedido
        ENDIF.
      ENDIF.
***USER STORY 72406* / Anderson Oenning
*
      IF l_tem_anep01 = abap_true OR
         l_tem_anep41 = abap_true.
        APPEND w_resultado    TO resultado.
        CLEAR w_resultado.
      ENDIF.

     CLEAR: ws_anlc, ws_ekkn, ws_ekbe, ws_rbkp, ws_lfa1.
    ENDLOOP.
  ENDIF.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  INICIO - BG
  DELETE  resultado WHERE tcode = '' .
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 05  FIM - BG
*
*************************************************************
* Agrupamento
*************************************************************
  LOOP AT resultado.
    MOVE-CORRESPONDING resultado TO resultado_tot.
    COLLECT resultado_tot.
  ENDLOOP.

ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  zf_exec_KOB1.
*&---------------------------------------------------------------------*
FORM zf_exec_kob1 USING i_dt_inicial TYPE sy-datum
                        i_dt_final TYPE sy-datum
                        i_sem_popup TYPE c.

  DATA: BEGIN OF tl_listout OCCURS 0,
          line(1024) TYPE c,
        END OF tl_listout.

  DATA: tl_seltab    TYPE TABLE OF rsparams,
        el_seltab_wa LIKE LINE OF tl_seltab.

  DATA: vl_col0  TYPE c.
  DATA: vl_htype LIKE dd01v-datatype.

  DATA: vl_dtfim  TYPE sy-datum.

  DATA: vl_variant TYPE disvariant-variant.

  DATA: tl_bdcdata TYPE TABLE OF bdcdata,
        el_bdcdata LIKE LINE OF tl_bdcdata.

  FIELD-SYMBOLS: <fs_tab>  TYPE ANY TABLE,
                 <fs_line> TYPE any.
  FIELD-SYMBOLS: <fs_value> TYPE any.

  DATA: lf_ref  TYPE REF TO data,
        lf_ref1 TYPE REF TO data.

  DATA el_opt TYPE ctu_params.

  DATA c_kokrs LIKE tka01-kokrs.

  REFRESH gt_kob1.
  IF i_sem_popup IS INITIAL.
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLSPO4'.
    el_bdcdata-dynpro = '0300'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.

    CLEAR el_bdcdata.
    READ TABLE gt_imak INTO DATA(w_imak) INDEX 1.
    el_bdcdata-fnam = 'SVALD-VALUE(01)'.
    el_bdcdata-fval = w_imak-vkokrs.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.

    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=FURT'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDIF.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RKAEP000'.
  el_bdcdata-dynpro = '0110'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_CURSOR'.
  el_bdcdata-fval = 'AUFNR-LOW'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=%005'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'SAPLALDB'.
  el_bdcdata-dynpro = '3000'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=NONE'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  LOOP AT gt_imakz INTO DATA(w_imakz).
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLALDB'.
    el_bdcdata-dynpro = '3000'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_CURSOR'.
    el_bdcdata-fval = 'RSCSEL_255-SLOW_I(01)'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'RSCSEL_255-SLOW_I(01)'.
    el_bdcdata-fval = w_imakz-objnr+2(12).
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=LINS'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDLOOP.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ACPT'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RKAEP000'.
  el_bdcdata-dynpro = '0110'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  READ TABLE gt_imak INTO w_imak INDEX 1.
  el_bdcdata-fnam = 'P_KOKRS'.
  el_bdcdata-fval = w_imak-vkokrs.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'R_BUDAT-LOW'.
  el_bdcdata-fval = i_dt_inicial+6(2) && '.' && i_dt_inicial+4(2) && '.' && i_dt_inicial(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'R_BUDAT-HIGH'.
  el_bdcdata-fval = i_dt_final+6(2) && '.' && i_dt_final+4(2) && '.' && i_dt_final(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ONLI'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_opt-dismode = 'N'.

  cl_salv_bs_runtime_info=>set(
  EXPORTING display = abap_false
  metadata = abap_true
  data = abap_true ).

  CLEAR c_kokrs.
  SET PARAMETER ID 'CAC' FIELD c_kokrs.

  CALL TRANSACTION 'KOB1' USING tl_bdcdata OPTIONS FROM el_opt.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data = lf_ref ).

      ASSIGN lf_ref->* TO <fs_tab>.

    CATCH cx_salv_bs_sc_runtime_info.


  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.

    CREATE DATA lf_ref1 LIKE LINE OF <fs_tab>.

    ASSIGN lf_ref1->* TO <fs_line>.
*
    REFRESH gt_kob1.

    LOOP AT <fs_tab> ASSIGNING <fs_line>.

      ASSIGN COMPONENT 'WERKS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_werks = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_kstar = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OBJNR_N1' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_objnr_n1	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BEKNZ' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_beknz   = <fs_value>.
        CALL FUNCTION 'CONVERSION_EXIT_BEKNZ_OUTPUT'
          EXPORTING
            input  = gs_kob1-k1_beknz
          IMPORTING
            output = gs_kob1-k1_beknz.
      ENDIF.
      ASSIGN COMPONENT 'GKONT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_gkont   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'CPUDT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_cpudt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_budat   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BLDAT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_bldat   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WSDAT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wsdat   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'CEL_KTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_cel_ktxt	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'GKONT_LTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_gkont_ltxt = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'SGTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_sgtxt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OBJ_TXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_obj_txt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'CEL_LTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_cel_ltxt	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'GSBER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_gsber   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_ebeln   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BSART' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_bsart   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_bukrs      = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_gjahr     = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'REFGJ' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_refgj   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_ebelp   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BUZEI' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_buzei   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_matnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'KWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_kwaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'TWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_twaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_owaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'RWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_rwaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BELNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_belnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_aufnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'PERIO' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_perio   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MBGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_mbgbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MAT_TXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_mat_txt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'EBTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_ebtxt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BLART' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_blart   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MEINB' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_meinb   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WRVBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wrvbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WTVBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wtvbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WTGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wtgbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WKGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wkgbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WOGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wogbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WRGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob1-k1_wrgbtr	= <fs_value>.
      ENDIF.

      COLLECT gs_kob1 INTO gt_kob1.
    ENDLOOP.
    SORT gt_kob1 BY k1_aufnr.
  ELSE.
    IF i_sem_popup IS INITIAL.
      PERFORM zf_exec_kob1 USING i_dt_inicial
                                 i_dt_final
                                 'X'.

    ENDIF.
  ENDIF.
ENDFORM.                    "zf_exec_KOB1

*&---------------------------------------------------------------------*
*&      Form  zf_exec_KOB2.
*&---------------------------------------------------------------------*
FORM zf_exec_kob2 USING i_dt_inicial TYPE sy-datum
                        i_dt_final   TYPE sy-datum
                        i_sem_popup  TYPE c.

  DATA: BEGIN OF tl_listout OCCURS 0,
          line(1024) TYPE c,
        END OF tl_listout.

  DATA: tl_seltab    TYPE TABLE OF rsparams,
        el_seltab_wa LIKE LINE OF tl_seltab.

  DATA: vl_col0  TYPE c.
  DATA: vl_htype LIKE dd01v-datatype.

  DATA: vl_dtfim  TYPE sy-datum.

  DATA: vl_variant TYPE disvariant-variant.

  DATA: tl_bdcdata TYPE TABLE OF bdcdata,
        el_bdcdata LIKE LINE OF tl_bdcdata.

  FIELD-SYMBOLS: <fs_tab>  TYPE ANY TABLE,
                 <fs_line> TYPE any.
  FIELD-SYMBOLS: <fs_value> TYPE any.

  DATA: lf_ref  TYPE REF TO data,
        lf_ref1 TYPE REF TO data.

  DATA el_opt TYPE ctu_params.

  DATA c_kokrs LIKE tka01-kokrs.

  REFRESH gt_kob2.
  IF i_sem_popup IS INITIAL.
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLSPO4'.
    el_bdcdata-dynpro = '0300'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.

    CLEAR el_bdcdata.
    READ TABLE gt_imak INTO DATA(w_imak) INDEX 1.
    el_bdcdata-fnam = 'SVALD-VALUE(01)'.
    el_bdcdata-fval = w_imak-vkokrs.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.

    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=FURT'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDIF.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RKAEP000'.
  el_bdcdata-dynpro = '0310'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_CURSOR'.
  el_bdcdata-fval = 'AUFNR-LOW'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=%005'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'SAPLALDB'.
  el_bdcdata-dynpro = '3000'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=NONE'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  LOOP AT gt_imakz INTO DATA(w_imakz).
    CLEAR el_bdcdata.
    el_bdcdata-program = 'SAPLALDB'.
    el_bdcdata-dynpro = '3000'.
    el_bdcdata-dynbegin = 'X'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_CURSOR'.
    el_bdcdata-fval = 'RSCSEL_255-SLOW_I(01)'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'RSCSEL_255-SLOW_I(01)'.
    el_bdcdata-fval = w_imakz-objnr+2(12).
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
    el_bdcdata-fnam = 'BDC_OKCODE'.
    el_bdcdata-fval = '=LINS'.
    APPEND el_bdcdata TO tl_bdcdata.
    CLEAR el_bdcdata.
  ENDLOOP.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ACPT'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  CLEAR el_bdcdata.
  el_bdcdata-program = 'RKAEP000'.
  el_bdcdata-dynpro = '0310'.
  el_bdcdata-dynbegin = 'X'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'R_OBDAT-LOW'.
  el_bdcdata-fval = i_dt_inicial+6(2) && '.' && i_dt_inicial+4(2) && '.' && i_dt_inicial(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'R_OBDAT-HIGH'.
  el_bdcdata-fval = '31.12.9999'.  " i_dt_final+6(2) && '.' && i_dt_final+4(2) && '.' && i_dt_inicial(4).
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_bdcdata-fnam = 'BDC_OKCODE'.
  el_bdcdata-fval = '=ONLI'.
  APPEND el_bdcdata TO tl_bdcdata.
  CLEAR el_bdcdata.

  el_opt-dismode = 'N'.

  cl_salv_bs_runtime_info=>set(
  EXPORTING display = abap_false
  metadata = abap_true
  data = abap_true ).

  CLEAR c_kokrs.
  SET PARAMETER ID 'CAC' FIELD c_kokrs.

  CALL TRANSACTION 'KOB2' USING tl_bdcdata OPTIONS FROM el_opt.

  TRY.

      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data = lf_ref ).

      ASSIGN lf_ref->* TO <fs_tab>.

    CATCH cx_salv_bs_sc_runtime_info.


  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <fs_tab> IS ASSIGNED.

    CREATE DATA lf_ref1 LIKE LINE OF <fs_tab>.

    ASSIGN lf_ref1->* TO <fs_line>.
*
    REFRESH gt_kob2.


    LOOP AT <fs_tab> ASSIGNING <fs_line>.

      ASSIGN COMPONENT 'BLDAT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_bldat   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'SAKTO' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_sakto   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'CEL_KTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_cel_ktxt	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'SGTXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_sgtxt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OBJ_TXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_obj_txt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_bukrs   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_gjahr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_lifnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'RFPOS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_rfpos   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MATNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_matnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'TWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_twaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_owaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'RWAER' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_rwaer   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'REFBN' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_refbn   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'AUFNR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_aufnr   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'PERIO' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_perio   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ORGWTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_orgwtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ORGWTK' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_orgwtk	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ORGWTO' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_orgwto	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'ORGWTT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_orgwtt	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MEGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_megbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WKURS' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_wkurs   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MAT_TXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_mat_txt   = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'OBJART_TXT' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_objart_txt = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'MEINH' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_meinh   = <fs_value>.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input          = gs_kob2-k2_meinh
            language       = sy-langu
          IMPORTING
*           LONG_TEXT      =
            output         = gs_kob2-k2_meinh
*           SHORT_TEXT     =
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDIF.
      ASSIGN COMPONENT 'WTGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_wtgbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WKGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_wkgbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WOGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_wogbtr	 = <fs_value>.
      ENDIF.
      ASSIGN COMPONENT 'WRGBTR' OF STRUCTURE <fs_line> TO <fs_value>.
      IF sy-subrc = 0.
        gs_kob2-k2_wrgbtr	 = <fs_value>.
      ENDIF.

      APPEND gs_kob2 TO gt_kob2.
    ENDLOOP.
    SORT gt_kob2 BY k2_aufnr.
  ELSE.
    IF  i_sem_popup IS INITIAL.
      PERFORM zf_exec_kob2 USING i_dt_inicial
                                 i_dt_final
                                 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    "zf_exec_KOB2
