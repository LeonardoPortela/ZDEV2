*----------------------------------------------------------------------*
***INCLUDE LZLES_PRICEF01.
*----------------------------------------------------------------------*

FORM f_adjustements_price_pauta TABLES t_xkomv STRUCTURE komv_index
                                       t_vtpa  STRUCTURE vtpa
                                 USING p_vttk  TYPE vttk
                                       p_lips  TYPE lips.


*------------------------------------------------------------------------------------------------------*
* Ajustes Condicoes ZPTA ZIPT ZICC
*------------------------------------------------------------------------------------------------------*

  CONSTANTS: c_zpta TYPE c LENGTH 4 VALUE 'ZPTA',
             c_zipt TYPE c LENGTH 4 VALUE 'ZIPT',
             c_zicc TYPE c LENGTH 4 VALUE 'ZICC'.

  DATA: wl_vtpa    TYPE vtpa,
        wl_tvro    TYPE tvro,
        wl_kna1_lr TYPE kna1,
        wl_lfa1_pc TYPE lfa1,
        wl_lfa1_pv TYPE lfa1,
        wl_vfscair TYPE vfscair,
        wl_vfscar1 TYPE vfscar1.

  DATA: t_a092      TYPE TABLE OF a902    WITH HEADER LINE,
        t_konp_a092 TYPE TABLE OF konp    WITH HEADER LINE,

        t_a908      TYPE TABLE OF a908    WITH HEADER LINE,
        t_konp_a908 TYPE TABLE OF konp    WITH HEADER LINE,

        t_a935      TYPE TABLE OF a935    WITH HEADER LINE,
        t_konp_a935 TYPE TABLE OF konp    WITH HEADER LINE,

        t_vfscair   TYPE TABLE OF vfscair WITH HEADER LINE,
        t_vfscar1   TYPE TABLE OF vfscar1 WITH HEADER LINE.

  DATA: v_peso  TYPE p LENGTH 8 DECIMALS 3,
        v_pauta TYPE p LENGTH 6 DECIMALS 2,
        v_real  TYPE p LENGTH 7 DECIMALS 2.

  CLEAR: wl_vtpa, wl_kna1_lr, wl_lfa1_pc, wl_lfa1_pv, wl_vfscair, wl_tvro, wl_vfscair, wl_vfscar1.

  CLEAR: t_a092[],    t_konp_a092[],
         t_a908[],    t_konp_a908[],
         t_a935[],    t_konp_a935[],
         t_vfscair[],
         t_vfscar1[].

  CHECK p_vttk-tknum IS NOT INITIAL.

  "Selecionar Agente Frete
  SELECT SINGLE *
    FROM lfa1 INTO @DATA(wl_lfa1_agente_frete)
   WHERE lifnr EQ @p_vttk-tdlnr.

  CHECK ( sy-subrc EQ 0 ) AND ( p_vttk-tdlnr IS NOT INITIAL ).

  "Selecionar Itinerario
  SELECT SINGLE *
    FROM tvro INTO wl_tvro
   WHERE route EQ p_vttk-route.

  CHECK ( sy-subrc EQ 0 ) AND ( p_vttk-route IS NOT INITIAL ).

  "Selecionar Item de escala: distância
  SELECT *
    FROM vfscair INTO TABLE t_vfscair
   WHERE scaid    EQ '0000000001'
     AND scaval_r LE wl_tvro-distz. "Distancia

  CLEAR: wl_vfscair.
  IF t_vfscair[] IS NOT INITIAL.
    SORT: t_vfscair BY scaval_r DESCENDING.         "Ordenar para pegar a maior distancia
    READ TABLE t_vfscair INTO wl_vfscair INDEX 1.
    wl_vfscair-slfnr = wl_vfscair-slfnr + 1.        "Incrementar Nº Item Escala
  ENDIF.

  "Selecionar Ponto de Coleta
  READ TABLE t_vtpa INTO wl_vtpa WITH KEY parvw = 'PC'.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
      FROM lfa1 INTO wl_lfa1_pc
     WHERE lifnr EQ wl_vtpa-lifnr.
  ENDIF.

  "Selecionar Local de Entrega
  READ TABLE t_vtpa INTO wl_vtpa WITH KEY parvw = 'LR'.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
      FROM kna1 INTO wl_kna1_lr
     WHERE kunnr EQ wl_vtpa-kunnr.
  ENDIF.

  "Selecionar Proprietario Veiculo
  READ TABLE t_vtpa INTO wl_vtpa WITH KEY parvw = 'PV'.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
      FROM lfa1 INTO wl_lfa1_pv
     WHERE lifnr EQ wl_vtpa-lifnr.
  ENDIF.

  IF ( wl_lfa1_pc-regio IS NOT INITIAL ) AND ( wl_kna1_lr-regio IS NOT INITIAL ).

    SELECT *
      FROM a902 AS a INTO TABLE t_a092
     WHERE kappl  EQ 'F'
       AND kschl  IN ('ZICC','ZIPT')
       AND shtyp  EQ p_vttk-shtyp
       AND aland  EQ 'BR'
       AND regioa EQ wl_lfa1_pc-regio
       AND regioz EQ wl_kna1_lr-regio
       AND kfrst  EQ space     "Liberado
       AND datab  LE sy-datum
       AND datbi  GE sy-datum
       AND EXISTS ( SELECT *
                      FROM konp
                     WHERE knumh    EQ a~knumh
                       AND loevm_ko EQ space ).

    IF t_a092[] IS NOT INITIAL.
      SELECT *
        FROM konp INTO TABLE t_konp_a092
         FOR ALL ENTRIES IN t_a092
       WHERE knumh    EQ t_a092-knumh
         AND loevm_ko EQ space
         AND krech    EQ 'A'.
    ENDIF.

  ENDIF.

*---------------------------------------------------------------------------------------------*
* Determinação ZICC
*---------------------------------------------------------------------------------------------*

  READ TABLE t_a092 WITH KEY kschl = c_zicc.
  IF sy-subrc EQ 0.
    READ TABLE t_konp_a092 WITH KEY knumh = t_a092-knumh.
    IF sy-subrc EQ 0.
      LOOP AT t_xkomv ASSIGNING FIELD-SYMBOL(<fs_xkomv>) WHERE kschl EQ c_zicc.
        <fs_xkomv>-kbetr  = t_konp_a092-kbetr.
      ENDLOOP.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------------------------------*
* Determinação ZPTA
*---------------------------------------------------------------------------------------------*

  CLEAR: t_vfscar1[].

  IF ( wl_vfscair-slfnr IS NOT INITIAL ) AND ( wl_lfa1_pv-regio IS NOT INITIAL ).

    CASE wl_lfa1_agente_frete-regio.
      WHEN 'RO'. "Agente de Frete Rondonia

        CASE wl_lfa1_pv-regio.
          WHEN 'RO'.

            SELECT *
              FROM a908 AS a INTO TABLE t_a908
             WHERE kappl  EQ 'F'
               AND kschl  EQ c_zpta
               AND shtyp  EQ p_vttk-shtyp
               AND aland  EQ 'BR'
               AND regioa EQ wl_lfa1_pv-regio
               AND matnr  EQ p_lips-matnr
               AND datab  LE sy-datum
               AND datbi  GE sy-datum
               AND EXISTS ( SELECT *
                              FROM konp
                             WHERE knumh    EQ a~knumh
                               AND loevm_ko EQ space ).

            IF t_a908[] IS NOT INITIAL.

              SELECT *
                FROM konp INTO TABLE t_konp_a908
                 FOR ALL ENTRIES IN t_a908
               WHERE knumh    EQ t_a908-knumh
                 AND loevm_ko EQ space.

              IF t_konp_a908[] IS NOT INITIAL.
                SELECT *
                  FROM vfscar1 INTO TABLE t_vfscar1
                   FOR ALL ENTRIES IN t_konp_a908
                 WHERE knumh  EQ t_konp_a908-knumh
                   AND slfnr1 EQ wl_vfscair-slfnr.
              ENDIF.

            ENDIF.

          WHEN OTHERS.

            IF wl_lfa1_pc-ktokk EQ 'ZFIC'.

              SELECT *
                FROM a935 AS a INTO TABLE t_a935
               WHERE kappl  EQ 'F'
                 AND kschl  EQ c_zpta
                 AND shtyp  EQ p_vttk-shtyp
                 AND aland  EQ 'BR'
                 AND vstel  EQ wl_lfa1_pc-lifnr+6(4)
                 AND matnr  EQ p_lips-matnr
                 AND datab  LE sy-datum
                 AND datbi  GE sy-datum
                 AND EXISTS ( SELECT *
                                FROM konp
                               WHERE knumh    EQ a~knumh
                                 AND loevm_ko EQ space ).

              IF t_a935[] IS NOT INITIAL.

                SELECT *
                  FROM konp INTO TABLE t_konp_a935
                   FOR ALL ENTRIES IN t_a935
                 WHERE knumh    EQ t_a935-knumh
                   AND loevm_ko EQ space.

                IF t_konp_a935[] IS NOT INITIAL.
                  SELECT *
                    FROM vfscar1 INTO TABLE t_vfscar1
                     FOR ALL ENTRIES IN t_konp_a935
                   WHERE knumh  EQ t_konp_a935-knumh
                     AND slfnr1 EQ wl_vfscair-slfnr.
                ENDIF.

              ENDIF.

            ENDIF.

        ENDCASE.

      WHEN OTHERS. "Agente de Frete de outros estados

        SELECT *
          FROM a908 AS a INTO TABLE t_a908
         WHERE kappl  EQ 'F'
           AND kschl  EQ c_zpta
           AND shtyp  EQ p_vttk-shtyp
           AND aland  EQ 'BR'
           AND regioa EQ wl_lfa1_pc-regio
           AND matnr  EQ p_lips-matnr
           AND datab  LE sy-datum
           AND datbi  GE sy-datum
           AND EXISTS ( SELECT *
                          FROM konp
                         WHERE knumh    EQ a~knumh
                           AND loevm_ko EQ space ).

        IF t_a908[] IS NOT INITIAL.

          SELECT *
            FROM konp INTO TABLE t_konp_a908
             FOR ALL ENTRIES IN t_a908
           WHERE knumh    EQ t_a908-knumh
             AND loevm_ko EQ space.

          IF t_konp_a908[] IS NOT INITIAL.
            SELECT *
              FROM vfscar1 INTO TABLE t_vfscar1
               FOR ALL ENTRIES IN t_konp_a908
             WHERE knumh  EQ t_konp_a908-knumh
               AND slfnr1 EQ wl_vfscair-slfnr.
          ENDIF.

        ENDIF.

    ENDCASE.

  ENDIF.

  READ TABLE t_vfscar1 INTO wl_vfscar1 INDEX 1.

  IF ( t_vfscar1[] IS NOT INITIAL ) AND ( wl_vfscar1-kbetr NE 0 ).

    v_peso   = p_lips-brgew / 1000.
    v_pauta  = wl_vfscar1-kbetr.
    v_real   = ( v_peso * v_pauta ).

    LOOP AT t_xkomv ASSIGNING <fs_xkomv> WHERE kschl EQ c_zpta.
      <fs_xkomv>-kawrt    = p_lips-brgew / 10.
      <fs_xkomv>-kbetr    = wl_vfscar1-kbetr.
      <fs_xkomv>-kwert    = v_real.
      <fs_xkomv>-kwert_k  = v_real.
    ENDLOOP.

  ELSE.

    LOOP AT t_xkomv ASSIGNING <fs_xkomv> WHERE kschl EQ c_zpta.
      <fs_xkomv>-kawrt    = 0.
      <fs_xkomv>-kbetr    = 0.
      <fs_xkomv>-kwert    = 0.
      <fs_xkomv>-kwert_k  = 0.
    ENDLOOP.

  ENDIF.

*---------------------------------------------------------------------------------------------*
* Determinação ZIPT
*---------------------------------------------------------------------------------------------*
  DATA(t_xkomv_aux) = t_xkomv[].

  READ TABLE t_a092 WITH KEY kschl = c_zipt.
  IF sy-subrc EQ 0.
    READ TABLE t_konp_a092 WITH KEY knumh = t_a092-knumh.
    IF sy-subrc EQ 0.
      LOOP AT t_xkomv ASSIGNING <fs_xkomv> WHERE kschl EQ c_zipt.
        "Base da condição busca da ZPTA
        LOOP AT t_xkomv_aux INTO DATA(wl_komv_aux) WHERE kschl EQ c_zpta.
          <fs_xkomv>-kawrt = wl_komv_aux-kwert.
        ENDLOOP.

        <fs_xkomv>-kbetr =  t_konp_a092-kbetr.
        <fs_xkomv>-kwert =  ( ( ( <fs_xkomv>-kbetr / 1000 ) * 100 ) *  <fs_xkomv>-kawrt ) / 100.
      ENDLOOP.
    ENDIF.
  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADJUSTEMENTS_PRICE_ZFRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_KOMV  text
*      -->P_TI_VTPA  text
*      -->P_WA_VTTK  text
*      -->P_WA_LIPS  text
*----------------------------------------------------------------------*
FORM f_adjustements_price_zfre TABLES t_xkomv STRUCTURE komv_index
                                      t_vtpa  STRUCTURE vtpa
                                 USING p_vttk  TYPE vttk
                                       p_lips  TYPE lips.

  DATA: achou_preco TYPE c.

*  PERFORM f_get_zfre_ordem_carregamento TABLES t_xkomv
*                                               t_vtpa
*                                         USING p_vttk
*                                               p_lips
*                                  CHANGING achou_preco.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
  PERFORM f_get_zfre_carga_insumos_saida TABLES t_xkomv
                                                t_vtpa
                                          USING p_vttk
                                                p_lips
                                       CHANGING achou_preco.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <-----

  CHECK achou_preco IS INITIAL.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GET_ZFRE_ORDEM_CARREGAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_XKOMV  text
*      -->P_T_VTPA  text
*      -->P_P_VTTK  text
*      -->P_P_LIPS  text
*      <--P_ACHOU_PRECO  text
*----------------------------------------------------------------------*
*FORM f_get_zfre_ordem_carregamento TABLES t_xkomv STRUCTURE komv_index
*                                          t_vtpa  STRUCTURE vtpa
*                                    USING p_vttk  TYPE vttk
*                                          p_lips  TYPE lips
*                                 CHANGING p_achou_preco.
*
*  CONSTANTS: c_zfre TYPE c LENGTH 4 VALUE 'ZFRE'.
*
*
*  DATA: v_vbeln         TYPE  vbeln,
*        v_placa_cav     TYPE  zplaca,
*        v_vlr_frete_neg TYPE  zvalor_frete.
*
**-----------------------------------------------------------------------------------------------------------------*
**      Checar alteração de preço de Frete para a Ordem Carregamento/Ordem de Venda - Transação ZLES0153
**      Buscar Preço definido na Viagem do CARGUERO
**-----------------------------------------------------------------------------------------------------------------*
*
*  IF ( p_lips-vgbel IS NOT INITIAL ) AND  "Ordem Venda
*     ( p_vttk-text1 IS NOT INITIAL ) AND
*     ( strlen( p_vttk-text1 ) >= 7 ). "Placa Cavalo
*
*    v_vbeln         = p_lips-vgbel.
*    v_placa_cav     = p_vttk-text1(7).
*
*    CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
*      EXPORTING
*        i_vbeln         = v_vbeln
*        i_placa_cav     = v_placa_cav
*        i_id_ordem      = p_vttk-id_ordem
*        i_shtyp         = p_vttk-shtyp
*      IMPORTING
*        e_vlr_frete_neg = v_vlr_frete_neg.
*
*    CHECK v_vlr_frete_neg > 0.
*
*    LOOP AT t_xkomv ASSIGNING FIELD-SYMBOL(<fs_xkomv>) WHERE kschl EQ c_zfre.
*      <fs_xkomv>-kbetr  = v_vlr_frete_neg.
*    ENDLOOP.
*
*    p_achou_preco = abap_true.
*
*  ELSEIF p_vttk-id_ordem IS NOT INITIAL AND
*         strlen( p_vttk-text1 ) >= 7.
*
*    v_placa_cav     = p_vttk-text1(7).
*
*    CALL FUNCTION 'ZLES_VALOR_FRETE_ORDEM_CAR'
*      EXPORTING
*        i_id_ordem      = p_vttk-id_ordem
*        i_placa_cav     = v_placa_cav
*        i_shtyp         = p_vttk-shtyp
*      IMPORTING
*        e_vlr_frete_neg = v_vlr_frete_neg.
*
*    CHECK v_vlr_frete_neg > 0.
*
*    LOOP AT t_xkomv ASSIGNING <fs_xkomv> WHERE kschl EQ c_zfre.
*      <fs_xkomv>-kbetr  = v_vlr_frete_neg.
*    ENDLOOP.
*
*    p_achou_preco = abap_true.
*
*  ENDIF.
*
*ENDFORM.

FORM f_get_zfre_carga_insumos_saida TABLES t_xkomv STRUCTURE komv_index
                                           t_vtpa  STRUCTURE vtpa
                                     USING p_vttk  TYPE vttk
                                           p_lips  TYPE lips
                                  CHANGING p_achou_preco.

  CONSTANTS: c_zfre TYPE c LENGTH 4 VALUE 'ZFRE'.


  CLEAR: p_achou_preco.

  CHECK p_vttk-vsart = '01'. "Rodoviario.

  READ TABLE t_xkomv ASSIGNING FIELD-SYMBOL(<fs_xkomv>) WITH KEY kschl = c_zfre.
  CHECK sy-subrc EQ 0.

  SELECT SINGLE id_interface, nro_cg
    FROM zsdt0001 INTO @DATA(lwa_zsdt0001)
    WHERE doc_rem EQ @p_lips-vbeln.

  CHECK sy-subrc EQ 0 AND lwa_zsdt0001-id_interface = '48' AND lwa_zsdt0001-nro_cg IS NOT INITIAL.

  zcl_carga_saida_insumos=>busca_dados_carga(
    EXPORTING
      i_nr_carga_single        = CONV #( lwa_zsdt0001-nro_cg )
    IMPORTING
      e_romaneios              = DATA(lit_romaneios) ).

  READ TABLE lit_romaneios INTO DATA(lwa_romaneio) WITH KEY  doc_rem = p_lips-vbeln.
  CHECK sy-subrc EQ 0.

  IF lwa_romaneio-itens[] IS NOT INITIAL.
    READ TABLE lwa_romaneio-itens INTO DATA(lwa_romaneio_item) WITH KEY matnr = p_lips-matnr
                                                                        charg = p_lips-charg.
    IF sy-subrc EQ 0 AND lwa_romaneio_item-preco_zfre IS NOT INITIAL.
      <fs_xkomv>-kbetr  = lwa_romaneio_item-preco_zfre.
      <fs_xkomv>-kwert  = lwa_romaneio_item-preco_zfre.
      p_achou_preco = abap_true.
    ENDIF.
  ELSEIF lwa_romaneio-preco_zfre IS NOT INITIAL.
    <fs_xkomv>-kbetr  = lwa_romaneio-preco_zfre.
    <fs_xkomv>-kwert  = lwa_romaneio-preco_zfre.
    p_achou_preco = abap_true.
  ENDIF.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADJUSTEMENTS_PRICE_ZISR_ZIRF
*&---------------------------------------------------------------------*
FORM f_adjustements_price_zisr_zirf  TABLES t_xkomv STRUCTURE komv_index
                                            t_vtpa  STRUCTURE vtpa
                                      USING p_vttk  TYPE vttk
                                            p_lips  TYPE lips.

  CONSTANTS: c_zisr TYPE c LENGTH 4 VALUE 'ZISR',
             c_zirf TYPE c LENGTH 4 VALUE 'ZIRF',
             c_zbir TYPE c LENGTH 4 VALUE 'ZBIR'.

  DATA: lva_qsatz TYPE t059z-qsatz,
        lwa_vtpa  TYPE vtpa,
        lva_lifnr TYPE lfbw-lifnr.

  CHECK p_vttk-add02 = '0000000004'.

  DATA(t_xkomv_aux) = t_xkomv[].

  READ TABLE t_vtpa INTO lwa_vtpa WITH KEY parvw = 'PV'.

  IF sy-subrc EQ 0.

    SELECT SINGLE lifnr
       INTO lva_lifnr
       FROM   lfbw
       WHERE  lifnr  = lwa_vtpa-lifnr
         AND  witht = 'IW'
         AND  wt_withcd = 'R5'.

    IF lva_lifnr IS NOT INITIAL.

      SELECT SINGLE qsatz
          INTO lva_qsatz
        FROM t059z
          WHERE land1 = 'BR'
            AND witht = 'IW'
            AND wt_withcd  = 'R5'.

      "Base da condição busca da ZBIR
      READ TABLE t_xkomv_aux INTO DATA(wl_komv_aux) WITH KEY kschl = c_zbir.

      IF wl_komv_aux IS NOT INITIAL.
        LOOP AT t_xkomv ASSIGNING FIELD-SYMBOL(<fs_xkomv>) WHERE kschl EQ c_zisr.
          <fs_xkomv>-kbetr    = lva_qsatz * 10.
          <fs_xkomv>-kwert =  ( wl_komv_aux-kwert * lva_qsatz ) / 100.
        ENDLOOP.

        LOOP AT t_xkomv ASSIGNING <fs_xkomv> WHERE kschl EQ c_zirf.
          <fs_xkomv>-kawrt =  ( wl_komv_aux-kwert ).
          <fs_xkomv>-kwert =  ( <fs_xkomv>-kawrt * lva_qsatz ) / 100.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.


*FORM f_adjustements_price_zbir TABLES t_xkomv STRUCTURE komv_index
*                                      t_vtpa  STRUCTURE vtpa
*                                USING p_vttk  TYPE vttk
*                                      p_lips  TYPE lips.

*  DATA: lv_kbetr_aux         TYPE konv-kbetr,
*        lv_kawrt_zbir        TYPE konv-kawrt,
*        lv_kawrt_zisr_zirf   TYPE konv-kawrt.
*
*  CLEAR: lv_kawrt_zbir.
*
*  READ TABLE t_vtpa INTO DATA(lwa_vtpa) WITH KEY parvw = 'PV'.
*  CHECK SY-SUBRC EQ 0.
*
*  select single *
*    from lfa1 into @data(lwa_lfa1_pv)
*   where lifnr eq @lwa_vtpa-lifnr.
*
*  CHECK SY-SUBRC EQ 0 AND lwa_lfa1_pv-stkzn IS INITIAL.
*
*  LOOP AT t_xkomv ASSIGNING FIELD-SYMBOL(<fs_konv_aux>) WHERE KSCHL EQ 'ZBH1' OR
*                                                              KSCHL EQ 'ZHI1'.
*    ADD <fs_konv_aux>-kwert TO lv_kawrt_zbir.
*  ENDLOOP.
*
*  CHECK lv_kawrt_zbir IS NOT INITIAL.
*
*  lv_kawrt_zisr_zirf = lv_kawrt_zbir / 10.
*
*  LOOP AT t_xkomv ASSIGNING FIELD-SYMBOL(<fs_konv>) WHERE KSCHL EQ 'ZBIR' OR
*                                                          KSCHL EQ 'ZISR'.
*
*    CASE <fs_konv>-kschl.
*      WHEN 'ZBIR'.
*        <fs_konv>-kawrt = lv_kawrt_zbir.
*        lv_kbetr_aux    = <fs_konv>-kbetr / 10.
*        <fs_konv>-kwert = ( lv_kbetr_aux * lv_kawrt_zbir ) / 100.
*      WHEN 'ZISR'.
*        lv_kbetr_aux    = ABS( <fs_konv>-kbetr / 10 ).
*        <fs_konv>-kwert = ( lv_kbetr_aux * lv_kawrt_zisr_zirf ) / 100.
*
*        READ TABLE t_xkomv ASSIGNING FIELD-SYMBOL(<fs_konv_zirf>) WITH KEY KSCHL = 'ZIRF'.
*        IF SY-SUBRC EQ 0.
*          <fs_konv_zirf>-kawrt = lv_kawrt_zisr_zirf.
*          <fs_konv_zirf>-kwert = <fs_konv>-kwert.
*        ENDIF.
*    ENDCASE.
*
*  ENDLOOP.

*ENDFORM.
