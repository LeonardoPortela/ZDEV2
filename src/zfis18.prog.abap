*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 29/08/2010                                              &*
*& Descrição: Variação Cambial Caixa                                  &*
*& Transação: ZFIS22                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK918386   30.08.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  zfis18.
TABLES: bsis, j_1bnfdoc.
TYPE-POOLS: slis, kkblo.
*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_bsis,
        bukrs TYPE bsis-bukrs,
        hkont TYPE bsis-hkont,
        gjahr TYPE bsis-gjahr,
        budat TYPE bsis-budat,
        belnr TYPE bsis-belnr,
        shkzg TYPE bsis-shkzg,
        dmbtr TYPE bsis-dmbtr,
        zuonr TYPE bsis-zuonr,
        gjahr2 TYPE bsis-gjahr,
        stblg TYPE bkpf-stblg,
        tcode TYPE bkpf-tcode,
       END OF ty_bsis,

       BEGIN OF ty_bsis2,
        bukrs TYPE bsis-bukrs,
        hkont TYPE bsis-hkont,
        gjahr TYPE bsis-gjahr,
        budat TYPE bsis-budat,
        belnr TYPE bsis-belnr,
        shkzg TYPE bsis-shkzg,
        dmbtr TYPE bsis-dmbtr,
        dmbe2 TYPE bsis-dmbe2,
        wrbtr TYPE bsis-wrbtr,
        zuonr TYPE bsis-zuonr,
        waers TYPE bsis-waers,
        gjahr2 TYPE bsis-gjahr,
        stblg TYPE bkpf-stblg,
        tcode TYPE bkpf-tcode,
       END OF ty_bsis2,

       BEGIN OF ty_bsis_banc,
        bukrs TYPE bsis-bukrs,
        hkont TYPE bsis-hkont,
        gjahr TYPE bsis-gjahr,
        budat TYPE bsis-budat,
        bldat TYPE bsis-bldat,
        belnr TYPE bsis-belnr,
        shkzg TYPE bsis-shkzg,
        dmbtr TYPE bsis-dmbtr,
        dmbe2 TYPE bsis-dmbe2,
        wrbtr TYPE bsis-wrbtr,
        waers TYPE bsis-waers,
        zuonr TYPE bsis-zuonr,
       END OF ty_bsis_banc,

       BEGIN OF ty_bsas_banc,
        bukrs TYPE bsas-bukrs,
        hkont TYPE bsas-hkont,
        gjahr TYPE bsas-gjahr,
        budat TYPE bsas-budat,
        bldat TYPE bsas-bldat,
        belnr TYPE bsas-belnr,
        shkzg TYPE bsas-shkzg,
        dmbtr TYPE bsas-dmbtr,
        dmbe2 TYPE bsas-dmbe2,
        wrbtr TYPE bsas-wrbtr,
        waers TYPE bsas-waers,
        zuonr TYPE bsas-zuonr,
       END OF ty_bsas_banc,

       BEGIN OF ty_total,
         hkont TYPE bsis-hkont,
         belnr TYPE bsis-belnr,
*         GJAHR TYPE BSIS-GJAHR,
         shkzg TYPE bsis-shkzg,
         dmbtr TYPE bsis-dmbtr,
       END OF ty_total,

       BEGIN OF ty_bsak,
        bukrs TYPE bsak-bukrs,
        belnr TYPE bsak-belnr,
        gjahr TYPE bsak-gjahr,
        lifnr TYPE bsak-lifnr,
        xblnr TYPE bsak-xblnr,
        augdt TYPE bsak-augdt,
        budat TYPE bsak-budat,
        bldat TYPE bsak-bldat,
        waers TYPE bsak-waers,
        blart TYPE bsak-blart,
        shkzg TYPE bsak-shkzg,
        dmbtr TYPE bsak-dmbtr,
        hkont TYPE bsak-hkont,
        dmbe2 TYPE bsak-dmbe2,
        wrbtr TYPE bsak-wrbtr,
        augbl TYPE bsak-augbl,
        zuonr TYPE bsak-zuonr,
        umsks TYPE bsak-umsks,
        umskz TYPE bsak-umskz,
       END OF ty_bsak,

      BEGIN OF ty_bsik,
        bukrs TYPE bsik-bukrs,
        belnr TYPE bsik-belnr,
        gjahr TYPE bsik-gjahr,
        lifnr TYPE bsik-lifnr,
        xblnr TYPE bsik-xblnr,
        augdt TYPE bsik-augdt,
        budat TYPE bsik-budat,
        bldat TYPE bsik-bldat,
        waers TYPE bsik-waers,
        blart TYPE bsik-blart,
        shkzg TYPE bsik-shkzg,
        dmbtr TYPE bsik-dmbtr,
        hkont TYPE bsik-hkont,
        dmbe2 TYPE bsik-dmbe2,
        wrbtr TYPE bsik-wrbtr,
        augbl TYPE bsik-augbl,
        zuonr TYPE bsik-zuonr,
       END OF ty_bsik,

       BEGIN OF ty_bsad,
        bukrs TYPE bsad-bukrs,
        belnr TYPE bsad-belnr,
        gjahr TYPE bsad-gjahr,
        kunnr TYPE bsad-kunnr,
        xblnr TYPE bsad-xblnr,
        augdt TYPE bsad-augdt,
        budat TYPE bsad-budat,
        bldat TYPE bsad-bldat,
        waers TYPE bsad-waers,
        blart TYPE bsad-blart,
        shkzg TYPE bsad-shkzg,
        dmbtr TYPE bsad-dmbtr,
        hkont TYPE bsad-hkont,
        dmbe2 TYPE bsad-dmbe2,
        wrbtr TYPE bsad-wrbtr,
        augbl TYPE bsad-augbl,
        zuonr TYPE bsad-zuonr,
        umsks TYPE bsak-umsks,
        umskz TYPE bsak-umskz,
       END OF ty_bsad,

       BEGIN OF ty_bsid,
        bukrs TYPE bsid-bukrs,
        belnr TYPE bsid-belnr,
        gjahr TYPE bsid-gjahr,
        kunnr TYPE bsid-kunnr,
        xblnr TYPE bsid-xblnr,
        augdt TYPE bsid-augdt,
        budat TYPE bsid-budat,
        bldat TYPE bsid-bldat,
        waers TYPE bsid-waers,
        blart TYPE bsid-blart,
        shkzg TYPE bsid-shkzg,
        dmbtr TYPE bsid-dmbtr,
        hkont TYPE bsid-hkont,
        dmbe2 TYPE bsid-dmbe2,
        wrbtr TYPE bsid-wrbtr,
        augbl TYPE bsid-augbl,
        zuonr TYPE bsid-zuonr,
       END OF ty_bsid,

       BEGIN OF ty_skb1,
         saknr TYPE skb1-saknr,
       END OF ty_skb1,

       BEGIN OF ty_ska1,
         saknr TYPE ska1-saknr,
         ktoks TYPE ska1-ktoks,
       END OF ty_ska1,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         kursf TYPE bkpf-kursf,
         stblg TYPE bkpf-stblg,
         tcode TYPE bkpf-tcode,
       END OF ty_bkpf,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_saida,
        bukrs TYPE bsad-bukrs,
        tipo(30),
        parid TYPE j_1bnfdoc-parid,
        nome(50),
        hkont TYPE bsad-hkont,
        belnr TYPE bsad-belnr,
        gjahr TYPE bsad-gjahr,
        xblnr TYPE bsad-xblnr,
        budat TYPE bsad-budat,
        bldat TYPE bsad-bldat,
        dmbtr TYPE bsad-dmbtr,
        dmbe2 TYPE bsad-dmbe2,
        taxa  TYPE bkpf-kursf,
        waers TYPE bsad-waers,
        kursf TYPE bkpf-kursf,
        augbl TYPE bsad-augbl,
        gjahr2 TYPE bsis-gjahr,
        augdt TYPE bsad-augdt,
        ctavar TYPE bsis-hkont,
        vargrp TYPE bsis-dmbtr,
        vlvar  TYPE bsis-dmbtr,
        blart TYPE bsad-blart,
*        shkzg TYPE bsad-shkzg,
        rec_var TYPE bsad-dmbtr,
        zuonr TYPE bsad-zuonr,
        colinfo TYPE kkblo_t_specialcol,
       END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
INCLUDE TYPE slis_fieldcat_main.
INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*----------------------------------------------------------------------*
* ranges
*----------------------------------------------------------------------*
RANGES: rg_hkont   FOR bsad-hkont,
        rg_hkont_r FOR bsad-hkont,
        rg_saknr   FOR skb1-saknr.
*----------------------------------------------------------------------*
* Tabelas e Workareas
*----------------------------------------------------------------------*
DATA: tg_bsis     TYPE TABLE OF ty_bsis,
      tg_bsis2    TYPE TABLE OF ty_bsis2,
      wg_bsis2    TYPE ty_bsis2,
      tg_bsis_aux TYPE TABLE OF ty_bsis,
      wg_bsis TYPE ty_bsis,
      tg_bsis_banc TYPE TABLE OF ty_bsis_banc,
      wg_bsis_banc TYPE ty_bsis_banc,
      tg_bsas_banc TYPE TABLE OF ty_bsas_banc,
      wg_bsas_banc TYPE ty_bsas_banc,
      tg_bsis_raza TYPE TABLE OF ty_bsis_banc,
      wg_bsis_raza TYPE ty_bsis_banc,
      tg_bsas_raza TYPE TABLE OF ty_bsas_banc,
      wg_bsas_raza TYPE ty_bsas_banc,
      tg_skb1     TYPE TABLE OF ty_skb1,
      wg_skb1     TYPE ty_skb1,
      tg_ska1     TYPE TABLE OF ty_ska1,
      wg_ska1     TYPE ty_ska1,
*      TG_HKONT TYPE TABLE OF TY_HKONT,
*      WG_HKONT TYPE TY_HKONT,
      tg_bsak TYPE TABLE OF ty_bsak,
      tg_bsak2 TYPE TABLE OF ty_bsak,
      tg_bsak_est TYPE TABLE OF ty_bsak,
      wg_bsak TYPE ty_bsak,
      tg_bsik TYPE TABLE OF ty_bsik,
      wg_bsik TYPE ty_bsik,
      tg_bsad TYPE TABLE OF ty_bsad,
      tg_bsad_est TYPE TABLE OF ty_bsad,
      tg_bsad2 TYPE TABLE OF ty_bsad,
      wg_bsad TYPE ty_bsad,
      tg_bsid     TYPE TABLE OF ty_bsid,
      wg_bsid     TYPE ty_bsid,
      tg_bkpf     TYPE TABLE OF ty_bkpf,
      tg_bkpf_est TYPE TABLE OF ty_bkpf,
      wg_bkpf     TYPE ty_bkpf,
      tg_lfa1     TYPE TABLE OF ty_lfa1,
      wg_lfa1     TYPE ty_lfa1,
      tg_kna1     TYPE TABLE OF ty_kna1,
      wg_kna1     TYPE ty_kna1,
      tg_total    TYPE TABLE OF ty_total,
      wg_total    TYPE ty_total,
      tg_setleaf  TYPE TABLE OF setleaf WITH HEADER LINE,
      tg_saida    TYPE TABLE OF ty_saida,
      wg_saida    TYPE ty_saida,
      wa_style    TYPE lvc_s_styl,
      style       TYPE lvc_t_styl WITH HEADER LINE.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      wa_colinfo   TYPE kkblo_specialcol,
      wa_layout    TYPE slis_layout_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader.

DATA: variante        LIKE disvariant,
      def_variante    LIKE disvariant,
      vg_variant        TYPE disvariant.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bsis-bukrs OBLIGATORY.

*PARAMETERS: p_lif RADIOBUTTON GROUP g1 DEFAULT 'X',
*            p_kun RADIOBUTTON GROUP g1.

SELECT-OPTIONS: s_hkont FOR bsis-hkont NO INTERVALS,
*                s_parid FOR j_1bnfdoc-parid,
                s_budat FOR bsis-budat OBLIGATORY NO-EXTENSION,
                s_waers FOR bsis-waers OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_varia TYPE disvariant-variant.

SELECTION-SCREEN: END OF BLOCK b2.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_parid-low.
*  PERFORM match_code CHANGING s_parid-low.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_parid-high.
*  PERFORM match_code CHANGING s_parid-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
*  vg_repid          = sy-repid.
  variante-report   = sy-repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
  ENDIF.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.
  PERFORM imprimi_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .

  SELECT *
    FROM setleaf
    INTO TABLE tg_setleaf
     WHERE setname EQ 'VAR_CAIXA'
        OR setname EQ 'VAR_CAIXA2'.

  REFRESH: rg_hkont, rg_hkont_r.
  SORT tg_setleaf BY setname.
  LOOP AT tg_setleaf.
    IF tg_setleaf-setname EQ 'VAR_CAIXA'.
      MOVE: tg_setleaf-valsign   TO rg_hkont-sign,
            tg_setleaf-valoption TO rg_hkont-option,
            tg_setleaf-valfrom   TO rg_hkont-low,
            tg_setleaf-valto     TO rg_hkont-high.
      APPEND rg_hkont.
      CLEAR rg_hkont.

    ELSEIF tg_setleaf-setname EQ 'VAR_CAIXA2'.
      MOVE: tg_setleaf-valsign   TO rg_hkont_r-sign,
            tg_setleaf-valoption TO rg_hkont_r-option,
            tg_setleaf-valfrom   TO rg_hkont_r-low,
            tg_setleaf-valto     TO rg_hkont_r-high.
      APPEND rg_hkont_r.
      CLEAR rg_hkont_r.
    ENDIF.
  ENDLOOP.

  REFRESH: rg_saknr.

  SELECT saknr "#EC CI_DB_OPERATION_OK[2431747]
    FROM skb1
    INTO TABLE tg_skb1
     WHERE bukrs IN s_bukrs
       AND fdlev NE space.                                  "'F0'.

  IF sy-subrc IS INITIAL.
    SELECT saknr ktoks "#EC CI_DB_OPERATION_OK[2389136]
      FROM ska1 "#EC CI_DB_OPERATION_OK[2431747]
      INTO TABLE tg_ska1
       FOR ALL ENTRIES IN tg_skb1
       WHERE ktopl EQ '0050'
         AND saknr EQ tg_skb1-saknr.


  ENDIF.

  SORT tg_ska1 BY saknr ktoks.

  LOOP AT tg_skb1 INTO wg_skb1.
    READ TABLE tg_ska1 INTO wg_ska1
      WITH KEY saknr = wg_skb1-saknr
               ktoks = 'YB04'
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      rg_saknr-sign   = 'I'.
      rg_saknr-option = 'EQ'.
      rg_saknr-low    = wg_skb1-saknr.
      APPEND rg_saknr.
      CLEAR: rg_saknr.
    ENDIF.
  ENDLOOP.

  LOOP AT s_hkont.
    IF s_hkont-low NOT IN rg_hkont.
      MESSAGE e836(sd) WITH 'A conta selecionada, não é uma conta de variação!'.
    ENDIF.
  ENDLOOP.

  IF rg_hkont[] IS NOT INITIAL.
    SELECT bukrs hkont gjahr budat belnr shkzg dmbtr dmbe2 wrbtr zuonr waers
      FROM bsis
      INTO TABLE tg_bsis2
       WHERE hkont IN rg_hkont
         AND bukrs IN s_bukrs
         AND budat IN s_budat
         AND waers IN s_waers.

    SELECT bukrs hkont gjahr budat belnr shkzg dmbtr zuonr
      FROM bsis
      INTO TABLE tg_bsis
       WHERE hkont IN rg_hkont
         AND bukrs IN s_bukrs
         AND budat IN s_budat
         AND waers IN s_waers.

    IF sy-subrc IS INITIAL.
      SELECT bukrs hkont gjahr budat bldat belnr shkzg dmbtr dmbe2 wrbtr waers zuonr
      FROM bsis
      INTO TABLE tg_bsis_banc
       FOR ALL ENTRIES IN tg_bsis
        WHERE belnr EQ tg_bsis-belnr
          AND bukrs EQ tg_bsis-bukrs
          AND gjahr EQ tg_bsis-gjahr
          AND hkont IN rg_saknr.

*      IF tg_bsis_banc[] IS NOT INITIAL.
      SELECT bukrs hkont gjahr budat bldat belnr shkzg dmbtr dmbe2 wrbtr waers zuonr
    FROM bsas
    INTO TABLE tg_bsas_banc
     FOR ALL ENTRIES IN tg_bsis
      WHERE belnr EQ tg_bsis-belnr
        AND bukrs EQ tg_bsis-bukrs
        AND gjahr EQ tg_bsis-gjahr
        AND hkont IN rg_saknr.
*      ENDIF.

      SELECT bukrs hkont gjahr budat bldat belnr shkzg dmbtr dmbe2 wrbtr waers zuonr
      FROM bsis
      INTO TABLE tg_bsis_raza
       FOR ALL ENTRIES IN tg_bsis
        WHERE belnr EQ tg_bsis-belnr
          AND bukrs EQ tg_bsis-bukrs
          AND gjahr EQ tg_bsis-gjahr
          AND hkont IN rg_hkont_r.

*      IF tg_bsis_raza[] IS NOT INITIAL.
      SELECT bukrs hkont gjahr budat bldat belnr shkzg dmbtr dmbe2 wrbtr waers zuonr
        FROM bsas
        INTO TABLE tg_bsas_raza
         FOR ALL ENTRIES IN tg_bsis
          WHERE belnr EQ tg_bsis-belnr
            AND bukrs EQ tg_bsis-bukrs
            AND gjahr EQ tg_bsis-gjahr
            AND hkont IN rg_hkont_r.
*      ENDIF.

      SELECT bukrs belnr gjahr kursf stblg tcode
          FROM bkpf
          INTO TABLE tg_bkpf
           FOR ALL ENTRIES IN tg_bsis
            WHERE bukrs EQ tg_bsis-bukrs
              AND belnr EQ tg_bsis-belnr
              AND gjahr EQ tg_bsis-gjahr.

      LOOP AT tg_bsis INTO wg_bsis.
        READ TABLE tg_bkpf INTO wg_bkpf
          WITH KEY bukrs = wg_bsis-bukrs
                   belnr = wg_bsis-belnr
                   gjahr = wg_bsis-gjahr.

        wg_bsis-gjahr2 = wg_bsis-gjahr - 2.
        wg_bsis-stblg  = wg_bkpf-stblg.
        wg_bsis-tcode  = wg_bkpf-tcode.
        MODIFY tg_bsis FROM wg_bsis.
        CLEAR: wg_bkpf.
      ENDLOOP.

*      IF p_lif IS NOT INITIAL.
      tg_bsis_aux[] = tg_bsis[].
      LOOP AT tg_bsis_aux INTO wg_bsis.
        IF wg_bsis-stblg IS NOT INITIAL.
          IF wg_bsis-tcode EQ 'FB08'.

          ELSE.
            DELETE TABLE tg_bsis_aux FROM wg_bsis.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF tg_bsis_aux[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr lifnr xblnr augdt budat bldat
               waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
               umsks umskz
          FROM bsak
          INTO TABLE tg_bsak
           FOR ALL ENTRIES IN tg_bsis_aux
           WHERE augbl EQ tg_bsis_aux-belnr
             AND bukrs EQ tg_bsis_aux-bukrs
             AND gjahr GE tg_bsis_aux-gjahr2
             AND gjahr LE tg_bsis_aux-gjahr
*               AND lifnr IN s_parid
             AND blart NE 'VC'.

**       Salvo residual
        SELECT bukrs belnr gjahr lifnr xblnr augdt budat bldat
              waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
              umsks umskz
         FROM bsak
         INTO TABLE tg_bsak2
          FOR ALL ENTRIES IN tg_bsis_aux
          WHERE belnr EQ tg_bsis_aux-belnr
            AND bukrs EQ tg_bsis_aux-bukrs
            AND gjahr EQ tg_bsis_aux-gjahr
*              AND lifnr IN s_parid
*            AND augdt NOT IN s_budat
            AND blart NE 'VC'.
      ENDIF.
      tg_bsis_aux[] = tg_bsis[].
*        DELETE TG_BSIS_AUX WHERE STBLG EQ SPACE
*                              OR TCODE EQ 'FB08'.
      LOOP AT tg_bsis_aux INTO wg_bsis.
        IF wg_bsis-stblg IS NOT INITIAL.
          IF wg_bsis-tcode NE 'FB08'.

          ELSE.
            DELETE TABLE tg_bsis_aux FROM wg_bsis.
          ENDIF.
        ELSE.
          DELETE TABLE tg_bsis_aux FROM wg_bsis.
        ENDIF.
      ENDLOOP.
      IF tg_bsis_aux[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr lifnr xblnr augdt budat bldat
               waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
               umsks umskz
          FROM bsak
          INTO TABLE tg_bsak_est
           FOR ALL ENTRIES IN tg_bsis_aux
            WHERE belnr EQ tg_bsis_aux-belnr
              AND bukrs EQ tg_bsis_aux-bukrs
              AND gjahr GE tg_bsis_aux-gjahr2
              AND gjahr LE tg_bsis_aux-gjahr
*                AND lifnr IN s_parid
              AND blart NE 'VC'.

      ENDIF.

      SELECT bukrs belnr gjahr lifnr xblnr augdt budat bldat
            waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
       FROM bsik
       INTO TABLE tg_bsik
        FOR ALL ENTRIES IN tg_bsis
        WHERE belnr EQ tg_bsis-belnr
          AND bukrs EQ tg_bsis-bukrs
          AND gjahr EQ tg_bsis-gjahr
*            AND lifnr IN s_parid
          AND blart NE 'VC'.

      IF tg_bsak[] IS NOT INITIAL.
        SELECT lifnr name1
          FROM lfa1
          INTO TABLE tg_lfa1
           FOR ALL ENTRIES IN tg_bsak
           WHERE lifnr EQ tg_bsak-lifnr.
      ENDIF.

      IF tg_bsak_est[] IS NOT INITIAL.
        SELECT lifnr name1
          FROM lfa1
          APPENDING TABLE tg_lfa1
           FOR ALL ENTRIES IN tg_bsak_est
           WHERE lifnr EQ tg_bsak_est-lifnr.
      ENDIF.

      IF tg_bsak2[] IS NOT INITIAL.
        SELECT lifnr name1
          FROM lfa1
          APPENDING TABLE tg_lfa1
           FOR ALL ENTRIES IN tg_bsak2
           WHERE lifnr EQ tg_bsak2-lifnr.
      ENDIF.

      IF tg_bsik[] IS NOT INITIAL.
        SELECT lifnr name1
          FROM lfa1
          APPENDING TABLE tg_lfa1
           FOR ALL ENTRIES IN tg_bsik
           WHERE lifnr EQ tg_bsik-lifnr.
      ENDIF.
*      ENDIF.
*      ELSE.
      tg_bsis_aux[] = tg_bsis[].
      LOOP AT tg_bsis_aux INTO wg_bsis.
        IF wg_bsis-stblg IS NOT INITIAL.
          IF wg_bsis-tcode EQ 'FB08'.

          ELSE.
            DELETE TABLE tg_bsis_aux FROM wg_bsis.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF tg_bsis_aux[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr kunnr xblnr augdt budat bldat
            waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
            umsks umskz
       FROM bsad
       INTO TABLE tg_bsad
        FOR ALL ENTRIES IN tg_bsis_aux
        WHERE augbl EQ tg_bsis_aux-belnr
          AND bukrs EQ tg_bsis_aux-bukrs
          AND gjahr GE tg_bsis_aux-gjahr2
          AND gjahr LE tg_bsis_aux-gjahr
*            AND kunnr IN s_parid
          AND blart NE 'VC'.

***       Saldo Residual
        SELECT bukrs belnr gjahr kunnr xblnr augdt budat bldat
               waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
               umsks umskz
          FROM bsad
          INTO TABLE tg_bsad2
           FOR ALL ENTRIES IN tg_bsis_aux
           WHERE belnr EQ tg_bsis_aux-belnr
             AND bukrs EQ tg_bsis_aux-bukrs
             AND gjahr EQ tg_bsis_aux-gjahr
*               AND kunnr IN s_parid
*             AND augdt NOT IN s_budat
             AND blart NE 'VC'.
      ENDIF.
      tg_bsis_aux[] = tg_bsis[].
      LOOP AT tg_bsis_aux INTO wg_bsis.
        IF wg_bsis-stblg IS NOT INITIAL.
          IF wg_bsis-tcode NE 'FB08'.

          ELSE.
            DELETE TABLE tg_bsis_aux FROM wg_bsis.
          ENDIF.
        ELSE.
          DELETE TABLE tg_bsis_aux FROM wg_bsis.
        ENDIF.
      ENDLOOP.
      IF tg_bsis_aux[] IS NOT INITIAL.
        SELECT bukrs belnr gjahr kunnr xblnr augdt budat bldat
            waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
            umsks umskz
       FROM bsad
       INTO TABLE tg_bsad_est
        FOR ALL ENTRIES IN tg_bsis_aux
        WHERE belnr EQ tg_bsis_aux-belnr
          AND bukrs EQ tg_bsis_aux-bukrs
          AND gjahr GE tg_bsis_aux-gjahr2
          AND gjahr LE tg_bsis_aux-gjahr
*            AND kunnr IN s_parid
          AND blart NE 'VC'.

      ENDIF.

      SELECT bukrs belnr gjahr kunnr xblnr augdt budat bldat
          waers blart shkzg dmbtr hkont dmbe2 wrbtr augbl zuonr
     FROM bsid
     INTO TABLE tg_bsid
      FOR ALL ENTRIES IN tg_bsis
      WHERE belnr EQ tg_bsis-belnr
        AND bukrs EQ tg_bsis-bukrs
        AND gjahr EQ tg_bsis-gjahr
*          AND kunnr IN s_parid
        AND blart NE 'VC'.

*        SELECT BUKRS BELNR GJAHR KURSF STBLG
*        FROM BKPF
*        INTO TABLE TG_BKPF
*         FOR ALL ENTRIES IN TG_BSIS
*          WHERE BUKRS EQ TG_BSIS-BUKRS
*            AND BELNR EQ TG_BSIS-BELNR
*            AND GJAHR EQ TG_BSIS-GJAHR.

      IF tg_bsad[] IS NOT INITIAL.
        SELECT kunnr name1
          FROM kna1
          INTO TABLE tg_kna1
           FOR ALL ENTRIES IN tg_bsad
            WHERE kunnr EQ tg_bsad-kunnr.
      ENDIF.

      IF tg_bsad_est[] IS NOT INITIAL.
        SELECT kunnr name1
          FROM kna1
          APPENDING TABLE tg_kna1
           FOR ALL ENTRIES IN tg_bsad_est
            WHERE kunnr EQ tg_bsad_est-kunnr.
      ENDIF.

      IF tg_bsad2[] IS NOT INITIAL.
        SELECT kunnr name1
          FROM kna1
          APPENDING TABLE tg_kna1
           FOR ALL ENTRIES IN tg_bsad2
            WHERE kunnr EQ tg_bsad2-kunnr.
      ENDIF.

      IF tg_bsid[] IS NOT INITIAL.
        SELECT kunnr name1
          FROM kna1
          APPENDING TABLE tg_kna1
           FOR ALL ENTRIES IN tg_bsid
            WHERE kunnr EQ tg_bsid-kunnr.
      ENDIF.
    ENDIF.

*    ENDIF.
  ENDIF.
ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .

  SORT: tg_bsis BY bukrs belnr gjahr,
        tg_bkpf BY bukrs belnr gjahr,
        tg_lfa1 BY lifnr,
        tg_kna1 BY kunnr.

  LOOP AT tg_bsis INTO wg_bsis.
    MOVE: wg_bsis-hkont TO wg_total-hkont,
          wg_bsis-belnr TO wg_total-belnr,
*          WG_BSIS-GJAHR TO WG_TOTAL-GJAHR,
          wg_bsis-shkzg TO wg_total-shkzg,
          wg_bsis-dmbtr TO wg_total-dmbtr.

    COLLECT wg_total INTO tg_total.
    CLEAR: wg_total, wg_bsis.
  ENDLOOP.
** Regra normal
*  IF p_lif IS NOT INITIAL.
***    Fornecedor
  LOOP AT tg_bsak INTO wg_bsak.
    IF wg_bsak-umsks EQ 'A'
    AND wg_bsak-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsak-bukrs
               belnr = wg_bsak-augbl
               gjahr = wg_bsak-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.

    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsak-bukrs
                 belnr = wg_bsak-augbl.
*                 gjahr = wg_bsak-gjahr
*                 BINARY SEARCH.
    ENDIF.
    READ TABLE tg_lfa1 INTO wg_lfa1
      WITH KEY lifnr = wg_bsak-lifnr
               BINARY SEARCH.

    MOVE: wg_bsak-bukrs TO wg_saida-bukrs,
          'Fornecedor'  TO wg_saida-tipo,
          wg_lfa1-lifnr TO wg_saida-parid,
          wg_lfa1-name1 TO wg_saida-nome,
          wg_bsak-hkont TO wg_saida-hkont,
          wg_bsak-belnr TO wg_saida-belnr,
          wg_bsak-gjahr TO wg_saida-gjahr,
          wg_bsak-xblnr TO wg_saida-xblnr,
          wg_bsak-budat TO wg_saida-budat,
          wg_bsak-bldat TO wg_saida-bldat,
          wg_bsak-dmbtr TO wg_saida-dmbtr,
          wg_bsak-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsak-augbl TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsak-augdt TO wg_saida-augdt,
          wg_bsak-blart TO wg_saida-blart,
          wg_bsak-zuonr TO wg_saida-zuonr.

    IF wg_bsak-waers EQ 'EUR'.
      MOVE wg_bsak-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsak-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsak-shkzg EQ 'H'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsak-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-dmbe2.
        ENDIF.
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    TRY.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.


    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_lfa1, wg_bsis, wg_bkpf.
  ENDLOOP.

*  ELSE.
***   Cliente
  LOOP AT tg_bsad INTO wg_bsad.
    IF wg_bsad-umsks EQ 'A'
    AND wg_bsad-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsad-bukrs
               belnr = wg_bsad-augbl
               gjahr = wg_bsad-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.

    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
         WITH KEY bukrs = wg_bsad-bukrs
                  belnr = wg_bsad-augbl.
*                  gjahr = wg_bsad-gjahr
*                  BINARY SEARCH.
    ENDIF.
    READ TABLE tg_kna1 INTO wg_kna1
      WITH KEY kunnr = wg_bsad-kunnr
               BINARY SEARCH.

    MOVE: wg_bsad-bukrs TO wg_saida-bukrs,
          'Cliente'  TO wg_saida-tipo,
          wg_kna1-kunnr TO wg_saida-parid,
          wg_kna1-name1 TO wg_saida-nome,
          wg_bsad-hkont TO wg_saida-hkont,
          wg_bsad-belnr TO wg_saida-belnr,
          wg_bsad-gjahr TO wg_saida-gjahr,
          wg_bsad-xblnr TO wg_saida-xblnr,
          wg_bsad-budat TO wg_saida-budat,
          wg_bsad-bldat TO wg_saida-bldat,
          wg_bsad-dmbtr TO wg_saida-dmbtr,
*          wg_bsad-dmbe2 TO wg_saida-dmbe2,
          wg_bsad-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsad-augbl TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsad-augdt TO wg_saida-augdt,
          wg_bsad-blart TO wg_saida-blart,
          wg_bsad-zuonr TO wg_saida-zuonr.

    IF wg_bsad-waers EQ 'EUR'.
      MOVE wg_bsad-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsad-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsad-shkzg EQ 'H'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsad-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-dmbe2.
        ENDIF.
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    TRY.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_kna1, wg_bsis, wg_bkpf.
  ENDLOOP.

*  ENDIF.
** Total de variacao cambial e suas contas  Regra normal
  LOOP AT tg_saida INTO wg_saida.
    IF wg_saida-belnr EQ wg_saida-augbl.
      DELETE TABLE tg_saida FROM wg_saida.

    ELSE.
      READ TABLE tg_total INTO wg_total
        WITH KEY belnr = wg_saida-augbl.
*                 GJAHR = WG_SAIDA-GJAHR.
      IF sy-subrc IS INITIAL.
        IF wg_total-shkzg EQ 'H'.

          MOVE: wg_total-hkont TO wg_saida-ctavar,
                wg_total-dmbtr TO wg_saida-vlvar.
          MULTIPLY wg_saida-vlvar BY -1.

          wa_colinfo-color-col = 6.
          wa_colinfo-fieldname = 'CTAVAR'.
          APPEND wa_colinfo TO wg_saida-colinfo.

          wa_colinfo-color-col = 6.
          wa_colinfo-fieldname = 'VLVAR'.
          APPEND wa_colinfo TO wg_saida-colinfo.
        ELSE.
          MOVE: wg_total-hkont TO wg_saida-ctavar,
                wg_total-dmbtr TO wg_saida-vlvar.

          wa_colinfo-color-col = 6.
          wa_colinfo-fieldname = 'CTAVAR'.
          APPEND wa_colinfo TO wg_saida-colinfo.

          wa_colinfo-color-col = 6.
          wa_colinfo-fieldname = 'VLVAR'.
          APPEND wa_colinfo TO wg_saida-colinfo.
        ENDIF.
        SHIFT wg_saida-ctavar LEFT DELETING LEADING '0'.
        MODIFY tg_saida FROM wg_saida.
        CLEAR: wg_saida.
        DELETE TABLE tg_total FROM wg_total.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  IF p_lif IS NOT INITIAL.
**  Doc Estornados.  / Fornecedor
  LOOP AT tg_bsak_est INTO wg_bsak.
    IF wg_bsak-umsks EQ 'A'
    AND wg_bsak-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsak-bukrs
               belnr = wg_bsak-belnr
               gjahr = wg_bsak-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
       WITH KEY bukrs = wg_bsak-bukrs
                belnr = wg_bsak-augbl.
*                 gjahr = wg_bsak-gjahr
*                 BINARY SEARCH.

    ENDIF.
    READ TABLE tg_lfa1 INTO wg_lfa1
      WITH KEY lifnr = wg_bsak-lifnr
               BINARY SEARCH.

    MOVE: wg_bsak-bukrs TO wg_saida-bukrs,
          'Fornecedor'  TO wg_saida-tipo,
          wg_lfa1-lifnr TO wg_saida-parid,
          wg_lfa1-name1 TO wg_saida-nome,
          wg_bsak-hkont TO wg_saida-hkont,
          wg_bsak-belnr TO wg_saida-belnr,
          wg_bsak-gjahr TO wg_saida-gjahr,
          wg_bsak-xblnr TO wg_saida-xblnr,
          wg_bsak-budat TO wg_saida-budat,
          wg_bsak-bldat TO wg_saida-bldat,
          wg_bsak-dmbtr TO wg_saida-dmbtr,
*          wg_bsak-dmbe2 TO wg_saida-dmbe2,
          wg_bsak-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsak-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsak-augdt TO wg_saida-augdt,
          wg_bsak-blart TO wg_saida-blart,
          wg_bsak-zuonr TO wg_saida-zuonr.
*            WG_BSIS-DMBTR TO WG_SAIDA-VLVAR.

    IF wg_bsak-waers EQ 'EUR'.
      MOVE wg_bsak-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsak-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

*      IF wg_bsak-augdt GT s_budat-low
*      AND wg_bsak-augdt GT s_budat-high.
*        IF wg_bsak-shkzg EQ 'H'.
*          MULTIPLY wg_saida-dmbtr BY -1.
*          MULTIPLY wg_saida-dmbe2 BY -1.
*        ENDIF.
*      ELSE.
    IF wg_bsak-shkzg EQ 'S'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.
*      ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsak-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_lfa1, wg_bsis, wg_bkpf.
  ENDLOOP.

** Regra adcional doc sem compensacao  / Fornecedor
  LOOP AT tg_bsik INTO wg_bsik.
    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsik-bukrs
               belnr = wg_bsik-belnr
               gjahr = wg_bsik-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsik-bukrs
                 belnr = wg_bsik-augbl.
*                 gjahr = wg_bsik-gjahr
*                 BINARY SEARCH.
    ENDIF.
    READ TABLE tg_lfa1 INTO wg_lfa1
      WITH KEY lifnr = wg_bsik-lifnr
               BINARY SEARCH.

    MOVE: wg_bsik-bukrs TO wg_saida-bukrs,
          'Fornecedor'  TO wg_saida-tipo,
          wg_lfa1-lifnr TO wg_saida-parid,
          wg_lfa1-name1 TO wg_saida-nome,
          wg_bsik-hkont TO wg_saida-hkont,
          wg_bsik-belnr TO wg_saida-belnr,
          wg_bsik-gjahr TO wg_saida-gjahr,
          wg_bsik-xblnr TO wg_saida-xblnr,
          wg_bsik-budat TO wg_saida-budat,
          wg_bsik-bldat TO wg_saida-bldat,
          wg_bsik-dmbtr TO wg_saida-dmbtr,
*          wg_bsik-dmbe2 TO wg_saida-dmbe2,
          wg_bsik-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsik-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsik-augdt TO wg_saida-augdt,
          wg_bsik-blart TO wg_saida-blart,
          wg_bsik-zuonr TO wg_saida-zuonr.

    IF wg_bsik-waers EQ 'EUR'.
      MOVE wg_bsik-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsik-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsik-shkzg EQ 'S'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsik-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsik-dmbtr / wg_bsik-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsik-dmbtr / wg_bsik-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsik-dmbtr / wg_bsik-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
*            WG_BSIS-HKONT TO WG_SAIDA-CTAVAR,
*            WG_BSIS-DMBTR TO WG_SAIDA-VLVAR.

    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_lfa1, wg_bsis, wg_bkpf.
  ENDLOOP.
***    Total de variacao cambial e conta razao / Doc estornados / Fornecedor
  LOOP AT tg_saida INTO wg_saida
    WHERE ctavar IS INITIAL.
    READ TABLE tg_total INTO wg_total
            WITH KEY belnr = wg_saida-augbl.
*                       GJAHR = WG_SAIDA-GJAHR.
    IF sy-subrc IS INITIAL.
      IF wg_total-shkzg EQ 'H'.

        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.
        MULTIPLY wg_saida-vlvar BY -1.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ELSE.
        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ENDIF.
      SHIFT wg_saida-ctavar LEFT DELETING LEADING '0'.
      MODIFY tg_saida FROM wg_saida.
      CLEAR: wg_saida.
      DELETE TABLE tg_total FROM wg_total.
    ENDIF.
  ENDLOOP.

*  ELSE.
**    Doc Estornados / Cliente
  LOOP AT tg_bsad_est INTO wg_bsad.
    IF wg_bsad-umsks EQ 'A'
    AND wg_bsad-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsad-bukrs
               belnr = wg_bsad-belnr
               gjahr = wg_bsad-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsad-bukrs
                 belnr = wg_bsad-augbl.
*                 gjahr = wg_bsad-gjahr
*                 BINARY SEARCH.
    ENDIF.
    READ TABLE tg_kna1 INTO wg_kna1
      WITH KEY kunnr = wg_bsad-kunnr
               BINARY SEARCH.

    MOVE: wg_bsad-bukrs TO wg_saida-bukrs,
          'Cliente'  TO wg_saida-tipo,
          wg_kna1-kunnr TO wg_saida-parid,
          wg_kna1-name1 TO wg_saida-nome,
          wg_bsad-hkont TO wg_saida-hkont,
          wg_bsad-belnr TO wg_saida-belnr,
          wg_bsad-gjahr TO wg_saida-gjahr,
          wg_bsad-xblnr TO wg_saida-xblnr,
          wg_bsad-budat TO wg_saida-budat,
          wg_bsad-bldat TO wg_saida-bldat,
          wg_bsad-dmbtr TO wg_saida-dmbtr,
*          wg_bsad-dmbe2 TO wg_saida-dmbe2,
          wg_bsad-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsad-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsad-augdt TO wg_saida-augdt,
          wg_bsad-blart TO wg_saida-blart,
          wg_bsad-zuonr TO wg_saida-zuonr.

    IF wg_bsad-waers EQ 'EUR'.
      MOVE wg_bsad-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsad-dmbe2 TO wg_saida-dmbe2.
    ENDIF.
*      IF wg_bsad-augdt GT s_budat-low
*      AND wg_bsad-augdt GT s_budat-high.
*        IF wg_bsad-shkzg EQ 'H'.
*          MULTIPLY wg_saida-dmbtr BY -1.
*          MULTIPLY wg_saida-dmbe2 BY -1.
*        ENDIF.
*      ELSE.
    IF wg_bsad-shkzg EQ 'S'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.
*      ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsad-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_kna1, wg_bsis, wg_bkpf.
  ENDLOOP.

  LOOP AT tg_bsid INTO wg_bsid.
    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsid-bukrs
               belnr = wg_bsid-belnr
               gjahr = wg_bsid-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsid-bukrs
                 belnr = wg_bsid-augbl.
*                 gjahr = wg_bsid-gjahr
*                 BINARY SEARCH.

    ENDIF.
    READ TABLE tg_kna1 INTO wg_kna1
      WITH KEY kunnr = wg_bsid-kunnr
               BINARY SEARCH.

    MOVE: wg_bsid-bukrs TO wg_saida-bukrs,
          'Cliente'  TO wg_saida-tipo,
          wg_kna1-kunnr TO wg_saida-parid,
          wg_kna1-name1 TO wg_saida-nome,
          wg_bsid-hkont TO wg_saida-hkont,
          wg_bsid-belnr TO wg_saida-belnr,
          wg_bsid-gjahr TO wg_saida-gjahr,
          wg_bsid-xblnr TO wg_saida-xblnr,
          wg_bsid-budat TO wg_saida-budat,
          wg_bsid-bldat TO wg_saida-bldat,
          wg_bsid-dmbtr TO wg_saida-dmbtr,
*          wg_bsid-dmbe2 TO wg_saida-dmbe2,
          wg_bsid-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsid-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsid-augdt TO wg_saida-augdt,
          wg_bsid-blart TO wg_saida-blart,
          wg_bsid-zuonr TO wg_saida-zuonr.
*            WG_BSIS-HKONT TO WG_SAIDA-CTAVAR,
*            WG_BSIS-DMBTR TO WG_SAIDA-VLVAR.

    IF wg_bsid-waers EQ 'EUR'.
      MOVE wg_bsid-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsid-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsid-shkzg EQ 'S'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsid-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsid-dmbtr / wg_bsid-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsid-dmbtr / wg_bsid-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsid-dmbtr / wg_bsid-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_kna1, wg_bsis, wg_bkpf.
  ENDLOOP.

***    Total de variacao cambial e conta razao / Doc estornados / Cliente
  LOOP AT tg_saida INTO wg_saida
     WHERE ctavar IS INITIAL.
    READ TABLE tg_total INTO wg_total
            WITH KEY belnr = wg_saida-augbl.
*                       GJAHR = WG_SAIDA-GJAHR.
    IF sy-subrc IS INITIAL.
      IF wg_total-shkzg EQ 'H'.

        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.
        MULTIPLY wg_saida-vlvar BY -1.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ELSE.
        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ENDIF.
      SHIFT wg_saida-ctavar LEFT DELETING LEADING '0'.
      MODIFY tg_saida FROM wg_saida.
      CLEAR: wg_saida.
      DELETE TABLE tg_total FROM wg_total.
    ENDIF.
  ENDLOOP.
*  ENDIF.

*  IF p_lif IS NOT INITIAL.
  LOOP AT tg_bsak2 INTO wg_bsak.
    IF wg_bsak-umsks EQ 'A'
    AND wg_bsak-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsak-bukrs
               belnr = wg_bsak-belnr
               gjahr = wg_bsak-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
       WITH KEY bukrs = wg_bsak-bukrs
                belnr = wg_bsak-augbl.
*                 gjahr = wg_bsak-gjahr
*                 BINARY SEARCH.


    ENDIF.
    READ TABLE tg_lfa1 INTO wg_lfa1
      WITH KEY lifnr = wg_bsak-lifnr
               BINARY SEARCH.

    IF wg_bsak-belnr EQ wg_bsak-augbl.
      CONTINUE.
    ENDIF.

    MOVE: wg_bsak-bukrs TO wg_saida-bukrs,
          'Fornecedor'  TO wg_saida-tipo,
          wg_lfa1-lifnr TO wg_saida-parid,
          wg_lfa1-name1 TO wg_saida-nome,
          wg_bsak-hkont TO wg_saida-hkont,
          wg_bsak-belnr TO wg_saida-belnr,
          wg_bsak-gjahr TO wg_saida-gjahr,
          wg_bsak-xblnr TO wg_saida-xblnr,
          wg_bsak-budat TO wg_saida-budat,
          wg_bsak-bldat TO wg_saida-bldat,
          wg_bsak-dmbtr TO wg_saida-dmbtr,
*          wg_bsak-dmbe2 TO wg_saida-dmbe2,
          wg_bsak-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsak-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsak-augdt TO wg_saida-augdt,
          wg_bsak-blart TO wg_saida-blart,
          wg_bsak-zuonr TO wg_saida-zuonr.

    IF wg_bsak-waers EQ 'EUR'.
      MOVE wg_bsak-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsak-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsak-shkzg NE 'H'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsak-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsak-dmbtr / wg_bsak-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.

    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_lfa1, wg_bsis, wg_bkpf.
  ENDLOOP.

*  ELSE.
  LOOP AT tg_bsad2 INTO wg_bsad.
    IF wg_bsad-umsks EQ 'A'
    AND wg_bsad-umskz EQ 'F'.
      CONTINUE.
    ENDIF.

    READ TABLE tg_bsis INTO wg_bsis
      WITH KEY bukrs = wg_bsad-bukrs
               belnr = wg_bsad-belnr
               gjahr = wg_bsad-gjahr
               BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis-bukrs
                 belnr = wg_bsis-belnr
                 gjahr = wg_bsis-gjahr
                 BINARY SEARCH.
    ELSE.
      READ TABLE tg_bkpf INTO wg_bkpf
       WITH KEY bukrs = wg_bsad-bukrs
                belnr = wg_bsad-augbl.
*                 gjahr = wg_bsad-gjahr
*                 BINARY SEARCH.

    ENDIF.
    READ TABLE tg_kna1 INTO wg_kna1
      WITH KEY kunnr = wg_bsad-kunnr
               BINARY SEARCH.

    IF wg_bsad-belnr EQ wg_bsad-augbl.
      CONTINUE.
    ENDIF.

    MOVE: wg_bsad-bukrs TO wg_saida-bukrs,
          'Cliente'  TO wg_saida-tipo,
          wg_kna1-kunnr TO wg_saida-parid,
          wg_kna1-name1 TO wg_saida-nome,
          wg_bsad-hkont TO wg_saida-hkont,
          wg_bsad-belnr TO wg_saida-belnr,
          wg_bsad-gjahr TO wg_saida-gjahr,
          wg_bsad-xblnr TO wg_saida-xblnr,
          wg_bsad-budat TO wg_saida-budat,
          wg_bsad-bldat TO wg_saida-bldat,
          wg_bsad-dmbtr TO wg_saida-dmbtr,
*          wg_bsad-dmbe2 TO wg_saida-dmbe2,
          wg_bsad-waers TO wg_saida-waers,
          wg_bkpf-kursf TO wg_saida-kursf,
          wg_bsad-belnr TO wg_saida-augbl,
          wg_bsis-gjahr TO wg_saida-gjahr2,
          wg_bsad-augdt TO wg_saida-augdt,
          wg_bsad-blart TO wg_saida-blart,
          wg_bsad-zuonr TO wg_saida-zuonr.
*            WG_BSIS-HKONT TO WG_SAIDA-CTAVAR,
*            WG_BSIS-DMBTR TO WG_SAIDA-VLVAR.

    IF wg_bsad-waers EQ 'EUR'.
      MOVE wg_bsad-wrbtr TO wg_saida-dmbe2.
    ELSE.
      MOVE wg_bsad-dmbe2 TO wg_saida-dmbe2.
    ENDIF.

    IF wg_bsad-shkzg NE 'H'.
      MULTIPLY wg_saida-dmbtr BY -1.
      MULTIPLY wg_saida-dmbe2 BY -1.
    ENDIF.

    SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
    TRY.
        IF wg_bsad-waers EQ 'EUR'.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-wrbtr.
        ELSE.
          wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-dmbe2.
        ENDIF.
*        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    TRY.
*        wg_saida-taxa = wg_bsad-dmbtr / wg_bsad-dmbe2.
        wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    APPEND wg_saida TO tg_saida.
    CLEAR: wg_saida, wg_kna1, wg_bsis, wg_bkpf.
  ENDLOOP.

*  ENDIF.
** Contas de banco ( aberto)
  LOOP AT tg_bsis_banc INTO wg_bsis_banc.
    READ TABLE tg_saida TRANSPORTING NO FIELDS
      WITH KEY augbl = wg_bsis_banc-belnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis_banc-bukrs
                 belnr = wg_bsis_banc-belnr.

      MOVE: wg_bsis_banc-bukrs TO wg_saida-bukrs,
            'Banco'            TO wg_saida-tipo,
            wg_bsis_banc-belnr TO wg_saida-belnr,
            wg_bsis_banc-gjahr TO wg_saida-gjahr,
            wg_bsis_banc-hkont TO wg_saida-hkont,
            wg_bsis_banc-budat TO wg_saida-budat,
            wg_bsis_banc-dmbtr TO wg_saida-dmbtr,
*            wg_bsis_banc-dmbe2 TO wg_saida-dmbe2,
            wg_bsis_banc-waers TO wg_saida-waers,
            wg_bsis_banc-belnr TO wg_saida-augbl,
            wg_bsis_banc-zuonr TO wg_saida-zuonr,
            wg_bkpf-kursf      TO wg_saida-kursf.

      IF wg_bsis_banc-waers EQ 'EUR'.
        MOVE wg_bsis_banc-wrbtr TO wg_saida-dmbe2.
      ELSE.
        MOVE wg_bsis_banc-dmbe2 TO wg_saida-dmbe2.
      ENDIF.

      IF wg_bsis_banc-shkzg NE 'H'.
        MULTIPLY wg_saida-dmbtr BY -1.
        MULTIPLY wg_saida-dmbe2 BY -1.
      ENDIF.

      SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
      TRY.
          IF wg_bsis_banc-waers EQ 'EUR'.
            wg_saida-taxa = wg_bsis_banc-dmbtr / wg_bsis_banc-wrbtr.
          ELSE.
            wg_saida-taxa = wg_bsis_banc-dmbtr / wg_bsis_banc-dmbe2.
          ENDIF.
*          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      TRY.
*          wg_saida-taxa = wg_bsis_banc-dmbtr / wg_bsis_banc-dmbe2.
          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      APPEND wg_saida TO tg_saida.
    ENDIF.
    CLEAR: wg_saida.
  ENDLOOP.

** Contas de banco (compensada )
  LOOP AT tg_bsas_banc INTO wg_bsas_banc.
    READ TABLE tg_saida TRANSPORTING NO FIELDS
      WITH KEY augbl = wg_bsas_banc-belnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsas_banc-bukrs
                 belnr = wg_bsas_banc-belnr.

      MOVE: wg_bsas_banc-bukrs TO wg_saida-bukrs,
            'Banco'            TO wg_saida-tipo,
            wg_bsas_banc-belnr TO wg_saida-belnr,
            wg_bsas_banc-gjahr TO wg_saida-gjahr,
            wg_bsas_banc-hkont TO wg_saida-hkont,
            wg_bsas_banc-budat TO wg_saida-budat,
            wg_bsas_banc-dmbtr TO wg_saida-dmbtr,
*            wg_bsas_banc-dmbe2 TO wg_saida-dmbe2,
            wg_bsas_banc-belnr TO wg_saida-augbl,
            wg_bsas_banc-zuonr TO wg_saida-zuonr,
            wg_bkpf-kursf      TO wg_saida-kursf.

      IF wg_bsas_banc-waers EQ 'EUR'.
        MOVE wg_bsas_banc-wrbtr TO wg_saida-dmbe2.
      ELSE.
        MOVE wg_bsas_banc-dmbe2 TO wg_saida-dmbe2.
      ENDIF.

      IF wg_bsas_banc-shkzg NE 'H'.
        MULTIPLY wg_saida-dmbtr BY -1.
        MULTIPLY wg_saida-dmbe2 BY -1.
      ENDIF.

      SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
      TRY.
          IF wg_bsas_banc-waers EQ 'EUR'.
            wg_saida-taxa = wg_bsas_banc-dmbtr / wg_bsas_banc-wrbtr.
          ELSE.
            wg_saida-taxa = wg_bsas_banc-dmbtr / wg_bsas_banc-dmbe2.
          ENDIF.
*          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      TRY.
*          wg_saida-taxa = wg_bsas_banc-dmbtr / wg_bsas_banc-dmbe2.
          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      APPEND wg_saida TO tg_saida.
    ENDIF.
    CLEAR: wg_saida.
  ENDLOOP.

** Contas Razao Divercas (aberto)
  LOOP AT tg_bsis_raza INTO wg_bsis_raza.
    READ TABLE tg_saida TRANSPORTING NO FIELDS
      WITH KEY augbl = wg_bsis_raza-belnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis_raza-bukrs
                 belnr = wg_bsis_raza-belnr.

      MOVE: wg_bsis_raza-bukrs TO wg_saida-bukrs,
            'Razão'            TO wg_saida-tipo,
            wg_bsis_raza-belnr TO wg_saida-belnr,
            wg_bsis_raza-gjahr TO wg_saida-gjahr,
            wg_bsis_raza-hkont TO wg_saida-hkont,
            wg_bsis_raza-budat TO wg_saida-budat,
            wg_bsis_raza-dmbtr TO wg_saida-dmbtr,
*            wg_bsis_raza-dmbe2 TO wg_saida-dmbe2,
            wg_bsis_raza-waers TO wg_saida-waers,
            wg_bsis_raza-belnr TO wg_saida-augbl,
            wg_bsis_raza-zuonr TO wg_saida-zuonr,
            wg_bkpf-kursf      TO wg_saida-kursf.

      IF wg_bsis_raza-waers EQ 'EUR'.
        MOVE wg_bsis_raza-wrbtr TO wg_saida-dmbe2.
      ELSE.
        MOVE wg_bsis_raza-dmbe2 TO wg_saida-dmbe2.
      ENDIF.

      IF wg_bsis_raza-shkzg EQ 'S'.
        MULTIPLY wg_saida-dmbtr BY -1.
        MULTIPLY wg_saida-dmbe2 BY -1.
      ENDIF.

      SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
      TRY.
          IF wg_bsis_raza-waers EQ 'EUR'.
            wg_saida-taxa = wg_bsis_raza-dmbtr / wg_bsis_raza-wrbtr.
          ELSE.
            wg_saida-taxa = wg_bsis_raza-dmbtr / wg_bsis_raza-dmbe2.
          ENDIF.
*          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      TRY.
*          wg_saida-taxa = wg_bsis_raza-dmbtr / wg_bsis_raza-dmbe2.
          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      APPEND wg_saida TO tg_saida.
    ENDIF.
    CLEAR: wg_saida.
  ENDLOOP.

** Contas Razao Divercas (compensado)
  LOOP AT tg_bsas_raza INTO wg_bsas_raza.
    READ TABLE tg_saida TRANSPORTING NO FIELDS
      WITH KEY augbl = wg_bsas_raza-belnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsas_raza-bukrs
                 belnr = wg_bsas_raza-belnr.

      MOVE: wg_bsas_raza-bukrs TO wg_saida-bukrs,
            'Razão'            TO wg_saida-tipo,
            wg_bsas_raza-belnr TO wg_saida-belnr,
            wg_bsas_raza-gjahr TO wg_saida-gjahr,
            wg_bsas_raza-hkont TO wg_saida-hkont,
            wg_bsas_raza-budat TO wg_saida-budat,
            wg_bsas_raza-dmbtr TO wg_saida-dmbtr,
*            wg_bsas_raza-dmbe2 TO wg_saida-dmbe2,
            wg_bsas_raza-waers TO wg_saida-waers,
            wg_bsas_raza-belnr TO wg_saida-augbl,
            wg_bsas_raza-zuonr TO wg_saida-zuonr,
            wg_bkpf-kursf      TO wg_saida-kursf.

      IF wg_bsas_raza-waers EQ 'EUR'.
        MOVE wg_bsas_raza-wrbtr TO wg_saida-dmbe2.
      ELSE.
        MOVE wg_bsas_raza-dmbe2 TO wg_saida-dmbe2.
      ENDIF.

      IF wg_bsas_raza-shkzg EQ 'S'.
        MULTIPLY wg_saida-dmbtr BY -1.
        MULTIPLY wg_saida-dmbe2 BY -1.
      ENDIF.

      SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
      TRY.
          IF wg_bsas_raza-waers EQ 'EUR'.
            wg_saida-taxa = wg_bsas_raza-dmbtr / wg_bsas_raza-wrbtr.
          ELSE.
            wg_saida-taxa = wg_bsas_raza-dmbtr / wg_bsas_raza-dmbe2.
          ENDIF.
*          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      TRY.
*          wg_saida-taxa = wg_bsas_raza-dmbtr / wg_bsas_raza-dmbe2.
          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      APPEND wg_saida TO tg_saida.
    ENDIF.
    CLEAR: wg_saida.
  ENDLOOP.

  LOOP AT tg_saida INTO wg_saida
     WHERE waers EQ 'BRL'.

    wg_saida-rec_var = 0.
    wg_saida-taxa    = wg_saida-kursf.
    TRY.
        wg_saida-dmbe2   = wg_saida-dmbtr / wg_saida-kursf.
      CATCH: cx_sy_arithmetic_overflow,
             cx_sy_zerodivide.
    ENDTRY.
    MODIFY tg_saida FROM wg_saida.
    CLEAR: wg_saida.
  ENDLOOP.

  LOOP AT tg_bsis2 INTO wg_bsis2
    WHERE dmbtr GT 0
      AND dmbe2 GT 0.

    READ TABLE tg_saida TRANSPORTING NO FIELDS
      WITH KEY augbl = wg_bsis2-belnr.
    IF sy-subrc IS INITIAL.
      READ TABLE tg_bkpf INTO wg_bkpf
        WITH KEY bukrs = wg_bsis2-bukrs
                 belnr = wg_bsis2-belnr.

      MOVE: wg_bsis2-bukrs TO wg_saida-bukrs,
            'Variação'     TO wg_saida-tipo,
            wg_bsis2-belnr TO wg_saida-belnr,
            wg_bsis2-gjahr TO wg_saida-gjahr,
            wg_bsis2-hkont TO wg_saida-hkont,
            wg_bsis2-budat TO wg_saida-budat,
            0              TO wg_saida-dmbtr,
*            wg_bsis2-dmbe2 TO wg_saida-dmbe2,
            wg_bsis2-wrbtr TO wg_saida-dmbe2,
            wg_bsis2-waers TO wg_saida-waers,
            wg_bsis2-belnr TO wg_saida-augbl,
            wg_bsis2-zuonr TO wg_saida-zuonr,
            wg_bkpf-kursf TO wg_saida-kursf.

      IF wg_bsis2-shkzg EQ 'S'.
        MULTIPLY wg_saida-dmbtr BY -1.
        MULTIPLY wg_saida-dmbe2 BY -1.
      ENDIF.

      SHIFT wg_saida-hkont LEFT DELETING LEADING '0'.
      TRY.
          wg_saida-taxa = wg_bsis2-dmbtr / wg_bsis2-dmbe2.
*          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      TRY.
*          wg_saida-taxa = wg_bsis2-dmbtr / wg_bsis2-dmbe2.
          wg_saida-rec_var = wg_saida-dmbtr - ( wg_saida-dmbe2 * wg_bkpf-kursf ).
        CATCH: cx_sy_arithmetic_overflow,
               cx_sy_zerodivide.
      ENDTRY.
      APPEND wg_saida TO tg_saida.
    ENDIF.
    CLEAR: wg_saida.


  ENDLOOP.

  LOOP AT tg_saida INTO wg_saida
    WHERE ctavar IS INITIAL.
*    IF wg_saida-belnr EQ wg_saida-augbl.
*      DELETE TABLE tg_saida FROM wg_saida.

*    ELSE.
    READ TABLE tg_total INTO wg_total
      WITH KEY belnr = wg_saida-augbl.
*                 GJAHR = WG_SAIDA-GJAHR.
    IF sy-subrc IS INITIAL.
      IF wg_total-shkzg EQ 'H'.

        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.
        MULTIPLY wg_saida-vlvar BY -1.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ELSE.
        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ENDIF.
      SHIFT wg_saida-ctavar LEFT DELETING LEADING '0'.
      MODIFY tg_saida FROM wg_saida.
      CLEAR: wg_saida.
      DELETE TABLE tg_total FROM wg_total.
    ENDIF.
*    ENDIF.
  ENDLOOP.

  LOOP AT tg_total INTO wg_total.
    READ TABLE tg_saida INTO wg_saida
     WITH KEY augbl = wg_total-belnr.
*                 GJAHR = WG_SAIDA-GJAHR.
    IF sy-subrc IS INITIAL.
      IF wg_total-shkzg EQ 'H'.

        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.
        MULTIPLY wg_saida-vlvar BY -1.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ELSE.
        MOVE: wg_total-hkont TO wg_saida-ctavar,
              wg_total-dmbtr TO wg_saida-vlvar.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'CTAVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.

        wa_colinfo-color-col = 6.
        wa_colinfo-fieldname = 'VLVAR'.
        APPEND wa_colinfo TO wg_saida-colinfo.
      ENDIF.
      SHIFT wg_saida-ctavar LEFT DELETING LEADING '0'.
      wg_saida-tipo  = 'Variação'.
      wg_saida-belnr = wg_saida-augbl.
      CLEAR: wg_saida-dmbtr, wg_saida-dmbe2, wg_saida-kursf, wg_saida-taxa, wg_saida-waers, wg_saida-augdt,
             wg_saida-parid, wg_saida-nome, wg_saida-hkont, wg_saida-budat, wg_saida-bldat.
      APPEND wg_saida TO tg_saida.
      CLEAR: wg_saida.
      DELETE TABLE tg_total FROM wg_total.
    ENDIF.
  ENDLOOP.
  IF tg_saida[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados dados para sua seleção!'.
    STOP.
  ENDIF.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMI_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimi_dados .

  PERFORM montar_layout.
  wa_layout-coltab_fieldname    = 'COLINFO'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_variant              = variante
      i_callback_program      = sy-repid
      i_callback_user_command = 'XUSER_COMMAND'
      it_fieldcat             = estrutura[]
      is_layout               = wa_layout
      i_save                  = 'A'
    TABLES
      t_outtab                = tg_saida.
ENDFORM.                    " IMPRIMI_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  PERFORM montar_estrutura USING:
        1 'BSIS'       'BUKRS' 'TG_SAIDA' 'BUKRS'  ' '  ' ' ' ',
        2 ' '          ' ' 'TG_SAIDA' 'TIPO'  'Tipo'  ' ' ' ',
        3 'J_1BNFDOC'  'PARID' 'TG_SAIDA' 'PARID'  ' '  ' ' ' ',
        3 ' '  ' ' 'TG_SAIDA' 'NOME'  'Desc. do Parç.'  ' ' ' ',
        4 'BSAD'  'HKONT' 'TG_SAIDA' 'HKONT'   'Conta'  ' ' ' ',
        5 'BSAD'  'BELNR' 'TG_SAIDA' 'BELNR'   ' '  ' ' 'X',
        5 'BSAD'  'GJAHR' 'TG_SAIDA' 'GJAHR'   ' '  ' ' ' ',
        6 'BSAD'  'XBLNR' 'TG_SAIDA' 'XBLNR'   'Referência'  ' ' ' ',
        7 'BSAD'  'BUDAT' 'TG_SAIDA' 'BUDAT'   ' '  ' ' ' ',
        7 'BSAD'  'BLDAT' 'TG_SAIDA' 'BLDAT'   ' '  ' ' ' ',
        7 'BSAD'  'DMBTR' 'TG_SAIDA' 'DMBTR'   'Valor R$'  ' ' ' ',
        7 'BSAD'  'DMBE2' 'TG_SAIDA' 'DMBE2'   'Valor US$'  ' ' ' ',
        7 'BKPF'  'KURSF' 'TG_SAIDA' 'TAXA'    'Taxa'  ' ' ' ',
        7 'BSAD'  'WAERS' 'TG_SAIDA' 'WAERS'   ' '  ' ' ' ',
        7 'BKPF'  'KURSF' 'TG_SAIDA' 'KURSF'   'Tx.Cambio'  ' ' ' ',
        7 'BSAD'  'AUGBL' 'TG_SAIDA' 'AUGBL'   ' '  ' ' 'X',
*        7 'BSIS'  'GJAHR' 'TG_SAIDA' 'GJAHR2'   ' '  ' ' ' ',
        7 'BSAD'  'AUGDT' 'TG_SAIDA' 'AUGDT'   ' '  ' ' ' ',
        7 ' '     ' '     'TG_SAIDA' 'CTAVAR'  'Cta.Var'  ' ' ' ',
*        7 'BSAD'  'DMBTR' 'TG_SAIDA' 'VARGRP'  'Var. Agrup.'  ' ' ' ',
        7 ' '     ' '     'TG_SAIDA' 'VLVAR'   'Vlr.Variação R$'  ' ' ' ',
        7 'BSAD'     'BLART'     'TG_SAIDA' 'BLART'   ' '  ' ' ' ',
        8 ' '     ' '     'TG_SAIDA' 'REC_VAR'   'Recalc.Variação'  ' ' ' ',
        8 'BSAD'     'ZUONR'     'TG_SAIDA' 'ZUONR'   ' '  ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING value(p_col_pos)       TYPE i
                            value(p_ref_tabname)   LIKE dd02d-tabname
                            value(p_ref_fieldname) LIKE dd03d-fieldname
                            value(p_tabname)       LIKE dd02d-tabname
                            value(p_field)         LIKE dd03d-fieldname
                            value(p_scrtext_l)     LIKE dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_hotspot).

  CLEAR wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-ddictxt      = 'M'.

  ENDIF.
  wa_estrutura-hotspot       = p_hotspot.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MATCH_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM match_code CHANGING p_parid.

  DATA: tl_dynpselect TYPE TABLE OF dselc WITH HEADER LINE,
       tl_dynpvaluetab TYPE TABLE OF dval WITH HEADER LINE,
       wl_help_info TYPE help_info,
       wl_selected  TYPE help_info-fldvalue.

  DATA: BEGIN OF dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynpfields.

* Parameter für F4IF_FIELD_VALUE_REQUEST
  DATA: mc_obj LIKE help_info-mcobj.
  DATA: return_values LIKE ddshretval OCCURS 0 WITH HEADER LINE.
  DATA: da_display TYPE c.

  REFRESH:dynpfields.
  MOVE 'P_LIF'  TO dynpfields-fieldname.
  APPEND dynpfields.

  MOVE 'P_KUN'  TO dynpfields-fieldname.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    TABLES
      dynpfields               = dynpfields
    EXCEPTIONS
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      OTHERS                   = 9.

  READ TABLE dynpfields
      WITH KEY fieldname = 'P_LIF'.

  wl_help_info-call	     = 'V'.
  wl_help_info-object	   = 'F'.
  wl_help_info-program   = sy-repid.
  wl_help_info-dynpro	   = sy-dynnr.

  IF dynpfields-fieldvalue EQ 'X'.
    wl_help_info-tabname   = 'LFA1'.
    wl_help_info-fieldname = 'LIFNR'.
  ELSE.
    wl_help_info-tabname   = 'KNA1'.
    wl_help_info-fieldname = 'KUNNR'.
  ENDIF.
*  wl_HELP_INFO-MCOBJ     = 'KRED_C'.
  wl_help_info-spras     = sy-langu.
  wl_help_info-menufunct = 'HC'.

  CALL FUNCTION 'HELP_START'
    EXPORTING
      help_infos   = wl_help_info
    IMPORTING
      select_value = wl_selected
    TABLES
      dynpselect   = tl_dynpselect
      dynpvaluetab = tl_dynpvaluetab.

  IF wl_selected IS NOT INITIAL.
*    p_parid = wl_selected.

  ENDIF.

*
*  IF P_KUN EQ 'X'.
*
*    READ TABLE DYNPFIELDS
*       WITH KEY FIELDNAME = 'P_PARID'.
**   Matchcodeobjekt aufrufen
*    MC_OBJ = 'C_KUNNR'.
*    IF DYNPFIELDS-FIELDINP EQ SPACE.
*      DA_DISPLAY = 'X'. "CHARX.
*    ENDIF.
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        TABNAME           = SPACE
*        FIELDNAME         = SPACE
*        SEARCHHELP        = MC_OBJ
*        DYNPROFIELD       = 'X'
*        VALUE             = DYNPFIELDS-FIELDVALUE
*        DISPLAY           = DA_DISPLAY
*      TABLES
*        RETURN_TAB        = RETURN_VALUES
*      EXCEPTIONS
*        FIELD_NOT_FOUND   = 1
*        NO_HELP_FOR_FIELD = 2
*        INCONSISTENT_HELP = 3
*        NO_VALUES_FOUND   = 4
*        OTHERS            = 5.
*    IF SY-SUBRC = 0 AND
*       DYNPFIELDS-FIELDINP NE SPACE.
*      P_PARID = RETURN_VALUES-FIELDVAL.
*    ENDIF.
*
*  ELSE.
*
*    READ TABLE DYNPFIELDS
*       WITH KEY FIELDNAME = 'P_PARID'.
**   Matchcodeobjekt aufrufen
*    MC_OBJ = 'KRED_C'.
*    IF DYNPFIELDS-FIELDINP EQ SPACE.
*      DA_DISPLAY = 'X'. "CHARX.
*    ENDIF.
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        TABNAME           = SPACE
*        FIELDNAME         = SPACE
*        SEARCHHELP        = MC_OBJ
*        DYNPROFIELD       = 'X'
*        VALUE             = DYNPFIELDS-FIELDVALUE
*        DISPLAY           = DA_DISPLAY
*      TABLES
*        RETURN_TAB        = RETURN_VALUES
*      EXCEPTIONS
*        FIELD_NOT_FOUND   = 1
*        NO_HELP_FOR_FIELD = 2
*        INCONSISTENT_HELP = 3
*        NO_VALUES_FOUND   = 4
*        OTHERS            = 5.
*    IF SY-SUBRC = 0 AND
*       DYNPFIELDS-FIELDINP NE SPACE.
*      P_PARID = RETURN_VALUES-FIELDVAL.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " MATCH_CODE
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command USING ucomm LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.
  selfield = selfield.                                      "#EC CALLED
  CASE ucomm.
    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE tg_saida INTO wg_saida INDEX selfield-tabindex.

      IF sy-subrc EQ 0.
* Se foi clicado na coluna EBELN.
        IF selfield-fieldname = 'AUGBL'.

          SET PARAMETER ID 'BLN' FIELD wg_saida-augbl.
          SET PARAMETER ID 'BUK' FIELD wg_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wg_saida-gjahr2.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ELSEIF selfield-fieldname = 'BELNR'.

          SET PARAMETER ID 'BLN' FIELD wg_saida-belnr.
          SET PARAMETER ID 'BUK' FIELD wg_saida-bukrs.
          SET PARAMETER ID 'GJR' FIELD wg_saida-gjahr2.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM. "XUSER_COMMAND
