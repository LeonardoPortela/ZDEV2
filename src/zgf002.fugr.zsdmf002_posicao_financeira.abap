FUNCTION zsdmf002_posicao_financeira.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BSID-BUKRS OPTIONAL
*"     REFERENCE(KUNNR) TYPE  BSID-KUNNR
*"     REFERENCE(WAERK) TYPE  WAERK OPTIONAL
*"     VALUE(UCOMM) TYPE  SY-UCOMM OPTIONAL
*"  EXPORTING
*"     VALUE(VLR_VENCIDO_BRL) TYPE  BSID-DMBTR
*"     VALUE(VLR_AVENCER_BRL) TYPE  BSID-DMBTR
*"     VALUE(VLR_ADIANTADO_BRL) TYPE  BSID-DMBTR
*"     VALUE(LIMITE_CREDITO) TYPE  KLIMK
*"     VALUE(TOTAL_BRL) TYPE  BSID-DMBTR
*"     VALUE(TOTAL_MOVIMENTO_BRL) TYPE  BSID-DMBTR
*"     VALUE(SALDO_DISPONIVEL_BRL) TYPE  BSID-DMBTR
*"     VALUE(UTILIZADO_LIMITE_BRL) TYPE  BSID-DMBTR
*"     REFERENCE(SDO_OV_EMIT) TYPE  VBAK-NETWR
*"     VALUE(VALIDACAO_POSICAO) TYPE  CHAR10
*"----------------------------------------------------------------------

  DATA: tl_bsid           TYPE TABLE OF bsid WITH HEADER LINE,
        tl_vbak           TYPE TABLE OF vbak WITH HEADER LINE,
        tl_vbfa           TYPE TABLE OF vbfa WITH HEADER LINE,
        tl_vencidas       TYPE TABLE OF bsid WITH HEADER LINE,
        tl_avencer        TYPE TABLE OF bsid WITH HEADER LINE,
        tl_adiantamento   TYPE TABLE OF bsid WITH HEADER LINE,
        tl_total_mov      TYPE TABLE OF bsid WITH HEADER LINE,
        tl_saida          TYPE TABLE OF ty_saida WITH HEADER LINE,
        tl_knka           TYPE TABLE OF knka WITH HEADER LINE,  "#EC CI_USAGE_OK[2227014]
        wl_knkk           TYPE knkk,                            "#EC CI_USAGE_OK[2227014]
        wl_knka           TYPE knka,                            "#EC CI_USAGE_OK[2227014]
        wl_dt_venci       TYPE bsid-zfbdt,
        wl_f_vencidas_brl TYPE bsid-dmbtr,
        wl_f_vencidas_usd TYPE bsid-dmbe2,
        wl_f_avencer_brl  TYPE bsid-dmbtr,
        wl_f_avencer_usd  TYPE bsid-dmbe2,
        wl_f_adianta_brl  TYPE bsid-dmbtr,
        wl_f_adianta_usd  TYPE bsid-dmbe2,
        tl_kna1           TYPE TABLE OF kna1 WITH HEADER LINE,
        wl_kna1           TYPE kna1,
        var_data_venc     TYPE sy-datum,
        var_data_taxa     TYPE gdatu_inv,
        var_taxa_cambio   TYPE ukurs_curr.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_knkk       TYPE STANDARD TABLE OF knkk, "#EC CI_USAGE_OK[2227014]
        lt_data_where TYPE STANDARD TABLE OF zknkk_key,
        wa_data_where TYPE zknkk_key.
* <--- S4 Migration - 18/07/2023 - CA

  DATA: gobj_zcl_util_sd TYPE REF TO zcl_util_sd.


  wg_bukrs = bukrs.
  wg_kunnr = kunnr.

  REFRESH: tl_bsid, tl_vencidas, tl_avencer,
           tl_adiantamento, tl_total_mov, tl_saida,
           tl_vbak, tl_vbfa, lt_knkk, lt_data_where.

  CLEAR: wl_f_vencidas_brl, wl_f_vencidas_usd, wl_f_avencer_brl,
         wl_f_avencer_usd, validacao_posicao, sdo_ov_emit, var_data_taxa,
         var_taxa_cambio, limite_credito.

  CLEAR: vlr_vencido_brl,
         vlr_avencer_brl,
         vlr_adiantado_brl.

  CHECK NOT waerk IS INITIAL.
  CHECK NOT kunnr IS INITIAL.

  IF kunnr IS NOT INITIAL.

    SELECT * FROM kna1
      INTO TABLE tl_kna1
    WHERE kunnr EQ kunnr.

    CHECK tl_kna1 IS NOT INITIAL.

    READ TABLE tl_kna1 INTO wl_kna1 INDEX 1.

    CHECK wl_kna1-ktokd NE 'ZCIC'.

    IF NOT ( wl_kna1-konzs IS INITIAL ).

      SELECT * FROM kna1
        APPENDING TABLE tl_kna1
        FOR ALL ENTRIES IN tl_kna1
     WHERE konzs EQ tl_kna1-konzs.

      SORT: tl_kna1 BY kunnr.
      DELETE ADJACENT DUPLICATES FROM tl_kna1.
    ENDIF.

    CREATE OBJECT gobj_zcl_util_sd.

    var_data_taxa = sy-datum.
    gobj_zcl_util_sd->set_kurst('B').
    gobj_zcl_util_sd->set_waerk('USD').
    gobj_zcl_util_sd->set_tcurr('BRL').
    gobj_zcl_util_sd->set_data( var_data_taxa ).
    var_taxa_cambio = gobj_zcl_util_sd->taxa_cambio( ).

    SELECT *
      FROM vbak
      INTO TABLE tl_vbak
     FOR ALL ENTRIES IN tl_kna1
   WHERE kunnr EQ tl_kna1-kunnr.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM vbfa
        INTO TABLE tl_vbfa
         FOR ALL ENTRIES IN tl_vbak
         WHERE vbelv EQ tl_vbak-vbeln
           AND ( vbtyp_n EQ 'M'
              OR vbtyp_n EQ 'N' )
           AND vbtyp_v EQ 'C'.

      SORT: tl_vbfa BY vbelv.

      LOOP AT tl_vbak.

        IF waerk NE tl_vbak-waerk.
          CASE tl_vbak-waerk.
            WHEN 'BRL'. DIVIDE   tl_vbak-netwr BY var_taxa_cambio.
            WHEN 'USD'. MULTIPLY tl_vbak-netwr BY var_taxa_cambio.
          ENDCASE.
        ENDIF.

        ADD tl_vbak-netwr TO sdo_ov_emit.

        LOOP AT tl_vbfa WHERE vbelv = tl_vbak-vbeln.

          IF waerk NE tl_vbfa-waers.
            CASE tl_vbfa-waers.
              WHEN 'BRL'. DIVIDE   tl_vbfa-rfwrt BY var_taxa_cambio.
            ENDCASE.
          ENDIF.

          IF tl_vbfa-vbtyp_n EQ 'M'.
            MULTIPLY tl_vbfa-rfwrt BY -1.
          ENDIF.

          ADD tl_vbfa-rfwrt TO sdo_ov_emit.

        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

* ---> S4 Migration - 18/07/2023 - CA
*  SELECT SINGLE *
*    FROM KNKK
*    INTO WL_KNKK
*     WHERE KUNNR EQ KUNNR
*       AND KKBER EQ 'MAGI'.

  wa_data_where-kunnr = kunnr.
  wa_data_where-kkber = 'MAGI'.
  APPEND wa_data_where TO lt_data_where.

  CALL FUNCTION 'Z_FROM_TO_KNKK'
    TABLES
      t_data_where = lt_data_where
      t_knkk       = lt_knkk.


  IF lt_knkk[] IS NOT INITIAL.
    READ TABLE lt_knkk INTO DATA(wa_knkk_aux) INDEX 1.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_knkk_aux TO wl_knkk.
    ENDIF.
  ENDIF.
* <--- S4 Migration - 18/07/2023 - CA

  SELECT * FROM knka  "#EC CI_USAGE_OK[2227014]
    INTO TABLE tl_knka
    FOR ALL ENTRIES IN tl_kna1
  WHERE kunnr EQ tl_kna1-kunnr.

  LOOP AT tl_knka.
    ADD tl_knka-klimg TO limite_credito.  "#EC CI_USAGE_OK[2227014]
  ENDLOOP.

  SELECT *
    FROM bsid
    INTO TABLE tl_bsid
    FOR ALL ENTRIES IN tl_kna1
    WHERE kunnr EQ tl_kna1-kunnr
    AND umskz NOT IN ('F', 'W')
    AND umsks NOT IN ( 'W' ).

  IF sy-subrc IS INITIAL.

*    DELETE TL_BSID WHERE UMSKZ EQ 'F'.
*    DELETE TL_BSID WHERE UMSKZ EQ 'W'.
*    DELETE TL_BSID WHERE UMSKS EQ 'W'.

    LOOP AT tl_bsid.

      ADD tl_bsid-zbd1t TO tl_bsid-zfbdt.

      IF tl_bsid-umsks IS NOT INITIAL AND tl_bsid-umskz NE 'F'.

        CASE tl_bsid-shkzg.
          WHEN 'S'.
            ADD tl_bsid-dmbtr TO wl_f_adianta_brl.
            ADD tl_bsid-dmbe2 TO wl_f_adianta_usd.
          WHEN 'H'.
            SUBTRACT tl_bsid-dmbtr FROM wl_f_adianta_brl.
            SUBTRACT tl_bsid-dmbe2 FROM wl_f_adianta_usd.
        ENDCASE.

        APPEND tl_bsid TO tl_adiantamento.

**    Faturas vencidas
      ELSEIF tl_bsid-zfbdt LT sy-datum.

        CASE tl_bsid-shkzg.
          WHEN 'S'.
            ADD tl_bsid-dmbtr TO wl_f_vencidas_brl.
            ADD tl_bsid-dmbe2 TO wl_f_vencidas_usd.
          WHEN 'H'.
            SUBTRACT tl_bsid-dmbtr FROM wl_f_vencidas_brl.
            SUBTRACT tl_bsid-dmbe2 FROM wl_f_vencidas_usd.
        ENDCASE.

        APPEND tl_bsid TO tl_vencidas.

        "Validação de 48Horas.
        var_data_venc = sy-datum - 2.
        IF ( tl_bsid-zfbdt < var_data_venc ).
          validacao_posicao = 'DT_VENC'.
        ENDIF.

**    Faturas a vencer
      ELSEIF tl_bsid-zfbdt GE sy-datum.

        CASE tl_bsid-shkzg.
          WHEN 'S'.
            ADD tl_bsid-dmbtr TO wl_f_avencer_brl.
            ADD tl_bsid-dmbe2 TO wl_f_avencer_usd.
          WHEN 'H'.
            SUBTRACT tl_bsid-dmbtr FROM wl_f_avencer_brl.
            SUBTRACT tl_bsid-dmbe2 FROM wl_f_avencer_usd.
        ENDCASE.

        APPEND tl_bsid TO tl_avencer.

      ENDIF.

      APPEND tl_bsid TO tl_total_mov.

    ENDLOOP.

  ENDIF.

  READ TABLE tl_knka INTO wl_knka INDEX 1.
  CASE waerk.
    WHEN: 'USD'.

      vlr_vencido_brl   =  wl_f_vencidas_usd.
      vlr_avencer_brl   =  wl_f_avencer_usd.
      vlr_adiantado_brl =  wl_f_adianta_usd.

      IF ( wl_knka-waers NE waerk ).  "#EC CI_USAGE_OK[2227014]
        DIVIDE limite_credito BY var_taxa_cambio.
      ENDIF.

      total_brl            = wl_f_vencidas_usd + wl_f_avencer_usd.
      total_movimento_brl  = total_brl + wl_f_adianta_usd.
      saldo_disponivel_brl = limite_credito - total_movimento_brl.

      TRY.
          utilizado_limite_brl = ( total_movimento_brl / limite_credito ) * 100.
        CATCH  cx_sy_zerodivide.
      ENDTRY.

    WHEN OTHERS.

      vlr_vencido_brl   =  wl_f_vencidas_brl.
      vlr_avencer_brl   =  wl_f_avencer_brl.
      vlr_adiantado_brl =  wl_f_adianta_brl.

      IF ( wl_knka-waers NE waerk ).  "#EC CI_USAGE_OK[2227014]
        MULTIPLY limite_credito BY var_taxa_cambio.
      ENDIF.

      total_brl            = wl_f_vencidas_brl + wl_f_avencer_brl.
      total_movimento_brl  = total_brl + vlr_adiantado_brl.
      saldo_disponivel_brl = limite_credito  - total_movimento_brl.

      TRY.
          utilizado_limite_brl = ( total_movimento_brl / limite_credito ) * 100.
        CATCH  cx_sy_zerodivide.
      ENDTRY.

  ENDCASE.

  IF ucomm IS NOT INITIAL.

    CASE ucomm.
      WHEN 'PF_VENCIDAS'.

        LOOP AT tl_vencidas.

          MOVE-CORRESPONDING: tl_vencidas TO tl_saida.

          IF tl_vencidas-shkzg EQ 'H'.
            MULTIPLY tl_saida-dmbtr BY -1.
            MULTIPLY tl_saida-dmbe2 BY -1.
          ENDIF.

          APPEND tl_saida.
          CLEAR: tl_saida.

        ENDLOOP.

      WHEN 'PF_AVENCER'.

        LOOP AT tl_avencer.

          MOVE-CORRESPONDING: tl_avencer TO tl_saida.

          IF tl_avencer-shkzg EQ 'H'.
            MULTIPLY tl_saida-dmbtr BY -1.
            MULTIPLY tl_saida-dmbe2 BY -1.
          ENDIF.

          APPEND tl_saida.
          CLEAR: tl_saida.

        ENDLOOP.

      WHEN 'PF_ADIANT'.

        LOOP AT tl_adiantamento.

          MOVE-CORRESPONDING: tl_adiantamento TO tl_saida.

          IF tl_adiantamento-shkzg EQ 'H'.
            MULTIPLY tl_saida-dmbtr BY -1.
            MULTIPLY tl_saida-dmbe2 BY -1.
          ENDIF.

          APPEND tl_saida.
          CLEAR: tl_saida.

        ENDLOOP.

      WHEN 'PF_TOTMOV'.

        LOOP AT tl_total_mov.

          MOVE-CORRESPONDING: tl_total_mov TO tl_saida.

          IF tl_total_mov-shkzg EQ 'H'.
            MULTIPLY tl_saida-dmbtr BY -1.
            MULTIPLY tl_saida-dmbe2 BY -1.
          ENDIF.

          APPEND tl_saida.
          CLEAR: tl_saida.

        ENDLOOP.

    ENDCASE.
  ENDIF.

  IF tl_saida[] IS NOT INITIAL.

    PERFORM montar_layout USING 'TL_SAIDA'.
    PERFORM definir_eventos.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = v_report
        it_fieldcat           = estrutura[]
        it_events             = events
        i_save                = 'A'
        i_screen_start_column = 1
        i_screen_start_line   = 1
        i_screen_end_column   = 165
        i_screen_end_line     = 25
      TABLES
        t_outtab              = tl_saida.

  ENDIF.

ENDFUNCTION.
