FUNCTION zfi_get_data_zsdt0090.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_NRO_SOL_OV) TYPE  ZSDED013 OPTIONAL
*"  TABLES
*"      T_SAIDA_01 STRUCTURE  ZDE_DATA_ZSDT0090_1_OUT
*"      T_SAIDA_02 STRUCTURE  ZDE_DATA_ZSDT0090_2_OUT
*"      T_SAIDA_03 STRUCTURE  ZDE_DATA_ZSDT0090_3_OUT
*"      T_SAIDA_04 STRUCTURE  ZDE_DATA_ZSDT0090_4_OUT
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_zsdt0094.
           INCLUDE STRUCTURE zsdt0094.
  TYPES:   bukrs_ctb TYPE bsak-bukrs.
  TYPES: END OF ty_zsdt0094.

  DATA: BEGIN OF lit_zsdt0051 OCCURS 0,
          nro_sol_ov TYPE zsded013,
          kunnr      TYPE zsdt0051-kunnr,
          auart      TYPE zsdt0051-auart,
          tp_venda   TYPE zsdt0051-tp_venda,
          vkorg      TYPE zsdt0051-vkorg,
        END OF lit_zsdt0051,

        BEGIN OF lit_zsdt0040 OCCURS 0,
          doc_simulacao TYPE zsded003,
          kunnr         TYPE kunag,
          auart         TYPE auart,
          tpsim         TYPE char2,
          vkorg         TYPE vkorg,
        END OF lit_zsdt0040,

        BEGIN OF lit_ekko  OCCURS 0,
          ebeln TYPE ebeln,
          lifnr TYPE elifn,
          kunnr TYPE kunnr,
          bukrs TYPE bukrs,
          bsart TYPE bsart,
        END OF lit_ekko,

        BEGIN OF lit_vbrk OCCURS 0,
          vbeln TYPE vbeln,
          kunag TYPE kunag,
        END OF lit_vbrk,

        BEGIN OF lit_bsak OCCURS 0,
          bukrs      TYPE bukrs,
          belnr      TYPE belnr,
          lifnr_bsak TYPE lifnr,
        END OF lit_bsak,

        BEGIN OF lit_bsik OCCURS 0,
          bukrs      TYPE bukrs,
          belnr      TYPE belnr,
          lifnr_bsik TYPE lifnr,
        END OF lit_bsik,

        lit_zsdt0094 TYPE TABLE OF ty_zsdt0094.

  RANGES: lra_data_registro FOR zfit0087-data_registro.
  DATA: lra_buzei          TYPE RANGE OF char4,
        lra_nr_sol_ov TYPE RANGE OF zsdt0094-nro_sol_ov.

  CLEAR:t_saida_01[], t_saida_02[], t_saida_03, t_saida_04.


  IF i_nro_sol_ov IS NOT INITIAL.
    APPEND VALUE #( sign = 'I'  option = 'EQ' low = i_nro_sol_ov ) TO lra_nr_sol_ov.
  ENDIF.

  CHECK ( i_data_ini IS NOT INITIAL AND
        i_data_fim IS NOT INITIAL ) OR lra_nr_sol_ov IS NOT INITIAL.

  IF i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.
    APPEND VALUE #( sign = 'I'  option = 'BT' low = i_data_ini high = i_data_fim ) TO lra_data_registro.
  ENDIF.

  "busca SAIDAS

  SELECT *
    FROM zsdt0094 INTO CORRESPONDING FIELDS OF TABLE lit_zsdt0094"lit_saida_01
   WHERE data_registro IN lra_data_registro
    AND nro_sol_ov in lra_nr_sol_ov.

  CHECK lit_zsdt0094[] IS NOT INITIAL. "lit_saida_01[]

  LOOP AT lit_zsdt0094  ASSIGNING FIELD-SYMBOL(<fs_zsdt0094>).
    <fs_zsdt0094>-bukrs_ctb = <fs_zsdt0094>-bezei.
  ENDLOOP.

  DATA(lit_zsdt0094_aux) = lit_zsdt0094[].
  SORT lit_zsdt0094_aux BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM lit_zsdt0094_aux COMPARING nro_sol_ov.

  IF lit_zsdt0094_aux[] IS NOT INITIAL.

    SELECT nro_sol_ov kunnr auart tp_venda vkorg
      FROM zsdt0051 INTO TABLE lit_zsdt0051
       FOR ALL ENTRIES IN lit_zsdt0094_aux
     WHERE nro_sol_ov EQ lit_zsdt0094_aux-nro_sol_ov.

    SELECT doc_simulacao kunnr auart tpsim vkorg
      FROM zsdt0040 INTO TABLE lit_zsdt0040
       FOR ALL ENTRIES IN lit_zsdt0094_aux
     WHERE doc_simulacao EQ lit_zsdt0094_aux-nro_sol_ov.

    SELECT ebeln lifnr kunnr bukrs bsart
    FROM ekko INTO TABLE lit_ekko
     FOR ALL ENTRIES IN lit_zsdt0094_aux
   WHERE ebeln EQ lit_zsdt0094_aux-nro_sol_ov.

    SELECT vbeln kunag
      FROM vbrk INTO TABLE lit_vbrk
       FOR ALL ENTRIES IN lit_zsdt0094_aux
     WHERE vbeln EQ lit_zsdt0094_aux-nro_sol_ov.

    SELECT bukrs belnr lifnr
      FROM bsak INTO  CORRESPONDING FIELDS OF TABLE lit_bsak
     FOR ALL ENTRIES IN lit_zsdt0094_aux
   WHERE belnr EQ lit_zsdt0094_aux-nro_sol_ov AND
         bukrs EQ lit_zsdt0094_aux-bukrs_ctb.

    SELECT bukrs belnr lifnr
      FROM bsik INTO CORRESPONDING FIELDS OF TABLE lit_bsik
     FOR ALL ENTRIES IN lit_zsdt0094_aux
   WHERE belnr EQ lit_zsdt0094_aux-nro_sol_ov
     AND bukrs EQ lit_zsdt0094_aux-bukrs_ctb.

*ajuste 19-07-2023 - BG
    SELECT vbeln, vkorg FROM vbak
      INTO TABLE @DATA(lit_vbak)
       FOR ALL ENTRIES IN @lit_zsdt0094
 WHERE vbeln EQ @lit_zsdt0094-vbeln.

  ENDIF.

**--------------------------------------------------------------------------------------*
**  Sorts
**--------------------------------------------------------------------------------------*
  SORT: lit_zsdt0051  BY nro_sol_ov,
        lit_zsdt0040  BY doc_simulacao,
        lit_ekko      BY ebeln,
        lit_vbrk      BY vbeln,
        lit_bsak      BY bukrs belnr,
        lit_bsik      BY bukrs belnr,
        "ajuste 19-07-2023 - BG
        lit_vbak      BY vbeln.

**--------------------------------------------------------------------------------------*
**  Montar Saidas
**--------------------------------------------------------------------------------------*

  LOOP AT lit_zsdt0094 ASSIGNING <fs_zsdt0094>.

*-----------------------------------------------------------------------------------------------------------------------*
*   Saida 1
*-----------------------------------------------------------------------------------------------------------------------*
    READ TABLE lit_zsdt0051 INTO DATA(lwa_zsdt0051) WITH KEY nro_sol_ov = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_saida_01 ASSIGNING FIELD-SYMBOL(<fs_saida01>).

      MOVE-CORRESPONDING <fs_zsdt0094> TO <fs_saida01>.

      <fs_saida01>-kunnr    = lwa_zsdt0051-kunnr.
      <fs_saida01>-auart    = lwa_zsdt0051-auart.
      <fs_saida01>-tp_venda = lwa_zsdt0051-tp_venda.
      <fs_saida01>-vkorg    = lwa_zsdt0051-vkorg.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------*
*   Saida 2
*-----------------------------------------------------------------------------------------------------------------------*
    READ TABLE lit_zsdt0040 INTO DATA(lwa_zsdt0040) WITH KEY doc_simulacao = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_saida_02 ASSIGNING FIELD-SYMBOL(<fs_saida02>).

      MOVE-CORRESPONDING <fs_zsdt0094> TO <fs_saida02>.

      <fs_saida02>-kunnr = lwa_zsdt0040-kunnr.
      <fs_saida02>-auart = lwa_zsdt0040-auart.
      <fs_saida02>-tpsim = lwa_zsdt0040-tpsim.
      <fs_saida02>-vkorg = lwa_zsdt0040-vkorg.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------*
*   Saida 3
*-----------------------------------------------------------------------------------------------------------------------*
    READ TABLE lit_ekko INTO DATA(lwa_ekko) WITH KEY ebeln  = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO t_saida_03 ASSIGNING FIELD-SYMBOL(<fs_saida03>).

      MOVE-CORRESPONDING <fs_zsdt0094> TO <fs_saida03>.

      <fs_saida03>-kunnr = lwa_ekko-kunnr.
      <fs_saida03>-lifnr = lwa_ekko-lifnr.
      <fs_saida03>-bukrs = lwa_ekko-bukrs.
      <fs_saida03>-bsart = lwa_ekko-bsart.
    ENDIF.

*-----------------------------------------------------------------------------------------------------------------------*
*   Saida 4
*-----------------------------------------------------------------------------------------------------------------------*
    APPEND INITIAL LINE TO t_saida_04 ASSIGNING FIELD-SYMBOL(<fs_saida04>).

    MOVE-CORRESPONDING <fs_zsdt0094> TO <fs_saida04>.

    CLEAR: <fs_saida04>-kunag, <fs_saida04>-lifnr_bsak, <fs_saida04>-lifnr_bsik.

    READ TABLE lit_vbrk INTO DATA(lwa_vbrk) WITH KEY vbeln  = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida04>-kunag = lwa_vbrk-kunag.
    ENDIF.

    READ TABLE lit_bsak INTO DATA(lwa_bsak) WITH KEY bukrs  = <fs_zsdt0094>-bukrs_ctb
                                                     belnr  = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida04>-lifnr_bsak = lwa_bsak-lifnr_bsak.
    ENDIF.

    READ TABLE lit_bsik INTO DATA(lwa_bsik) WITH KEY bukrs  = <fs_zsdt0094>-bukrs_ctb
                                                     belnr  = <fs_zsdt0094>-nro_sol_ov BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida04>-lifnr_bsik = lwa_bsik-lifnr_bsik.
    ENDIF.
*ajuste 19-07-2023 - BG
    READ TABLE lit_vbak INTO DATA(lwa_vbak) WITH KEY vbeln  = <fs_zsdt0094>-vbeln BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      <fs_saida04>-vkorg = lwa_vbak-vkorg.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
