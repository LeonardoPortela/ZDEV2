FUNCTION zsd_get_data_solicitacao_ov .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DATA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_CHARG_INI) TYPE  CHARG_D OPTIONAL
*"     VALUE(I_CHARG_FIM) TYPE  CHARG_D OPTIONAL
*"     VALUE(I_DATA_VENDA_INI) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DATA_VENDA_FIM) TYPE  ERDAT OPTIONAL
*"     VALUE(I_DADOS_FLUXO) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_HD_VTWEG STRUCTURE  ZHD_VTWEG OPTIONAL
*"      T_HD_SPART STRUCTURE  ZHD_SPART OPTIONAL
*"      T_HD_TP_VENDA STRUCTURE  ZHD_TP_VENDA OPTIONAL
*"      T_HD_NO_TP_VENDA STRUCTURE  ZHD_TP_VENDA OPTIONAL
*"      T_HD_STATUS STRUCTURE  ZHD_STATUS OPTIONAL
*"      T_HD_VKORG STRUCTURE  ZSDE_VKORG OPTIONAL
*"      T_HD_MATNR STRUCTURE  EDMASN_MATNR_TS OPTIONAL
*"      T_HD_VKBUR STRUCTURE  ZSDE_VKBUR OPTIONAL
*"      T_IT_MATNR STRUCTURE  EDMASN_MATNR_TS OPTIONAL
*"      T_IT_STATUS STRUCTURE  ZHD_STATUS_ZSDT0053 OPTIONAL
*"      T_VBELN_OV STRUCTURE  ZSDE_VBELN OPTIONAL
*"      T_SAIDA_ZSDT0051 STRUCTURE  ZSDT0051
*"      T_SAIDA_ZSDT0052 STRUCTURE  ZSDT0052
*"      T_SAIDA_ZSDT0053 STRUCTURE  ZSDT0053
*"      T_SAIDA_ZSDT0059 STRUCTURE  ZSDT0059
*"      T_SAIDA_ZSDT0069 STRUCTURE  ZSDT0069
*"      T_SAIDA_ZSDT0084 STRUCTURE  ZSDT0084
*"      T_SAIDA_ZSDT0076 STRUCTURE  ZSDT0076
*"      T_SAIDA_VBFA STRUCTURE  ZSAIDA_VBFA
*"      T_SAIDA_J_1BNFLIN STRUCTURE  ZSAIDA_J_1BNFLIN
*"      T_SAIDA_J_1BNFDOC STRUCTURE  ZSAIDA_J_1BNFDOC
*"      T_SAIDA_VBAP STRUCTURE  ZSAIDA_VBAP
*"      T_SAIDA_MAKT STRUCTURE  ZSAIDA_MAKT
*"      T_SAIDA_T001W STRUCTURE  ZSAIDA_T001W
*"      T_SAIDA_ZIB_PROD_CRUSHF STRUCTURE  ZIB_PROD_CRUSHF
*"----------------------------------------------------------------------
INCLUDE zafl_macros.
**initialize logger. It should be always on the top of the FUNCTION.
  /afl/log_init.

  TYPES: BEGIN OF ty_0069,
           nro_sol_ov TYPE zsdt0069-nro_sol_ov,
         END OF ty_0069,

         BEGIN OF ty_refkey,
           refkey TYPE j_1bnflin-refkey,
         END OF ty_refkey.

  DATA: lt_zsdt0069            TYPE STANDARD TABLE OF ty_0069,
        lt_range_data          TYPE RANGE OF zsdt0051-data_atual,
        lt_range_charg         TYPE RANGE OF zsdt0053-charg,
        lt_range_data_venda    TYPE RANGE OF zsdt0051-data_atual,
        lt_rg_t_hd_vtweg       TYPE RANGE OF zsdt0051-vtweg,
        lt_rg_t_hd_spart       TYPE RANGE OF zsdt0051-spart,
        lt_rg_t_hd_tp_venda    TYPE RANGE OF zsdt0051-tp_venda,
        lt_rg_t_hd_no_tp_venda TYPE RANGE OF zsdt0051-tp_venda,
        lt_rg_t_hd_status      TYPE RANGE OF zsdt0051-status,
        lt_rg_t_hd_vkorg       TYPE RANGE OF zsdt0051-vkorg,
        lt_rg_t_hd_matnr       TYPE RANGE OF zsdt0051-matnr,
        lt_rg_t_hd_vkbur       TYPE RANGE OF zsdt0051-vkbur,
        lt_rg_t_it_matnr       TYPE RANGE OF zsdt0053-matnr,
        lt_rg_t_it_status      TYPE RANGE OF zsdt0053-status,
        lt_rg_t_vbeln_ov       TYPE RANGE OF vbap-vbeln,
        it_refkey              TYPE TABLE OF ty_refkey,
        lit_sol_proc           TYPE TABLE OF ty_0069.

  IF  i_data_ini IS NOT INITIAL AND i_data_fim IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I' option = 'BT' low  = i_data_ini high = i_data_fim ) TO lt_range_data.
  ELSE.
    IF i_data_ini IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'GE' low  = i_data_ini ) TO lt_range_data.
    ENDIF.

    IF i_data_fim IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'LE' low  =  i_data_fim ) TO lt_range_data.
    ENDIF.
  ENDIF.

  SORT lt_range_data BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_range_data COMPARING ALL FIELDS.

  IF  i_charg_ini IS NOT INITIAL AND i_charg_fim IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I' option = 'BT' low  = i_charg_ini high = i_charg_fim ) TO lt_range_charg.
  ELSE.
    IF i_charg_ini IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'GE' low  = i_charg_ini ) TO lt_range_charg.
    ENDIF.

    IF i_charg_fim IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'LE' low  =  i_charg_fim ) TO lt_range_charg.
    ENDIF.
  ENDIF.

  SORT lt_range_charg BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_range_charg COMPARING ALL FIELDS.

  IF i_data_venda_ini IS NOT INITIAL AND i_data_venda_fim IS NOT INITIAL.
    APPEND VALUE #(  sign = 'I' option = 'BT' low  = i_data_venda_ini high = i_data_venda_fim ) TO lt_range_data_venda.

  ELSE.
    IF i_data_venda_ini IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'GE' low  = i_data_venda_ini ) TO lt_range_data_venda.
    ENDIF.

    IF i_data_venda_fim IS NOT INITIAL.
      APPEND VALUE #(  sign = 'I' option = 'LE' low  =  i_data_venda_fim ) TO lt_range_data_venda.
    ENDIF.
  ENDIF.

  SORT lt_range_data_venda BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_range_data_venda COMPARING ALL FIELDS.

  LOOP AT t_hd_vtweg INTO DATA(lwa_filtro1).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  = lwa_filtro1 ) TO lt_rg_t_hd_vtweg.
  ENDLOOP.

  SORT lt_rg_t_hd_vtweg BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_vtweg COMPARING ALL FIELDS.

  LOOP AT t_hd_spart INTO DATA(lwa_filtro2).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  = lwa_filtro2 ) TO lt_rg_t_hd_spart.
  ENDLOOP.

  SORT lt_rg_t_hd_spart BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_spart COMPARING ALL FIELDS.

  LOOP AT t_hd_tp_venda INTO DATA(lwa_filtro3).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro3 ) TO lt_rg_t_hd_tp_venda.
  ENDLOOP.

  LOOP AT t_hd_no_tp_venda INTO DATA(lwa_filtro4).
    APPEND VALUE #(  sign = 'E' option = 'EQ' low  =  lwa_filtro4 ) TO lt_rg_t_hd_tp_venda.
  ENDLOOP.

  SORT lt_rg_t_hd_tp_venda BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_tp_venda COMPARING ALL FIELDS.

  LOOP AT t_hd_status INTO DATA(lwa_filtro5).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro5 ) TO lt_rg_t_hd_status.
  ENDLOOP.

  SORT lt_rg_t_hd_status BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_status COMPARING ALL FIELDS.

  LOOP AT t_hd_vkorg INTO DATA(lwa_filtro6).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro6 ) TO lt_rg_t_hd_vkorg.
  ENDLOOP.

  SORT lt_rg_t_hd_vkorg BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_vkorg COMPARING ALL FIELDS.

  LOOP AT t_hd_matnr INTO DATA(lwa_filtro7).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro7 ) TO lt_rg_t_hd_matnr.
  ENDLOOP.

  SORT lt_rg_t_hd_matnr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_matnr COMPARING ALL FIELDS.

  LOOP AT t_hd_vkbur INTO DATA(lwa_filtro8).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro8 ) TO lt_rg_t_hd_vkbur.
  ENDLOOP.

  SORT lt_rg_t_hd_vkbur BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_hd_vkbur COMPARING ALL FIELDS.

  LOOP AT t_it_matnr INTO DATA(lwa_filtro9).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro9 ) TO lt_rg_t_it_matnr.
  ENDLOOP.

  SORT lt_rg_t_it_matnr BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_it_matnr COMPARING ALL FIELDS.

  LOOP AT t_it_status INTO DATA(lwa_filtro10).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro10 ) TO lt_rg_t_it_status.
  ENDLOOP.

  SORT lt_rg_t_it_status BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_it_status COMPARING ALL FIELDS.

  LOOP AT t_vbeln_ov INTO DATA(lwa_filtro11).
    APPEND VALUE #(  sign = 'I' option = 'EQ' low  =  lwa_filtro11 ) TO lt_rg_t_vbeln_ov.
  ENDLOOP.

  SORT lt_rg_t_vbeln_ov BY low ASCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_rg_t_vbeln_ov COMPARING ALL FIELDS.

  SELECT DISTINCT cb~nro_sol_ov
  FROM zsdt0051 AS cb
      LEFT JOIN zsdt0053 AS it  ON  it~nro_sol_ov = cb~nro_sol_ov
  INTO TABLE @lit_sol_proc
 WHERE  cb~vtweg     IN @lt_rg_t_hd_vtweg
   AND cb~spart      IN @lt_rg_t_hd_spart
   AND cb~tp_venda   IN @lt_rg_t_hd_tp_venda
   AND cb~status     IN @lt_rg_t_hd_status
   AND cb~vkorg      IN @lt_rg_t_hd_vkorg
   AND cb~matnr      IN @lt_rg_t_hd_matnr
   AND cb~vkbur      IN @lt_rg_t_hd_vkbur
   AND cb~data_venda IN @lt_range_data_venda
   AND cb~data_atual IN @lt_range_data
   AND it~charg      IN @lt_range_charg
   AND it~matnr      IN @lt_rg_t_it_matnr
   AND it~status     IN @lt_rg_t_it_status
   AND cb~nro_sol_ov IS NOT NULL.

  IF  i_charg_ini IS NOT INITIAL AND i_charg_fim IS NOT INITIAL.

    CLEAR lt_range_data.
    CONCATENATE i_charg_ini '01' '01'  INTO DATA(v_data_in).
    CONCATENATE i_charg_fim '12' '31' INTO DATA(v_data_fim).
    APPEND VALUE #(  sign = 'I' option = 'BT' low  = v_data_in high = v_data_fim ) TO lt_range_data.

    SELECT DISTINCT nro_sol_ov FROM zsdt0059
    APPENDING CORRESPONDING FIELDS OF TABLE lit_sol_proc
    WHERE data_atual IN lt_range_data.

  ENDIF.

  SORT lit_sol_proc BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM  lit_sol_proc COMPARING nro_sol_ov.
  CHECK lit_sol_proc[] IS NOT INITIAL.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0051
*------------------------------------------------------------------------------------------------------------------------*

  SELECT DISTINCT *
    FROM zsdt0051
    APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0051
    FOR ALL ENTRIES IN  lit_sol_proc
    WHERE nro_sol_ov = lit_sol_proc-nro_sol_ov.

  SORT t_saida_zsdt0051 BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM t_saida_zsdt0051 COMPARING ALL FIELDS.

*  select distinct  nro_sol_ov
*   FROM zsdt0069
*  INTO CORRESPONDING FIELDS OF TABLE lt_zsdt0069
*  FOR ALL ENTRIES IN  lit_sol_proc
*  WHERE data_atual IN lt_range_data
*        AND nro_sol_ov = lit_sol_proc-nro_sol_ov.
*
*  IF sy-subrc IS INITIAL.
*
*    select distinct *
*      FROM zsdt0051
*      APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0051
*      FOR ALL ENTRIES IN lt_zsdt0069
*      WHERE nro_sol_ov EQ lt_zsdt0069-nro_sol_ov.
*
*    IF sy-subrc IS INITIAL.
*      SORT t_saida_zsdt0051 BY nro_sol_ov.
*    ENDIF.
*  ENDIF.

*  SORT t_saida_zsdt0051 BY nro_sol_ov.
*  DELETE ADJACENT DUPLICATES FROM t_saida_zsdt0051 COMPARING nro_sol_ov.

  IF  t_saida_zsdt0051[] IS NOT INITIAL.
    SELECT DISTINCT matnr maktx spras maktg
    APPENDING TABLE t_saida_makt
    FROM  makt
    FOR ALL ENTRIES IN t_saida_zsdt0051
    WHERE   matnr = t_saida_zsdt0051-matnr.

    SORT t_saida_makt BY matnr.
    DELETE ADJACENT DUPLICATES FROM t_saida_makt COMPARING ALL FIELDS.
  ENDIF.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0052
*------------------------------------------------------------------------------------------------------------------------*

  SELECT DISTINCT *
    FROM zsdt0052
    APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0052
    FOR ALL ENTRIES IN lit_sol_proc
    WHERE nro_sol_ov = lit_sol_proc-nro_sol_ov.

  SORT t_saida_zsdt0052 BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM t_saida_zsdt0052 COMPARING ALL FIELDS.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0053
*------------------------------------------------------------------------------------------------------------------------*


*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
  SELECT DISTINCT *
   FROM zsdt0053 AS it
   APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0053
    FOR ALL ENTRIES IN lit_sol_proc
   WHERE nro_sol_ov    EQ lit_sol_proc-nro_sol_ov
     AND it~charg      IN lt_range_charg
     AND it~matnr      IN lt_rg_t_it_matnr
     AND it~status     IN lt_rg_t_it_status.

  SORT t_saida_zsdt0052 BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM t_saida_zsdt0053 COMPARING ALL FIELDS.
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  IF t_saida_zsdt0053[] IS NOT INITIAL.

    DATA(lit_zsdt0053_aux) = t_saida_zsdt0053[].

    SORT lit_zsdt0053_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt0053_aux COMPARING matnr.

    SELECT DISTINCT matnr maktx spras maktg
    FROM makt
    APPENDING TABLE t_saida_makt
    FOR ALL ENTRIES IN lit_zsdt0053_aux
    WHERE matnr = lit_zsdt0053_aux-matnr.

    SORT t_saida_makt BY  matnr spras.
    DELETE ADJACENT DUPLICATES FROM t_saida_makt COMPARING  matnr spras.

  ENDIF.

  IF t_saida_zsdt0053[] IS NOT INITIAL AND i_dados_fluxo EQ abap_true. "Busca Dados de Fluxo de Vendas

    lit_zsdt0053_aux = t_saida_zsdt0053[].

    SORT lit_zsdt0053_aux BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt0053_aux COMPARING vbeln.

    SELECT DISTINCT vbeln posnr ntgew kwmeng zmeng
    FROM vbap
    APPENDING TABLE t_saida_vbap
    FOR ALL ENTRIES IN lit_zsdt0053_aux
    WHERE vbeln = lit_zsdt0053_aux-vbeln.

    SORT t_saida_vbap BY vbeln.
    DELETE ADJACENT DUPLICATES FROM t_saida_vbap COMPARING ALL FIELDS.

    SELECT DISTINCT vbelv, posnv, vbeln, posnn, vbtyp_n, rfmng, erdat, erzet
    FROM vbfa
    INTO TABLE @DATA(lit_vbfa_1)
    FOR ALL ENTRIES IN @lit_zsdt0053_aux
    WHERE vbelv = @lit_zsdt0053_aux-vbeln .

    SORT lit_vbfa_1 BY vbeln.
    DELETE ADJACENT DUPLICATES FROM lit_vbfa_1 COMPARING ALL FIELDS.

    IF lit_vbfa_1[] IS NOT INITIAL.

      SELECT DISTINCT vbelv ,posnv,  vbeln, posnn, vbtyp_n , rfmng, erdat, erzet
      FROM vbfa
      INTO TABLE @DATA(lit_vbfa_2)
      FOR ALL ENTRIES IN @lit_vbfa_1
      WHERE vbelv = @lit_vbfa_1-vbeln.

      SORT lit_vbfa_2 BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lit_vbfa_2 COMPARING ALL FIELDS.

      SELECT DISTINCT vbelv ,posnv,  vbeln, posnn, vbtyp_n , rfmng, erdat, erzet
      FROM vbfa
      INTO TABLE @DATA(lit_vbfa_3)
      FOR ALL ENTRIES IN @lit_vbfa_1
      WHERE vbeln = @lit_vbfa_1-vbeln.

      SORT lit_vbfa_3 BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lit_vbfa_3 COMPARING ALL FIELDS.


      SELECT DISTINCT vbelv ,posnv,  vbeln, posnn, vbtyp_n , rfmng, erdat, erzet
      FROM vbfa
      INTO TABLE @DATA(lit_vbfa_4)
      FOR ALL ENTRIES IN @lit_vbfa_1
      WHERE vbelv = @lit_vbfa_1-vbelv.

      SORT lit_vbfa_4 BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lit_vbfa_4 COMPARING ALL FIELDS.

      IF lit_vbfa_2[] IS NOT INITIAL.

        SELECT DISTINCT vbelv ,posnv,  vbeln, posnn, vbtyp_n , rfmng, erdat, erzet
        FROM vbfa
        INTO TABLE @DATA(lit_vbfa_5)
        FOR ALL ENTRIES IN @lit_vbfa_2
        WHERE vbelv = @lit_vbfa_2-vbelv.

        SORT lit_vbfa_5 BY vbeln.
        DELETE ADJACENT DUPLICATES FROM lit_vbfa_5 COMPARING ALL FIELDS.

        LOOP AT  lit_vbfa_2 INTO DATA(w_vbfa_2).
          APPEND VALUE #( refkey = w_vbfa_2-vbeln ) TO it_refkey.
        ENDLOOP.

        SELECT DISTINCT refkey  docnum refitm  "*-Equalização RISE x PRD - 19.07.2023 - JT
        FROM j_1bnflin
        APPENDING TABLE t_saida_j_1bnflin
          FOR ALL ENTRIES IN it_refkey
        WHERE refkey = it_refkey-refkey.

        SORT t_saida_j_1bnflin BY docnum.
        DELETE ADJACENT DUPLICATES FROM t_saida_j_1bnflin COMPARING ALL FIELDS.

        IF sy-subrc IS INITIAL.
          SELECT DISTINCT  docnum cancel candat  "*-Equalização RISE x PRD - 19.07.2023 - JT
          FROM j_1bnfdoc
          APPENDING TABLE t_saida_j_1bnfdoc
          FOR ALL ENTRIES IN t_saida_j_1bnflin
          WHERE docnum = t_saida_j_1bnflin-docnum.

          SORT t_saida_j_1bnfdoc BY docnum.
          DELETE ADJACENT DUPLICATES FROM  t_saida_j_1bnfdoc COMPARING ALL FIELDS.

        ENDIF.

      ENDIF.
    ENDIF.

    lit_zsdt0053_aux = t_saida_zsdt0053[].

    SORT lit_zsdt0053_aux BY doc_precedente.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt0053_aux COMPARING doc_precedente.

    SELECT DISTINCT vbelv ,posnv,  vbeln, posnn, vbtyp_n , rfmng, erdat, erzet
    FROM vbfa
    INTO TABLE @DATA(lit_vbfa_6)
    FOR ALL ENTRIES IN @lit_zsdt0053_aux
    WHERE vbelv = @lit_zsdt0053_aux-doc_precedente.

    SORT lit_vbfa_6 BY vbeln.
    DELETE ADJACENT DUPLICATES FROM  lit_vbfa_6 COMPARING ALL FIELDS.


    lit_zsdt0053_aux = t_saida_zsdt0053[].

    SORT lit_zsdt0053_aux BY werks.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt0053_aux COMPARING werks.

    SELECT DISTINCT werks j_1bbranch
    FROM   t001w
    APPENDING TABLE t_saida_t001w
      FOR ALL ENTRIES IN lit_zsdt0053_aux
    WHERE werks = lit_zsdt0053_aux-werks.

    SORT t_saida_t001w BY werks.
    DELETE ADJACENT DUPLICATES FROM  t_saida_t001w COMPARING ALL FIELDS.

    lit_zsdt0053_aux = t_saida_zsdt0053[].

    SORT lit_zsdt0053_aux BY charg.
    DELETE ADJACENT DUPLICATES FROM lit_zsdt0053_aux COMPARING charg.

    SELECT DISTINCT *
    FROM zib_prod_crushf
    APPENDING TABLE t_saida_zib_prod_crushf
    FOR ALL ENTRIES IN lit_zsdt0053_aux
    WHERE safra  EQ lit_zsdt0053_aux-charg.

    SORT t_saida_zib_prod_crushf BY safra.
    DELETE ADJACENT DUPLICATES FROM  t_saida_zib_prod_crushf COMPARING ALL FIELDS.

*---> 30/05/2023 - Migração S4 - JS

*    LOOP AT lit_vbfa_1 INTO DATA(w_lit_vbfa_1).
*      APPEND w_lit_vbfa_1 TO t_saida_vbfa.
*    ENDLOOP.
*    LOOP AT lit_vbfa_2 INTO DATA(w_lit_vbfa_2).
*      APPEND w_lit_vbfa_2 TO t_saida_vbfa.
*    ENDLOOP.
*    LOOP AT lit_vbfa_3 INTO DATA(w_lit_vbfa_3).
*      APPEND w_lit_vbfa_3 TO t_saida_vbfa.
*    ENDLOOP.
*    LOOP AT lit_vbfa_4 INTO DATA(w_lit_vbfa_4).
*      APPEND w_lit_vbfa_4 TO t_saida_vbfa.
*    ENDLOOP.
*    LOOP AT lit_vbfa_5 INTO DATA(w_lit_vbfa_5).
*      APPEND w_lit_vbfa_5 TO t_saida_vbfa.
*    ENDLOOP.
*    LOOP AT lit_vbfa_6 INTO DATA(w_lit_vbfa_6).
*      APPEND w_lit_vbfa_6 TO t_saida_vbfa.
*    ENDLOOP.

    DATA: wa_zsaida_vbfa TYPE zsaida_vbfa.

    CLEAR: wa_zsaida_vbfa.

    LOOP AT lit_vbfa_1 INTO DATA(w_lit_vbfa_1).
      MOVE-CORRESPONDING w_lit_vbfa_1 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
    ENDLOOP.

    CLEAR: wa_zsaida_vbfa.

    LOOP AT lit_vbfa_2 INTO DATA(w_lit_vbfa_2).
      MOVE-CORRESPONDING w_lit_vbfa_2 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
      CLEAR: wa_zsaida_vbfa.
    ENDLOOP.

    LOOP AT lit_vbfa_3 INTO DATA(w_lit_vbfa_3).
      MOVE-CORRESPONDING w_lit_vbfa_3 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
      CLEAR: wa_zsaida_vbfa.
    ENDLOOP.

    LOOP AT lit_vbfa_4 INTO DATA(w_lit_vbfa_4).
      MOVE-CORRESPONDING w_lit_vbfa_4 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
      CLEAR: wa_zsaida_vbfa.
    ENDLOOP.

    LOOP AT lit_vbfa_5 INTO DATA(w_lit_vbfa_5).
      MOVE-CORRESPONDING w_lit_vbfa_5 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
      CLEAR: wa_zsaida_vbfa.
    ENDLOOP.

    LOOP AT lit_vbfa_6 INTO DATA(w_lit_vbfa_6).
      MOVE-CORRESPONDING w_lit_vbfa_6 TO wa_zsaida_vbfa.
      APPEND wa_zsaida_vbfa TO t_saida_vbfa.
      CLEAR: wa_zsaida_vbfa.
    ENDLOOP.
*<--- 30/05/2023 - Migração S4 - JS

    SORT t_saida_vbfa.
    DELETE ADJACENT DUPLICATES FROM t_saida_vbfa COMPARING ALL FIELDS.

    IF t_saida_vbfa[] IS NOT INITIAL.

      DATA(lit_vbfa_aux) = t_saida_vbfa[].
      SORT lit_vbfa_aux BY vbeln.
      DELETE ADJACENT DUPLICATES FROM lit_vbfa_aux COMPARING vbeln.

      SELECT DISTINCT vbeln posnr ntgew kwmeng zmeng
        FROM vbap APPENDING TABLE t_saida_vbap
         FOR ALL ENTRIES IN lit_vbfa_aux
       WHERE vbeln = lit_vbfa_aux-vbeln.

      SORT t_saida_vbap BY vbeln.
      DELETE ADJACENT DUPLICATES FROM  t_saida_vbap COMPARING ALL FIELDS.

    ENDIF.

  ENDIF.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0059
*------------------------------------------------------------------------------------------------------------------------*

  SELECT DISTINCT *
   FROM zsdt0059
   APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0059
    FOR ALL ENTRIES IN lit_sol_proc
   WHERE nro_sol_ov = lit_sol_proc-nro_sol_ov.

  SORT t_saida_zsdt0059 BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM  t_saida_zsdt0059 COMPARING ALL FIELDS.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0084
*------------------------------------------------------------------------------------------------------------------------*

  SELECT DISTINCT *
   FROM zsdt0084
   APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0084
    FOR ALL ENTRIES IN lit_sol_proc
   WHERE nro_sol_ov = lit_sol_proc-nro_sol_ov.

  SORT t_saida_zsdt0084 BY nro_sol_ov.
  DELETE ADJACENT DUPLICATES FROM  t_saida_zsdt0084 COMPARING ALL FIELDS.

*------------------------------------------------------------------------------------------------------------------------*
*  Dados Tabelas ZSDT0076
*------------------------------------------------------------------------------------------------------------------------*

  SELECT DISTINCT *
   FROM zsdt0076
    APPENDING CORRESPONDING FIELDS OF TABLE t_saida_zsdt0076.

  SORT t_saida_zsdt0076 BY id_bolsa matnr nr_mes.
  DELETE ADJACENT DUPLICATES FROM t_saida_zsdt0076 COMPARING ALL FIELDS.

*------------------------------------------------------------------------------------------------------------------------*
*  Seleçao Auxiliares
*------------------------------------------------------------------------------------------------------------------------*

  IF lt_rg_t_vbeln_ov IS NOT INITIAL.
    SELECT DISTINCT vbeln  posnr ntgew kwmeng zmeng
    FROM vbap
    APPENDING CORRESPONDING FIELDS OF TABLE t_saida_vbap
    WHERE vbeln IN lt_rg_t_vbeln_ov.

    SORT  t_saida_vbap BY vbeln.
    DELETE ADJACENT DUPLICATES FROM  t_saida_vbap COMPARING ALL FIELDS.

  ENDIF.

  IF t_saida_vbap[] IS NOT INITIAL.
    SORT t_saida_vbap.
    DELETE ADJACENT DUPLICATES FROM t_saida_vbap COMPARING ALL FIELDS.
  ENDIF.

  IF t_saida_vbfa[] IS NOT INITIAL.
    LOOP AT t_saida_vbfa[] ASSIGNING FIELD-SYMBOL(<wa_vbfa>).
      IF <wa_vbfa>-vbtyp_n EQ 'O' AND <wa_vbfa>-vbtyp_v EQ 'H'.
        SELECT SINGLE doc~cancel
          FROM j_1bnflin AS lin
          INNER JOIN j_1bnfdoc AS doc ON lin~docnum = doc~docnum
          INTO @DATA(v_cancelada)
          WHERE lin~refkey EQ @<wa_vbfa>-vbeln AND
                lin~refitm EQ @<wa_vbfa>-posnn AND
                doc~cancel EQ 'X'.
        IF sy-subrc IS INITIAL.
          <wa_vbfa>-nf_cancel = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


**optional, you can specify at most 3 fields for search.
  "/afl/set_custom_fields '2020' '1001' '2000000001'.

**optional, you can save a status code and message text for search.
  "/afl/set_status 'S' 'message'.

**save logs. It should be always on the bottom of the FUNCTION.
  /afl/save.


ENDFUNCTION.
