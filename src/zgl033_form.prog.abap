*----------------------------------------------------------------------*
***INCLUDE ZGL033_FORM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis .

  DATA: wl_bukrs(50)   TYPE c,
        wl_laufd(50)   TYPE c,
        wl_laufi(50)   TYPE c,
        wl_lifnr(50)   TYPE c,

        wl_layout1(20) VALUE 'Empresa:',
        wl_layout2(20) VALUE '',
        wl_layout3(20) VALUE '',
        wl_layout4(20) VALUE '',

        wl_data        VALUE '.',
        wl_space       VALUE '-',
        wl_ate(3)      VALUE 'até',

        wl_butxt       TYPE butxt.

  REFRESH: t_top.

  SELECT SINGLE butxt
    FROM t001 INTO wl_butxt
   WHERE bukrs IN s_bukrs.

  CONCATENATE wl_layout1 s_bukrs+3(4) wl_space wl_butxt  INTO wl_bukrs SEPARATED BY space.

  PERFORM f_construir_cabecalho USING 'H' TEXT-003.

  PERFORM f_construir_cabecalho USING 'S' wl_bukrs.

  v_report = sy-repid.
  gs_variant-report      = sy-repid.

ENDFORM.


FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_hotspot).

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
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-ddictxt       = 'L'.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_carregar_eventos USING name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " F_CARREGAR_EVENTOS

FORM definir_eventos .
  PERFORM f_carregar_eventos USING: slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.

FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE

FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN: '&IC1'.

      IF ( rs_selfield-fieldname EQ 'SEQ_LCTO').

        CLEAR: wa_saida.
        READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

        CHECK wa_saida-seq_lcto IS NOT INITIAL.

        SET PARAMETER ID 'SLCTO' FIELD wa_saida-seq_lcto.
        TRY.
            CALL TRANSACTION 'ZGL047' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
          CATCH cx_sy_authorization_error.
        ENDTRY.

      ELSEIF ( rs_selfield-fieldname EQ 'REF_SEQ_LCTO' ).

        CLEAR: wa_saida.
        READ TABLE it_saida INTO wa_saida INDEX rs_selfield-tabindex.

        CHECK wa_saida-ref_seq_lcto IS NOT INITIAL.

        SET PARAMETER ID 'SLCTO' FIELD wa_saida-ref_seq_lcto.
        TRY.
            CALL TRANSACTION 'ZGL047' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN .
          CATCH cx_sy_authorization_error.
        ENDTRY.

      ENDIF.

    WHEN: 'RESAPOXLS'.
      IF it_saida_exp[] IS NOT INITIAL.
        PERFORM gera_arquivo_excel.
      ENDIF.

    WHEN: 'RESAPOPDF'.

      PERFORM fm_totaliza_saldo.

      IF it_saida_exp[] IS NOT INITIAL.
        PERFORM z_call_form.
      ENDIF.

  ENDCASE.

ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  Z_CALL_FORM                                              *
*&---------------------------------------------------------------------*
*                            Chama Formulário                          *
*----------------------------------------------------------------------*
FORM z_call_form.

  DATA: vl_formname    TYPE tdsfname,
        vl_name        TYPE rs38l_fnam,
        vl_dt_base(10) TYPE c.

  DATA: wl_butxt TYPE butxt.
  "LP -  USER STORY 75852 Logo para empresas distintas
  " vl_formname = 'ZGLF0001'.
  vl_formname = 'ZGLF0002'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF p_comp IS NOT INITIAL.
    vl_dt_base = p_comp(2) && '/' && p_comp+2(4).
  ELSE.
    vl_dt_base = 'TODOS'.
  ENDIF.

  SELECT SINGLE butxt
    FROM t001 INTO wl_butxt
   WHERE bukrs IN s_bukrs.

  CALL FUNCTION vl_name
    EXPORTING
      p_butxt          = wl_butxt
      p_dt_base        = vl_dt_base
      lv_burks         = s_bukrs-low " USER STORY 75852
    TABLES
      t_zsgl0001       = it_saida_exp
      t_zsgl0001_f     = it_saida_exp_f
      t_zsgl0001_g     = it_saida_exp_g
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.


  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_CALL_FORM

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .

  DATA: wl_zib_chv TYPE zib_contabil_chv,
        vl_objkey  TYPE zib_contabil_chv-obj_key.

  CLEAR: vg_not_found, vg_dt_aprop_ini, vg_dt_aprop_fim.

  REFRESH: tg_050, tg_068, tg_073, tg_anla.


  IF sy-calld NE 'X'.
    "Check de permissão de visão
    AUTHORITY-CHECK OBJECT 'ZGL056'
      ID 'BUKRS' FIELD s_bukrs-low.

    IF sy-subrc <> 0.
      SELECT SINGLE *
        FROM user_addrp INTO @DATA(wl_user)
       WHERE bname = @sy-uname.

      IF ( sy-subrc = 0 ) AND ( wl_user-name_first IS NOT INITIAL ).
        MESSAGE | { wl_user-name_first }, seu perfil está sem acesso as apólices da empresa selecionada! | TYPE 'S'.
      ELSE.
        MESSAGE | Perfil do usuário sem acesso as apólices da empresa selecionada! | TYPE 'S'.
      ENDIF.

      vg_not_found = 'X'.
      EXIT.
    ENDIF.
  ENDIF.

*----------------------------------------------------------*
*  Valida Competência
*----------------------------------------------------------*

  PERFORM valida_competencia USING p_comp
                          CHANGING vg_not_found
                                   vg_dt_aprop_ini
                                   vg_dt_aprop_fim.

  CHECK vg_not_found IS INITIAL.




*----------------------------------------------------------*
*  Seleção Dados Cabeçalho
*----------------------------------------------------------*

  "Recuperar Ano Archive
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_archive)
   WHERE name EQ 'MAGGI_DATA_ARCHIVE'.

  DATA(lva_data_archive) = '19001231'.
  IF sy-subrc EQ 0 AND lwa_archive-low IS NOT INITIAL.
    lva_data_archive = lwa_archive-low.
  ENDIF.

  SELECT *
    FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE tg_050
   WHERE bukrs        IN s_bukrs
     AND vig_de       IN s_vgde
     AND vig_ate      IN s_vgate
     AND cod_seguradora IN s_lifnr
     AND seq_tipo     IN s_stipo
     AND tp_opr       IN s_tpopr
     AND seq_lcto     IN s_slcto
     AND nro_apolice  IN s_nr_apl
     AND loekz        EQ ''.

*----------------------------------------------------------*
*  Referencia Filha REF_SEQ_LCTO
*----------------------------------------------------------*

  lr_seqlcto[] = VALUE #( FOR v1 IN tg_050 ( option = 'EQ' sign = 'I' low = v1-seq_lcto ) ).

  SELECT DISTINCT f~*
    FROM zglt050 AS p
  INNER JOIN zglt050 AS f ON f~seq_lcto = p~ref_seq_lcto
  WHERE p~seq_lcto IN @lr_seqlcto
   INTO CORRESPONDING FIELDS OF TABLE @tg_050_ref.
  SORT lr_seqlcto BY low.
  DELETE ADJACENT DUPLICATES FROM lr_seqlcto COMPARING ALL FIELDS.
**********************************************************************
  DELETE tg_050 WHERE vig_de <= lva_data_archive.

  IF vg_dt_aprop_fim IS NOT INITIAL AND vg_dt_aprop_fim NE '00000000'.
    DELETE tg_050 WHERE dt_lcto_ctb > vg_dt_aprop_fim.
  ENDIF.

  IF p_sald IS NOT INITIAL.
    DELETE tg_050 WHERE tp_opr NE 'N' AND tp_opr NE 'E' AND tp_opr NE 'P' .
  ENDIF.

  IF tg_050[] IS INITIAL.
    vg_not_found = 'X'.
    RETURN.
  ENDIF.

*----------------------------------------------------------*
*  Seleção de Tipos
*----------------------------------------------------------*
  SELECT *
    FROM zglt064 INTO CORRESPONDING FIELDS OF TABLE tg_064
     FOR ALL ENTRIES IN tg_050
   WHERE seq_tipo EQ tg_050-seq_tipo.


  SELECT *
    FROM zglt032 INTO CORRESPONDING FIELDS OF TABLE tg_032
    FOR ALL ENTRIES IN tg_064
    WHERE tp_lcto EQ tg_064-tp_lcto_aprop.

*----------------------------------------------------------*
*  Seleção Dados Bens Segurados
*----------------------------------------------------------*

  SELECT *
    FROM zglt068 INTO CORRESPONDING FIELDS OF TABLE tg_068
     FOR ALL ENTRIES IN tg_050
   WHERE seq_lcto EQ tg_050-seq_lcto
     AND werks    IN s_werks
     AND anln1    IN s_anln1
     AND matnr    IN s_matnr
     AND descr_bens IN s_dsbem.

*----------------------------------------------------------*
*  Seleção Dados Mestres Imobilizado
*----------------------------------------------------------*
  IF tg_068[] IS NOT INITIAL.

    LOOP AT tg_068.
      READ TABLE tg_050 WITH KEY seq_lcto = tg_068-seq_lcto.
      CHECK sy-subrc = 0.
      tg_068-bukrs = tg_050-bukrs.
      MODIFY tg_068.
    ENDLOOP.


    SELECT *
      FROM anla INTO TABLE tg_anla
      FOR ALL ENTRIES IN tg_068
     WHERE bukrs = tg_068-bukrs
       AND anln1 = tg_068-anln1
       AND anln2 = tg_068-anln2.
  ENDIF.

*----------------------------------------------------------*
*  Seleção Apolices de Baixa e seus bens
*----------------------------------------------------------*
  SELECT *
    FROM zglt050 INTO TABLE tg_050_bx
     FOR ALL ENTRIES IN tg_050
   WHERE tp_opr = 'B'
     AND ref_seq_lcto = tg_050-seq_lcto
     AND loekz        EQ ''.

  SELECT *
    FROM zglt068 INTO CORRESPONDING FIELDS OF TABLE tg_068_bx
     FOR ALL ENTRIES IN tg_050_bx
   WHERE seq_lcto EQ tg_050_bx-seq_lcto.

*----------------------------------------------------------*
*  Seleção Dados Apropriações
*----------------------------------------------------------*
  SELECT *
    FROM zglt073 INTO CORRESPONDING FIELDS OF TABLE tg_073
     FOR ALL ENTRIES IN tg_050
   WHERE seq_lcto EQ tg_050-seq_lcto.



  LOOP AT tg_073.

    DATA(_del) = ''.

    CLEAR: wl_zib_chv.
    CONCATENATE 'ZGL17' tg_073-doc_lcto tg_073-dt_apropr(4) INTO vl_objkey.

    SELECT SINGLE *
      FROM zib_contabil_chv INTO wl_zib_chv
     WHERE obj_key = vl_objkey.

    IF ( sy-subrc = 0 ) AND ( wl_zib_chv-belnr IS NOT INITIAL ).
      tg_073-belnr = wl_zib_chv-belnr.

      SELECT SINGLE *
        FROM bkpf INTO @DATA(_wl_bkpf)
       WHERE bukrs = @wl_zib_chv-bukrs
         AND belnr = @wl_zib_chv-belnr.

      IF ( sy-subrc NE 0 ).
        _del = 'X'.
      ELSE.
        IF ( _wl_bkpf-stblg IS NOT INITIAL ).
          SELECT SINGLE *
            FROM bkpf INTO @DATA(_wl_bkpf_estorno)
           WHERE bukrs = @_wl_bkpf-bukrs
             AND belnr = @_wl_bkpf-stblg.

          IF ( sy-subrc = 0 ) AND _wl_bkpf_estorno-budat >= '20180226'.
            DELETE FROM zglt073 WHERE seq_lcto = tg_073-seq_lcto
                                  AND nro_parc = tg_073-nro_parc
                                  AND doc_lcto = tg_073-doc_lcto.
            _del = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ( vg_dt_aprop_fim IS NOT INITIAL ) AND ( tg_073-dt_apropr > vg_dt_aprop_fim ).
        _del = 'X'.
      ENDIF.

    ELSE.
      _del = 'X'.
    ENDIF.

    IF _del IS NOT INITIAL.
      DELETE tg_073.
    ENDIF.

  ENDLOOP.

  LOOP AT tg_068.
    CHECK tg_068-doc_lcto_ajus IS NOT INITIAL.

    CLEAR: wl_zib_chv.
    CONCATENATE 'ZGL17' tg_068-doc_lcto_ajus tg_068-dt_lcto_ctb_ajus(4) INTO vl_objkey.

    SELECT SINGLE *
      FROM zib_contabil_chv INTO wl_zib_chv
     WHERE obj_key = vl_objkey.

    IF ( sy-subrc = 0 ) AND ( wl_zib_chv-belnr IS NOT INITIAL ).

      IF ( vg_dt_aprop_fim IS NOT INITIAL ) AND ( tg_068-dt_lcto_ctb_ajus > vg_dt_aprop_fim ).
        tg_068-vlr_aprop_aj_usd = 0.
        tg_068-vlr_aprop_aj_brl = 0.
        MODIFY tg_068.
      ENDIF.

    ELSE.
      tg_068-vlr_aprop_aj_usd = 0.
      tg_068-vlr_aprop_aj_brl = 0.
      MODIFY tg_068.
    ENDIF.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_dados .

  DATA: wl_color    TYPE kkblo_specialcol,
        vl_col      LIKE wl_color-color-col,
        vl_dt_baixa TYPE zglt068-dt_baixa,
        vvig_de     TYPE c LENGTH 6.

  LOOP AT tg_068.
    CLEAR: wa_saida, tg_050, tg_064, wl_color, vl_col,vl_dt_baixa, tg_anla.

    READ TABLE tg_050 WITH KEY seq_lcto = tg_068-seq_lcto.
    CHECK sy-subrc = 0.

    READ TABLE tg_064 WITH KEY seq_tipo = tg_050-seq_tipo.
    CHECK sy-subrc = 0.

    READ TABLE tg_anla WITH KEY bukrs = tg_068-bukrs
                                anln1 = tg_068-anln1
                                anln2 = tg_068-anln2.

    wa_saida-nro_apolice      = tg_050-nro_apolice.
    wa_saida-vig_de           = tg_050-vig_de.
    wa_saida-vig_ate          = tg_050-vig_ate.
    wa_saida-seq_lcto         = tg_050-seq_lcto.
    wa_saida-seq_tipo         = tg_050-seq_tipo.
    wa_saida-descr_tipo       = tg_064-descr.
    wa_saida-tp_opr           = tg_050-tp_opr.
    wa_saida-nr_item          = tg_068-nr_item.
    wa_saida-bukrs            = tg_050-bukrs.
    wa_saida-werks            = tg_068-werks.
    wa_saida-anln1            = tg_068-anln1.
    wa_saida-anln2            = tg_068-anln2.
    wa_saida-invnr            = tg_068-invnr.
    wa_saida-sernr            = tg_068-sernr.
    wa_saida-matnr            = tg_068-matnr.
    wa_saida-descr_bens       = tg_068-descr_bens.
    wa_saida-txa50            = tg_anla-txa50.
    wa_saida-zugdt            = tg_anla-zugdt.
    wa_saida-kostl            = tg_068-kostl.
    wa_saida-wkurs            = tg_068-wkurs.
    wa_saida-vlr_premio_usd   = tg_068-vlr_premio_usd.
    wa_saida-vlr_premio_brl   = tg_068-vlr_premio_brl.
    wa_saida-vlr_risco_usd    = tg_068-vlr_risco_usd.
    wa_saida-vlr_risco_brl    = tg_068-vlr_risco_brl.
    wa_saida-dt_lcto_ctb      = tg_050-dt_lcto_ctb.
    wa_saida-aufnr            = tg_068-aufnr.
    wa_saida-vornr            = tg_068-vornr.
    wa_saida-ref_seq_lcto     = tg_050-ref_seq_lcto.

    IF wa_saida-tp_opr NE 'B'. "Diferente Baixa

      SELECT SINGLE a~dt_baixa INTO vl_dt_baixa
        FROM zglt068 AS a INNER JOIN zglt050 AS b ON a~seq_lcto = b~seq_lcto
       WHERE b~tp_opr       = 'B' "Baixa.
         AND b~ref_seq_lcto = wa_saida-seq_lcto
         AND a~nr_item      = wa_saida-nr_item
         AND a~dt_baixa     NE '00000000'.

      IF ( sy-subrc = 0 ) AND ( vl_dt_baixa IS NOT INITIAL ).
        wa_saida-dt_baixa = vl_dt_baixa.
      ENDIF.
    ELSE.
      wa_saida-dt_baixa  = tg_068-dt_baixa.
    ENDIF.

    LOOP AT tg_073 WHERE seq_lcto   = tg_068-seq_lcto
                     AND nr_item    = tg_068-nr_item.
      ADD tg_073-vlr_premio_usd TO wa_saida-aprop_usd.
      ADD tg_073-vlr_premio_brl TO wa_saida-aprop_brl.

      ADD 1 TO wa_saida-nro_aprop.
    ENDLOOP.

    wa_saida-vlr_aprop_aj_usd = tg_068-vlr_aprop_aj_usd.
    wa_saida-vlr_aprop_aj_brl = tg_068-vlr_aprop_aj_brl.

    LOOP AT tg_050_bx WHERE ref_seq_lcto = tg_068-seq_lcto.
      LOOP AT tg_068_bx WHERE seq_lcto   = tg_050_bx-seq_lcto
                          AND nr_item    = tg_068-nr_item.

        IF ( vg_dt_aprop_fim IS NOT INITIAL ) AND ( tg_068_bx-dt_baixa > vg_dt_aprop_fim ).
          CONTINUE.
        ENDIF.

        ADD tg_068_bx-vlr_premio_usd TO wa_saida-vlr_bx_usd.
        ADD tg_068_bx-vlr_premio_brl TO wa_saida-vlr_bx_brl.
      ENDLOOP.
    ENDLOOP.

    IF wa_saida-vlr_bx_brl IS INITIAL.
      CLEAR: wa_saida-dt_baixa.
    ENDIF.

    wa_saida-saldo_usd = wa_saida-vlr_premio_usd - ( ( wa_saida-aprop_usd - wa_saida-vlr_aprop_aj_usd ) + wa_saida-vlr_bx_usd ).
    wa_saida-saldo_brl = wa_saida-vlr_premio_brl - ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ).

    IF wa_saida-vlr_premio_brl > 0.
      wa_saida-perc_aprop = ( ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ) / wa_saida-vlr_premio_brl ) * 100. "#EC CI_FLDEXT_OK[2610650]
    ENDIF.
    IF p_sald IS NOT INITIAL AND wa_saida-perc_aprop >= 100.
      CLEAR wa_saida.
      CONTINUE.
    ENDIF.



    PERFORM f_calcula_intervalo_data USING wa_saida-vig_de
                                           wa_saida-vig_ate
                                  CHANGING wa_saida-meses.
    IF wa_saida-meses > 0.
      wa_saida-vlr_aprop_mes_usd =  wa_saida-vlr_premio_usd / wa_saida-meses.
      wa_saida-vlr_aprop_mes_brl =  wa_saida-vlr_premio_brl / wa_saida-meses.
    ENDIF.

    IF ( wa_saida-perc_aprop = 0 ).
      vl_col = 0.
    ELSEIF  ( wa_saida-perc_aprop > 0  ) AND ( wa_saida-perc_aprop <= 50 ).
      vl_col = 3.
    ELSEIF  ( wa_saida-perc_aprop > 50 ) AND ( wa_saida-perc_aprop <= 99 ).
      vl_col = 4.
    ELSEIF  ( wa_saida-perc_aprop = 100 ).
      vl_col = 5.
    ENDIF.

    LOOP AT tg_032 WHERE tp_lcto = tg_064-tp_lcto_aprop. "2000015278 - FT - 07.08.2024

      SELECT SINGLE * FROM skb1 INTO @DATA(wa_skb1) "#EC CI_DB_OPERATION_OK[2431747]
         WHERE saknr EQ @tg_032-hkont
          AND  bukrs EQ @tg_050-bukrs.

      SELECT SINGLE *  FROM ska1 INTO @DATA(wa_sna1) "#EC CI_DB_OPERATION_OK[2431747]
      WHERE saknr EQ @wa_skb1-saknr.   "#EC CI_DB_OPERATION_OK[2389136]

      SELECT SINGLE * FROM t077z INTO @DATA(wa_t077z)
      WHERE ktopl EQ @wa_sna1-ktopl
        AND ktoks EQ @wa_sna1-ktoks
        AND spras EQ @sy-langu.

      IF wa_t077z-ktoks = 'YB06'.
        wa_saida-hkont = tg_032-hkont.
      ENDIF.

    ENDLOOP.

    wl_color-fieldname = 'PERC_APROP'.
    wl_color-color-col = vl_col.
    wl_color-color-inv = vl_col.
    APPEND wl_color TO wa_saida-color.


**********************************************************************
* 108333 CS2023000257 ZGL056 - Inclusão de coluna com quantidade de meses de vigencia PSA
**********************************************************************
    CLEAR: qtd_dias, qtd_meses.
**********************************************************************
* Se tiver Sequencia Endoso e for diferente de B (Baixa)
**********************************************************************

    CLEAR: qtd_dias,qtd_meses.

    IF tg_050-ref_seq_lcto <> '0000000000' AND tg_050-tp_opr <> 'B'.

      READ TABLE tg_050_ref INTO DATA(tg_050_aux) WITH KEY seq_lcto = tg_050-ref_seq_lcto.

      qtd_dias = tg_050_aux-vig_ate - tg_050_aux-vig_de.
      CONDENSE qtd_dias NO-GAPS.
      wa_saida-qtde_dias_vig      = qtd_dias.
      PERFORM calcula_meses_dias.

      wa_saida-qtde_meses_vig      = qtd_meses.

    ELSE.

      qtd_dias = tg_050-vig_ate - tg_050-vig_de.
      CONDENSE qtd_dias NO-GAPS.
      wa_saida-qtde_dias_vig      = qtd_dias.

      PERFORM calcula_meses_dias.
      wa_saida-qtde_meses_vig      = qtd_meses.


    ENDIF.

**********************************************************************


    CONCATENATE tg_050-vig_de+0(4) tg_050-vig_de+4(2) INTO vvig_de.

    DATA: comp(6) TYPE c.

    IF p_comp IS NOT INITIAL.
      CONCATENATE p_comp+2(4) p_comp+0(2) INTO comp.

      IF vvig_de <= comp.
        wa_saida-tipo_competencia = 'Em Vigência'.
      ELSE.
        wa_saida-tipo_competencia = 'Vigência Futura'.
      ENDIF.
    ENDIF.

*    IF p_comp IS NOT INITIAL.
*
*      CONCATENATE p_comp+2(4) p_comp+0(2) INTO comp.
*
*      IF vvig_de <= comp.
*        APPEND wa_saida TO it_saida.
**        PERFORM f_insert_tab_xls_pdf.
*      ENDIF.
*    ELSE.
    APPEND wa_saida TO it_saida.
*      PERFORM f_insert_tab_xls_pdf.
*    ENDIF.


    CLEAR: wa_skb1, wa_sna1,wa_t077z.

  ENDLOOP.

  SORT it_saida BY nro_apolice vig_de vig_ate.

  PERFORM fm_totaliza_saldo.

ENDFORM.

FORM f_insert_tab_xls_pdf.

  IF wa_saida-tipo_competencia = 'Em Vigência'.
    CLEAR: wa_saida_exp.
    READ TABLE it_saida_exp INTO wa_saida_exp WITH KEY nro_apolice = wa_saida-nro_apolice
                                                       vig_de      = wa_saida-vig_de
                                                       vig_ate     = wa_saida-vig_ate    .
    IF sy-subrc = 0.
      vg_vlr_premio_brl    = vg_vlr_premio_brl + wa_saida-vlr_premio_brl.
      vg_vlr_bx_brl        = vg_vlr_bx_brl + wa_saida-vlr_bx_brl.
      vg_aprop_brl         = vg_aprop_brl + wa_saida-aprop_brl.
      vg_vlr_aprop_aj_brl  = vg_vlr_aprop_aj_brl + wa_saida-vlr_aprop_aj_brl.

      wa_saida_exp-perc_aprop  = ( ( ( vg_aprop_brl - vg_vlr_aprop_aj_brl ) + vg_vlr_bx_brl ) / vg_vlr_premio_brl ) * 100.
      wa_saida_exp-saldo_brl   = wa_saida_exp-saldo_brl + ( wa_saida-vlr_premio_brl - ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ) ).
      wa_saida_exp-saldo_usd   = wa_saida_exp-saldo_usd + ( wa_saida-vlr_premio_usd - ( ( wa_saida-aprop_usd - wa_saida-vlr_aprop_aj_usd ) + wa_saida-vlr_bx_usd ) ).
      MODIFY it_saida_exp FROM wa_saida_exp INDEX sy-tabix.
    ELSE.
      wa_saida_exp-nro_apolice = wa_saida-nro_apolice.
      wa_saida_exp-vig_de      = wa_saida-vig_de     .
      wa_saida_exp-vig_ate     = wa_saida-vig_ate    .
      wa_saida_exp-perc_aprop  = ( ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ) / wa_saida-vlr_premio_brl ) * 100.
      vg_vlr_premio_brl        = wa_saida-vlr_premio_brl.
      vg_vlr_bx_brl            = wa_saida-vlr_bx_brl.
      vg_aprop_brl             = wa_saida-aprop_brl.
      vg_vlr_aprop_aj_brl      = wa_saida-vlr_aprop_aj_brl.
      wa_saida_exp-saldo_brl   = wa_saida-vlr_premio_brl - ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ).
      wa_saida_exp-saldo_usd   = wa_saida-vlr_premio_usd - ( ( wa_saida-aprop_usd - wa_saida-vlr_aprop_aj_usd ) + wa_saida-vlr_bx_usd ).
      APPEND wa_saida_exp TO it_saida_exp.
    ENDIF.
  ELSE.
    CLEAR: wa_saida_exp.
    READ TABLE it_saida_exp_f INTO wa_saida_exp WITH KEY nro_apolice = wa_saida-nro_apolice
                                                      vig_de      = wa_saida-vig_de
                                                      vig_ate     = wa_saida-vig_ate    .
    IF sy-subrc = 0.
      vg_vlr_premio_brl_f      = vg_vlr_premio_brl_f + wa_saida-vlr_premio_brl.
      vg_vlr_bx_brl_f          = vg_vlr_bx_brl_f + wa_saida-vlr_bx_brl.
      vg_aprop_brl_f           = vg_aprop_brl_f + wa_saida-aprop_brl.
      vg_vlr_aprop_aj_brl_f    = vg_vlr_aprop_aj_brl_f + wa_saida-vlr_aprop_aj_brl.

      wa_saida_exp-perc_aprop  = ( ( ( vg_aprop_brl_f - vg_vlr_aprop_aj_brl_f ) + vg_vlr_bx_brl_f ) / vg_vlr_premio_brl_f ) * 100.
      wa_saida_exp-saldo_brl   = wa_saida_exp-saldo_brl + ( wa_saida-vlr_premio_brl - ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ) ).
      wa_saida_exp-saldo_usd   = wa_saida_exp-saldo_usd + ( wa_saida-vlr_premio_usd - ( ( wa_saida-aprop_usd - wa_saida-vlr_aprop_aj_usd ) + wa_saida-vlr_bx_usd ) ).
      MODIFY it_saida_exp_f FROM wa_saida_exp INDEX sy-tabix.
    ELSE.
      wa_saida_exp-nro_apolice = wa_saida-nro_apolice.
      wa_saida_exp-vig_de      = wa_saida-vig_de     .
      wa_saida_exp-vig_ate     = wa_saida-vig_ate    .
      wa_saida_exp-perc_aprop  = ( ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ) / wa_saida-vlr_premio_brl ) * 100.
      vg_vlr_premio_brl_f      = wa_saida-vlr_premio_brl.
      vg_vlr_bx_brl_f          = wa_saida-vlr_bx_brl.
      vg_aprop_brl_f           = wa_saida-aprop_brl.
      vg_vlr_aprop_aj_brl_f    = wa_saida-vlr_aprop_aj_brl.
      wa_saida_exp-saldo_brl   = wa_saida-vlr_premio_brl - ( ( wa_saida-aprop_brl - wa_saida-vlr_aprop_aj_brl ) + wa_saida-vlr_bx_brl ).
      wa_saida_exp-saldo_usd   = wa_saida-vlr_premio_usd - ( ( wa_saida-aprop_usd - wa_saida-vlr_aprop_aj_usd ) + wa_saida-vlr_bx_usd ).
      APPEND wa_saida_exp TO it_saida_exp_f.
    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERA_ARQUIVO_EXCEL
*&---------------------------------------------------------------------*
FORM gera_arquivo_excel.

  DATA: v_file     TYPE localfile.
  DATA: v_filename TYPE  string.

  DATA: BEGIN OF heading OCCURS 0,
          text(10) TYPE c,
        END OF heading.

  " gerando o arquivo em txt e salvando no disco
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ''
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = v_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF v_file IS NOT INITIAL.

    heading-text = 'Apólice'.
    APPEND heading.

    heading-text = 'Dt. Início vigência'.
    APPEND heading.

    heading-text = 'Dt. Fim vigência'.
    APPEND heading.

    heading-text = '% apropriado'.
    APPEND heading.

    heading-text = 'Valor BRL'.
    APPEND heading.

    heading-text = 'Valor USD'.
    APPEND heading.

    v_filename = v_file.
    FIND '.XLS' IN v_filename.
    IF sy-subrc NE 0.
      FIND '.XLSX' IN v_filename.
      IF sy-subrc NE 0.
        FIND '.xlsx' IN v_filename.
        IF sy-subrc NE 0.
          FIND '.xls' IN v_filename.
          IF sy-subrc NE 0.
            CONCATENATE v_filename '.XLS' INTO v_filename.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    LOOP AT it_saida_exp INTO wa_saida_exp.
      MOVE-CORRESPONDING wa_saida_exp TO wa_saida_exc.
      REPLACE '.' WITH ',' INTO wa_saida_exc-saldo_brl.
      REPLACE '.' WITH ',' INTO wa_saida_exc-saldo_usd.
      APPEND wa_saida_exc TO it_saida_exc.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = v_filename
        filetype                = 'ASC'
        write_field_separator   = '#'
        show_transfer_status    = abap_true
      TABLES
        data_tab                = it_saida_exc
        fieldnames              = heading
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_dados .

  DATA: wl_layout TYPE slis_layout_alv.

  PERFORM definir_eventos.
  PERFORM montar_layout.

  wl_layout-colwidth_optimize = 'X'.
  wl_layout-coltab_fieldname = 'COLOR'.
  wl_layout-edit_mode = 'A'.
  "WL_LAYOUT-EDIT = 'X'.

*-CS2022000122 - 07.03.2022 - JT - inicio
  IF p_call = abap_true.
    MOVE-CORRESPONDING it_saida[] TO it_saida2[].

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = v_report
        is_variant               = gs_variant_c
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        it_fieldcat              = estrutura[]
        is_layout                = wl_layout
        i_save                   = 'X'
        it_events                = events
        is_print                 = t_print
      TABLES
        t_outtab                 = it_saida2.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = v_report
        is_variant               = gs_variant_c
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        it_fieldcat              = estrutura[]
        is_layout                = wl_layout
        i_save                   = 'X'
        it_events                = events
        is_print                 = t_print
      TABLES
        t_outtab                 = it_saida.
  ENDIF.
*-CS2022000122 - 07.03.2022 - JT - fim

ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.        "#EC CALLED
  SET PF-STATUS 'ZSTANDARD_FULLSCREEN'.
ENDFORM. "Set_pf_status
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .

  PERFORM montar_estrutura USING:


  01   'ZGLT050'  'NRO_APOLICE'       'IT_SAIDA' 'NRO_APOLICE'          'Nro. Apólice'       '' '',
  02   'ZGLT050'  'VIG_DE'            'IT_SAIDA' 'VIG_DE'               'Dt.Ini.Vig.'        '' '',
  03   'ZGLT050'  'VIG_ATE'           'IT_SAIDA' 'VIG_ATE'              'Dt.Fim.Vig.'        '' '',
  03   'ZGLT050'  'DT_LCTO_CTB'       'IT_SAIDA' 'DT_LCTO_CTB'          'Dt.Lcto.Contábil.'  '' '',
  03   ''         ''                  'IT_SAIDA' 'QTDE_MESES_VIG'       'Meses '             '' '', "Qtd Diferença em Dias PSA
  03   ''         ''                  'IT_SAIDA' 'QTDE_DIAS_VIG'        'Dias  '             '' '',
  04   'ZGLT050'  'SEQ_LCTO'          'IT_SAIDA' 'SEQ_LCTO'             'Seq.Lcto'           '' 'X',
  04   'ZGLT050'  'REF_SEQ_LCTO'      'IT_SAIDA' 'REF_SEQ_LCTO'         'Ref.Seq.Lcto'       '' 'X',
  04   'ZGLT050'  'SEQ_TIPO'          'IT_SAIDA' 'SEQ_TIPO'             'Tipo'               '' '',
  04   'ZGLT064'  'DESCR'             'IT_SAIDA' 'DESCR_TIPO'           'Desc.Tp.'           '' '',
  04   'ZGLT050'  'TP_OPR'            'IT_SAIDA' 'TP_OPR'               'Tipo Opr.'          '' '',
  05   'ZGLT068'  'NR_ITEM'           'IT_SAIDA' 'NR_ITEM'              'Item'               '' '',
  06   'ZGLT050'  'BUKRS'             'IT_SAIDA' 'BUKRS'                'Empresa'            '' '',
  07   'ZGLT068'  'WERKS'             'IT_SAIDA' 'WERKS'                'Filial'             '' '',
  08   'ZGLT068'  'ANLN1'             'IT_SAIDA' 'ANLN1'                'Imobilizado'        '' '',
  09   'ZGLT068'  'ANLN2'             'IT_SAIDA' 'ANLN2'                'Sub.Nro'            '' '',
  10   'ANLA'     'INVNR'             'IT_SAIDA' 'INVNR'                'Chassi'             '' '',
  11   'ANLA'     'SERNR'             'IT_SAIDA' 'SERNR'                'Série'              '' '',
  12   'ZGLT068'  'MATNR'             'IT_SAIDA' 'MATNR'                'Material'           '' '',
  13   'ZGLT068'  'DESCR_BENS'        'IT_SAIDA' 'DESCR_BENS'           'Descrição'          '' '',
  14   'ZGLT032'  'HKONT'             'IT_SAIDA' 'HKONT'                'Conta Contábil'     '' '',
  15   'ANLA'     'TXA50'             'IT_SAIDA' 'TXA50'                'Descr.Pt2'          '' '',
  16   'ANLA'     'ZUGDT'             'IT_SAIDA' 'ZUGDT'                'Dt.Aquisição'       '' '',
  17   'ZGLT068'  'KOSTL'             'IT_SAIDA' 'KOSTL'                'C.Custo'            '' '',
  18   'ZGLT068'  'WKURS'             'IT_SAIDA' 'WKURS'                'Tx.Câmbio'          '' '',
  19   'ZGLT068'  'VLR_PREMIO_USD'    'IT_SAIDA' 'VLR_PREMIO_USD'       'Valor Prêmio USD'   '' '', "US #179329 - MMSILVA - 16.05.2025 - Alterado de Vlr. USD para Valor Prêmio USD
  20   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'VLR_PREMIO_BRL'       'Valor Prêmio BRL'   '' '', "US #179329 - MMSILVA - 16.05.2025 - Alterado de Vlr. BRL para Valor Prêmio BRL
  21   'ZGLT068'  'DT_BAIXA'          'IT_SAIDA' 'DT_BAIXA'             'Dt.Baixa'           '' '',
  22   'ZGLT068'  'VLR_PREMIO_USD'    'IT_SAIDA' 'VLR_BX_USD'           'Vlr.Bx.USD'         '' '',
  23   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'VLR_BX_BRL'           'Vlr.Bx.BRL'         '' '',
  24   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'APROP_USD'            'Apropriado USD'     '' '',
  25   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'APROP_BRL'            'Apropriado BRL'     '' '',
  26   'ZGLT068'  'VLR_APROP_AJ_USD'  'IT_SAIDA' 'VLR_APROP_AJ_USD'     'Vlr.Aj.Aprop.USD'   '' '',
  27   'ZGLT068'  'VLR_APROP_AJ_BRL'  'IT_SAIDA' 'VLR_APROP_AJ_BRL'     'Vlr.Aj.Aprop.BRL'   '' '',
  28   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'SALDO_USD'            'Saldo USD'          '' '',
  29   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_SAIDA' 'SALDO_BRL'            'Saldo BRL'          '' '',
  30   ''         ''                  'IT_SAIDA' 'PERC_APROP'           '% Apropriado'       '' '',
  31   ''         ''                  'IT_SAIDA' 'NRO_APROP'            'Parc.Apropriadas'   '' '',
  32   'ZGLT068'  'VLR_RISCO_USD'     'IT_SAIDA' 'VLR_RISCO_USD'        'Vlr.Risco USD'      '' '',
  33   'ZGLT068'  'VLR_RISCO_BRL'     'IT_SAIDA' 'VLR_RISCO_BRL'        'Vlr.Risco BRL'      '' '',
  34   'ZGLT068'  'VLR_RISCO_BRL'     'IT_SAIDA' 'VLR_APROP_MES_USD'    'Vlr.Aprop.Mes.USD'  '' '',
  35   'ZGLT068'  'VLR_RISCO_BRL'     'IT_SAIDA' 'VLR_APROP_MES_BRL'    'Vlr.Aprop.Mes.BRL'  '' '',
  36   'ZGLT068'  'AUFNR'             'IT_SAIDA' 'AUFNR'                'Ordem'              '' '',
  36   'ZGLT068'  'VORNR'             'IT_SAIDA' 'VORNR'                'Nr.Op.'             '' '',
  37   ''         ''                  'IT_SAIDA' 'TIPO_COMPETENCIA'     'Tipo Competência'   '' ''.




ENDFORM.

FORM help_search USING p_field TYPE char30.

  DATA: vl_dyfield  TYPE help_info-dynprofld,
        vl_retfield TYPE dfies-fieldname.

  DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  CASE p_field.
    WHEN 'S_STIPO-LOW'.

      DATA: BEGIN OF gt_seq_tipo OCCURS 0,
              seq_tipo TYPE zglt064-seq_tipo,
              descr    TYPE zglt064-descr,
            END OF gt_seq_tipo.

      CLEAR gt_seq_tipo[].

      SELECT *
        FROM zglt064
        INTO CORRESPONDING FIELDS OF TABLE gt_seq_tipo.

      SORT gt_seq_tipo BY seq_tipo.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'SEQ_TIPO'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = 'S_STIPO-LOW'
          value_org       = 'S'
        TABLES
          value_tab       = gt_seq_tipo
          return_tab      = gt_return_tab
          dynpfld_mapping = gt_dselc.

    WHEN 'S_SLCTO-LOW' OR 'S_SLCTO-HIGH'.

      DATA: BEGIN OF gt_seq_lcto OCCURS 0,
              seq_lcto       TYPE zglt050-seq_lcto,
              tp_lcto        TYPE zglt050-tp_lcto,
              dep_resp       TYPE zglt050-dep_resp,
              bukrs          TYPE zglt050-bukrs,
              nro_apolice    TYPE zglt050-nro_apolice,
              cod_seguradora TYPE zglt050-cod_seguradora,
              dt_criacao     TYPE zglt050-dt_criacao,
              usnam          TYPE zglt050-usnam,
            END OF gt_seq_lcto.

      vl_retfield = 'SEQ_LCTO'.
      vl_dyfield  = p_field.

      REFRESH gt_seq_lcto.
      CLEAR gt_seq_lcto.

      SELECT seq_lcto tp_lcto dep_resp bukrs nro_apolice
             cod_seguradora dt_criacao usnam
        FROM zglt050 INTO CORRESPONDING FIELDS OF TABLE gt_seq_lcto
       WHERE loekz      EQ ''.

      SORT gt_seq_lcto BY seq_lcto.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = vl_retfield
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          dynprofield     = vl_dyfield
          value_org       = 'S'
        TABLES
          value_tab       = gt_seq_lcto
          return_tab      = gt_return_tab
          dynpfld_mapping = gt_dselc.

  ENDCASE.


ENDFORM.

FORM valida_competencia  USING    p_competencia
                         CHANGING p_return_status
                                  p_dt_ini
                                  p_dt_fim.

  DATA: vl_mes         TYPE i,
        vl_ano         TYPE i,
        vl_dt_ini(8)   TYPE c,
        vl_dt_fim(8)   TYPE c,
        vl_dt_low      TYPE sy-datum,
        vl_dt_high_in  TYPE sy-datum,
        vl_dt_high_out TYPE sy-datum.

  CLEAR: p_return_status, p_dt_ini, p_dt_fim.

  CHECK p_comp IS NOT INITIAL.

  vl_mes = p_competencia(02).
  vl_ano = p_competencia+2(04).

  IF ( vl_mes = 0 ) OR ( vl_mes > 12 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH TEXT-e01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF ( vl_ano = 0 ).
    p_return_status = 'X'.
    MESSAGE s836(sd) WITH TEXT-e02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_ini.

  CONCATENATE p_competencia+2(4) p_competencia(2) '01' INTO vl_dt_fim.

  vl_dt_low     = vl_dt_ini.
  vl_dt_high_in = vl_dt_fim.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = vl_dt_high_in
    IMPORTING
      last_day_of_month = vl_dt_high_out.

  p_dt_ini  = vl_dt_low.
  p_dt_fim  = vl_dt_high_out.

ENDFORM.

FORM f_calcula_intervalo_data USING p_ini_vig TYPE sy-datum
                                    p_fim_vig TYPE sy-datum
                           CHANGING c_meses   TYPE i.

  DATA: v_months TYPE tfmatage.

  CLEAR: c_meses, v_months.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from    = p_ini_vig
      i_date_to      = p_fim_vig
      i_flg_separate = ' '
    IMPORTING
      e_months       = v_months.

  c_meses = v_months.

ENDFORM.

FORM calcula_meses_dias.

  CLEAR: tot, tot_int, tot_dec,resultado,v_mes.
  CONDENSE qtd_dias NO-GAPS.

  DATA : p_vlr LIKE cats_its_fields-num_value.

  CALL FUNCTION 'CATS_ITS_MAKE_STRING_NUMERICAL'
    EXPORTING
      input_string  = qtd_dias
    IMPORTING
      value         = p_vlr
    EXCEPTIONS
      not_numerical = 1
      OTHERS        = 2.


  tot = p_vlr  / 30.
  tot_int = trunc( tot ).
  tot_dec = frac( tot ).

* convert meses e dias

*  v_meses = CONV i( tot_int ) .
*  IF tot_int > 0 AND tot_dec > 0.
*    resultado = tot_dec * 30.
*    v_dias = CONV i( resultado ).
*    qtd_meses = v_meses && 'M/' && v_dias && 'D'.
*  ELSE.
*    IF tot_int = 0 AND tot_dec > 0.
*      qtd_meses = v_dias && 'D'.
*    ELSEIF tot_int > 0 AND tot_dec = 0.
*      qtd_meses = v_meses && 'M'.
*    ENDIF.
*  ENDIF.


  IF tot_dec > 0.
    resultado = tot_dec * 30.
    IF resultado > 15 .
      v_mes = tot_int + 1.
    ELSE.
      v_mes = tot_int.
    ENDIF.

  ELSE.
    v_mes = tot_int.
  ENDIF.

  qtd_meses = CONV i( v_mes ).
  CONDENSE qtd_meses NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FM_TOTALIZA_SALDO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_totaliza_saldo .

*** Stefanini - IR196357 - 10/09/2024 - LAZAROSR - Início de Alteração
*  FREE: it_saida_exp_g, it_saida_exp_g, it_saida_exp_f.
  FREE: it_saida_exp_g, it_saida_exp, it_saida_exp_f.
*** Stefanini - IR196357 - 10/09/2024 - LAZAROSR - Fim de Alteração

  LOOP AT it_saida INTO wa_saida.

    PERFORM f_insert_tab_xls_pdf.

    "Total geral.
    wa_saida_exp_g-nro_apolice = wa_saida-nro_apolice.
    wa_saida_exp_g-vig_de      = wa_saida-vig_de.
    wa_saida_exp_g-vig_ate     = wa_saida-vig_ate.
    wa_saida_exp_g-saldo_brl   = wa_saida-saldo_brl.
    wa_saida_exp_g-saldo_usd   = wa_saida-saldo_usd.
    APPEND wa_saida_exp_g TO it_saida_exp_g.
  ENDLOOP.

*** Stefanini - IR196357 - 10/09/2024 - LAZAROSR - Início de Alteração
  SORT it_saida_exp BY nro_apolice
                       vig_de
                       vig_ate.
*** Stefanini - IR196357 - 10/09/2024 - LAZAROSR - Fim de Alteração

ENDFORM.
