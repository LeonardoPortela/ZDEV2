*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MAD |09/06/2025 |Inclusão do campo de Check-List&*
*&                                    |de Documentação Jurídica.      &*
*&                                    |Chamado: 174343.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato &*
*&                                    |Compra.                        &*
*&                                    |Chamado: 168919 2ª Parte.      &*
*&--------------------------------------------------------------------&*
**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados USING p_doc_simulacao.

  DATA: BEGIN OF tl_nro_sol_cp OCCURS 0,
          mandt    TYPE mandt,
          nr_venda TYPE znr_venda,
        END   OF tl_nro_sol_cp.

  FREE: t_tpvenda,  t_tipodoc,  t_status,
        t_zsdt0312, t_zsdt0313, t_zsdt0090.

*-------------------------------------------
* selecao dados
*-------------------------------------------
**<<<------"168919 - NMS - INI------>>>
  IF NOT p_insumo IS INITIAL      AND
         p_tpinsm EQ sy-abcde+2(1). "C - Compra
    IF p_doc_simulacao IS INITIAL.
      SELECT a~* FROM zmmt0035 AS a
        INNER JOIN t001w AS b
         ON a~werks EQ b~werks
        INNER JOIN tvko AS c
         ON b~vkorg EQ c~vkorg
        INTO TABLE @tg_zmmt0035
      WHERE a~nro_sol_cp IN @s_nosol
        AND a~safra      IN @s_safr2
        AND a~werks      IN @s_werks
        AND a~lifnr      IN @s_lifnr
        AND a~ebeln      IN @s_ebeln
        AND a~data_atual IN @s_dtatl
        AND c~bukrs      IN @s_bukrs.

    ELSE.
      SELECT * FROM zmmt0035 INTO TABLE tg_zmmt0035 WHERE nro_sol_cp EQ p_doc_simulacao.

    ENDIF.

    IF NOT tg_zmmt0035[] IS INITIAL.
      LOOP AT tg_zmmt0035 ASSIGNING FIELD-SYMBOL(<fs_zmmt0035>).
        <fs_zmmt0035>-lifnr = |{ <fs_zmmt0035>-lifnr ALPHA = IN WIDTH = 10 }|.

      ENDLOOP.

      tl_nro_sol_cp[] = tg_zmmt0035[].

      SELECT * FROM zsdt0310
        INTO TABLE t_zsdt0310
         FOR ALL ENTRIES IN tl_nro_sol_cp
       WHERE area     EQ 'IN'
         AND tipo_doc EQ 'CTC'
         AND nr_venda EQ tl_nro_sol_cp-nr_venda
         AND status   NE '10'.

    ENDIF.

  ELSE.
**<<<------"168919 - NMS - FIM------>>>
    IF p_doc_simulacao IS INITIAL.
      SELECT *
        FROM zsdt0040
        INTO TABLE t_zsdt0040
       WHERE vkorg         IN s_vkorg
         AND vkbur         IN s_vkbur
         AND doc_simulacao IN s_docsi
         AND kunnr         IN s_kunnr
         AND erdat         IN s_erdat
         AND cultura       IN s_cultu
         AND safra         IN s_safra
         AND spart         IN s_spart
         AND waerk         IN s_moeda.
    ELSE.
      SELECT *
        FROM zsdt0040
        INTO TABLE t_zsdt0040
       WHERE doc_simulacao = p_doc_simulacao.
    ENDIF.

    IF t_zsdt0040[] IS INITIAL.
      EXIT.
    ENDIF.

    SELECT *
      FROM zsdt0041
      INTO TABLE t_zsdt0041
       FOR ALL ENTRIES IN t_zsdt0040
     WHERE doc_simulacao = t_zsdt0040-doc_simulacao.

    SELECT *
      FROM zsdt0310
      INTO TABLE t_zsdt0310
       FOR ALL ENTRIES IN t_zsdt0040
     WHERE nr_venda  = t_zsdt0040-doc_simulacao
       AND status   <> '10'.
**<<<------"168919 - NMS - INI------>>>
  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  IF t_zsdt0310[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0311
      INTO TABLE t_zsdt0311
       FOR ALL ENTRIES IN t_zsdt0310
     WHERE id_documento  = t_zsdt0310-id_documento.

    SELECT *
      FROM zsdt0312
      INTO TABLE t_zsdt0312
       FOR ALL ENTRIES IN t_zsdt0310
     WHERE id_documento  = t_zsdt0310-id_documento.

    SELECT *
      FROM zsdt0313
      INTO TABLE t_zsdt0313
       FOR ALL ENTRIES IN t_zsdt0310
     WHERE id_doc_agrupador  = t_zsdt0310-id_documento.


    SELECT *
      FROM zsdt0314
      INTO TABLE t_zsdt0314
       FOR ALL ENTRIES IN t_zsdt0310
     WHERE nr_doc_gerado  = t_zsdt0310-nr_doc_gerado.

    SELECT *
      FROM zsdt0090
      INTO TABLE t_zsdt0090
       FOR ALL ENTRIES IN t_zsdt0310
     WHERE doc_simulacao  = t_zsdt0310-nr_venda.
  ENDIF.
**<<<------"168919 - NMS - INI------>>>
  IF NOT p_insumo IS INITIAL      AND
         p_tpinsm EQ sy-abcde+2(1). "C - Compra

    IF NOT tg_zmmt0035[] IS INITIAL.
      SELECT *
        FROM lfa1
        INTO TABLE tg_lfa1
         FOR ALL ENTRIES IN tg_zmmt0035
       WHERE lifnr EQ tg_zmmt0035-lifnr.

    ENDIF.

  ELSE.
**<<<------"168919 - NMS - FIM------>>>
    SELECT *
      FROM tvkbt
      INTO TABLE t_tvkbt
       FOR ALL ENTRIES IN t_zsdt0040
     WHERE spras  = sy-langu
       AND vkbur  = t_zsdt0040-vkbur.

    SELECT *
      FROM kna1
      INTO TABLE t_kna1
       FOR ALL ENTRIES IN t_zsdt0040
     WHERE kunnr  =  t_zsdt0040-kunnr.

    SELECT zsdt0271~filial
           zsdt0271~cod_regional
           zsdt0270~regional
      FROM zsdt0271
      INNER JOIN zsdt0270 ON zsdt0270~cod_regional = zsdt0271~cod_regional
      INTO TABLE t_regional
       FOR ALL ENTRIES IN t_zsdt0040
     WHERE zsdt0271~filial = t_zsdt0040-vkbur.

    LOOP AT t_zsdt0040      INTO w_zsdt0040.
      l_tabix = sy-tabix.
      READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY nr_venda = w_zsdt0040-doc_simulacao.
      IF sy-subrc <> 0.
        DELETE t_zsdt0040  INDEX l_tabix.
      ENDIF.
    ENDLOOP.
**<<<------"168919 - NMS - INI------>>>
  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
*-------------------------------------------
* selecao dados
*-------------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'MAGGI_ZSDT0044_05'
      class           = '0000'
      no_descriptions = abap_false
    TABLES
      set_values      = t_tpvenda
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZTIPO_DOC'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_tipodoc
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZSTATUS_DOC'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_status
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  SORT t_zsdt0310 BY nr_venda.

  SORT t_zsdt0313 BY id_doc_agrupador
                     data DESCENDING
                     hora DESCENDING.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0313
                        COMPARING id_doc_agrupador.
**<<<------"174343 - NMS - INI------>>>
  CHECK NOT p_sintet IS INITIAL.
* Seleciona dados da Checagem da Lista de documentação jurídica.
  PERFORM zf_slct_chk_lst_doc_jrdc.
**<<<------"174343 - NMS - FIM------>>>
ENDFORM.

**********************************************************************
* processa dados sintetico
**********************************************************************
FORM f_processa_dados_sintetico.

  FREE: t_sinte.

  t_zsdt0310_aux[] = t_zsdt0310[].

  LOOP AT t_zsdt0040 INTO w_zsdt0040.

    CLEAR: w_sinte, w_tvkbt, w_kna1, w_regional, w_tpvenda.

    READ TABLE t_tvkbt    INTO w_tvkbt    WITH KEY vkbur  = w_zsdt0040-vkbur.
    READ TABLE t_kna1     INTO w_kna1     WITH KEY kunnr  = w_zsdt0040-kunnr.
    READ TABLE t_regional INTO w_regional WITH KEY filial = w_zsdt0040-vkbur.
    READ TABLE t_tpvenda  INTO w_tpvenda  WITH KEY from   = w_zsdt0040-tpsim.

    w_sinte-vkbur          = w_zsdt0040-vkbur.
    w_sinte-bezei          = w_tvkbt-bezei.
    w_sinte-name1          = w_kna1-name1.
    w_sinte-regional       = w_regional-regional.
    w_sinte-doc_simulacao  = w_zsdt0040-doc_simulacao.
    w_sinte-tipo_venda     = w_tpvenda-title.
    w_sinte-waerk          = w_zsdt0040-waerk.
    w_sinte-erdat          = w_zsdt0040-erdat.
    w_sinte-safra          = w_zsdt0040-safra.
    w_sinte-cultura        = w_zsdt0040-cultura.

    PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                    'CTR'
                                    0
                           CHANGING w_sinte-status_ctr
                                    w_sinte-contrato.

    PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                    'OVD'
                                    0
                           CHANGING w_sinte-status_ovd
                                    w_sinte-ped_venda.

    PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                    'DTR'
                                    0
                           CHANGING w_sinte-status_dtr
                                    w_sinte-distrato.

    PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                    'ADT'
                                    0
                           CHANGING w_sinte-status_adt
                                    w_sinte-aditivos.

    PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                    'DCR'
                                    0
                           CHANGING w_sinte-status_dcr
                                    w_sinte-declar_rec.

    IF p_pend   = abap_true AND ( w_sinte-status_ctr = 'C' OR  w_sinte-status_ovd = 'C' OR
                                  w_sinte-status_dtr = 'C' OR  w_sinte-status_adt = 'C' OR
                                  w_sinte-status_dcr = 'C' ).
      CONTINUE.
    ENDIF.
    IF p_conclu = abap_true AND ( w_sinte-status_ctr = 'P' OR  w_sinte-status_ovd = 'P' OR
                                  w_sinte-status_dtr = 'P' OR  w_sinte-status_adt = 'P' OR
                                  w_sinte-status_dcr = 'P' ).
      CONTINUE.
    ENDIF.
**<<<------"174343 - NMS - INI------>>>
* Processa dados da Checagem da Lista de documentação jurídica.
    PERFORM zf_proc_chk_lst_doc_jrdc CHANGING w_sinte.
**<<<------"174343 - NMS - FIM------>>>
    APPEND w_sinte         TO t_sinte.
  ENDLOOP.

ENDFORM.

**********************************************************************
* processa dados sintetico
**********************************************************************
FORM f_processa_dados_analitico.

  FREE: t_anali.

  t_zsdt0310_aux[] = t_zsdt0310[].

  LOOP AT t_zsdt0310 INTO w_zsdt0310.

    IF w_zsdt0310-tipo_doc = 'CTR'.
      READ TABLE t_zsdt0312 INTO w_zsdt0312 WITH KEY id_documento = w_zsdt0310-id_documento.
      PERFORM f_monta_saida_analitico.
    ELSE.
      LOOP AT t_zsdt0312 INTO w_zsdt0312 WHERE id_documento = w_zsdt0310-id_documento..
        PERFORM f_monta_saida_analitico.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

  SORT t_anali BY doc_simulacao tpdoc tipo_doc id_documento.
  DELETE ADJACENT DUPLICATES FROM t_anali
                        COMPARING doc_simulacao tpdoc tipo_doc id_documento.

*------------------------------------
*- ordenar por doc_simulacao + tp_doc
*------------------------------------
  LOOP AT t_anali    INTO w_anali.
    w_anali-ordem_saida = sy-tabix.
    MODIFY t_anali   FROM w_anali INDEX sy-tabix TRANSPORTING ordem_saida.
  ENDLOOP.

ENDFORM.

**********************************************************************
* monta saida analitico
**********************************************************************
FORM f_monta_saida_analitico.

  CLEAR: w_anali,   w_zsdt0040, w_zsdt0090, w_tvkbt, w_kna1, w_regional, w_tpvenda,
         w_tipodoc, w_status.
**<<<------"168919 - NMS - INI------>>>
  IF NOT p_insumo IS INITIAL      AND
         p_tpinsm EQ sy-abcde+2(1). "C - Compra
    CHECK line_exists( tg_zmmt0035[ nro_sol_cp = w_zsdt0310-nr_venda ] ).
    DATA(el_zmmt0035) = tg_zmmt0035[ nro_sol_cp = w_zsdt0310-nr_venda ].
    READ TABLE tg_lfa1 INTO DATA(el_lfa1) WITH KEY lifnr = el_zmmt0035-lifnr.

    w_zsdt0040-tpsim         = w_zsdt0310-tpsim.
    w_zsdt0040-cultura       = el_zmmt0035-cultura.
    w_zsdt0040-doc_simulacao = el_zmmt0035-nro_sol_cp.
    w_kna1-name1             = el_lfa1-name1.
    w_anali-safr2            = el_zmmt0035-safra.

  ELSE.
**<<<------"168919 - NMS - FIM------>>>
    READ TABLE t_zsdt0040 INTO w_zsdt0040 WITH KEY doc_simulacao = w_zsdt0310-nr_venda.
    READ TABLE t_kna1     INTO w_kna1     WITH KEY kunnr         = w_zsdt0040-kunnr.
**<<<------"168919 - NMS - INI------>>>
  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  READ TABLE t_tvkbt    INTO w_tvkbt    WITH KEY vkbur         = w_zsdt0040-vkbur.
  READ TABLE t_regional INTO w_regional WITH KEY filial        = w_zsdt0040-vkbur.
  READ TABLE t_tpvenda  INTO w_tpvenda  WITH KEY from          = w_zsdt0040-tpsim.
  READ TABLE t_tipodoc  INTO w_tipodoc  WITH KEY domvalue_l    = w_zsdt0310-tipo_doc.
  READ TABLE t_status   INTO w_status   WITH KEY domvalue_l    = w_zsdt0310-status.

  LOOP AT t_zsdt0090 INTO w_zsdt0090 WHERE doc_simulacao = w_zsdt0310-nr_venda
                                       AND categoria    <> abap_off.
    EXIT.
  ENDLOOP.

  w_anali-doc_simulacao  = w_zsdt0310-nr_venda.
  w_anali-tpdoc          = w_zsdt0310-tipo_doc.
  w_anali-tipo_doc       = w_tipodoc-ddtext.
  w_anali-id_documento   = w_zsdt0310-id_documento.
  w_anali-nr_doc_gerado  = w_zsdt0310-nr_doc_gerado.
  w_anali-tipo_assina    = COND #( WHEN w_zsdt0310-tipo_doc_digital = 'S' THEN 'Eletrônica'
                                   WHEN w_zsdt0310-tipo_doc_digital = 'N' THEN 'Manual' ).
  w_anali-status_cod     = w_zsdt0310-status.
  w_anali-status         = w_status-ddtext.
  w_anali-tipo_venda     = w_tpvenda-title.
  w_anali-vkbur          = w_zsdt0040-vkbur.
  w_anali-bezei          = w_tvkbt-bezei.
  w_anali-regional       = w_regional-regional.
  w_anali-name1          = w_kna1-name1.
  w_anali-safra          = w_zsdt0040-safra.
  w_anali-cultura        = w_zsdt0040-cultura.
  w_anali-categ_adit     = w_zsdt0090-categoria.
  w_anali-vbeln          = COND #( WHEN w_zsdt0310-tipo_doc = 'OVD' OR w_zsdt0310-tipo_doc = 'ADT' THEN w_zsdt0312-vbeln
                                                                                                   ELSE abap_off ).
  w_anali-posnr          = COND #( WHEN                                w_zsdt0310-tipo_doc = 'ADT' THEN w_zsdt0312-posnr
                                                                                                   ELSE abap_off ).
  w_anali-data           = w_zsdt0310-data.
  w_anali-hora           = w_zsdt0310-hora.
  w_anali-usname         = w_zsdt0310-usname.

  PERFORM f_status_documento  USING w_zsdt0310-id_documento
                           CHANGING w_anali-documento.

  PERFORM f_status_assinatura USING w_zsdt0310-id_documento
                           CHANGING w_anali-assinatura.

  PERFORM f_status_aditivo    USING w_zsdt0310-nr_venda
                           CHANGING w_anali-seq_adit.

  PERFORM f_status_log        USING w_zsdt0310-id_documento
                           CHANGING w_anali-log.

  PERFORM f_cor_linha         USING w_zsdt0310-id_documento
                           CHANGING w_anali-line_color.

  PERFORM f_verifica_status USING w_zsdt0040-doc_simulacao
                                  w_zsdt0310-tipo_doc
                                  w_zsdt0310-id_documento
                         CHANGING w_anali-status_e
                                  w_anali-contrato.

  IF p_contra = abap_true AND w_anali-tpdoc <> 'CTR'.
    EXIT.
  ENDIF.
  IF p_venda  = abap_true AND w_anali-tpdoc <> 'OVD'.
    EXIT.
  ENDIF.
  IF p_distra = abap_true AND w_anali-tpdoc <> 'DTR'.
    EXIT.
  ENDIF.
  IF p_aditiv = abap_true AND w_anali-tpdoc <> 'ADT'.
    EXIT.
  ENDIF.
  IF p_decrec = abap_true AND w_anali-tpdoc <> 'DCR'.
    EXIT.
  ENDIF.

  IF p_pend = abap_true.
    IF p_contra = abap_true AND w_anali-tpdoc = 'CTR' AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
    IF p_venda  = abap_true AND w_anali-tpdoc = 'OVD' AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
    IF p_distra = abap_true AND w_anali-tpdoc = 'DTR' AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
    IF p_aditiv = abap_true AND w_anali-tpdoc = 'ADT' AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
    IF p_decrec = abap_true AND w_anali-tpdoc = 'DCR' AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
    IF p_doctod = abap_true AND w_anali-status_e = 'C'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_conclu = abap_true.
    IF p_contra = abap_true AND w_anali-tpdoc = 'CTR' AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
    IF p_venda  = abap_true AND w_anali-tpdoc = 'OVD' AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
    IF p_distra = abap_true AND w_anali-tpdoc = 'DTR' AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
    IF p_aditiv = abap_true AND w_anali-tpdoc = 'ADT' AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
    IF p_decrec = abap_true AND w_anali-tpdoc = 'DCR' AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
    IF p_doctod = abap_true AND w_anali-status_e = 'P'.
      EXIT.
    ENDIF.
  ENDIF.

  APPEND w_anali          TO t_anali.

ENDFORM.

**********************************************************************
* STATUS DOCumento
**********************************************************************
FORM f_status_documento   USING p_id_documento
                       CHANGING p_icon.

  CLEAR p_icon.

  READ TABLE t_zsdt0310_aux INTO w_zsdt0310_aux WITH KEY id_documento = p_id_documento.

  CASE w_zsdt0310_aux-status.
    WHEN '00' OR '11'.
      p_icon = icon_execute_object.
    WHEN '01'.
      p_icon = icon_activity.
    WHEN '02' OR '03' OR '12' OR '13'.
      p_icon = icon_select_detail.
*   WHEN '06'.
*     p_icon = icon_checked.
    WHEN '04' OR '05' OR '06' OR '07'.
      p_icon = icon_select_detail.
    WHEN OTHERS.
      p_icon = icon_activity.
  ENDCASE.

ENDFORM.

**********************************************************************
* STATUS assinaturaDOCumento
**********************************************************************
FORM f_status_assinatura  USING p_id_documento
                       CHANGING p_icon.

  CLEAR p_icon.

  CLEAR w_zsdt0310_aux.
  READ TABLE t_zsdt0310_aux INTO w_zsdt0310_aux WITH KEY id_documento = p_id_documento.

  IF w_zsdt0310_aux-tipo_doc_digital = 'S'.
    IF     w_zsdt0310_aux-status = '02' OR
           w_zsdt0310_aux-status = '12'.
      p_icon = icon_execute_object.
    ELSEIF w_zsdt0310_aux-status = '03'.
      p_icon = icon_activity.
    ELSEIF w_zsdt0310_aux-status = '04' OR
           w_zsdt0310_aux-status = '05' OR
           w_zsdt0310_aux-status = '06' OR
           w_zsdt0310_aux-status = '07'.
      p_icon =  icon_checked.
    ELSEIF w_zsdt0310_aux-status = '13'.
      p_icon = icon_cancel.
    ENDIF.
  ELSE.
    IF     w_zsdt0310_aux-status = '05'.
      p_icon = icon_import.
    ELSEIF w_zsdt0310_aux-status = '06'.
      p_icon = icon_checked.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* STATUS log
**********************************************************************
FORM f_status_log         USING p_id_documento
                       CHANGING p_icon.

  CLEAR p_icon.

  READ TABLE t_zsdt0313 INTO w_zsdt0313 WITH KEY id_doc_agrupador = p_id_documento.

  IF sy-subrc = 0.
    p_icon = icon_history.
  ENDIF.

ENDFORM.

**********************************************************************
* cor linha alv
**********************************************************************
FORM f_cor_linha          USING p_id_documento
                       CHANGING p_icon.

  CLEAR p_icon.

  CLEAR w_zsdt0310_aux.
  READ TABLE t_zsdt0310_aux INTO w_zsdt0310_aux WITH KEY id_documento = p_id_documento.

  IF w_zsdt0310_aux-status = '11' OR
     w_zsdt0310_aux-status = '12'.
    p_icon = 'C600'.
  ENDIF.

ENDFORM.

**********************************************************************
* STATUS ADITIVO
**********************************************************************
FORM f_status_aditivo     USING p_id_documento
                       CHANGING p_icon.

  CLEAR p_icon.

  READ TABLE t_zsdt0090 INTO w_zsdt0090 WITH KEY doc_simulacao = p_id_documento.

  IF sy-subrc = 0.
    p_icon = icon_display_text.
  ENDIF.

ENDFORM.

**********************************************************************
* STATUS DOC
**********************************************************************
FORM f_verifica_status    USING p_docsimu
                                p_tipodoc
                                p_id_documento
                       CHANGING p_status
                                p_icon.

  CLEAR: p_status, p_icon, l_pend, l_tem_0041, l_tem_0310.

  IF p_id_documento = 0.
    LOOP AT t_zsdt0310_aux INTO w_zsdt0310_aux WHERE nr_venda     = p_docsimu
                                                 AND tipo_doc     = p_tipodoc.
      IF w_zsdt0310_aux-status <> '06' AND
         w_zsdt0310_aux-status <> '07'.
        l_pend = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT t_zsdt0310_aux INTO w_zsdt0310_aux WHERE nr_venda     = p_docsimu
                                                 AND tipo_doc     = p_tipodoc
                                                 AND id_documento = p_id_documento.
      IF w_zsdt0310_aux-status <> '06' AND
         w_zsdt0310_aux-status <> '07'.
        l_pend = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF sy-subrc <> 0.
    IF p_tipodoc = 'DTR' OR
       p_tipodoc = 'ADT' OR
       p_tipodoc = 'DCR'.
      p_status = 'X'.
      p_icon   = icon_dummy.   "nao ha pendencia
      EXIT.
    ELSE.
      p_status = 'P'.
      p_icon   = icon_led_yellow.   "pendente
      EXIT.
    ENDIF.
  ENDIF.

  IF l_pend = abap_true.
    p_status = 'P'.
    p_icon   = icon_led_yellow.   "pendente
  ELSE.
    p_status = 'C'.
    p_icon   = icon_okay.   "ok
  ENDIF.
ENDFORM.

**********************************************************************
* imprime documento
**********************************************************************
FORM f_gerar_documento USING p_id_documento
                             p_tipo_doc
                             p_status.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

  FREE: t_doc_simulacao,
        t_sel_0041,
        t_0310_ctr,
        t_0310_ovd,
        t_0310_ass_ctr,
        t_0310_ass_ovd,
        t_0310_ass_pdf,
        t_pdf_files.

  p_vlr        = abap_off.
  p_manual     = abap_off.
  p_eletronica = abap_on.

  IF p_id_documento IS INITIAL.
    IF g_popup = abap_false.
      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ELSE.
      CALL METHOD g_grid_pop->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ENDIF.
  ELSE.
    FREE: t_rows, w_zsdt0310.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = p_id_documento.
    READ TABLE t_anali    INTO w_anali    WITH KEY id_documento = p_id_documento.
    w_rows-index   = sy-tabix.
    APPEND w_rows TO t_rows.
  ENDIF.

*---------------------------------
* verifica chamada telas
*---------------------------------
  LOOP AT t_rows INTO w_rows.
    READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    CHECK w_zsdt0310-status = '00' OR
          w_zsdt0310-status = '02' OR
          w_zsdt0310-status = '03' OR
          w_zsdt0310-status = '11' OR
          w_zsdt0310-status = '12' OR
          w_zsdt0310-status = '13' OR
          w_zsdt0310-status = '04' OR
          w_zsdt0310-status = '05' OR
          w_zsdt0310-status = '06' OR
          w_zsdt0310-status = '07'.

*-------------------------
*-- parametrizacao
*-------------------------
    IF     w_zsdt0310-status = '07'.
      CASE w_zsdt0310-tipo_doc.
        WHEN 'CTR'.
          APPEND w_zsdt0310  TO t_0310_ctr.
        WHEN 'OVD'.
          APPEND w_zsdt0310  TO t_0310_ovd.
      ENDCASE.
    ELSEIF w_zsdt0310-status = '00' OR
           w_zsdt0310-status = '11'.

      CASE w_zsdt0310-tipo_doc.
**<<<------"168919 - NMS - INI------>>>
*        WHEN 'CTR'.
        WHEN 'CTR' OR "Contrato
             'CTC'.   "Contrato de Compra
**<<<------"168919 - NMS - FIM------>>>
          SELECT SINGLE *
           FROM zsdt0309
           INTO @DATA(w_0309)
          WHERE vkorg     = @w_zsdt0310-vkorg
            AND vkbur     = @w_zsdt0310-vkbur
            AND cancelado = @abap_off.

          IF sy-subrc = 0.
            APPEND w_zsdt0310  TO t_0310_ctr.
          ELSE.
            APPEND w_zsdt0310  TO t_0310_ass_ctr.
          ENDIF.

        WHEN 'OVD'.
          SELECT SINGLE *
           FROM zsdt0309
           INTO w_0309
          WHERE vkorg     = w_zsdt0310-vkorg
            AND vkbur     = w_zsdt0310-vkbur
            AND cancelado = abap_off.

          IF sy-subrc = 0.
            APPEND w_zsdt0310  TO t_0310_ovd.
          ELSE.
            APPEND w_zsdt0310  TO t_0310_ass_ovd.
          ENDIF.
      ENDCASE.

    ELSEIF w_zsdt0310-status = '02' OR
           w_zsdt0310-status = '03' OR
           w_zsdt0310-status = '04' OR
           w_zsdt0310-status = '05' OR
           w_zsdt0310-status = '06' OR
           w_zsdt0310-status = '12' OR
           w_zsdt0310-status = '13'.
      APPEND w_zsdt0310        TO t_0310_ass_pdf.
    ENDIF.
  ENDLOOP.

*--------------------------------------
* Tratamento impressao contrato
*--------------------------------------
  IF t_0310_ctr[] IS NOT INITIAL.
    PERFORM f_imprime_contrato.
  ENDIF.

*--------------------------------------
* Tratamento impressao Ordem Venda
*--------------------------------------
  IF t_0310_ovd[] IS NOT INITIAL.
    CALL SCREEN 400 STARTING AT 65 08.

    IF ok_code = 'OK'.
      PERFORM f_imprime_ov.
    ENDIF.
  ENDIF.

  IF t_0310_ass_ctr[] IS NOT INITIAL.
    CALL SCREEN 500 STARTING AT 65 08.

    IF ok_code = 'OK'.
      PERFORM f_grava_assinatura_ctr.
    ENDIF.
  ENDIF.

  IF t_0310_ass_ovd[] IS NOT INITIAL.
    CALL SCREEN 500 STARTING AT 65 08.

    IF ok_code = 'OK'.
      CALL SCREEN 400 STARTING AT 65 08.

      IF ok_code = 'OK'.
        PERFORM f_grava_assinatura_ovd.
      ENDIF.
    ENDIF.
  ENDIF.

  IF t_0310_ass_pdf[] IS NOT INITIAL.
    PERFORM f_imprime_doc_sigam.
  ENDIF.

  PERFORM f_imprime_formularios.

ENDFORM.

**********************************************************************
* gerar assinatura
**********************************************************************
FORM f_gerar_assinatura USING p_id_documento
                              p_tipo_doc
                              p_status.

  DATA: l_msg  TYPE string.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*------------------------------------
* exibe status dos assinantes
*------------------------------------
  IF p_id_documento IS NOT INITIAL.
    IF ( p_status = '03' OR
         p_status = '04' OR
         p_status = '06' OR
         p_status = '13' ).
      CALL FUNCTION 'ZSD_INSUMOS_STATUS_ASSINANTES'
        EXPORTING
          i_id_documento = p_id_documento
        EXCEPTIONS
          sem_status     = 1
          OTHERS         = 2.

      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Status ainda não está disponível.' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      EXIT.
    ENDIF.

    IF p_status = '05'.
      PERFORM f_upload_pdf_assinado USING p_id_documento.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_id_documento IS INITIAL.
    IF g_popup = abap_false.
      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ELSE.
      CALL METHOD g_grid_pop->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ENDIF.
  ELSE.
    FREE: t_rows, w_zsdt0310.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = p_id_documento.
    READ TABLE t_anali    INTO w_anali    WITH KEY id_documento = p_id_documento.
    w_rows-index   = sy-tabix.
    APPEND w_rows TO t_rows.
  ENDIF.

*---------------------------------
* verifica chamada telas
*---------------------------------
  LOOP AT t_rows INTO w_rows.
    READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    CHECK ( w_zsdt0310-status           = '02'    OR
            w_zsdt0310-status           = '12' ) AND
            w_zsdt0310-tipo_doc_digital = 'S'.
    w_zsdt0310-status   = '03'.
    MODIFY zsdt0310  FROM w_zsdt0310.

    l_msg = 'Assinatura Eletrônica foi Solicitada.'.

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_zsdt0310-nr_venda
                                    i_tipo_doc     = w_zsdt0310-tipo_doc
                                    i_id_documento = w_zsdt0310-id_documento
                                    i_mensagem     = l_msg ).

    COMMIT WORK AND WAIT.

*-----------------------------------------
*-- solicitar assinatura Bry
*-----------------------------------------
    DATA(l_task) = 'SOLICITAR_ASSINATURA' && w_zsdt0310-nr_venda && w_zsdt0310-tipo_doc.

    CALL FUNCTION 'ZSD_INSUMOS_ASSINATURA_BRY' STARTING NEW TASK l_task
      EXPORTING
        i_doc_simulacao = w_zsdt0310-nr_venda
        i_tipo_doc      = w_zsdt0310-tipo_doc
        i_id_documento  = w_zsdt0310-id_documento.

  ENDLOOP.

ENDFORM.

**********************************************************************
* cancelar coleta
**********************************************************************
FORM f_cancelar_documento  USING p_id_documento
                                 p_tipo_doc
                                 p_status.

  DATA: l_msg  TYPE string,
        t_user TYPE TABLE OF rgsb4,
        w_user TYPE rgsb4.

  FREE: l_erro.

  IF p_id_documento IS INITIAL.
    IF g_popup = abap_false.
      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ELSE.
      CALL METHOD g_grid_pop->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.
    ENDIF.
  ELSE.
    FREE: t_rows, w_zsdt0310.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = p_id_documento.
    READ TABLE t_anali    INTO w_anali    WITH KEY id_documento = p_id_documento.
    w_rows-index   = sy-tabix.
    APPEND w_rows TO t_rows.
  ENDIF.

*-------------------------------
* ser usuarios podem cancealar doctos assinados
*-------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'ZSDT0203_USER_CANC'
      no_descriptions = abap_off
    TABLES
      set_values      = t_user
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

*-------------------------------
* checa se linhas selecionadas estao no mesmo status
*-------------------------------
  LOOP AT t_rows INTO w_rows.
    READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    IF w_zsdt0310-status = '04' OR
       w_zsdt0310-status = '06' OR
       w_zsdt0310-status = '07'.
      l_erro = abap_true.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_false.
**<<<------"168919 - NMS - INI------>>>
    IF w_zsdt0310-status NE '01' AND "Aguardando Documento
       w_zsdt0310-status NE '02'.    "Documento Gerado
**<<<------"168919 - NMS - FIM------>>>
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        EXPORTING
          defaultoption  = 'N'
          diagnosetext1  = 'Documento(s) Selecionado(s) em fase de Assinatura.'
          textline1      = 'Deseja Cancelar este(s) Documento(s)?'
          titel          = 'Atenção'
          start_column   = 50
          start_row      = 10
          cancel_display = abap_false
        IMPORTING
          answer         = l_resp
        EXCEPTIONS
          OTHERS         = 1.

      IF l_resp = 'N'.
        EXIT.
      ENDIF.
**<<<------"168919 - NMS - INI------>>>
    ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  ELSE.
    READ TABLE t_user INTO w_user WITH KEY from = sy-uname.
    IF sy-subrc = 0.
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        EXPORTING
          defaultoption  = 'N'
          diagnosetext1  = 'Há Documento(s) com Assinatura já Finalizada!'
          textline1      = 'Deseja Realmente Cancelar este(s) Documento(s)?'
          titel          = 'Atenção'
          start_column   = 50
          start_row      = 10
          cancel_display = abap_false
        IMPORTING
          answer         = l_resp
        EXCEPTIONS
          OTHERS         = 1.

      IF l_resp = 'N'.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE s024(sd) WITH 'Documento(s) não podem ser Cancelado(s)!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*---------------------------------
* verifica chamada telas
*---------------------------------
  LOOP AT t_rows INTO w_rows.
    READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    IF w_zsdt0310-status = '13'.
*-----------------------------------------
*---- solicitar cancelamento documento - o documento volta ao
*---- seu estagio inicial
*-----------------------------------------
      DATA(l_task) = 'SOLICITAR_CANCELAMENTO' && w_zsdt0310-nr_venda && w_zsdt0310-tipo_doc.

      CALL FUNCTION 'ZSD_INSUMOS_CANCELAR_COLETA' STARTING NEW TASK l_task
        EXPORTING
          i_id_documento = w_zsdt0310-id_documento.

      MESSAGE s024(sd) WITH 'Solicitado Cancelamento do(s) Documento(s).'.
    ELSE.
*-----------------------------------------
*---- solicitar cancelamento documento - é gerado
*---- um novo ID de documento e o anterior cancelado
*---- para os casos que o Tipo de Insumo não seja compra.     "<<<------"168919 - NMS ------>>>
*-----------------------------------------
      PERFORM f_regerar_registros USING w_zsdt0310.

      MESSAGE s024(sd) WITH 'Documento(s) cancelado(s) com Sucesso!'.
    ENDIF.
  ENDLOOP.

ENDFORM.

**********************************************************************
* regerar documentos cancelados
**********************************************************************
FORM f_regerar_registros USING p_zsdt0310. " STRUCTURE zsdt0310.

  DATA: w_0310     TYPE zsdt0310,
        w_0310_par TYPE zsdt0310,
        w_0311     TYPE zsdt0311,
        w_0312     TYPE zsdt0312.

  MOVE p_zsdt0310             TO w_0310_par.

*-----------------------------------------
*-- cancelar documento BRY / SIGAM
*-----------------------------------------
  DATA(l_task) = 'SOLICITAR_CANCELAMENTO' && w_0310_par-nr_venda && w_0310_par-tipo_doc.

  CALL FUNCTION 'ZSD_INSUMOS_CANCELAR_COLETA' STARTING NEW TASK l_task
    EXPORTING
      i_id_documento       = w_0310_par-id_documento
      i_cancelar_documento = abap_true.
**<<<------"168919 - NMS - INI------>>>
  IF p_tpinsm NE sy-abcde+2(1). "C - Compra
**<<<------"168919 - NMS - FIM------>>>
*-----------------------------
*-- proximo numero
*-----------------------------
    PERFORM f_gera_seq_ov      USING 'ZSD_ID_DOC'
                            CHANGING l_id_documento.

*-----------------------------
*-- cria novo registro
*-----------------------------
    MOVE p_zsdt0310             TO w_0310.

    w_0310-id_documento          = l_id_documento.
    w_0310-id_doc_agrupador      = 0.
    w_0310-status                = '00'.
    w_0310-tipo_doc_digital      = abap_off.
    w_0310-assinatura_sequencial = abap_off.
    w_0310-proibir_rejeicao      = abap_off.
    w_0310-id_assinatura         = abap_off.
    w_0310-nr_doc_gerado         = abap_off.
    w_0310-usname                = sy-uname.
    w_0310-data                  = sy-datum.
    w_0310-hora                  = sy-uzeit.

    CLEAR: w_0310-data_doc_gerado,
           w_0310-hora_doc_gerado,
           w_0310-usnam_canc,
           w_0310-data_canc,
           w_0310-hora_canc.

    MODIFY zsdt0310           FROM w_0310.
**<<<------"168919 - NMS - INI------>>>
  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
*-----------------------------
*-- anula o atual
*-----------------------------
  MOVE p_zsdt0310             TO w_0310.

  w_0310-status                = '10'.
  w_0310-usnam_canc            = sy-uname.
  w_0310-data_canc             = sy-datum.
  w_0310-hora_canc             = sy-uzeit.
  MODIFY zsdt0310           FROM w_0310.
**<<<------"168919 - NMS - INI------>>>
  IF p_tpinsm NE sy-abcde+2(1). "C - Compra
**<<<------"168919 - NMS - FIM------>>>
*-----------------------------
*-- clientes
*-----------------------------
    LOOP AT t_zsdt0311 INTO w_zsdt0311 WHERE id_documento = w_0310_par-id_documento.
      MOVE w_zsdt0311    TO w_0311.

      w_0311-id_documento = l_id_documento.
      MODIFY zsdt0311  FROM w_0311.
    ENDLOOP.

*-----------------------------
*-- itens documento
*-----------------------------
    LOOP AT t_zsdt0312 INTO w_zsdt0312 WHERE id_documento = w_0310_par-id_documento.
      MOVE w_zsdt0312    TO w_0312.

      w_0312-id_documento = l_id_documento.
      MODIFY zsdt0312  FROM w_0312.
    ENDLOOP.
**<<<------"168919 - NMS - INI------>>>
  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  COMMIT WORK AND WAIT.

ENDFORM.

**********************************************************************
* imprimir documento SIGAM
**********************************************************************
FORM f_imprime_doc_sigam.

  LOOP AT t_0310_ass_pdf INTO w_0310_ass_pdf.

    " 03.12.2024 - RAMON - US #158242 -->
    IF w_0310_ass_pdf-nr_doc_gerado IS NOT INITIAL
      AND w_0310_ass_pdf-id_doc_agrupador IS NOT INITIAL.

      SELECT SINGLE *
               INTO w_zsdt0314
               FROM zsdt0314
              WHERE nr_doc_gerado = w_0310_ass_pdf-nr_doc_gerado
                AND id_doc_agrupador = w_0310_ass_pdf-id_doc_agrupador.

      " 03.12.2024 - RAMON - US #158242 <--

    ELSEIF w_0310_ass_pdf-nr_doc_gerado IS NOT INITIAL.
      SELECT SINGLE *
               INTO w_zsdt0314
               FROM zsdt0314
              WHERE nr_doc_gerado = w_0310_ass_pdf-nr_doc_gerado.
    ELSE.
      SELECT SINGLE *
               INTO w_zsdt0314
               FROM zsdt0314
              WHERE id_doc_agrupador =  w_0310_ass_pdf-id_doc_agrupador.
    ENDIF.

    CHECK sy-subrc = 0.

    IF w_zsdt0314-doc_pdf_assinado IS NOT INITIAL.
      CLEAR w_pdf_files.
      w_pdf_files-data       = w_zsdt0314-doc_pdf_assinado.
      w_pdf_files-len        = xstrlen( w_zsdt0314-doc_pdf_assinado ).
      APPEND w_pdf_files    TO t_pdf_files.

    ELSEIF w_zsdt0314-chave_pdf_assinado IS NOT INITIAL AND
           w_zsdt0314-doc_pdf_assinado   IS INITIAL.

*---------------------------------
*---- baixar PDF assinado
*---------------------------------
      TRY .
          zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
             )->get_baixar_pdf_assinado( EXPORTING i_nr_doc_gerado    = w_zsdt0314-nr_doc_gerado
                                                   i_id_doc_agrupador = w_zsdt0314-id_doc_agrupador
                                         IMPORTING e_pdf_assinado     = w_zsdt0314-doc_pdf_assinado ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          MESSAGE s024(sd) WITH 'Não foi pos~sível baixar PDF Assinado!' DISPLAY LIKE 'E'.
          EXIT.
        CATCH zcx_error INTO DATA(ex_error).
          MESSAGE s024(sd) WITH 'Não foi pos~sível baixar PDF Assinado!' DISPLAY LIKE 'E'.
          EXIT.
      ENDTRY.

      CLEAR w_pdf_files.
      w_pdf_files-data       = w_zsdt0314-doc_pdf_assinado.
      w_pdf_files-len        = xstrlen( w_zsdt0314-doc_pdf_assinado ).
      APPEND w_pdf_files    TO t_pdf_files.

    ELSE.
      CLEAR w_pdf_files.
      w_pdf_files-data       = w_zsdt0314-pdf_doc_original.
      w_pdf_files-len        = xstrlen( w_zsdt0314-pdf_doc_original ).
      APPEND w_pdf_files    TO t_pdf_files.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
* imprimir formularios
**********************************************************************
FORM f_imprime_formularios.

  DATA: l_doc_simulacao TYPE zid_documento.

  CHECK t_pdf_files[] IS NOT INITIAL.

*-----------------------------------------
* agrupa documentos
*-----------------------------------------
*-#149060-19.08.2024-JT-inicio
  IF lines( t_pdf_files[] ) = 1.
    TRY.
        l_pdf_xtring = t_pdf_files[ 1 ]-data.
      CATCH cx_sy_itab_line_not_found INTO DATA(l_error).
    ENDTRY.
  ELSE.
    TRY.
        l_pdf_xtring = zcl_faturamento=>zif_faturamento~get_instance(
                           )->get_merge_pdf( EXPORTING t_pdf_files = t_pdf_files
                           ).

      CATCH zcx_faturamento.
      CATCH zcx_error.
    ENDTRY.
  ENDIF.
*-#149060-19.08.2024-JT-fim

*-----------------------------------------
* define se habilita botao de assinatura
* na visualizacao do PDF
*-----------------------------------------
  CLEAR l_doc_simulacao.

  IF lines( t_0310_ass_pdf[] ) = 1.
    READ TABLE t_0310_ass_pdf INTO DATA(w_0310) INDEX 1.
    IF ( w_0310-status           = '02'    OR
         w_0310-status           = '12' ) AND
         w_0310-tipo_doc_digital = 'S'.
      l_doc_simulacao = w_0310-id_documento.
    ENDIF.
  ENDIF.

*-----------------------------------------
* visualiza PDF
*-----------------------------------------
  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      i_pdf                    = l_pdf_xtring
      i_doc_simulacao          = l_doc_simulacao
      i_salvar                 = abap_true       "#149060-19.08.2024-JT
    EXCEPTIONS
      convert_otf_to_pdf_error = 1
      cntl_error               = 2
      OTHERS                   = 3.

ENDFORM.

**********************************************************************
* grava assinatura para contratos
**********************************************************************
FORM f_grava_assinatura_ctr.

  DATA: l_msg  TYPE string.

  LOOP AT t_0310_ass_ctr  INTO w_0310_ass_ctr.

    w_0310_ass_ctr-status           = '01'.
    w_0310_ass_ctr-tipo_doc_digital = COND #( WHEN p_manual = abap_true THEN 'N'
                                                                        ELSE 'S' ).
    MODIFY zsdt0310  FROM w_0310_ass_ctr.

    l_msg = COND #( WHEN p_manual = abap_true THEN 'Usuário solicitou a criação do Documento com Assinatura Manual.'
                                              ELSE 'Usuário solicitou a criação do Documento com Assinatura Eletrônica.' ).

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ass_ctr-nr_venda
                                    i_tipo_doc     = w_0310_ass_ctr-tipo_doc
                                    i_id_documento = w_0310_ass_ctr-id_documento
                                    i_mensagem     = l_msg ).

    COMMIT WORK AND WAIT.

*-----------------------------------------
* gerar documento
*-----------------------------------------
    DATA(l_task) = 'GERAR_DOC_SIGAM' && w_0310_ass_ctr-nr_venda && w_0310_ass_ctr-tipo_doc.
**<<<------"168919 - NMS - INI------>>>
*    IF w_0310_ass_ctr-tipo_doc = 'CTR'.
    IF w_0310_ass_ctr-tipo_doc = 'CTR' OR "Contrato
       w_0310_ass_ctr-tipo_doc = 'CTC'.   "Contrato de Compra
**<<<------"168919 - NMS - FIM------>>>
      CALL FUNCTION 'ZSD_INSUMOS_GERAR_DOC_SIGAM' STARTING NEW TASK l_task
        EXPORTING
          i_doc_simulacao = w_0310_ass_ctr-nr_venda
          i_tipo_doc      = w_0310_ass_ctr-tipo_doc
          i_id_documento  = w_0310_ass_ctr-id_documento.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
* grava assinatura para ordem vendas
**********************************************************************
FORM f_grava_assinatura_ovd.

  DATA: l_msg        TYPE string,
        l_status_old TYPE zstatus_doc.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

  FREE l_erro.

  LOOP AT t_0310_ass_ovd         INTO w_0310_ass_ovd.

    w_0310_ass_ovd-status           = '01'.
    w_0310_ass_ovd-imp_valor_unit   = COND #( WHEN p_vlr    = abap_true THEN 'S'
                                                                        ELSE 'N' ).
    w_0310_ass_ovd-tipo_doc_digital = COND #( WHEN p_manual = abap_true THEN 'N'
                                                                        ELSE 'S' ).
    MODIFY zsdt0310              FROM w_0310_ass_ovd.

    l_msg = COND #( WHEN p_manual = abap_true THEN 'Usuário solicitou a criação do Documento O.V. com Assinatura Manual.'
                                              ELSE 'Usuário solicitou a criação do Documento O.V. com Assinatura Eletrônica.' ).

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ass_ovd-nr_venda
                                    i_tipo_doc     = w_0310_ass_ovd-tipo_doc
                                    i_id_documento = w_0310_ass_ovd-id_documento
                                    i_mensagem     = l_msg ).

    COMMIT WORK AND WAIT.

*-----------------------------------------
* gerar documento
*-----------------------------------------
    DATA(l_task) = 'GERAR_DOC_SIGAM_OVD' && w_0310_ass_ovd-nr_venda && w_0310_ass_ovd-tipo_doc.

    IF w_0310_ass_ovd-tipo_doc = 'OVD'.
      CALL FUNCTION 'ZSD_INSUMOS_GERAR_DOC_SIGAM' STARTING NEW TASK l_task
        EXPORTING
          i_doc_simulacao = w_0310_ass_ovd-nr_venda
          i_tipo_doc      = w_0310_ass_ovd-tipo_doc
          i_id_documento  = w_0310_ass_ovd-id_documento.
    ENDIF.

  ENDLOOP.

ENDFORM.

**********************************************************************
* grava assinatura para ordem vendas (modo antigo)
**********************************************************************
FORM f_grava_assinatura_ovd_old.

  DATA: l_msg        TYPE string,
        l_status_old TYPE zstatus_doc.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

  FREE l_erro.

  LOOP AT t_0310_ass_ovd    INTO w_0310_ass_ovd.

    l_status_old = w_0310_ass_ovd-status.

    FREE: t_sel_0041.

    LOOP AT t_zsdt0312      INTO w_zsdt0312 WHERE id_documento = w_0310_ass_ovd-id_documento.
      w_sel_0041-doc_simulacao = w_0310_ass_ovd-nr_venda.
      w_sel_0041-vbeln         = w_zsdt0312-vbeln.
      w_sel_0041-posnr         = w_zsdt0312-posnr.
      APPEND w_sel_0041       TO t_sel_0041.
    ENDLOOP.

*---------------------------------
*-- sequenciador doc gerado
*---------------------------------
    PERFORM f_gera_seq_ov      USING 'ZSD_ID_DOC'
                            CHANGING l_id_documento.

    w_0310_ass_ovd-status           = COND #( WHEN p_manual = abap_true THEN '05'
                                                                        ELSE '02' ).
    w_0310_ass_ovd-nr_doc_gerado    = l_id_documento.
    w_0310_ass_ovd-data_doc_gerado  = sy-datum.
    w_0310_ass_ovd-hora_doc_gerado  = sy-uzeit    .
    w_0310_ass_ovd-tipo_doc_digital = COND #( WHEN p_manual = abap_true THEN 'N'
                                                                        ELSE 'S' ).
    MODIFY zsdt0310              FROM w_0310_ass_ovd.

    COMMIT WORK AND WAIT.

*-----------------------------------------
*-- api obter participantes
*-----------------------------------------
    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->get_obter_participantes( EXPORTING i_nr_venda     = w_0310_ass_ovd-nr_venda
                                                 i_tipo_doc     = w_0310_ass_ovd-tipo_doc
                                                 i_id_documento = w_0310_ass_ovd-id_documento ).

      CATCH zcx_integracao INTO DATA(ex_integra).
        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.

        w_0310_ass_ovd-status           = l_status_old.
        w_0310_ass_ovd-nr_doc_gerado    = abap_off.
        w_0310_ass_ovd-tipo_doc_digital = abap_off.
        MODIFY zsdt0310              FROM w_0310_ass_ovd.

        COMMIT WORK AND WAIT.

        l_erro = abap_true.
        EXIT.

      CATCH zcx_error INTO DATA(ex_error).
        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.

        w_0310_ass_ovd-status           = l_status_old.
        w_0310_ass_ovd-nr_doc_gerado    = abap_off.
        w_0310_ass_ovd-tipo_doc_digital = abap_off.
        MODIFY zsdt0310              FROM w_0310_ass_ovd.

        COMMIT WORK AND WAIT.

        l_erro = abap_true.
        EXIT.
    ENDTRY.

*---------------------------------
*-- gerar pdf ordem venda
*---------------------------------
    CALL FUNCTION 'Z_GERA_OV_CONTRATO'
      EXPORTING
        i_vlr              = p_vlr
        i_no_print         = abap_true
        i_id_documento     = w_0310_ass_ovd-id_documento
      IMPORTING
        e_xstring_document = l_pdf_xtring
      TABLES
        vbeln              = t_sel_0041.

*-------------------------------
*-- atualizar tabela com PDF
*-------------------------------
    CLEAR w_zsdt0314.

    w_zsdt0314-nr_doc_gerado        = l_id_documento.
    w_zsdt0314-id_doc_agrupador     = w_0310_ass_ovd-id_documento.
    w_zsdt0314-pdf_doc_original     = l_pdf_xtring.
    w_zsdt0314-data                 = sy-datum.
    w_zsdt0314-hora                 = sy-uzeit.
    MODIFY zsdt0314              FROM w_zsdt0314.

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    l_msg = COND #( WHEN p_manual = abap_true THEN |{ 'Gerado Documento com Assinatura Manual.' }     { 'Documento:' } { l_id_documento }|
                                              ELSE |{ 'Gerado Documento com Assinatura Eletronica.' } { 'Documento:' } { l_id_documento }| ).

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ass_ovd-nr_venda
                                    i_tipo_doc     = w_0310_ass_ovd-tipo_doc
                                    i_id_documento = w_0310_ass_ovd-id_documento
                                    i_mensagem     = l_msg ).

    COMMIT WORK AND WAIT.

  ENDLOOP.

ENDFORM.

**********************************************************************
* imprime contrato
**********************************************************************
FORM f_imprime_contrato.

  DATA: l_answer TYPE char1,
        l_msg    TYPE string.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*------------------------
* atualuzar tabelas
*------------------------
  LOOP AT t_0310_ctr INTO w_0310_ctr.
    l_tabix = sy-tabix.

*-- sequenciador doc gerado
    PERFORM f_gera_seq_ov      USING 'ZSD_SEQOV'
                            CHANGING l_id_documento.

    w_0310_ctr-nr_doc_gerado    = l_id_documento.
    w_0310_ctr-status           = '05'.
    w_0310_ctr-id_doc_agrupador = w_0310_ctr-id_documento.
    w_0310_ctr-tipo_doc_digital = 'N'.

    MODIFY t_0310_ctr        FROM w_0310_ctr INDEX l_tabix.
    MODIFY zsdt0310          FROM w_0310_ctr.

    COMMIT WORK.

*-----------------------------------------
*-- api obter participantes
*-----------------------------------------
*    TRY .
*        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
*           )->get_obter_participantes( EXPORTING i_nr_venda     = w_0310_ctr-nr_venda
*                                                 i_tipo_doc     = w_0310_ctr-tipo_doc
*                                                 i_id_documento = w_0310_ctr-id_documento ).
*      CATCH zcx_integracao INTO DATA(ex_integra).
*        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.
*        l_erro = abap_true.
*        EXIT.
*
*      CATCH zcx_error INTO DATA(ex_error).
*        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.
*        l_erro = abap_true.
*        EXIT.
*    ENDTRY.

*---------------------------------
* gerar pdf contrato
*---------------------------------
    CALL FUNCTION 'Z_GERA_CONTRATO'
      EXPORTING
        i_dir              = l_answer
        i_doc              = w_0310_ctr-nr_venda
        i_no_print         = abap_true
      IMPORTING
        e_xstring_document = l_pdf_xtring.

*------------------------
* atualuzar tabelas
*------------------------
    CLEAR w_zsdt0314.
    w_zsdt0314-nr_doc_gerado    = w_0310_ctr-nr_doc_gerado.
    w_zsdt0314-id_doc_agrupador = w_0310_ctr-id_doc_agrupador.
    w_zsdt0314-pdf_doc_original = l_pdf_xtring.
    w_zsdt0314-data             = sy-datum.
    w_zsdt0314-hora             = sy-uzeit.
    MODIFY zsdt0314          FROM w_zsdt0314.

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    l_msg   = 'Solicitada Impressão do Formulário do Contrato'.

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ctr-nr_venda
                                    i_tipo_doc     = w_0310_ctr-tipo_doc
                                    i_id_documento = w_0310_ctr-id_documento
                                    i_mensagem     = l_msg ).
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

**********************************************************************
* imprime ov
**********************************************************************
FORM f_imprime_ov.

  DATA: l_msg    TYPE string.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*-----------------------------------------
*-atualiza dados
*-----------------------------------------
  LOOP AT t_0310_ovd INTO w_0310_ovd.
    l_tabix = sy-tabix.

    FREE: t_sel_0041.
    LOOP AT t_zsdt0041 INTO w_zsdt0041 WHERE doc_simulacao = w_0310_ovd-nr_venda
                                         AND vbeln         = w_0310_ovd-vbeln.
      MOVE w_zsdt0041    TO w_sel_0041.
      APPEND w_sel_0041  TO t_sel_0041.
    ENDLOOP.

*-- sequenciador doc gerado
    PERFORM f_gera_seq_ov      USING 'ZSD_SEQOV'
                            CHANGING l_id_documento.

    w_0310_ovd-nr_doc_gerado    = l_id_documento.
    w_0310_ovd-status           = '05'.
    w_0310_ovd-id_doc_agrupador = w_0310_ovd-id_documento.
    w_0310_ovd-imp_valor_unit   = COND #( WHEN p_vlr = abap_true THEN 'S'
                                                                 ELSE 'N' ).
    w_0310_ovd-tipo_doc_digital = 'N'.

    MODIFY t_0310_ovd        FROM w_0310_ovd INDEX l_tabix.
    MODIFY zsdt0310          FROM w_0310_ovd.

    COMMIT WORK.

*-----------------------------------------
*-- api obter participantes
*-----------------------------------------
*    TRY .
*        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
*           )->get_obter_participantes( EXPORTING i_nr_venda     = w_0310_ovd-nr_venda
*                                                 i_tipo_doc     = w_0310_ovd-tipo_doc
*                                                 i_id_documento = w_0310_ovd-id_documento ).
*      CATCH zcx_integracao INTO DATA(ex_integra).
*        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.
*        l_erro = abap_true.
*        EXIT.
*
*      CATCH zcx_error INTO DATA(ex_error).
*        MESSAGE s024(sd) WITH 'Não foi possível obter os participantes!' DISPLAY LIKE 'E'.
*        l_erro = abap_true.
*        EXIT.
*    ENDTRY.

*---------------------------------
* gerar pdf ordem venda
*---------------------------------
    CALL FUNCTION 'Z_GERA_OV_CONTRATO'
      EXPORTING
        i_vlr              = p_vlr
        i_no_print         = abap_true
        i_id_documento     = w_0310_ovd-id_documento
      IMPORTING
        e_xstring_document = l_pdf_xtring
      TABLES
        vbeln              = t_sel_0041.

*-------------------------------
*-- atualizar tabela com PDF
*-------------------------------
    CLEAR w_zsdt0314.
    w_zsdt0314-nr_doc_gerado    = w_0310_ovd-nr_doc_gerado.
    w_zsdt0314-id_doc_agrupador = w_0310_ovd-id_doc_agrupador.
    w_zsdt0314-pdf_doc_original = l_pdf_xtring.
    w_zsdt0314-data             = sy-datum.
    w_zsdt0314-hora             = sy-uzeit.
    MODIFY zsdt0314          FROM w_zsdt0314.

*-----------------------------------------
*-- gravar log
*-----------------------------------------
    l_msg   = 'Solicitada Impressão do Formulário do Pedido Venda'.

    zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
       )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ovd-nr_venda
                                    i_tipo_doc     = w_0310_ovd-tipo_doc
                                    i_id_documento = w_0310_ovd-id_documento
                                    i_mensagem     = l_msg ).
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

**********************************************************************
* upload documento assinado
**********************************************************************
FORM f_upload_pdf_assinado USING p_id_documento.

  DATA: l_title    TYPE string,
        l_filter   TYPE string,
        l_initial  TYPE string,
        l_filename TYPE string,
        l_msg      TYPE string,
        l_rc       TYPE i,
        l_resp     TYPE char1,
        l_flen     TYPE i,
        t_lines    TYPE STANDARD TABLE OF tline,
        t_otfdata  TYPE tsfotf,
        t_file     TYPE filetable,
        w_file     TYPE file_table.

  l_title   = 'Upload Documento Assinado'.
  l_filter  = 'Files PDF (*.pdf)|*.pdf|'.
* l_initial = 'C:\'.

*-------------------------------
* local do arquivo
*-------------------------------
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = l_title
      file_filter             = l_filter
*     initial_directory       = l_initial
    CHANGING
      file_table              = t_file
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK t_file[] IS NOT INITIAL.

*-------------------------------
* confirmacao
*-------------------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = 'Confirma Upload do Documento Assinado?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      start_column          = 40
      start_row             = 8
      display_cancel_button = abap_true
    IMPORTING
      answer                = l_resp.

  CHECK l_resp = '1'.

*-------------------------------
* upload do arquivo
*-------------------------------
  READ TABLE t_file INTO w_file INDEX 1.

  l_filename = w_file-filename.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = l_flen
*     header                  = l_pdf_xtring
    CHANGING
      data_tab                = t_otfdata
    EXCEPTIONS
      file_read_error         = 3
      invalid_type            = 4
      no_batch                = 5
      gui_refuse_filetransfer = 7
      OTHERS                  = 99.

*-------------------------------
* convesao PDF
*-------------------------------
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = l_flen
      bin_file              = l_pdf_xtring
    TABLES
      otf                   = t_otfdata[]
      lines                 = t_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

*-------------------------------
* atualisa status documentotabelas
*-------------------------------
  READ TABLE t_zsdt0310 INTO w_0310_ass_ovd WITH KEY id_documento = p_id_documento.

  w_0310_ass_ovd-status   = '06'.
  MODIFY zsdt0310      FROM w_0310_ass_ovd.

*-------------------------------
* grava pdf assinado
*-------------------------------
  UPDATE zsdt0314 SET doc_pdf_assinado = l_pdf_xtring
                      url_pdf_assinado = l_filename
                      data             = sy-datum
                      hora             = sy-uzeit
                WHERE nr_doc_gerado    = w_0310_ass_ovd-nr_doc_gerado
                  AND id_doc_agrupador = w_0310_ass_ovd-id_documento.

*-----------------------------------------
*-- gravar log
*-----------------------------------------
  l_msg = 'Upload do Documento Assinado Concluído!'.

  zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
     )->set_gravar_log( EXPORTING i_nr_venda     = w_0310_ass_ovd-nr_venda
                                  i_tipo_doc     = w_0310_ass_ovd-tipo_doc
                                  i_id_documento = w_0310_ass_ovd-id_documento
                                  i_mensagem     = l_msg ).

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* gerar sequenciador OV
************************************************************************
FORM f_gera_seq_ov    USING p_objeto
                   CHANGING p_seq.

  FREE p_seq.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = p_objeto
    IMPORTING
      number                  = p_seq
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

ENDFORM.

**********************************************************************
* GRAVAR LOG
**********************************************************************
*FORM f_gravar_log   USING p_id_doc_agrupador
*                          p_mensagem.
*
*  DATA: l_time  TYPE timestampl.
*
*  GET TIME STAMP FIELD l_time.
*
*  CLEAR w_zsdt0313.
*  w_zsdt0313-mandt             = sy-mandt.
*  w_zsdt0313-id_doc_agrupador  = p_id_doc_agrupador.
*  w_zsdt0313-id_seq            = l_time.
*  w_zsdt0313-id_log            = 1.
*  w_zsdt0313-mensagem          = p_mensagem.
*  w_zsdt0313-usname            = sy-uname.
*  w_zsdt0313-data              = sy-datum.
*  w_zsdt0313-hora              = sy-uzeit.
*
*  MODIFY zsdt0313           FROM w_zsdt0313.
*
*ENDFORM.

**********************************************************************
**********************************************************************
*&---------------------------------------------------------------------*
*& Form f_refresh_doc
*&---------------------------------------------------------------------*
FORM f_refresh_doc CHANGING cv_erro. " 25.11.2024 - RAMON - US #158242.
  BREAK rblima.
  DATA lo_bry TYPE REF TO zcl_integracao_bry_assina_get.
  DATA lv_data TYPE zinc_bry_in_coleta.

  CLEAR cv_erro.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows IS INITIAL.

    MESSAGE s016(ds) WITH 'Selecionar uma linha' DISPLAY LIKE 'E'.

    cv_erro = abap_true.

    EXIT.

  ENDIF.

  DATA(lv_url) = zcl_integracao_bry_assina_get=>zif_integracao_bry_assina_get~get_url_environment( ).

  LOOP AT t_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

    READ TABLE t_anali    INTO w_anali       INDEX <fs_row>-index.

    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    READ TABLE t_zsdt0314 INTO w_zsdt0314
      WITH KEY nr_doc_gerado = w_zsdt0310-nr_doc_gerado
               id_doc_agrupador = w_zsdt0310-id_doc_agrupador.


    CHECK w_zsdt0310-status NE '06' AND w_zsdt0310-status NE '07'.

    CHECK w_zsdt0314-chave_pdf_assinado IS INITIAL.

    CREATE OBJECT lo_bry.

    DATA(r_docs_assinados) = lo_bry->zif_integracao_bry_assina_get~get_docs_assinados_by_wf_id( w_zsdt0314-chave_workflow ).

    CHECK r_docs_assinados IS NOT INITIAL.

    lv_data-chave =  w_zsdt0314-chave_workflow.
    lv_data-status = 'CONCLUIDO'.
    lv_data-protocoloassinatura-linkrelatorio = lv_url && '/scad/rest/documentos/' && w_zsdt0314-chave_workflow.

    APPEND INITIAL LINE TO lv_data-documentosassinados ASSIGNING FIELD-SYMBOL(<fs_doc_assi>).

    <fs_doc_assi>-chave = r_docs_assinados[ 1 ]-chavedocumento.
    <fs_doc_assi>-link = lv_url && '/scad/rest/documento/assinado/' && <fs_doc_assi>-chave.

    TRY .
        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
           )->get_obter_pdf_assinado( i_data = lv_data ). "<--- esse metodo atualiza as chaves do pdf assinado, nao baixa o arquivo

        zcl_integracao_insumos=>zif_integracao_insumos~get_instance(
          )->get_baixar_pdf_assinado(
        EXPORTING i_id_doc_agrupador = w_zsdt0310-id_doc_agrupador
                  i_nr_doc_gerado = w_zsdt0310-nr_doc_gerado ). "<--- esse metodo baixa o arquivo, mas tem que ter a chave primeiro

      CATCH zcx_integracao INTO DATA(ex_integra).
      CATCH zcx_error INTO DATA(ex_error).

    ENDTRY.

  ENDLOOP.

ENDFORM.
**<<<------"174343 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_slct_chk_lst_doc_jrdc
*&---------------------------------------------------------------------*
*& Seleciona dados da Checagem da Lista de documentação jurídica
*&---------------------------------------------------------------------*
FORM zf_slct_chk_lst_doc_jrdc.

* Verifica qual é a Área.
  CASE abap_on.
    WHEN p_insumo. "Insumos
      DATA(tl_zsdt0040) = t_zsdt0040.
      DELETE tl_zsdt0040 WHERE tpsim NE 'VP'.
      CHECK NOT tl_zsdt0040[] IS INITIAL.

      SELECT * FROM zsdt0381
        INTO TABLE tg_zsdt0381
        FOR ALL ENTRIES IN t_zsdt0040
      WHERE doc_simulacao EQ t_zsdt0040-doc_simulacao.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZSDCHCK_STAT'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = tg_chklstjd
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.

      ENDIF.

    WHEN p_merint. "Mercado Interno
* Do nothing.
    WHEN OTHERS.
*   Do nothing
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_proc_chk_lst_doc_jrdc
*&---------------------------------------------------------------------*
*& Processa dados da Checagem da Lista de documentação jurídica
*&---------------------------------------------------------------------*
*&     <-- CE_SINTE WA da Estrutura de exibição do Relsatório Sintético
*&---------------------------------------------------------------------*
FORM zf_proc_chk_lst_doc_jrdc CHANGING ce_sinte LIKE LINE OF t_sinte.

  CONSTANTS: cl_na TYPE char3 VALUE 'N/A'.

  ce_sinte-chklstjd = REDUCE #( INIT vl_chklstjd TYPE char30
                                FOR el_zsdt0381 IN tg_zsdt0381 WHERE ( doc_simulacao EQ ce_sinte-doc_simulacao )
                                FOR el_chklstjd IN tg_chklstjd WHERE ( domvalue_l    EQ el_zsdt0381-status     )
                                NEXT vl_chklstjd = el_chklstjd-ddtext
                              ).

  ce_sinte-chklstjd = COND #( WHEN ce_sinte-chklstjd IS INITIAL THEN cl_na ELSE ce_sinte-chklstjd ).

ENDFORM.
**<<<------"174343 - NMS - FIM------>>>
