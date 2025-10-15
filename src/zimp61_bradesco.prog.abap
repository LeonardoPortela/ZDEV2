*&---------------------------------------------------------------------*
*& Include          ZIMP61_BRADESCO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form zf_monta_arquivo_bradesco
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_monta_arquivo_bradesco .

  CLEAR: v_tot_tit.
  DATA: v_tp_lanc(2).

  REFRESH t_arquivo.

*------------------------------
*-verifica qual segmento
*------------------------------
  CLEAR v_segmento.

  READ TABLE t_zimp_cabecalho ASSIGNING <f_zimp_cabecalho> INDEX 1.

  IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
    v_segmento = 'O'.
  ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
    v_segmento = 'O'.
  ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
    CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
    SELECT SINGLE * FROM bnka INTO @DATA(wa_bnka)
     WHERE bankl LIKE @v_codbanco.
    IF sy-subrc IS INITIAL.
      v_segmento = 'J'.
    ELSE.
      v_segmento = 'O'.
    ENDIF.
  ELSE.
    v_segmento = 'N'.
  ENDIF.
*------------------------------
  CLEAR codigo_banco.
  PERFORM zf_codigo_banco      USING codigo_banco.
  SORT t_zimp_detalhe BY doc_imposto cod_abertura bukrs.
  lote_servico = 1.
  sequencial_reg_lote = 1.
  PERFORM prenche_header_arquivo.
  APPEND w_header_arquivo TO t_arquivo1.

** BUG - 184742 - CBRAND - Inicio
  IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
    LOOP AT t_zimp_cabecalho INTO DATA(w_zimp).
      CONCATENATE w_zimp-cod_barras+0(3) '%' INTO v_codbanco.
      SELECT SINGLE * FROM bnka INTO @DATA(w_bnka_aux)
            WHERE bankl LIKE @v_codbanco.
      IF sy-subrc IS INITIAL.
        APPEND w_zimp TO t_zimp_cabecalho_2.
      ELSE.
        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ENDIF.
    ENDLOOP.
  ELSE.
** BUG - 184742 - CBRAND - Fim
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- INICIO
    LOOP AT t_zimp_cabecalho INTO w_zimp.
      IF codigo_banco NE w_zimp-cod_barras(3).

        APPEND w_zimp TO t_zimp_cabecalho_aux.
      ELSE.
        APPEND w_zimp TO t_zimp_cabecalho_2.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF t_zimp_cabecalho_2[] IS NOT INITIAL.
    sequencial_reg_lote = 1.
    PERFORM preenche_header_lote USING '30'.
    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.

** BUG - 184742 - CBRAND - Inicio
      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '20'.
        w_header_lote_bbd-forma_lancamento   =  '31'.
        w_header_lote_bbd-num_vs_layout	     =   '040'.
      ENDIF.
** BUG - 184742 - CBRAND - Fim
      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_2 ASSIGNING <f_zimp_cabecalho>.
      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU

        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_bnka)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.
      v_tot_tit = v_tot_tit + v_tit.
      sequencial_reg_lote = sequencial_reg_lote + 1.

    ENDLOOP.
    PERFORM preenche_trailer_lote.
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.

  IF t_zimp_cabecalho_aux[] IS NOT INITIAL.
    sequencial_reg_lote = 1.
    lote_servico = 2.
    CLEAR: w_header_lote, v_tot_tit.
    PERFORM preenche_header_lote USING '31'.
    IF p_bncemp = 'BBD'.
      sequencial_reg_lote = 0.
      MOVE-CORRESPONDING w_header_lote TO w_header_lote_bbd.

** BUG - 184742 - CBRAND - Inicio
      IF <f_zimp_cabecalho>-tp_imposto EQ '11' .
        w_header_lote_bbd-tipo_servico       =  '22'.
        w_header_lote_bbd-forma_lancamento   =  '11'.
        w_header_lote_bbd-num_vs_layout	     =  '012'.
      ENDIF.
** BUG - 184742 - CBRANND - Fim

      APPEND w_header_lote_bbd    TO t_arquivo1.
    ELSE.
      APPEND w_header_lote    TO t_arquivo1.
    ENDIF.

    LOOP AT t_zimp_cabecalho_aux ASSIGNING <f_zimp_cabecalho>.

      CASE <f_zimp_cabecalho>-tp_imposto.
        WHEN'01'. " darf simples
          PERFORM zf_monta_tp_darf_simples.
        WHEN'02'. " darf normal/preto
          PERFORM zf_monta_tp_darf.
        WHEN '03'.
          PERFORM zf_monta_tp_gps.
        WHEN '04' OR '05' OR '06'.
          PERFORM zf_monta_tp_gare.
        WHEN '07'.
          PERFORM zf_monta_tp_fgts.
        WHEN '13'.
          PERFORM zf_monta_tp_ipva.
        WHEN '15'.
          PERFORM zf_monta_tp_licenciamento.
        WHEN '16'.
          PERFORM zf_monta_tp_darj.
      ENDCASE.

      APPEND w_arquivo TO t_arquivo.

      v_regs_proc = v_regs_proc + 1.

      PERFORM f_tbatch.
      IF <f_zimp_cabecalho>-tp_imposto EQ '07'.
        PERFORM preenche_segmento_o.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '14'.
        PERFORM preenche_segmento_o_gru.
        sequencial_reg_lote =  sequencial_reg_lote + 1.
        PERFORM preenche_segmento_w_gru.
      ELSEIF  <f_zimp_cabecalho>-tp_imposto EQ '09' OR <f_zimp_cabecalho>-tp_imposto EQ '10' OR <f_zimp_cabecalho>-tp_imposto EQ '11' OR <f_zimp_cabecalho>-tp_imposto EQ '12'. " IPTU
        CLEAR: v_codbanco.
        CONCATENATE  <f_zimp_cabecalho>-cod_barras+0(3) '%' INTO v_codbanco.
        SELECT SINGLE * FROM bnka INTO @DATA(w_bnk)
         WHERE bankl LIKE @v_codbanco.
        IF sy-subrc IS INITIAL.
          PERFORM preenche_segmento_j.
          PERFORM preenche_segmento_j52.
        ELSE.
          PERFORM preenche_segmento_o.
        ENDIF.
      ELSEIF <f_zimp_cabecalho>-tp_imposto EQ '08'.
        PERFORM preenche_segmento_o.
      ELSE.
        PERFORM preenche_segmento_n.
      ENDIF.

      v_tot_tit = v_tot_tit + v_tit.
      sequencial_reg_lote = sequencial_reg_lote + 1.

    ENDLOOP.
    PERFORM preenche_trailer_lote.
    APPEND w_trailer_lote    TO t_arquivo1.
  ENDIF.
*Analisar o tipo 10 - ISS em forma de boleto #180566 - BG -- FIM

  PERFORM preenche_trailer_arquivo.
  APPEND w_trailer_arquivo TO t_arquivo1.

  "Adicionar mais uma linha em branco quando for banco Bradesco, em branco.
  IF p_bncemp EQ 'BBD'.
    CLEAR: w_trailer_arquivo.
    APPEND w_trailer_arquivo TO t_arquivo1.
  ENDIF.

*-PBI 71420 - 12.01.2022 - JT - inicio
  DESCRIBE TABLE t_arquivo1 LINES DATA(l_lines_arq).
  LOOP AT t_arquivo1  INTO DATA(w_arq).
    IF p_bncemp = 'BBD'.
      CHECK sy-tabix < l_lines_arq.
    ENDIF.
    w_arq+240(2)         = cl_abap_char_utilities=>cr_lf.
    MODIFY t_arquivo1 FROM w_arq INDEX sy-tabix.
  ENDLOOP.
*-PBI 71420 - 12.01.2022 - JT - fim

  PERFORM zf_grava_arquivo.
  lote_servico = lote_servico + 1.

ENDFORM.
