*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_CLASS
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
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_hotspot_click_analit FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.

    DATA: w_opt TYPE ctu_params.

    FREE: t_bdc.

    READ TABLE t_sinte INTO w_sinte INDEX e_row_id-index.

    DATA(l_status) = COND #( WHEN p_pend   = abap_true THEN 'P'
                             WHEN p_conclu = abap_true THEN 'C'
                             WHEN p_todos  = abap_true THEN 'T' ).

    CASE e_column_id.
      WHEN 'DOC_SIMULACAO'.
        PERFORM f_preencher_dynpro USING:
                    'X' 'ZSDR016'                      '0100',
                    ' ' 'WG_HEADER-DOC_SIMULACAO'      w_sinte-doc_simulacao,
                    ' ' 'BDC_OKCODE'                   'ATUAL'.

        w_opt-dismode = 'E'.
        w_opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0044' USING t_bdc OPTIONS FROM w_opt.

      WHEN 'CONTRATO'.
        CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
          EXPORTING
            i_popup         = abap_true
            i_analit        = abap_true
            i_doc_simulacao = w_sinte-doc_simulacao
            i_contra        = abap_true
            i_pend          = p_pend_sel
            i_conclu        = p_conclu_sel
            i_todos         = p_todos_sel.

        PERFORM f_selecao_dados   USING 0.
        PERFORM f_processa_dados_sintetico.

      WHEN 'PED_VENDA'.
        CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
          EXPORTING
            i_popup         = abap_true
            i_analit        = abap_true
            i_doc_simulacao = w_sinte-doc_simulacao
            i_venda         = abap_true
            i_pend          = p_pend_sel
            i_conclu        = p_conclu_sel
            i_todos         = p_todos_sel.

        PERFORM f_selecao_dados   USING 0.
        PERFORM f_processa_dados_sintetico.

      WHEN 'DISTRATO'.
        CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
          EXPORTING
            i_popup         = abap_true
            i_analit        = abap_true
            i_doc_simulacao = w_sinte-doc_simulacao
            i_distra        = abap_true
            i_pend          = p_pend_sel
            i_conclu        = p_conclu_sel
            i_todos         = p_todos_sel.

        PERFORM f_selecao_dados   USING 0.
        PERFORM f_processa_dados_sintetico.

      WHEN 'ADITIVOS'.
        CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
          EXPORTING
            i_popup         = abap_true
            i_analit        = abap_true
            i_doc_simulacao = w_sinte-doc_simulacao
            i_aditiv        = abap_true
            i_pend          = p_pend_sel
            i_conclu        = p_conclu_sel
            i_todos         = p_todos_sel.

        PERFORM f_selecao_dados   USING 0.
        PERFORM f_processa_dados_sintetico.

      WHEN 'DECLAR_REC'.
        CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
          EXPORTING
            i_popup         = abap_true
            i_analit        = abap_true
            i_doc_simulacao = w_sinte-doc_simulacao
            i_decrec        = abap_true
            i_pend          = p_pend_sel
            i_conclu        = p_conclu_sel
            i_todos         = p_todos_sel.

        PERFORM f_selecao_dados   USING 0.
        PERFORM f_processa_dados_sintetico.
**<<<------"174343 - NMS - INI------>>>
      WHEN 'CHKLSTJD'.
        CHECK w_sinte-chklstjd NE 'N/A'.
        DATA(rl_docsi) = s_docsi[].
        CLEAR: rl_docsi, rl_docsi[].
        APPEND INITIAL LINE TO rl_docsi ASSIGNING FIELD-SYMBOL(<fsr_docsi>).
        <fsr_docsi> = 'IEQ'.
        <fsr_docsi>+3 = w_sinte-doc_simulacao.
* Chama o programa "Quest.Jurídico Insumos X Simulador"
        SUBMIT zsdr0201 WITH so_simu  IN rl_docsi
                        WITH p_co_chk EQ abap_on
                        AND RETURN.
**<<<------"174343 - NMS - FIM------>>>
    ENDCASE.

    IF g_popup = abap_false.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ELSE.
      CALL METHOD g_grid_pop->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.

  ENDMETHOD.

  METHOD on_hotspot_click_analit.

    DATA: w_opt TYPE ctu_params.

    FREE: t_bdc.

    READ TABLE t_anali    INTO w_anali    INDEX e_row_id-index.
    READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.

    CASE e_column_id.

      WHEN 'DOC_SIMULACAO'.
**<<<------"168919 - NMS - INI------>>>
        IF NOT p_insumo IS INITIAL      AND
               p_tpinsm EQ sy-abcde+2(1). "C - Compra
          PERFORM f_preencher_dynpro USING:
                      'X' 'ZMMR149'                      '0100',
                      ' ' 'WG_CADLAN-NRO_SOL_CP'         w_anali-doc_simulacao,
                      ' ' 'BDC_OKCODE'                   'DISPLA'.

          w_opt-dismode = 'E'.
          w_opt-defsize = ' '.

          CALL TRANSACTION 'ZMM0149' USING t_bdc OPTIONS FROM w_opt.

        ELSE.
**<<<------"168919 - NMS - FIM------>>>
          PERFORM f_preencher_dynpro USING:
                      'X' 'ZSDR016'                      '0100',
                      ' ' 'WG_HEADER-DOC_SIMULACAO'      w_anali-doc_simulacao,
                      ' ' 'BDC_OKCODE'                   'ATUAL'.

          w_opt-dismode = 'E'.
          w_opt-defsize = ' '.

          CALL TRANSACTION 'ZSDT0044' USING t_bdc OPTIONS FROM w_opt.
**<<<------"168919 - NMS - INI------>>>
        ENDIF.
**<<<------"168919 - NMS - FIM------>>>
      WHEN 'DOCUMENTO'.
        PERFORM f_gerar_documento   USING w_anali-id_documento
                                          w_anali-tpdoc
                                          w_anali-status_cod.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

      WHEN 'ASSINATURA'.
        PERFORM f_gerar_assinatura  USING w_anali-id_documento
                                          w_anali-tpdoc
                                          w_anali-status_cod.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

      WHEN 'SEQ_ADIT'.

        CALL FUNCTION 'ZSD_INSUMOS_LISTA_ADITIVOS'
          EXPORTING
            i_doc_simulacao = w_anali-doc_simulacao.

      WHEN 'VBELN'.
        IF w_anali-vbeln IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD w_anali-vbeln.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'LOG'.
        CALL FUNCTION 'ZSD_INSUMOS_LISTA_LOG'
          EXPORTING
            i_id_doc_agrupador = w_zsdt0310-id_documento.

    ENDCASE.

    IF g_popup = abap_false.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ELSE.
      CALL METHOD g_grid_pop->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.

    CASE e_ucomm.

      WHEN 'GERAR'.
        IF g_popup = abap_false.
          CALL METHOD g_grid->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ELSE.
          CALL METHOD g_grid_pop->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ENDIF.

        IF t_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- checa se linhas selecionadas estao no mesmo status
*-------------------------------
        FREE: l_erro.

        LOOP AT t_rows INTO w_rows.
          READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
          READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.
          IF w_zsdt0310-status <> '00' AND
             w_zsdt0310-status <> '11'.
            l_erro = abap_true.
          ENDIF.
        ENDLOOP.

        IF l_erro = abap_true.
          MESSAGE s024(sd) WITH TEXT-260 TEXT-261 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- gerar documentos
*-------------------------------
        PERFORM f_gerar_documento   USING 0 '' ''.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

      WHEN 'ASSINAR'.
        IF g_popup = abap_false.
          CALL METHOD g_grid->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ELSE.
          CALL METHOD g_grid_pop->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ENDIF.

        IF t_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- checa se linhas selecionadas estao no mesmo status
*-------------------------------
        FREE: l_erro.

        LOOP AT t_rows INTO w_rows.
          READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
          READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.
          IF w_zsdt0310-status           <> '02' AND
             w_zsdt0310-tipo_doc_digital <> 'S'.
            l_erro = abap_true.
          ENDIF.
        ENDLOOP.

        IF l_erro = abap_true.
          MESSAGE s024(sd) WITH TEXT-260 TEXT-261 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- gerar assinatura
*-------------------------------
        PERFORM f_gerar_assinatura  USING 0 '' ''.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

      WHEN 'CANCELAR'.
        IF g_popup = abap_false.
          CALL METHOD g_grid->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ELSE.
          CALL METHOD g_grid_pop->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ENDIF.

        IF t_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- cancelar coleta
*-------------------------------
        PERFORM f_cancelar_documento  USING 0 '' ''.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

      WHEN 'VERPDF'.
        IF g_popup = abap_false.
          CALL METHOD g_grid->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ELSE.
          CALL METHOD g_grid_pop->get_selected_rows
            IMPORTING
              et_index_rows = t_rows.
        ENDIF.

        IF t_rows[] IS INITIAL.
          MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- checa se linhas selecionadas estao no mesmo status
*-------------------------------
        FREE: l_erro.

        LOOP AT t_rows INTO w_rows.
          READ TABLE t_anali    INTO w_anali       INDEX w_rows-index.
          READ TABLE t_zsdt0310 INTO w_zsdt0310 WITH KEY id_documento = w_anali-id_documento.
          IF w_zsdt0310-status <> '02' AND
             w_zsdt0310-status <> '04' AND
             w_zsdt0310-status <> '05' AND
             w_zsdt0310-status <> '06' AND
             w_zsdt0310-status <> '07'.
            l_erro = abap_true.
          ENDIF.
        ENDLOOP.

        IF l_erro = abap_true.
          MESSAGE s024(sd) WITH TEXT-260 TEXT-261 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*-------------------------------
* ----- visualiza PDF
*-------------------------------
        PERFORM f_gerar_documento   USING 0 '' ''.

        IF g_popup = abap_false.
          PERFORM f_selecao_dados   USING 0.
        ELSE.
          PERFORM f_selecao_dados   USING g_doc_simulacao.
        ENDIF.

        PERFORM f_processa_dados_analitico.

    ENDCASE.

    IF g_popup = abap_false.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ELSE.
      CALL METHOD g_grid_pop->refresh_table_display
        EXPORTING
          is_stable = w_stable.
    ENDIF.

    IF lines( t_rows ) > 0.
      IF g_popup = abap_false.
        CALL METHOD g_grid->set_selected_rows
          EXPORTING
            it_index_rows = t_rows.
      ELSE.
        CALL METHOD g_grid_pop->set_selected_rows
          EXPORTING
            it_index_rows = t_rows.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD toolbar.

    DATA wa_tool TYPE stb_button.

    CHECK p_analit = abap_true.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function  = 'GERAR'.
    wa_tool-icon      = icon_execute_object.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Gerar Documento'.
    wa_tool-text      = 'Gerar Documento'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'ASSINAR'.
    wa_tool-icon      = icon_execute_object.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Assinar Documento'.
    wa_tool-text      = 'Assinar Documento'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'VERPDF'.
    wa_tool-icon      = icon_select_detail.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Visualizar PDF'.
    wa_tool-text      = 'Visualizar PDF'.
    APPEND wa_tool TO e_object->mt_toolbar.

    wa_tool-function  = 'CANCELAR'.
    wa_tool-icon      = icon_cancel.
    wa_tool-disabled  = abap_false.
    wa_tool-quickinfo = 'Cancelar Documento'.
    wa_tool-text      = 'Cancelar Documento'.
    APPEND wa_tool TO e_object->mt_toolbar.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
