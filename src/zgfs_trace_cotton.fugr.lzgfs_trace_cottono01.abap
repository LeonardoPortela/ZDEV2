*----------------------------------------------------------------------*
***INCLUDE LZGFS_TRACE_COTTONO01.
*----------------------------------------------------------------------*

***********************************************************************
* selecao lotes
***********************************************************************
FORM f_selecao  USING p_nro_sol
                      p_posnr
                      p_contrato.

  FREE: t_lotes, t_zsdt0045, t_zsdt0066, t_zsdt0328.  "*-CS2023000189-04.09.2023-#122555-JT

*  IF g_somente_exibe = abap_true.
*    SELECT *
*      FROM zsdt0328
*      INTO TABLE t_zsdt0328
*     WHERE nro_sol_ov  = p_nro_sol
*       AND posnr       = p_posnr
*       AND cancelado   = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT
*
*    IF t_zsdt0328[] IS NOT INITIAL.
*      SELECT *
*        FROM zsdt0045
*        INTO TABLE t_zsdt0045
*         FOR ALL ENTRIES IN t_zsdt0328
*       WHERE zseq_inst   = t_zsdt0328-zseq_inst
*         AND objek       = t_zsdt0328-objek
*         AND objecttable = t_zsdt0328-objecttable.
*    ENDIF.

*   LOOP AT t_zsdt0045             INTO w_zsdt0045.
*     MOVE-CORRESPONDING w_zsdt0045  TO w_lotes.
*     MOVE abap_off                  TO w_lotes-mark.
*     APPEND w_lotes                 TO t_lotes.
*   ENDLOOP.

*  ELSE.

  SELECT *
    FROM zsdt0045
    INTO TABLE t_zsdt0045
   WHERE objek    = p_nro_sol
     AND contrato = p_contrato.

* SORT t_zsdt0045 BY instrucao werks charg zseq_inst DESCENDING.
* DELETE ADJACENT DUPLICATES FROM t_zsdt0045
*                        COMPARING instrucao werks charg.

*-CS2023000189-04.09.2023-#122555-JT-inicio
  IF t_zsdt0045[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0328
      INTO TABLE t_zsdt0328
       FOR ALL ENTRIES IN t_zsdt0045
     WHERE zseq_inst   = t_zsdt0045-zseq_inst
       AND objek       = t_zsdt0045-objek
       AND objecttable = t_zsdt0045-objecttable
       AND cancelado   = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT

*    SELECT *
*      FROM zsdt0066
*      INTO TABLE t_zsdt0066
*       FOR ALL ENTRIES IN t_zsdt0045
*     WHERE instrucao = t_zsdt0045-instrucao
*       AND werks     = t_zsdt0045-werks
*       AND status   <> 'D'.  "*-CS2023000189-04.09.2023-#122555-JT
*
*    IF t_zsdt0066[] IS NOT INITIAL.
*      SELECT *
*        FROM zsdt0328
*        INTO TABLE t_zsdt0328
*         FOR ALL ENTRIES IN t_zsdt0066
*       WHERE nro_sol_ov  = t_zsdt0066-nro_sol_ov
*         AND posnr       = t_zsdt0066-posnr
*         AND cancelado   = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT
*    ENDIF.
*  ENDIF.
  ENDIF.
*-CS2023000189-04.09.2023-#122555-JT-fim

  LOOP AT t_zsdt0045             INTO w_zsdt0045.

    FREE: l_quantidade.
*   LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE instrucao = w_zsdt0045-instrucao
*                                        AND werks     = w_zsdt0045-werks
*                                        AND charg_ori = w_zsdt0045-charg.
*     l_quantidade = l_quantidade + w_zsdt0328-quantidade.
*   ENDLOOP.
    LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE zseq_inst   = w_zsdt0045-zseq_inst
                                         AND objek       = w_zsdt0045-objek
                                         AND objecttable = w_zsdt0045-objecttable.
      l_quantidade = l_quantidade + w_zsdt0328-quantidade.
    ENDLOOP.

    l_restante     = w_zsdt0045-quantidade - l_quantidade.

    IF l_restante <= 0 AND g_editar = abap_false AND g_somente_exibe = abap_false.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING w_zsdt0045  TO w_lotes.
    MOVE 0                         TO w_lotes-quantidade.
    MOVE l_quantidade              TO w_lotes-quantidade_util.
    MOVE l_restante                TO w_lotes-quantidade_disp.
    MOVE l_quantidade              TO w_lotes-quantidade_util_orig.
    MOVE l_restante                TO w_lotes-quantidade_disp_orig.
    MOVE g_referencia              TO w_lotes-referencia.

    READ TABLE t_entrada         INTO w_entrada WITH KEY zseq_inst   = w_zsdt0045-zseq_inst
                                                         objek       = w_zsdt0045-objek
                                                         objecttable = w_zsdt0045-objecttable
                                                         referencia  = g_referencia.
    IF sy-subrc = 0.
      MOVE abap_on                  TO w_lotes-mark.
      IF g_editar = abap_off.
        w_lotes-quantidade           = w_entrada-quantidade.
        w_lotes-quantidade_disp      = w_lotes-quantidade_disp_orig - w_lotes-quantidade.
        w_lotes-quantidade_util      = w_lotes-quantidade_util_orig + w_lotes-quantidade.
      ELSE.
        w_lotes-quantidade           = w_entrada-quantidade.
        w_lotes-quantidade_disp      = w_lotes-quantidade_disp_orig.
        w_lotes-quantidade_util      = w_lotes-quantidade_util_orig.
        w_lotes-quantidade_disp_orig = w_lotes-quantidade_disp_orig + w_lotes-quantidade.
        w_lotes-quantidade_util_orig = w_lotes-quantidade_util_orig - w_lotes-quantidade.
      ENDIF.
    ELSE.
      MOVE abap_off                TO w_lotes-mark.
    ENDIF.

    IF g_somente_exibe = abap_false.
      APPEND w_lotes               TO t_lotes.
    ELSEIF sy-subrc = 0.
      APPEND w_lotes               TO t_lotes.
    ENDIF.
  ENDLOOP.

ENDFORM.


*** Inicio - Rubenilson Pereira - 03.03.2025 - US164130
***********************************************************************
* selecao lotes
***********************************************************************
FORM f_selecao2  USING p_nro_sol
                       p_posnr
                       p_contrato.

  FREE: t_lotes, t_zsdt0045, t_zsdt0066, t_zsdt0328.  "*-CS2023000189-04.09.2023-#122555-JT

  SELECT *
    FROM zsdt0045
    INTO TABLE t_zsdt0045
    WHERE incoterm NE space.

  IF t_zsdt0045[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0328
      INTO TABLE t_zsdt0328
       FOR ALL ENTRIES IN t_zsdt0045
     WHERE zseq_inst   = t_zsdt0045-zseq_inst
       AND objek       = t_zsdt0045-objek
       AND objecttable = t_zsdt0045-objecttable
       AND cancelado   = abap_off.

  ENDIF.

  LOOP AT t_zsdt0045             INTO w_zsdt0045.

    FREE: l_quantidade.

    LOOP AT t_zsdt0328 INTO w_zsdt0328 WHERE zseq_inst   = w_zsdt0045-zseq_inst
                                         AND objek       = w_zsdt0045-objek
                                         AND objecttable = w_zsdt0045-objecttable.
      l_quantidade = l_quantidade + w_zsdt0328-quantidade.
    ENDLOOP.

    l_restante     = w_zsdt0045-quantidade - l_quantidade.

    IF l_restante <= 0 AND g_editar = abap_false AND g_somente_exibe = abap_false.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING w_zsdt0045  TO w_lotes.
    MOVE 0                         TO w_lotes-quantidade.
    MOVE l_quantidade              TO w_lotes-quantidade_util.
    MOVE l_restante                TO w_lotes-quantidade_disp.
    MOVE l_quantidade              TO w_lotes-quantidade_util_orig.
    MOVE l_restante                TO w_lotes-quantidade_disp_orig.
    MOVE g_referencia              TO w_lotes-referencia.

    READ TABLE t_entrada         INTO w_entrada WITH KEY zseq_inst   = w_zsdt0045-zseq_inst
                                                         objek       = w_zsdt0045-objek
                                                         objecttable = w_zsdt0045-objecttable
                                                         referencia  = g_referencia.
    IF sy-subrc = 0.
      MOVE abap_on                  TO w_lotes-mark.
      IF g_editar = abap_off.
        w_lotes-quantidade           = w_entrada-quantidade.
        w_lotes-quantidade_disp      = w_lotes-quantidade_disp_orig - w_lotes-quantidade.
        w_lotes-quantidade_util      = w_lotes-quantidade_util_orig + w_lotes-quantidade.
      ELSE.
        w_lotes-quantidade           = w_entrada-quantidade.
        w_lotes-quantidade_disp      = w_lotes-quantidade_disp_orig.
        w_lotes-quantidade_util      = w_lotes-quantidade_util_orig.
        w_lotes-quantidade_disp_orig = w_lotes-quantidade_disp_orig + w_lotes-quantidade.
        w_lotes-quantidade_util_orig = w_lotes-quantidade_util_orig - w_lotes-quantidade.
      ENDIF.
    ELSE.
      MOVE abap_off                TO w_lotes-mark.
    ENDIF.

    IF g_somente_exibe = abap_false.
      APPEND w_lotes               TO t_lotes.
    ELSEIF sy-subrc = 0.
      APPEND w_lotes               TO t_lotes.
    ENDIF.
  ENDLOOP.

ENDFORM.
*** Fim - Rubenilson Pereira - 03.03.2025 - US164130

***********************************************************************
* valida lotes selecinados
***********************************************************************
FORM f_valida_selecao CHANGING p_error.

  FREE: t_saida, p_error.

  READ TABLE t_lotes INTO w_lotes WITH KEY referencia = g_referencia
                                           mark       = abap_on.
  IF sy-subrc <> 0.
    p_error = abap_on.
    MESSAGE s024(sd) WITH 'Escolher pelo menos uma Instrução!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE t_lotes INTO w_lotes_sel WITH KEY referencia = g_referencia
                                               mark       = abap_on.

  LOOP AT t_lotes INTO w_lotes WHERE referencia = g_referencia
                                 AND mark       = abap_on.
    IF w_lotes-werks     <> w_lotes_sel-werks     OR
       w_lotes-matnr     <> w_lotes_sel-matnr     OR
       w_lotes-instrucao <> w_lotes_sel-instrucao OR
       w_lotes-dmbtr     <> w_lotes_sel-dmbtr.
      p_error = abap_on.
      MESSAGE s024(sd) WITH 'Escolher Lotes de mesmo '
                            'CENTRO, MATERIAL, INSTRUÇÂO e PREÇO.' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF w_lotes-quantidade = 0.
      p_error = abap_on.
      MESSAGE s024(sd) WITH 'Há Lote sem Quantidade Informada' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF w_lotes-quantidade > w_lotes-quantidade_disp_orig.
      p_error = abap_on.
      MESSAGE s024(sd) WITH 'Quantidade informada não disponível!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK p_error = abap_off.

*------------------------
* monta tabela saida
*------------------------
  DELETE t_entrada WHERE referencia = g_referencia.

  LOOP AT t_lotes             INTO w_lotes WHERE referencia = g_referencia
                                             AND mark       = abap_on.
    MOVE-CORRESPONDING w_lotes  TO w_saida.
    MOVE w_lotes-charg          TO w_saida-charg_ori.
    APPEND w_saida              TO t_saida.
  ENDLOOP.

  APPEND LINES OF t_entrada[]   TO t_saida[].

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

  PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort_alv.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
* w_layout-no_totarr    = abap_false.
* w_layout-no_totexp    = abap_false.
* w_layout-no_totline   = abap_false.
* w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                 lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>user_command     FOR g_grid,
                 lcl_event_handler=>toolbar          FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = pt_exclude
      CHANGING
        it_outtab                     = t_lotes[]
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  DATA: l_edita  TYPE char1.

  l_edita = COND #( WHEN g_somente_exibe = abap_true THEN abap_false
                                                     ELSE abap_true ).

  FREE t_fieldcat[].

  IF g_somente_exibe = abap_false.
    PERFORM f_estrutura_alv USING:
      01  ''      ''     'T_LOTES' 'MARK'                ''                         '03'  'X' ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' ' '.
  ENDIF.

  PERFORM f_estrutura_alv USING:
    02  ''      ''       'T_LOTES' 'ZSEQ_INST'           'Seq.Instrução'            '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    03  ''      ''       'T_LOTES' 'INSTRUCAO'           'Instrucao'                '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    04  ''      ''       'T_LOTES' 'CHARG'               'Lote'                     '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_LOTES' 'QUANTIDADE'          'Quantidade'               '14'  l_edita ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''      ''       'T_LOTES' 'QUANTIDADE_DISP'     'Quant.Disponivel'         '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_LOTES' 'QUANTIDADE_UTIL'     'Quant.Utilizada'          '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''      ''       'T_LOTES' 'DMBTR'               'Preço'                    '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  'MARA'  'MATNR'  'T_LOTES' 'MATNR'               'Material'                 '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_LOTES' 'WERKS'               'Centro'                   '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  'LFA1'  'LIFNR'  'T_LOTES' 'PONTO_C'             'Fornecedor'               '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    12  'LFA1'  'LIFNR'  'T_LOTES' 'TERMINAL'            'Terminal'                 '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM f_sort_alv.
  FREE: t_sort, w_sort.

  w_sort-spos      = '1'.
  w_sort-fieldname = 'INSTRUCAO'.
  w_sort-up        = abap_true.
* w_sort-subtot    = abap_false.
  APPEND w_sort   TO t_sort.

  w_sort-spos      = '2'.
  w_sort-fieldname = 'WERKS'.
  w_sort-up        = abap_true.
* w_sort-subtot    = abap_false.
  APPEND w_sort   TO t_sort.
*
  w_sort-spos      = '3'.
  w_sort-fieldname = 'MATNR'.
  w_sort-up        = abap_true.
* w_sort-subtot    = abap_true.
  APPEND w_sort  TO t_sort.

ENDFORM.

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
*-US 119847 - 01.08.2023 - JT - inicio
* APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
*-US 119847 - 01.08.2023 - JT - fim
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: ok_code, f_code.

  IF g_somente_exibe = abap_true.
    APPEND 'SELECAO' TO f_code.
  ENDIF.

  SET PF-STATUS 'FORMALOTE01' EXCLUDING f_code.
  SET TITLEBAR 'FORMALOTE01'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  FREE: l_error.

  CASE ok_code.

    WHEN 'SELECAO'.
      CALL METHOD g_grid->check_changed_data.

      PERFORM f_valida_selecao CHANGING l_error.

      IF l_error = abap_off.
        CALL METHOD g_custom_container->free.
        FREE: g_grid.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      t_saida[] = t_entrada[].
      g_back = abap_true.

      CALL METHOD g_custom_container->free.
      FREE: g_grid.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.


*** Inicio - Rubenilson Pereira - 03.03.2025 - US164130
*****************************************************************************
*****************************************************************************
*&---------------------------------------------------------------------*
*& Form f_agrupamento
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- T_RETORNO
*&---------------------------------------------------------------------*
FORM f_agrupamento  CHANGING t_retorno TYPE tb_lotes3.

  DATA: lt_saida  TYPE TABLE OF zsdt0328,
        lw_lotes2 TYPE ty_lotes2,
        lt_lotes2 TYPE TABLE OF ty_lotes2.

  DELETE t_entrada WHERE referencia = g_referencia.

  LOOP AT t_lotes             INTO w_lotes.

    MOVE-CORRESPONDING w_lotes TO lw_lotes2.

    COLLECT lw_lotes2 INTO lt_lotes2.

  ENDLOOP.

  SORT t_zsdt0045 BY instrucao matnr werks dmbtr.
  SORT t_lotes    BY instrucao matnr werks dmbtr.

  DATA(lt_retorno) = lt_lotes2.
  SORT lt_retorno BY werks.
  DELETE ADJACENT DUPLICATES FROM lt_retorno COMPARING werks.
  IF lt_retorno IS NOT INITIAL.
    SELECT werks, regio
      FROM t001w
      INTO TABLE @DATA(lt_001w)
      FOR ALL ENTRIES IN @lt_retorno
      WHERE werks = @lt_retorno-werks.
    IF sy-subrc IS INITIAL.
      SORT lt_001w BY werks.
    ENDIF.
  ENDIF.

  LOOP AT lt_lotes2 ASSIGNING FIELD-SYMBOL(<fs_lotes2>).

    APPEND INITIAL LINE TO t_retorno ASSIGNING FIELD-SYMBOL(<fs_retorno>).
    MOVE-CORRESPONDING <fs_lotes2> TO <fs_retorno>.

    READ TABLE t_lotes ASSIGNING FIELD-SYMBOL(<fs_lote>)
    WITH KEY instrucao = <fs_retorno>-instrucao
             matnr     = <fs_retorno>-matnr
             werks     = <fs_retorno>-werks
             dmbtr     = <fs_retorno>-dmbtr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_Retorno>-seq         = <fs_lote>-zseq_inst.

      READ TABLE t_zsdt0045 ASSIGNING FIELD-SYMBOL(<fs_0045>)
      WITH KEY instrucao = <fs_retorno>-instrucao
         matnr    = <fs_retorno>-matnr
         werks    = <fs_retorno>-werks
         dmbtr    = <fs_retorno>-dmbtr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_Retorno>-inco1       = <fs_0045>-incoterm.
        <fs_Retorno>-contrato    = <fs_0045>-contrato.
      ENDIF.

      <fs_Retorno>-objek       = <fs_lote>-objek.
      <fs_Retorno>-objecttable = <fs_lote>-objecttable.
      <fs_Retorno>-referencia  = '0000001001'.
      <fs_Retorno>-charg       = <fs_lote>-charg.
    ENDIF.

    READ TABLE lt_001w ASSIGNING FIELD-SYMBOL(<fs_001w>)
    WITH KEY werks = <fs_retorno>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_retorno>-region = <fs_001w>-regio.
    ENDIF.

  ENDLOOP.

ENDFORM.
*** Fim - Rubenilson Pereira - 03.03.2025 - US164130
