*&---------------------------------------------------------------------*
*& Report  ZSDR0126                                                    *&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Associa RTC SIAGRI x Armazém Responsável                *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0126.

TYPE-POOLS : slis, icon, kkblo.

*******************************************************************************************
* TABELAS
*******************************************************************************************

*******************************************************************************************
* FIELD SYNBOLS
*******************************************************************************************

*******************************************************************************************
* classes / btree
*******************************************************************************************
CLASS cl_gui_column_tree     DEFINITION LOAD.
CLASS cl_gui_cfw             DEFINITION LOAD.

DATA: tree1               TYPE REF TO cl_hrpayna_gui_alv_tree, "cl_gui_alv_tree.
      mr_toolbar          TYPE REF TO cl_gui_toolbar,
      g_container         TYPE scrfname VALUE 'CONTAINER1',
      g_container2        TYPE scrfname VALUE 'CONTAINER2',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_zsdt0259.
         INCLUDE STRUCTURE zsdt0259.
         TYPES: cpf2(14)    TYPE c,
         status2(10) TYPE c,
         modif       TYPE c.
TYPES: END OF ty_zsdt0259.

TYPES: BEGIN OF ty_armaz,
*        lifnr TYPE zsdt0266-cod_armazem, "*-CS2021000218-03.10.2022-#91289-JT-inicio
         werks TYPE zsdt0266-werks,       "*-CS2021000218-03.10.2022-#91289-JT-inicio
         name1 TYPE lfa1-name1,
         ort01 TYPE lfa1-ort01,
         regio TYPE lfa1-regio.
TYPES: END   OF ty_armaz.

*-CS2021000218-03.10.2022-#91289-JT-inicio
*TYPES: BEGIN OF ty_lfa1,
*         lifnr TYPE lfa1-lifnr,
*         name1 TYPE lfa1-name1,
*         ort01 TYPE lfa1-ort01,
*         regio TYPE lfa1-regio.
*TYPES: END   OF ty_lfa1.

TYPES: BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
         ort01 TYPE t001w-ort01,
         regio TYPE t001w-regio.
TYPES: END   OF ty_t001w.
*-CS2021000218-03.10.2022-#91289-JT-fim

*******************************************************************************************
* tabelas / works
*******************************************************************************************

DATA: ok_code         TYPE sy-ucomm,
      g_cpf(14)       TYPE c,
      g_nome          TYPE zsdt0259-nome,
      g_email         TYPE zsdt0259-email,
      g_assin_eletro  TYPE char1,    "*-CS2021000218-03.10.2022-#91289-JT-inicio
      g_assin_manual  TYPE char1,    "*-CS2021000218-03.10.2022-#91289-JT-inicio
      g_index_259     TYPE sy-tabix, "*-CS2021000218-03.10.2022-#91289-JT-inicio
*
      t_zsdt0259      TYPE TABLE OF ty_zsdt0259,
      t_zsdt0266      TYPE TABLE OF zsdt0266,
      t_zsdt0266_aux  TYPE TABLE OF zsdt0266,
*     t_lfa1          TYPE TABLE OF ty_lfa1,
      t_t001w         TYPE TABLE OF ty_t001w,
      t_armaz         TYPE TABLE OF ty_armaz,
      w_zsdt0259      TYPE ty_zsdt0259,
      w_zsdt0266      TYPE zsdt0266,
      w_zsdt0266_aux  TYPE zsdt0266,
      w_armaz         TYPE ty_armaz,
*     w_lfa1          TYPE ty_lfa1,
      w_t001w         TYPE ty_t001w,
*
      l_ok            TYPE c,
      l_lines         TYPE i,
      l_edit          TYPE c,
      l_erro          TYPE c,
      l_tab_cpf       TYPE zsdt0259-cpf,
*
      t_fieldcatalog  TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldcatalog2 TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab        TYPE slis_t_extab,
      w_item_layout   TYPE lvc_s_laci,
      w_layout        TYPE lvc_s_layo,
      ls_fieldcatalog TYPE lvc_s_fcat,
      ls_exclude      TYPE ui_func,
      pt_exclude      TYPE ui_functions,
      pt_exclude2     TYPE ui_functions,
      t_del_rows      TYPE lvc_t_row,
      w_del_rows      TYPE lvc_s_row,
      t_sel_cols      TYPE lvc_t_col,
      w_sel_cols      TYPE lvc_s_col,
      l_row_id        TYPE lvc_s_row,
      l_column_id     TYPE lvc_s_col,
      l_stable        TYPE lvc_s_stbl,
      l_tabix         TYPE sy-tabix.

DATA : t_fcat_lvc TYPE lvc_s_fcat OCCURS 0 WITH HEADER LINE,
       t_fcat_kkb TYPE kkblo_t_fieldcat,
       w_stable   TYPE lvc_s_stbl.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdr126_toolbar_event_receiver.
INCLUDE zsdr126_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
* START-OF-SELECRTION
*******************************************************************************************
START-OF-SELECTION.

  l_edit = abap_false.

  g_assin_manual = abap_true. "*-CS2021000218-03.10.2022-#91289-JT-inicio

  PERFORM f_selecao_dados.

  CALL SCREEN 100.

*******************************************************************************************
* selecao dados
*******************************************************************************************
FORM f_selecao_dados.

  FREE: t_zsdt0259,
        t_zsdt0266,
        t_armaz,
        g_cpf,
        g_nome,
        g_email.

  DELETE FROM zsdt0266 WHERE werks = abap_off.  "*-CS2021000218-03.10.2022-#91289-JT-inicio

  IF l_edit = abap_false.
    SELECT *
      FROM zsdt0266
      INTO TABLE t_zsdt0266.

    IF t_zsdt0266[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0259
        INTO TABLE t_zsdt0259
         FOR ALL ENTRIES IN t_zsdt0266
       WHERE cpf = t_zsdt0266-cpf.
    ENDIF.
  ELSE.
    SELECT *
      FROM zsdt0259
      INTO TABLE t_zsdt0259.

    IF t_zsdt0259[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0266
        INTO TABLE t_zsdt0266
         FOR ALL ENTRIES IN t_zsdt0259
       WHERE cpf = t_zsdt0259-cpf.
    ENDIF.
  ENDIF.

  LOOP AT t_zsdt0259 INTO w_zsdt0259.
    l_tabix = sy-tabix.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = w_zsdt0259-cpf
      IMPORTING
        output = w_zsdt0259-cpf2.

    IF w_zsdt0259-status = 'S'.
      w_zsdt0259-status2 = 'Ativo'.
    ELSE.
      w_zsdt0259-status2 = 'Inativo'.
    ENDIF.

    MODIFY t_zsdt0259 FROM w_zsdt0259 INDEX l_tabix.
  ENDLOOP.

*-CS2021000218-03.10.2022-#91289-JT-inicio
* IF t_zsdt0266[] IS NOT INITIAL.
*  SELECT lifnr name1 ort01 regio
*    FROM lfa1
*    INTO TABLE t_lfa1.
*    FOR ALL ENTRIES IN t_zsdt0266
*  WHERE lifnr = t_zsdt0266-cod_armazem.
*  ENDIF.
  SELECT werks name1 ort01 regio
    FROM t001w
    INTO TABLE t_t001w.

  SORT t_t001w BY werks.
* SORT t_lfa1 BY lifnr.
*-CS2021000218-03.10.2022-#91289-JT-inicio
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZSDR0126'.
  SET TITLEBAR 'ZSDR0126'.

  PERFORM init_alv.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  w_stable = 'XX'.

  CASE ok_code.

    WHEN '&SHOW'.
      l_edit = abap_false.
      PERFORM f_selecao_dados.

    WHEN '&EDIT'.
      l_edit = abap_true.
      PERFORM f_selecao_dados.

    WHEN '&SAVE'.
      PERFORM f_salva.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDMODULE.

*******************************************************************************************
* SALVAR
*******************************************************************************************
FORM f_salva.

  DELETE t_zsdt0266 WHERE cpf IS INITIAL.

  SORT t_zsdt0266 BY cpf werks.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
  DELETE ADJACENT DUPLICATES FROM t_zsdt0266
                         COMPARING cpf werks. "*-CS2021000218-03.10.2022-#91289-JT-inicio

  t_zsdt0266_aux[] = t_zsdt0266[].

  SORT t_zsdt0266_aux BY cpf.
  DELETE ADJACENT DUPLICATES FROM t_zsdt0266_aux
                         COMPARING cpf.

*-CS2021000218-03.10.2022-#91289-JT-inicio
  LOOP AT t_zsdt0259 INTO w_zsdt0259 WHERE modif = abap_true.
    UPDATE zsdt0259 SET ass_eletronica = w_zsdt0259-ass_eletronica
                        usnam          = sy-uname
                        data           = sy-datum
                        hora           = sy-uzeit
                  WHERE cpf            = w_zsdt0259-cpf.
  ENDLOOP.
*-CS2021000218-03.10.2022-#91289-JT-fim

  LOOP AT t_zsdt0266_aux INTO w_zsdt0266_aux.
    DELETE FROM zsdt0266 WHERE cpf = w_zsdt0266_aux-cpf.

    LOOP AT t_zsdt0266 INTO w_zsdt0266 WHERE cpf    = w_zsdt0266_aux-cpf
                                         AND werks <> abap_off.  "*-CS2021000218-03.10.2022-#91289-JT-inicio
      INSERT zsdt0266  FROM w_zsdt0266.
    ENDLOOP.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  FREE: t_fieldcatalog,
        t_fieldcatalog2.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.
  ENDIF.

  PERFORM fill_it_fieldcatalog1 USING:
          01 'CPF2'                 'ZSDT0259'   ' '  ' '  '    '  ' '   ' '   ' '   ' '   ' '   'CPF'    '14',
          02 'NOME'                 'ZSDT0259'   ' '  ' '  '    '  ' '   ' '   ' '   ' '   ' '   'Nome'   '30',
          03 'EMAIL'                'ZSDT0259'   ' '  ' '  '    '  ' '   ' '   ' '   ' '   ' '   'Email'  '25',
          04 'STATUS2'              'ZSDT0259'   ' '  ' '  '    '  ' '   ' '   ' '   ' '   ' '   'Status' '05'.

  PERFORM fill_it_fieldcatalog2 USING:
          01 'WERKS'                'T001W'      ' '  l_edit  ' '  ' '   ' '   ' '   ' '   ' '   'Cód.Centro Forn.'  '16', "*-CS2021000218-03.10.2022-#91289-JT-inicio
          02 'NAME1'                'T001W'      ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Descrição'         '38',
          03 'ORT01'                'T001W'      ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Cidade'            '30',
          04 'REGIO'                'T001W'      ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Estado'            '05'.

  PERFORM toolbar_alv1.
  PERFORM toolbar_alv2.

  w_layout-zebra      = abap_false.
* w_layout-edit       = abap_true. " Makes all Grid editable
  w_layout-no_totarr  = abap_true.
  w_layout-no_totexp  = abap_true.
  w_layout-no_totline = abap_true.
  w_layout-no_toolbar = abap_false.
  w_layout-sel_mode   = 'A'.
  w_layout-cwidth_opt = abap_false.

  IF m_event_handler IS INITIAL.
    CREATE OBJECT m_event_handler.
*   SET HANDLER : m_event_handler->toolbar FOR g_grid.
    SET HANDLER : m_event_handler->user_command FOR g_grid.
  ENDIF.

  SET HANDLER: lcl_event_handler=>on_double_click  FOR g_grid.

  IF m_event_handler2 IS INITIAL.
    CREATE OBJECT m_event_handler2.
    SET HANDLER : m_event_handler2->toolbar FOR g_grid2.
    SET HANDLER : m_event_handler2->user_command FOR g_grid2.
  ENDIF.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
      it_outtab            = t_zsdt0259.

  CALL METHOD g_grid2->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude2
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog2
      it_outtab            = t_armaz.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

* CALL METHOD g_grid->refresh_table_display.

ENDFORM.                    " init_tree

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM fill_it_fieldcatalog1 USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_f4)
                                 VALUE(p_header)
                                 VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-no_convext = abap_false.

  APPEND wa_fieldcatalog TO t_fieldcatalog.

ENDFORM.

*******************************************************************************************
* Form  build_fieldcatalog
*******************************************************************************************
FORM fill_it_fieldcatalog2 USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_f4)
                                 VALUE(p_header)
                                 VALUE(p_outputlen).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.

  APPEND wa_fieldcatalog TO t_fieldcatalog2.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv1.

  FREE: pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv2.

  FREE: pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude2.

  IF l_edit = abap_false.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude2.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  LOOP AT SCREEN.
    IF screen-name(6) = 'G_ASSI'.
      IF w_zsdt0259-status = 'N'.
        screen-input = 0.
      ELSEIF l_edit = abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  PERFORM init_alv2.
  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv2.

  FREE: t_fieldcatalog2.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.
  ENDIF.

  IF w_zsdt0259-status = 'S'.
    PERFORM fill_it_fieldcatalog2 USING:
            01 'WERKS'                'T001W'      ' '  l_edit  ' '  ' '   ' '   ' '   ' '   ' '   'Cód.Centro Forn.'  '13', "*-CS2021000218-03.10.2022-#91289-JT-inicio
            02 'NAME1'                'T001W'      ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Descrição'         '38',
            03 'ORT01'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Cidade'            '30',
            04 'REGIO'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Estado'            '05'.
  ELSE.
    PERFORM fill_it_fieldcatalog2 USING:
            01 'WERKS'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Cód.Centro Forn.'  '13', "*-CS2021000218-03.10.2022-#91289-JT-inicio
            02 'NAME1'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Descrição'         '38',
            03 'ORT01'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Cidade'            '30',
            04 'REGIO'                'T001W'       ' '  ' '     ' '  ' '   ' '   ' '   ' '   ' '   'Estado'            '05'.
  ENDIF.

  PERFORM toolbar_alv2.

  w_layout-zebra      = abap_false.
* w_layout-edit       = abap_true. " Makes all Grid editable
  w_layout-no_totarr  = abap_true.
  w_layout-no_totexp  = abap_true.
  w_layout-no_totline = abap_true.
  w_layout-no_toolbar = abap_false.
  w_layout-sel_mode   = 'A'.
  w_layout-cwidth_opt = abap_false.

  IF m_event_handler2 IS INITIAL.
    CREATE OBJECT m_event_handler2.
    SET HANDLER : m_event_handler2->toolbar FOR g_grid2.
    SET HANDLER : m_event_handler2->user_command FOR g_grid2.
  ENDIF.

  CALL METHOD g_grid2->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = pt_exclude2
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog2
      it_outtab            = t_armaz.

  CALL METHOD g_grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid2.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

* CALL METHOD g_grid->refresh_table_display.

ENDFORM.                    " init_tree
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  w_stable = 'XX'.

  CASE ok_code.

*-CS2021000218-03.10.2022-#91289-JT-inicio
    WHEN '&ASSIN'.
      IF g_assin_eletro = abap_true.
        w_zsdt0259-ass_eletronica = abap_true.
      ELSE.
        w_zsdt0259-ass_eletronica = abap_false.
      ENDIF.

      w_zsdt0259-modif            = abap_true.

      MODIFY t_zsdt0259 FROM w_zsdt0259 INDEX g_index_259 TRANSPORTING ass_eletronica modif.
*-CS2021000218-03.10.2022-#91289-JT-fim

    WHEN '&SHOW'.
      l_edit = abap_false.
      PERFORM f_selecao_dados.

    WHEN '&EDIT'.
      l_edit = abap_true.
      PERFORM f_selecao_dados.

    WHEN '&SAVE'.
      PERFORM f_salva.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDMODULE.
