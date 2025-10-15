*******************************************************************************************
*& Report  ZSDR0140
*******************************************************************************************
*                            AMAGGI
*******************************************************************************************
* Parametriz. de Restr./Exceção na Exibição dos Lotes Produtos
*******************************************************************************************
* Histórico das modificações
*******************************************************************************************
* Data | Nome | Request | Descrição
*******************************************************************************************
REPORT zsdr0140.

TYPE-POOLS : slis, vbak.

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
      g_container3        TYPE scrfname VALUE 'HEADER',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_grid2             TYPE REF TO cl_gui_alv_grid,
      g_grid3             TYPE REF TO cl_gui_alv_grid,
      w_tool              TYPE stb_button.

*******************************************************************************************
* Types
*******************************************************************************************
TYPES: BEGIN OF ty_zsdt0301.
         INCLUDE STRUCTURE zsdt0301.
       TYPES:
                matkl2  TYPE t023t-matkl,
                wgbez   TYPE t023t-wgbez60,
                maktx   TYPE makt-maktx,
                modif   TYPE c,
                criado  TYPE c,
                celltab TYPE lvc_t_styl.
TYPES: END OF ty_zsdt0301.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         maktx TYPE makt-maktx.
TYPES: END OF ty_mara.

TYPES: BEGIN OF ty_t023,
         matkl TYPE t023t-matkl,
         wgbez TYPE t023t-wgbez60.
TYPES: END OF ty_t023.

*******************************************************************************************
* tabelas / works
*******************************************************************************************
DATA: ok_code         TYPE sy-ucomm,
*
      t_matkl         TYPE TABLE OF ty_zsdt0301,
      t_matnr         TYPE TABLE OF ty_zsdt0301,
      t_matkl_del     TYPE TABLE OF ty_zsdt0301,
      t_matnr_del     TYPE TABLE OF ty_zsdt0301,
      t_mara          TYPE TABLE OF ty_mara,
      t_t023          TYPE TABLE OF ty_t023,
      w_matkl         TYPE ty_zsdt0301,
      w_matnr         TYPE ty_zsdt0301,
      w_mara          TYPE ty_mara,
      w_t023          TYPE ty_t023,
      w_zsdt0301      TYPE zsdt0301,
*
      l_ok            TYPE c,
      l_lines         TYPE i,
      l_erro          TYPE c,
*
      t_fieldcatalog  TYPE lvc_t_fcat, "Fieldcatalog
      t_fieldcatalog2 TYPE lvc_t_fcat, "Fieldcatalog
      t_exctab        TYPE slis_t_extab,
      w_item_layout   TYPE lvc_s_laci,
      w_layout        TYPE lvc_s_layo,
      ls_fieldcatalog TYPE lvc_s_fcat,
      obj_dyndoc_id   TYPE REF TO cl_dd_document,
      cl_container_95 TYPE REF TO cl_gui_docking_container,
      t_estilo        TYPE lvc_t_styl,
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
       w_stable   TYPE lvc_s_stbl VALUE 'XX'.

*******************************************************************************************
* includes
*******************************************************************************************
INCLUDE <icon>.
INCLUDE zsdr140_toolbar_event_receiver.
INCLUDE zsdr140_tree_event_receiver.

DATA: toolbar_event_receiver TYPE REF TO lcl_toolbar_event_receiver,
      m_event_handler        TYPE REF TO lcl_event,
      m_event_handler2       TYPE REF TO lcl_event2.

*******************************************************************************************
* TELA SELECAO
*******************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_vkorg LIKE vbak-vkorg MODIF ID t1 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.

  SELECT SINGLE vkorg
    INTO @DATA(l_vkorg)
    FROM tvko
   WHERE vkorg = @p_vkorg.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH text-110 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*******************************************************************************************
* START-OF-SELECRTION
*******************************************************************************************
START-OF-SELECTION.

  PERFORM f_selecao_dados.

  CALL SCREEN 100.

*******************************************************************************************
* selecao dados
*******************************************************************************************
FORM f_selecao_dados.

  FREE: t_matkl, t_matnr, t_matkl_del, t_matnr_del, t_mara, t_t023.

  SELECT *
    FROM zsdt0301
    INTO CORRESPONDING FIELDS OF TABLE t_matnr
   WHERE vkorg     = p_vkorg
     AND matnr    <> abap_off
     AND cancelado = abap_off.

  SELECT *
    FROM zsdt0301
    INTO CORRESPONDING FIELDS OF TABLE t_matkl
   WHERE vkorg     = p_vkorg
     AND matkl    <> abap_off
     AND cancelado = abap_off.

  IF t_matnr[] IS NOT INITIAL.
    SELECT mara~matnr mara~matkl makt~maktx
      FROM mara
     INNER JOIN makt ON makt~matnr = mara~matnr
                    AND makt~spras = sy-langu
      INTO TABLE t_mara
       FOR ALL ENTRIES IN t_matnr
     WHERE mara~matnr   = t_matnr-matnr.
  ENDIF.

  SELECT matkl wgbez60
    FROM t023t
    INTO TABLE t_t023
   WHERE spras   = sy-langu.

  SORT t_mara BY matnr.
  SORT t_t023 BY matkl.

  LOOP AT t_matkl INTO w_matkl.
    l_tabix = sy-tabix.

    CLEAR: w_t023.

    READ TABLE t_t023 INTO w_t023 WITH KEY matkl = w_matkl-matkl
                                  BINARY SEARCH.
    w_matkl-wgbez = w_t023-wgbez.

    MODIFY t_matkl FROM w_matkl INDEX l_tabix.
  ENDLOOP.

  LOOP AT t_matnr INTO w_matnr.
    l_tabix = sy-tabix.

    CLEAR: w_mara, w_t023.

    READ TABLE t_mara INTO w_mara WITH KEY matnr = w_matnr-matnr
                                  BINARY SEARCH.
    READ TABLE t_t023 INTO w_t023 WITH KEY matkl = w_mara-matkl
                                  BINARY SEARCH.
    w_matnr-maktx  = w_mara-maktx.
    w_matnr-matkl2 = w_mara-matkl.
    w_matnr-wgbez  = w_t023-wgbez.

    MODIFY t_matnr FROM w_matnr INDEX l_tabix.
  ENDLOOP.

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

    WHEN '&SAVE'.
      PERFORM f_salva.
      PERFORM f_selecao_dados.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  CALL METHOD g_grid2->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDMODULE.

*******************************************************************************************
* SALVAR
*******************************************************************************************
FORM f_salva.

  CLEAR w_zsdt0301.

*------------------------------------
* grupo de material
*------------------------------------
  LOOP AT t_matkl  INTO w_matkl WHERE modif = abap_true.
    CLEAR w_zsdt0301.

    SELECT SINGLE matkl
      INTO @DATA(l_matkl_x)
      FROM t023
     WHERE matkl = @w_matkl-matkl.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING w_matkl     TO w_zsdt0301.
    w_zsdt0301-mandt                = sy-mandt.
    w_zsdt0301-vkorg                = p_vkorg.

    IF w_matkl-criado = abap_true.
      w_zsdt0301-usnam              = sy-uname.
      w_zsdt0301-data               = sy-datum.
      w_zsdt0301-hora               = sy-uzeit.
    ELSE.
      w_zsdt0301-usnam_edit         = sy-uname.
      w_zsdt0301-data_edit          = sy-datum.
      w_zsdt0301-hora_edit          = sy-uzeit.
    ENDIF.

    MODIFY zsdt0301              FROM w_zsdt0301.
  ENDLOOP.

  LOOP AT t_matkl_del  INTO w_matkl.
    CLEAR w_zsdt0301.

    SELECT matkl
      INTO @DATA(l_matkl)
      FROM zsdt0301
        UP TO 1 ROWS
     WHERE vkorg = @p_vkorg
       AND matkl = @w_matkl-matkl.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING w_matkl   TO w_zsdt0301.
    w_zsdt0301-mandt              = sy-mandt.
    w_zsdt0301-vkorg              = p_vkorg.
    w_zsdt0301-cancelado          = abap_true.
    w_zsdt0301-usnam_edit         = sy-uname.
    w_zsdt0301-data_edit          = sy-datum.
    w_zsdt0301-hora_edit          = sy-uzeit.
    MODIFY zsdt0301            FROM w_zsdt0301.
  ENDLOOP.

*------------------------------------
* material
*------------------------------------
  LOOP AT t_matnr  INTO w_matnr WHERE modif = abap_true.
    CLEAR w_zsdt0301.

    SELECT SINGLE matnr
      INTO @DATA(l_matnr_x)
      FROM mara
     WHERE matnr = @w_matnr-matnr.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING w_matnr     TO w_zsdt0301.
    w_zsdt0301-mandt                = sy-mandt.
    w_zsdt0301-vkorg                = p_vkorg.

    IF w_matnr-criado = abap_true.
      w_zsdt0301-usnam              = sy-uname.
      w_zsdt0301-data               = sy-datum.
      w_zsdt0301-hora               = sy-uzeit.
    ELSE.
      w_zsdt0301-usnam_edit         = sy-uname.
      w_zsdt0301-data_edit          = sy-datum.
      w_zsdt0301-hora_edit          = sy-uzeit.
    ENDIF.

    MODIFY zsdt0301              FROM w_zsdt0301.
  ENDLOOP.

  LOOP AT t_matnr_del  INTO w_matnr.
    CLEAR w_zsdt0301.

    SELECT matnr
      INTO @DATA(l_matnr)
      FROM zsdt0301
        UP TO 1 ROWS
     WHERE vkorg = @p_vkorg
       AND matnr = @w_matnr-matnr.
    ENDSELECT.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING w_matnr   TO w_zsdt0301.
    w_zsdt0301-mandt              = sy-mandt.
    w_zsdt0301-vkorg              = p_vkorg.
    w_zsdt0301-cancelado          = abap_true.
    w_zsdt0301-usnam_edit         = sy-uname.
    w_zsdt0301-data_edit          = sy-datum.
    w_zsdt0301-hora_edit          = sy-uzeit.
    MODIFY zsdt0301            FROM w_zsdt0301.
  ENDLOOP.

  IF w_zsdt0301 IS NOT INITIAL.
    MESSAGE s024(sd) WITH text-120.
  ENDIF.

  COMMIT WORK AND WAIT.

ENDFORM.

*******************************************************************************************
* INIT ALV
*******************************************************************************************
FORM init_alv.

  FREE: t_fieldcatalog,
        t_fieldcatalog2.

  w_layout-zebra      = abap_false.
* w_layout-edit       = abap_true. " Makes all Grid editable
  w_layout-no_totarr  = abap_true.
  w_layout-no_totexp  = abap_true.
  w_layout-no_totline = abap_true.
  w_layout-no_toolbar = abap_false.
  w_layout-sel_mode   = 'A'.
  w_layout-cwidth_opt = abap_false.
  w_layout-stylefname = 'CELLTAB'.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.

    CREATE OBJECT g_custom_container EXPORTING container_name = g_container3.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    PERFORM f_alv_header.

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    PERFORM fill_it_fieldcatalog1 USING:
            01 'MATKL'                   'ZSDT0301'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Cód.Grp.Mercadoria'                      '18',
            02 'WGBEZ'                   'T023T'      ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Desc.Grp.Mercadoria'                     '60',
            03 'QTD_DIAS_VENCIMENTO'     'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   ' '   ' '   ' '   'Qtde. Dias Gerais'                       '18',
            04 'BLOQ_LOTES_VENCIDOS'     'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   'X'   ' '   ' '   'Não Exibir Lotes Vencidos'               '20',
            05 'LOTE_VENCMTO_PROXIMO'    'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   'X'   ' '   ' '   'Vincular Lotes Vencto. mais Próximos'    '31'.

    PERFORM toolbar_alv1.

    IF m_event_handler IS INITIAL.
      CREATE OBJECT m_event_handler.
      SET HANDLER : m_event_handler->toolbar FOR g_grid.
      SET HANDLER : m_event_handler->user_command FOR g_grid.
    ENDIF.

    " SET_TABLE_FOR_FIRST_DISPLAY
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        it_toolbar_excluding = pt_exclude
        i_save               = 'U' "abap_true
      CHANGING
        it_fieldcatalog      = t_fieldcatalog
        it_outtab            = t_matkl.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler=>on_data_changed4   FOR g_grid.
    SET HANDLER : lcl_event_handler=>handle_on_f1       FOR g_grid.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.

    PERFORM fill_it_fieldcatalog2 USING:
            01 'MATNR'                   'ZSDT0301'   ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Cód.Material'                            '18',
            02 'MAKTX'                   'MAKT'       ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Desc.Material'                           '29',
            03 'WGBEZ'                   'T023T'      ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Desc.Grp.Mercadoria'                     '30',
            04 'QTD_DIAS_VENCIMENTO'     'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   ' '   ' '   ' '   'Qtde. Dias Específico'                   '18',
            05 'BLOQ_LOTES_VENCIDOS'     'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   'X'   ' '   ' '   'Não Exibir Lotes Vencidos'               '20',
            06 'LOTE_VENCMTO_PROXIMO'    'ZSDT0301'   ' '  'X'  ' '  ' '  ' '   'X'   ' '   ' '   'Vincular Lotes Vencto. mais Próximos'    '31'.

    PERFORM toolbar_alv2.

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
        it_outtab            = t_matnr.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler2=>on_data_changed4 FOR g_grid2.
    SET HANDLER : lcl_event_handler2=>handle_on_f1     FOR g_grid.

  ELSE.
    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

*  CALL METHOD g_grid->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

*  CALL METHOD g_grid2->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 1.

ENDFORM.                    " init_tree

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  SELECT SINGLE vtext
    INTO @DATA(l_vtext)
    FROM tvkot
   WHERE spras = @sy-langu
     AND vkorg = @p_vkorg.

  IF sy-subrc <> 0.
    CLEAR l_vtext.
  ENDIF.

  CONCATENATE  'Organização de Vendas:' p_vkorg '-' l_vtext
          INTO wl_linha SEPARATED BY space.

  wl_text = wl_linha.
  CALL METHOD obj_dyndoc_id->new_line.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_fontsize = cl_dd_area=>large.

ENDFORM.                    " ZF_ALV_HEADER

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
* wa_fieldcatalog-reptext    = p_header.
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
* APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude2.
* APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude2.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude2.

ENDFORM.
