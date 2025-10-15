*----------------------------------------------------------------------*
***INCLUDE ZMMR118_STATUS_0206.
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0206a DEFINITION DEFERRED.
CLASS lcl_event_receiver_0206 DEFINITION DEFERRED.
CLASS lcl_event_receiver_0206a DEFINITION DEFERRED.

DATA: lc_pedido_item_sel TYPE ty_pedi_itens_alv,
      it_lote_caract     TYPE zib_nfe_dist_lca_t,
      wa_lote_caract     TYPE zib_nfe_dist_lca.

DATA: cl_grid_0206        TYPE REF TO cl_gui_alv_grid,
      container_0206      TYPE REF TO cl_gui_custom_container,
      it_function_0206    TYPE ui_functions,
      wa_layout_0206      TYPE lvc_s_layo,
      it_fieldcat_0206    TYPE lvc_t_fcat,
      it_sort_0206        TYPE lvc_t_sort,
      wa_variant_0206     TYPE disvariant,
      wa_stable_0206      TYPE lvc_s_stbl,
      it_f4_0206          TYPE lvc_t_f4,
      wa_f4_0206          TYPE lvc_s_f4,
      event_receiver_0206 TYPE REF TO lcl_event_receiver_0206.

DATA: cl_grid_0206a        TYPE REF TO cl_gui_alv_grid,
      container_0206a      TYPE REF TO cl_gui_custom_container,
      it_function_0206a    TYPE ui_functions,
      wa_layout_0206a      TYPE lvc_s_layo,
      it_fieldcat_0206a    TYPE lvc_t_fcat,
      it_sort_0206a        TYPE lvc_t_sort,
      wa_variant_0206a     TYPE disvariant,
      wa_stable_0206a      TYPE lvc_s_stbl,
      it_f4_0206a          TYPE lvc_t_f4,
      wa_f4_0206a          TYPE lvc_s_f4,
      obg_toolbar_0206a    TYPE REF TO lcl_alv_toolbar_0206a,
      toolbarmanager_0206a TYPE REF TO cl_alv_grid_toolbar_manager,
      it_selected_0206a    TYPE lvc_t_row,
      wa_selected_0206a    TYPE lvc_s_row,
      event_receiver_0206a TYPE REF TO lcl_event_receiver_0206a.

CLASS lcl_alv_toolbar_0206a DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_event_receiver_0206 DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS handle_data_changed_0206 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
  PRIVATE SECTION.
    METHODS: perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
ENDCLASS.


CLASS lcl_event_receiver_0206a DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click_0206a FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.

CLASS lcl_alv_toolbar_0206a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_0206a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_false.

    IF zib_nfe_dist_ter-st_fisico NE zcl_nfe_inbound=>st_fisico_00.
      ck_permissao = abap_true.
    ENDIF.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = text-005.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = text-006.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_0206a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_lotes_alv_sel, it_lotes_alv_sel[].

    CALL METHOD cl_grid_0206a->get_selected_rows
      IMPORTING
        et_index_rows = DATA(it_selected_0206a).

    LOOP AT it_selected_0206a INTO DATA(wa_selected_0206a).
      READ TABLE it_lotes_alv_u INTO DATA(lc_lote_alv) INDEX wa_selected_0206a-index.
      APPEND lc_lote_alv TO it_lotes_alv_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        DATA(lc_sucesso) = abap_false.
        PERFORM incluir_lote_ped CHANGING lc_sucesso.
      WHEN 'DEL'.
        PERFORM deletar_lote_ped.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

CLASS lcl_event_receiver_0206 IMPLEMENTATION.
  METHOD handle_data_changed_0206.

    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data EQ abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ELSE.
      PERFORM data_changed_0206 USING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
    ENDIF.
  ENDMETHOD.

  METHOD perform_semantic_checks.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good) WHERE fieldname EQ 'ATWRT'.

      IF ls_good-value IS NOT INITIAL.
        READ TABLE it_carac_alv_u INTO DATA(wa_carac_alv_u) INDEX ls_good-row_id.
        TRY.
            zcl_charg=>valida_valor_caracteristica( i_class     = zib_nfe_dist_lot-class
                                                    i_classtype = zib_nfe_dist_lot-klart
                                                    i_atinn     = wa_carac_alv_u-atinn
                                                    i_atwrt     = CONV #( ls_good-value ) ).
          CATCH zcx_charg_exception INTO ex_charg.
            error_in_data = abap_true.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = ex_charg->if_t100_message~t100key-msgid
                i_msgno     = ex_charg->if_t100_message~t100key-msgno
                i_msgty     = ex_charg->msgty
                i_msgv1     = ex_charg->msgv1
                i_msgv2     = ex_charg->msgv2
                i_msgv3     = ex_charg->msgv3
                i_msgv4     = ex_charg->msgv4
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
        ENDTRY.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver_0206a IMPLEMENTATION.
  METHOD handle_double_click_0206a.
    PERFORM double_click_0206a USING e_row e_column es_row_no.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0206  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0206 OUTPUT.
  SET PF-STATUS 'PF0206'.
  SET TITLEBAR 'TL0206'.

  IF ck_alterou_lote_info EQ abap_true OR ck_alterou_lote EQ abap_true.

    CLEAR: it_lote_caract.
    LOOP AT it_carac_alv_u.
      MOVE-CORRESPONDING it_carac_alv_u TO wa_lote_caract.
      APPEND wa_lote_caract TO it_lote_caract.
    ENDLOOP.

    obj_nfe->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = it_lote_caract ).

    READ TABLE it_lotes_alv_u ASSIGNING FIELD-SYMBOL(<fs_lote>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_lote>.

    READ TABLE it_lotes_alv_t ASSIGNING FIELD-SYMBOL(<fs_loteu>) WITH KEY cd_lote_item = zib_nfe_dist_lot-cd_lote_item.
    MOVE-CORRESPONDING zib_nfe_dist_lot TO <fs_loteu>.

    CLEAR: it_carac_alv_u[].

    LOOP AT it_lote_caract INTO DATA(rt_metodo).
      MOVE-CORRESPONDING rt_metodo TO it_carac_alv_u.
      READ TABLE it_carac_alv ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = rt_metodo-cd_lote_item
                                                            atinn        = rt_metodo-atinn.
      MOVE-CORRESPONDING rt_metodo TO <fs_carac>.
      APPEND it_carac_alv_u.
    ENDLOOP.
    ck_alterou_lote_info = abap_false.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name(16) EQ 'ZIB_NFE_DIST_LOT'.
      SPLIT screen-name AT '-' INTO DATA(str1_1602) DATA(str2_1602).
      i_campo = str2_1602.
      IF obj_nfe->valida_atributo_alteravel_lote(
            EXPORTING
              i_campo = i_campo
              i_ebeln = lc_pedido_item_sel-ebeln
              i_ebelp = lc_pedido_item_sel-ebelp
              i_lote  = zib_nfe_dist_lot-charg ) EQ abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF ck_alterou_lote EQ abap_true.

    IF cl_grid_0206 IS NOT INITIAL.
      cl_grid_0206->free( ).
    ENDIF.
    CLEAR: cl_grid_0206.

    IF cl_grid_0206a IS NOT INITIAL.
      cl_grid_0206a->free( ).
    ENDIF.
    CLEAR: cl_grid_0206a.

    IF container_0206 IS NOT INITIAL.
      container_0206->free( ).
    ENDIF.
    CLEAR: container_0206.

    IF container_0206a IS NOT INITIAL.
      container_0206a->free( ).
    ENDIF.
    CLEAR: container_0206a.

    ck_alterou_lote = abap_false.
  ENDIF.

  "Lotes
  IF container_0206 IS INITIAL.
    CLEAR wa_layout_0206.
    wa_layout_0206a-zebra      = abap_true.
    wa_stable_0206a-row        = abap_true.
    wa_stable_0206a-col        = abap_true.
    wa_layout_0206a-stylefname = 'STYLE'.
    wa_layout_0206a-sel_mode   = 'A'.
    wa_layout_0206a-cwidth_opt = 'X'.
    wa_layout_0206a-col_opt    = 'X'.
    wa_layout_0206a-info_fname = 'LINE_COLOR'.
    wa_layout_0206a-ctab_fname = 'COLOR_CELL'.
    wa_layout_0206a-no_toolbar = abap_false.

    IF zib_nfe_dist_ter-st_fisico NE zcl_nfe_inbound=>st_fisico_00.
      wa_layout_0206a-no_toolbar = abap_true.
    ENDIF.

    CREATE OBJECT container_0206a
      EXPORTING
        container_name = 'ALV_LOTES'.

    PERFORM fill_it_fieldcatalog_0206a.

*   Fill info for layout variant
    PERFORM fill_gs_variant_0206a.

    CREATE OBJECT cl_grid_0206a
      EXPORTING
        i_parent = container_0206a.

    CREATE OBJECT obg_toolbar_0206a
      EXPORTING
        io_alv_grid = cl_grid_0206a.

    SET HANDLER obg_toolbar_0206a->on_toolbar FOR cl_grid_0206a.
    SET HANDLER obg_toolbar_0206a->handle_user_command FOR cl_grid_0206a.

    CREATE OBJECT event_receiver_0206a.
    SET HANDLER event_receiver_0206a->handle_double_click_0206a FOR cl_grid_0206a.

    CALL METHOD cl_grid_0206a->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_0206a
        i_save                        = 'A'
        is_layout                     = wa_layout_0206a
        it_toolbar_excluding          = it_function_0206a
      CHANGING
        it_outtab                     = it_lotes_alv_u[]
        it_fieldcatalog               = it_fieldcat_0206a
        it_sort                       = it_sort_0206a
*       IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  "Características
  IF container_0206 IS INITIAL.
    CLEAR wa_layout_0206.
    wa_layout_0206-zebra      = abap_true.
    wa_stable_0206-row        = abap_true.
    wa_stable_0206-col        = abap_true.
    wa_layout_0206-stylefname = 'STYLE'.
    wa_layout_0206-sel_mode   = 'A'.
    wa_layout_0206-cwidth_opt = 'X'.
    wa_layout_0206-col_opt    = 'X'.
    wa_layout_0206-info_fname = 'LINE_COLOR'.
    wa_layout_0206-ctab_fname = 'COLOR_CELL'.
    wa_layout_0206-no_toolbar = abap_true.

    CREATE OBJECT container_0206
      EXPORTING
        container_name = 'ALV_CARACTERISTICAS'.

    PERFORM fill_it_fieldcatalog_0206.

*   Fill info for layout variant
    PERFORM fill_gs_variant_0206.

    CREATE OBJECT cl_grid_0206
      EXPORTING
        i_parent = container_0206.

    cl_grid_0206->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    cl_grid_0206->register_edit_event( EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_check             TO it_function_0206.
    APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_function_0206.

    CREATE OBJECT event_receiver_0206.
    SET HANDLER event_receiver_0206->handle_data_changed_0206 FOR cl_grid_0206.

    CALL METHOD cl_grid_0206->set_table_for_first_display
      EXPORTING
        is_variant                    = wa_variant_0206
        i_save                        = 'A'
        is_layout                     = wa_layout_0206
        it_toolbar_excluding          = it_function_0206
      CHANGING
        it_outtab                     = it_carac_alv_u[]
        it_fieldcatalog               = it_fieldcat_0206
        it_sort                       = it_sort_0206
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

  CALL METHOD cl_grid_0206a->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0206a.

  CALL METHOD cl_grid_0206->refresh_table_display
    EXPORTING
      is_stable = wa_stable_0206.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0206 .

  DATA: i_contador_2 TYPE lvc_colpos,
        i_campo	     TYPE name_feld,
        i_prod_item	 TYPE j_1bitmnum,
        i_lote       TYPE charg_d.

  CLEAR: it_fieldcat_0206[], it_fieldcat_0206.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LCA'
    CHANGING
      ct_fieldcat      = it_fieldcat_0206.

  i_contador_2 = 3.
  i_campo     = 'VFDAT'.
  "I_PROD_ITEM = WA_ITENS_SEL_LOTE-PROD_ITEM.
  "I_LOTE      = ZIB_NFE_DIST_LOT-CHARG.

  DATA(ck_altera) = obj_nfe->valida_atributo_alteravel_lote(
                      EXPORTING
                        i_campo = i_campo
                        i_ebeln = lc_pedido_item_sel-ebeln
                        i_ebelp = lc_pedido_item_sel-ebelp
                        i_lote  = zib_nfe_dist_lot-charg ).

  LOOP AT it_fieldcat_0206 ASSIGNING FIELD-SYMBOL(<fs_0206>).
    <fs_0206>-edit = abap_false.
    CASE <fs_0206>-fieldname.
      WHEN 'SMBEZ'.
        <fs_0206>-col_pos   = 1.
        <fs_0206>-outputlen = 20.
      WHEN 'ATWRT'.
        <fs_0206>-edit      = ck_altera.
        <fs_0206>-outputlen = 20.
        <fs_0206>-col_pos   = 2.
        <fs_0206>-outputlen = 30.
      WHEN OTHERS.
        <fs_0206>-no_out  = abap_true.
        <fs_0206>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1602
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0206a .

  DATA: i_contador_2 TYPE lvc_colpos.

  CLEAR: it_fieldcat_0206a[], it_fieldcat_0206a.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZIB_NFE_DIST_LOT'
    CHANGING
      ct_fieldcat      = it_fieldcat_0206a.

  i_contador_2 = 3.

  LOOP AT it_fieldcat_0206a ASSIGNING FIELD-SYMBOL(<fs_0206a>).
    <fs_0206a>-edit = abap_false.
    CASE <fs_0206a>-fieldname.
      WHEN 'CHARG'.
        <fs_0206a>-col_pos   = 1.
        <fs_0206a>-outputlen = 11.
      WHEN 'PROD_QTD_COMERCI'.
        <fs_0206a>-edit      = abap_false.
        <fs_0206a>-col_pos   = 2.
        <fs_0206a>-outputlen = 30.
      WHEN OTHERS.
        <fs_0206a>-no_out  = abap_true.
        <fs_0206a>-col_pos = i_contador_2.
        ADD 1 TO i_contador_2.
    ENDCASE.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0206 .

  wa_variant_0206-report      = sy-repid.
  wa_variant_0206-handle      = '0206'.
  wa_variant_0206-log_group   = abap_false.
  wa_variant_0206-username    = abap_false.
  wa_variant_0206-variant     = abap_false.
  wa_variant_0206-text        = abap_false.
  wa_variant_0206-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0206A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0206a.

  wa_variant_0206a-report      = sy-repid.
  wa_variant_0206a-handle      = '0207'.
  wa_variant_0206a-log_group   = abap_false.
  wa_variant_0206a-username    = abap_false.
  wa_variant_0206a-variant     = abap_false.
  wa_variant_0206a-text        = abap_false.
  wa_variant_0206a-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_0206A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM double_click_0206a USING    p_e_row     TYPE lvc_s_row
                                 p_e_column  TYPE lvc_s_col
                                 p_es_row_no TYPE lvc_s_roid.
  IF p_e_row-rowtype IS INITIAL.
    READ TABLE it_lotes_alv_u INDEX p_e_row-index INTO DATA(wa_lotes_alv_u).
    PERFORM selecionar_lote_ped USING lc_pedido_item_sel-ebeln lc_pedido_item_sel-ebelp wa_lotes_alv_u-cd_lote_item abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_0206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_changed_0206  USING  e_onf4           TYPE char01
                               e_onf4_after     TYPE char01
                               e_onf4_before    TYPE char01
                               e_ucomm          TYPE sy-ucomm
                               er_data_changed  TYPE REF TO cl_alv_changed_data_protocol.

  DATA: lc_atwrt  TYPE atwrt,
        i_caract  TYPE zib_nfe_dist_lca_t,
        wa_caract TYPE zib_nfe_dist_lca.

  CLEAR: i_caract.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_mod_cells).

    READ TABLE it_carac_alv_u ASSIGNING FIELD-SYMBOL(<fs_caracu>) INDEX ls_mod_cells-row_id.
    READ TABLE it_carac_alv   ASSIGNING FIELD-SYMBOL(<fs_carac>) WITH KEY cd_lote_item = <fs_caracu>-cd_lote_item
                                                                          atinn        = <fs_caracu>-atinn.
    CASE ls_mod_cells-fieldname.
      WHEN  'ATWRT'.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_mod_cells-row_id
            i_fieldname = ls_mod_cells-fieldname
          IMPORTING
            e_value     = lc_atwrt.

        IF lc_atwrt NE <fs_caracu>-atwrt.
          <fs_caracu>-atwrt = lc_atwrt.
          <fs_carac>-atwrt  = lc_atwrt.
          MOVE-CORRESPONDING <fs_carac> TO wa_caract.
          APPEND wa_caract TO i_caract.
        ENDIF.
    ENDCASE.

    IF i_caract IS NOT INITIAL.
      obj_nfe->set_lote_item( CHANGING i_lote = zib_nfe_dist_lot i_lote_caract = i_caract ).
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0206_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0206_exit INPUT.

  PERFORM limpar_tela_lote_0206.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_LOTE_0206
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_tela_lote_0206 .

  IF cl_grid_0206 IS NOT INITIAL.
    cl_grid_0206->free( ).
  ENDIF.
  CLEAR: cl_grid_0206.

  IF cl_grid_0206a IS NOT INITIAL.
    cl_grid_0206a->free( ).
  ENDIF.
  CLEAR: cl_grid_0206a.

  IF container_0206 IS NOT INITIAL.
    container_0206->free( ).
  ENDIF.
  CLEAR: container_0206.

  IF container_0206a IS NOT INITIAL.
    container_0206a->free( ).
  ENDIF.
  CLEAR: container_0206a.

ENDFORM.
