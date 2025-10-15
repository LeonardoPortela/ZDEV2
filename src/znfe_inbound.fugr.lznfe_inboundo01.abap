*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO01.
*----------------------------------------------------------------------*


CLASS lcl_alv_toolbar_1000 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1000t DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1000a DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1000c DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1000d DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION


DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      dg_dyndoc_id       TYPE REF TO cl_dd_document,
      dg_splitter        TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
      dg_parent_html     TYPE REF TO cl_gui_container,
      dg_parent_html1    TYPE REF TO cl_gui_container,
      dg_parent_html2    TYPE REF TO cl_gui_container,
      picture            TYPE REF TO cl_gui_picture,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      g_alv              TYPE REF TO cl_gui_alv_grid,
      gs_layout          TYPE lvc_s_layo,
      gs_variant         TYPE disvariant,
      it_fieldcatalog    TYPE lvc_t_fcat,
      table_element      TYPE REF TO cl_dd_table_element,
      table_element2     TYPE REF TO cl_dd_table_element,
      column             TYPE REF TO cl_dd_area,
      column_1           TYPE REF TO cl_dd_area,
      column_2           TYPE REF TO cl_dd_area,
      dg_html_cntrl      TYPE REF TO cl_gui_html_viewer.

DATA: ctl_con_1000       TYPE REF TO cl_gui_custom_container,
      ctl_alv_1000       TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbar_1000 TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_lay_1000        TYPE lvc_s_layo,
      gs_var_1000        TYPE disvariant,
      gs_scroll_col_1000 TYPE lvc_s_col,
      gs_scroll_row_1000 TYPE lvc_s_roid,
      it_catalog_1000    TYPE lvc_t_fcat,
      obg_toolbar_1000   TYPE REF TO lcl_alv_toolbar_1000,
      it_exclude_1000    TYPE ui_functions,
      wa_exclude_1000    LIKE LINE OF it_exclude_1000,
      it_selected_1000   TYPE lvc_t_row,
      wa_selected_1000   TYPE lvc_s_row.

DATA: ctl_con_1000t       TYPE REF TO cl_gui_custom_container,
      ctl_alv_1000t       TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbar_1000t TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_lay_1000t        TYPE lvc_s_layo,
      gs_var_1000t        TYPE disvariant,
      gs_scroll_col_1000t TYPE lvc_s_col,
      gs_scroll_row_1000t TYPE lvc_s_roid,
      it_catalog_1000t    TYPE lvc_t_fcat,
      obg_toolbar_1000t   TYPE REF TO lcl_alv_toolbar_1000t,
      it_exclude_1000t    TYPE ui_functions,
      wa_exclude_1000t    LIKE LINE OF it_exclude_1000t,
      it_selected_1000t   TYPE lvc_t_row,
      wa_selected_1000t   TYPE lvc_s_row.

DATA: ctl_con_1000a       TYPE REF TO cl_gui_custom_container,
      ctl_alv_1000a       TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbar_1000a TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_lay_1000a        TYPE lvc_s_layo,
      gs_var_1000a        TYPE disvariant,
      gs_scroll_col_1000a TYPE lvc_s_col,
      gs_scroll_row_1000a TYPE lvc_s_roid,
      it_catalog_1000a    TYPE lvc_t_fcat,
      obg_toolbar_1000a   TYPE REF TO lcl_alv_toolbar_1000a,
      it_exclude_1000a    TYPE ui_functions,
      wa_exclude_1000a    LIKE LINE OF it_exclude_1000a,
      it_selected_1000a   TYPE lvc_t_row,
      wa_selected_1000a   TYPE lvc_s_row.

DATA: ctl_con_1000c       TYPE REF TO cl_gui_custom_container,
      ctl_alv_1000c       TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbar_1000c TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_lay_1000c        TYPE lvc_s_layo,
      gs_var_1000c        TYPE disvariant,
      gs_scroll_col_1000c TYPE lvc_s_col,
      gs_scroll_row_1000c TYPE lvc_s_roid,
      it_catalog_1000c    TYPE lvc_t_fcat,
      obg_toolbar_1000c   TYPE REF TO lcl_alv_toolbar_1000c,
      it_exclude_1000c    TYPE ui_functions,
      wa_exclude_1000c    LIKE LINE OF it_exclude_1000c,
      it_selected_1000c   TYPE lvc_t_row,
      wa_selected_1000c   TYPE lvc_s_row.

DATA: ctl_con_1000d       TYPE REF TO cl_gui_custom_container,
      ctl_alv_1000d       TYPE REF TO cl_gui_alv_grid,
      c_alv_toolbar_1000d TYPE REF TO cl_alv_grid_toolbar_manager,
      gs_lay_1000d        TYPE lvc_s_layo,
      gs_var_1000d        TYPE disvariant,
      gs_scroll_col_1000d TYPE lvc_s_col,
      gs_scroll_row_1000d TYPE lvc_s_roid,
      it_catalog_1000d    TYPE lvc_t_fcat,
      obg_toolbar_1000d   TYPE REF TO lcl_alv_toolbar_1000d,
      it_exclude_1000d    TYPE ui_functions,
      wa_exclude_1000d    LIKE LINE OF it_exclude_1000d,
      it_selected_1000d   TYPE lvc_t_row,
      wa_selected_1000d   TYPE lvc_s_row.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: gs_scroll_col_0001 TYPE lvc_s_col,
      gs_scroll_row_0001 TYPE lvc_s_roid.

DATA: it_selected_rows_0001 TYPE lvc_t_row,
      wa_selected_rows_0001 TYPE lvc_s_row.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_1000 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_1000 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_1000
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbar_1000->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_1000, it_selected_1000[].

    CALL METHOD ctl_alv_1000->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1000.

    CLEAR: it_zmmt0073_sel[].

    LOOP AT it_selected_1000 INTO wa_selected_1000.
      READ TABLE it_zmmt0073_alv INTO DATA(wa_zmmt0073_alv) INDEX wa_selected_1000-index.
      APPEND wa_zmmt0073_alv TO it_zmmt0073_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_documento.
      WHEN 'DEL'.
        PERFORM deletar_documento.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_1000t IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_1000t IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_1000t
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbar_1000t->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_1000t, it_selected_1000t[].

    CALL METHOD ctl_alv_1000t->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1000t.

    CLEAR: it_zmmt0075_sel[].

    LOOP AT it_selected_1000t INTO wa_selected_1000t.
      READ TABLE it_zmmt0075_alv INTO DATA(wa_zmmt0075_alv) INDEX wa_selected_1000t-index.
      APPEND wa_zmmt0075_alv TO it_zmmt0075_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_documento_tipo.
      WHEN 'DEL'.
        PERFORM deletar_documento_tipo.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_1000A IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_1000a IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_1000a
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbar_1000a->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_1000a, it_selected_1000a[].

    CALL METHOD ctl_alv_1000a->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1000a.

    CLEAR: it_zmmt0076_sel[].

    LOOP AT it_selected_1000a INTO wa_selected_1000a.
      READ TABLE it_zmmt0076_alv INTO DATA(wa_zmmt0076_alv) INDEX wa_selected_1000a-index.
      APPEND wa_zmmt0076_alv TO it_zmmt0076_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_documento_armazem.
      WHEN 'DEL'.
        PERFORM deletar_documento_armazem.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_1000C IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_1000c IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_1000c
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbar_1000c->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_1000c, it_selected_1000c[].

    CALL METHOD ctl_alv_1000c->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1000c.

    CLEAR: it_zmmt0077_sel[].

    LOOP AT it_selected_1000c INTO wa_selected_1000c.
      READ TABLE it_zmmt0077_alv INTO DATA(wa_zmmt0077_alv) INDEX wa_selected_1000c-index.
      APPEND wa_zmmt0077_alv TO it_zmmt0077_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_documento_cfop.
      WHEN 'DEL'.
        PERFORM deletar_documento_cfop.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_1000C IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_1000d IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_1000d
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    IF ck_consulta EQ abap_true.
      ty_toolbar-disabled  = abap_true.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbar_1000d->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_selected_1000d, it_selected_1000d[].

    CALL METHOD ctl_alv_1000d->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1000d.

    CLEAR: it_zmmt0119_sel[].

    LOOP AT it_selected_1000d INTO wa_selected_1000d.
      READ TABLE it_zmmt0119_alv INTO DATA(wa_zmmt0119_alv) INDEX wa_selected_1000d-index.
      APPEND wa_zmmt0119_alv TO it_zmmt0119_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_documento_cfop_retorno.
      WHEN 'DEL'.
        PERFORM deletar_documento_cfop_retorno.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_0300 IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table.

  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CREATE OBJECT g_alv
      EXPORTING
        i_parent = dg_parent_alv.

    PERFORM fill_it_fieldcatalog.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    gs_layout-sel_mode   = 'A'.

    CALL METHOD g_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_zmmt0072[].

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-008.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.

    "SDYDO_TEXT_ELEMENT = ''.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

*PBUKRS	Sociedad
    "SDYDO_TEXT_ELEMENT = 'Departamentos NF-e Inbound'.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_html1
      EXCEPTIONS
        html_display_error = 1.

  ENDIF.

  CALL METHOD g_alv->refresh_table_display.

  CALL METHOD g_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0001
      es_row_no   = gs_scroll_row_0001.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZMMT0072'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'ATUALIZAR'.
      CLEAR: ok_code.
      PERFORM atualizar_consulta.
    WHEN 'PNOVO'.
      CLEAR: ok_code.
      PERFORM novo_departamento.
    WHEN 'PABRIR'.
      CLEAR: ok_code.
      PERFORM abrir_departamento.
    WHEN 'PEDITAR'.
      CLEAR: ok_code.
      PERFORM editar_departamento.
    WHEN 'PEXCLUIR'.
      CLEAR: ok_code.
      PERFORM excluir_departamento.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_CONSULTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_consulta .

  DATA: objeto TYPE REF TO zcl_mm_departamento.

  CREATE OBJECT objeto.

  TRY .
      objeto->zif_pesquisa~pesquisar( EXPORTING i_filtros = lc_filtro IMPORTING e_registros = it_zmmt0072 ).
    CATCH zcx_nfe_inbound_exception.
  ENDTRY.

  CLEAR: objeto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_DEPARTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluir_departamento .

  DATA: obj_excluir TYPE REF TO zcl_mm_departamento,
        p_excluiu   TYPE c LENGTH 1.
  DATA: answer TYPE c LENGTH 1.


  IF it_zmmt0072_sel[] IS INITIAL.
    MESSAGE s022(znfe_distri).
    RETURN.
  ENDIF.

  READ TABLE it_zmmt0072_sel INDEX 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = TEXT-009
      textline1 = TEXT-010
      textline2 = TEXT-011
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN 'J'.
      CREATE OBJECT obj_excluir
        EXPORTING
          i_cd_departamento = it_zmmt0072_sel-cd_departamento.

      p_excluiu = obj_excluir->excluir_registro( ).

      CLEAR: obj_excluir.

      IF p_excluiu EQ abap_true.
        PERFORM atualizar_consulta.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_0001 INPUT.

  CALL METHOD g_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0001
      es_row_no   = gs_scroll_row_0001.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_0001 INPUT.

  CLEAR it_selected_rows_0001.

  CALL METHOD g_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows_0001.

  CLEAR it_zmmt0072_sel[].

  LOOP AT it_selected_rows_0001 INTO wa_selected_rows_0001.
    READ TABLE it_zmmt0072 INTO it_zmmt0072_sel INDEX wa_selected_rows_0001-index.
    APPEND it_zmmt0072_sel.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  NOVO_DEPARTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM novo_departamento .

  DATA: i_gravou TYPE char01.

  CALL FUNCTION 'ZNFE_CADASTRO_DEPARTAMENTO_CAD'
    EXPORTING
      i_consulta = abap_false
    IMPORTING
      i_gravou   = i_gravou.

  IF i_gravou EQ abap_true.
    PERFORM atualizar_consulta .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CADASTRO_DEPARTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cadastro_departamento .

  CLEAR: it_zmmt0073[], it_zmmt0073.
  CLEAR: it_zmmt0075[], it_zmmt0075.
  CLEAR: it_zmmt0076[], it_zmmt0076.
  CLEAR: it_zmmt0077[], it_zmmt0077.
  CLEAR: it_zmmt0119[], it_zmmt0119.

  ck_novo_departamento    = abap_false.
  ck_alterou_departamento = abap_false.
  obj_departamento->get_registro( IMPORTING e_registro = wa_zmmt0072 ).

  IF wa_zmmt0072-cd_departamento IS INITIAL.
    ck_novo_departamento = abap_true.
  ELSE.
    MOVE wa_zmmt0072 TO zmmt0072.
  ENDIF.

  it_zmmt0073 = obj_departamento->get_grupo_mercadoria( ).
  it_zmmt0075 = obj_departamento->get_tipo_pedido_compra( ).
  it_zmmt0076 = obj_departamento->get_armazens( ).
  it_zmmt0077 = obj_departamento->get_cfops( ).
  it_zmmt0119 = obj_departamento->get_cfops_retorno( ).

  PERFORM atribui_zmmt0043_alv.

  CALL SCREEN 1000 STARTING AT 50 01.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_DEPARTAMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_departamento INPUT.
  ck_alterou_departamento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  PERFORM atribui_zmmt0043_alv_desc.

  CHECK ck_alterou_departamento IS INITIAL.

  CASE ok_code.
    WHEN 'GRAVAR'.

      CLEAR: it_zmmt0073.
      LOOP AT it_zmmt0073_alv INTO DATA(wa_alv).
        MOVE-CORRESPONDING wa_alv TO wa_zmmt0073.
        IF wa_alv-matkl IS NOT INITIAL.
          APPEND wa_zmmt0073 TO it_zmmt0073.
        ENDIF.
      ENDLOOP.

      CLEAR: it_zmmt0075.
      LOOP AT it_zmmt0075_alv INTO DATA(wa_alv5).
        MOVE-CORRESPONDING wa_alv5 TO wa_zmmt0075.
        IF wa_alv5-bsart IS NOT INITIAL AND wa_alv5-bstyp IS NOT INITIAL.
          APPEND wa_zmmt0075 TO it_zmmt0075.
        ENDIF.
      ENDLOOP.

      CLEAR: it_zmmt0076.
      LOOP AT it_zmmt0076_alv INTO DATA(wa_alv6).
        MOVE-CORRESPONDING wa_alv6 TO wa_zmmt0076.
        IF wa_alv6-lifnr IS NOT INITIAL.
          APPEND wa_zmmt0076 TO it_zmmt0076.
        ENDIF.
      ENDLOOP.

      CLEAR: it_zmmt0077.
      LOOP AT it_zmmt0077_alv INTO DATA(wa_alv7).
        MOVE-CORRESPONDING wa_alv7 TO wa_zmmt0077.
        IF wa_alv7-cfop IS NOT INITIAL.
          APPEND wa_zmmt0077 TO it_zmmt0077.
        ENDIF.
      ENDLOOP.

      CLEAR: it_zmmt0119.
      LOOP AT it_zmmt0119_alv INTO DATA(wa_alv119).
        MOVE-CORRESPONDING wa_alv119 TO wa_zmmt0119.
        IF wa_zmmt0119-cfop IS NOT INITIAL.
          APPEND wa_zmmt0119 TO it_zmmt0119.
        ENDIF.
      ENDLOOP.

      obj_departamento->set_grupo_mercadoria( i_grupos = it_zmmt0073 ).
      obj_departamento->set_tipo_pedido_compra( i_tipos = it_zmmt0075 ).
      obj_departamento->set_armazens( i_armazens = it_zmmt0076 ).
      obj_departamento->set_cfops( i_cfops = it_zmmt0077 ).
      obj_departamento->set_cfops_retorno( i_cfops = it_zmmt0119 ).

      IF obj_departamento->get_cd_departamento( ) IS INITIAL.

        obj_departamento->set_cd_departamento( zmmt0072-cd_departamento ).

        CHECK obj_departamento->get_cd_departamento( ) IS NOT INITIAL.

        IF obj_departamento->gravar_registro( ) NE abap_true.
          obj_departamento->set_cd_departamento( space ).
          PERFORM atribui_zmmt0043_alv.
        ELSE.
          ck_gravado = abap_true.
          LEAVE TO SCREEN 0.
        ENDIF.

      ELSE.

        IF obj_departamento->gravar_registro( ) EQ abap_true.
          ck_gravado = abap_true.
          LEAVE TO SCREEN 0.
        ELSE.
          PERFORM atribui_zmmt0043_alv.
        ENDIF.

      ENDIF.

      CLEAR: ok_code.

*-CS2025000249-17.04.2025-#173311-JT-inicio
    WHEN 'BT_PARM'.
      PERFORM f_outra_parametrizacao USING 'A'.
    WHEN 'BT_PARM2'.
      PERFORM f_outra_parametrizacao USING 'B'.
*-CS2025000249-17.04.2025-#173311-JT-fim

  ENDCASE.

ENDMODULE.

*-CS2025000249-17.04.2025-#173311-JT-inicio
*****************************************************
* outras parametrizacoes
*****************************************************
FORM f_outra_parametrizacao USING p_tipo.

  DATA: lv_cd_departamento TYPE zde_departamento,
        lv_ck_consulta     TYPE char01.

  lv_cd_departamento = zmmt0072-cd_departamento.
  lv_ck_consulta     = ck_consulta.

  EXPORT lv_cd_departamento TO MEMORY ID 'CD_DEPARTAMENTO'.
  EXPORT lv_ck_consulta     TO MEMORY ID 'CK_CONSULTA'.

  CASE p_tipo.
    WHEN 'A'.
      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0210'
                            WITH p_stcnam = 'ZMMT0210_OUT'
                            WITH p_scmant = '0295'
                            WITH p_nocwid = abap_true
                            WITH p_nosave = abap_true
                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
                            WITH p_act_02 = 'CFOP x MIRO Autom.ZMM0110'
                            WITH p_title  = 'Parametros Empresa x Tipo Pedido'
                        AND RETURN.

    WHEN 'B'.
      SUBMIT zregister_data WITH p_db_tab = 'ZMMT0213'
                            WITH p_stcnam = 'ZMMT0213_OUT'
                            WITH p_scmant = '0297'
                            WITH p_nocwid = abap_true
                            WITH p_nosave = abap_true
                            WITH p_act_01 = 'Visualizar Log de Alteracoes'
                            WITH p_title  = 'Parametros Empresa x Classe'
                        AND RETURN.
  ENDCASE.

  FREE MEMORY ID: 'CD_DEPARTAMENTO', 'CK_CONSULTA'.

ENDFORM.
*-CS2025000249-17.04.2025-#173311-JT-fim

*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  CLEAR: it_ucomm.

  IF ck_consulta EQ abap_true.
    APPEND ok_gravar TO it_ucomm.
  ENDIF.

  SET PF-STATUS 'PF1000' EXCLUDING it_ucomm.

  IF ck_consulta = abap_true.
    SET TITLEBAR 'TL1000' WITH TEXT-001.
  ELSE.
    IF obj_departamento->get_cd_departamento( ) IS INITIAL.
      SET TITLEBAR 'TL1000' WITH TEXT-003.
    ELSE.
      SET TITLEBAR 'TL1000' WITH TEXT-002.
    ENDIF.
  ENDIF.

  IF ck_alterou_departamento EQ abap_true.
    obj_departamento->set_ds_departamento( i_ds_departamento = zmmt0072-ds_departamento ).
    obj_departamento->set_ck_sem_ret_grupo( i_ck_sem_ret_grupo = zmmt0072-ck_sem_ret_grupo ).
    obj_departamento->set_ck_sem_ret_pedido( i_ck_sem_ret_pedido = zmmt0072-ck_sem_ret_pedido ).
    ck_alterou_departamento = abap_false.
  ENDIF.

  IF ck_consulta EQ abap_true.
    LOOP AT SCREEN .
      IF screen-name = 'BOTAO_PARAM' OR  "*-CS2025000249-17.04.2025-#173311-JT
         screen-name = 'BOTAO_PARAM2'.   "*-CS2025000249-17.04.2025-#173311-JT
        screen-input = 1.                "*-CS2025000249-17.04.2025-#173311-JT
        MODIFY SCREEN.                   "*-CS2025000249-17.04.2025-#173311-JT
      ELSE.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name(08) EQ 'ZMMT0072'.
        SPLIT screen-name AT '-' INTO DATA(str1) DATA(str2).
        i_campo = str2.
        IF obj_departamento->valida_atributo_alteravel( EXPORTING i_campo = i_campo ) EQ abap_true.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ctl_con_1000 IS INITIAL.

    CREATE OBJECT ctl_con_1000
      EXPORTING
        container_name = 'ALV_GRUPOS'.

    CREATE OBJECT ctl_alv_1000
      EXPORTING
        i_parent = ctl_con_1000.

    CREATE OBJECT obg_toolbar_1000
      EXPORTING
        io_alv_grid = ctl_alv_1000.

    SET HANDLER obg_toolbar_1000->on_toolbar FOR ctl_alv_1000.
    SET HANDLER obg_toolbar_1000->handle_user_command FOR ctl_alv_1000.

    PERFORM fill_it_fieldcatalog_1000.
*   Fill info for layout variant

    PERFORM fill_gs_variant_1000.
*   Set layout parameters for ALV grid

    gs_lay_1000-sel_mode   = 'A'.
    gs_lay_1000-zebra      = abap_true.
    gs_lay_1000-grid_title = TEXT-007.

    CALL METHOD ctl_alv_1000->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_1000
        is_variant           = gs_var_1000
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_1000
      CHANGING
        it_fieldcatalog      = it_catalog_1000
        it_outtab            = it_zmmt0073_alv[].

    CALL METHOD ctl_alv_1000->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_1000->refresh_table_display.
  ENDIF.

  IF ctl_con_1000t IS INITIAL.

    CREATE OBJECT ctl_con_1000t
      EXPORTING
        container_name = 'ALV_TIPOS'.

    CREATE OBJECT ctl_alv_1000t
      EXPORTING
        i_parent = ctl_con_1000t.

    CREATE OBJECT obg_toolbar_1000t
      EXPORTING
        io_alv_grid = ctl_alv_1000t.

    SET HANDLER obg_toolbar_1000t->on_toolbar FOR ctl_alv_1000t.
    SET HANDLER obg_toolbar_1000t->handle_user_command FOR ctl_alv_1000t.

    PERFORM fill_it_fieldcatalog_1000t.
*   Fill info for layout variant

    PERFORM fill_gs_variant_1000t.
*   Set layout parameters for ALV grid

    gs_lay_1000t-sel_mode   = 'A'.
    gs_lay_1000t-zebra      = abap_true.
    gs_lay_1000t-grid_title = TEXT-012.

    CALL METHOD ctl_alv_1000t->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_1000t
        is_variant           = gs_var_1000t
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_1000t
      CHANGING
        it_fieldcatalog      = it_catalog_1000t
        it_outtab            = it_zmmt0075_alv[].

    CALL METHOD ctl_alv_1000t->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_1000t->refresh_table_display.
  ENDIF.

  IF ctl_con_1000a IS INITIAL.

    CREATE OBJECT ctl_con_1000a
      EXPORTING
        container_name = 'ALV_ARMAZENS'.

    CREATE OBJECT ctl_alv_1000a
      EXPORTING
        i_parent = ctl_con_1000a.

    CREATE OBJECT obg_toolbar_1000a
      EXPORTING
        io_alv_grid = ctl_alv_1000a.

    SET HANDLER obg_toolbar_1000a->on_toolbar FOR ctl_alv_1000a.
    SET HANDLER obg_toolbar_1000a->handle_user_command FOR ctl_alv_1000a.

    PERFORM fill_it_fieldcatalog_1000a.
*   Fill info for layout variant

    PERFORM fill_gs_variant_1000a.
*   Set layout parameters for ALV grid

    gs_lay_1000a-sel_mode   = 'A'.
    gs_lay_1000a-zebra      = abap_true.
    gs_lay_1000a-grid_title = TEXT-013.

    CALL METHOD ctl_alv_1000a->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_1000a
        is_variant           = gs_var_1000a
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_1000a
      CHANGING
        it_fieldcatalog      = it_catalog_1000a
        it_outtab            = it_zmmt0076_alv[].

    CALL METHOD ctl_alv_1000a->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_1000a->refresh_table_display.
  ENDIF.

  IF ctl_con_1000c IS INITIAL.

    CREATE OBJECT ctl_con_1000c
      EXPORTING
        container_name = 'ALV_CFOPS'.

    CREATE OBJECT ctl_alv_1000c
      EXPORTING
        i_parent = ctl_con_1000c.

    CREATE OBJECT obg_toolbar_1000c
      EXPORTING
        io_alv_grid = ctl_alv_1000c.

    SET HANDLER obg_toolbar_1000c->on_toolbar FOR ctl_alv_1000c.
    SET HANDLER obg_toolbar_1000c->handle_user_command FOR ctl_alv_1000c.

    PERFORM fill_it_fieldcatalog_1000c.
*   Fill info for layout variant

    PERFORM fill_gs_variant_1000c.
*   Set layout parameters for ALV grid

    gs_lay_1000c-sel_mode   = 'A'.
    gs_lay_1000c-zebra      = abap_true.
    gs_lay_1000c-grid_title = TEXT-014.

    CALL METHOD ctl_alv_1000c->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_1000c
        is_variant           = gs_var_1000c
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_1000c
      CHANGING
        it_fieldcatalog      = it_catalog_1000c
        it_outtab            = it_zmmt0077_alv[].

    CALL METHOD ctl_alv_1000c->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_1000c->refresh_table_display.
  ENDIF.

  IF ctl_con_1000d IS INITIAL.

    CREATE OBJECT ctl_con_1000d
      EXPORTING
        container_name = 'ALV_RETORNOS'.

    CREATE OBJECT ctl_alv_1000d
      EXPORTING
        i_parent = ctl_con_1000d.

    CREATE OBJECT obg_toolbar_1000d
      EXPORTING
        io_alv_grid = ctl_alv_1000d.

    SET HANDLER obg_toolbar_1000d->on_toolbar FOR ctl_alv_1000d.
    SET HANDLER obg_toolbar_1000d->handle_user_command FOR ctl_alv_1000d.

    PERFORM fill_it_fieldcatalog_1000d.
*   Fill info for layout variant

    PERFORM fill_gs_variant_1000d.
*   Set layout parameters for ALV grid

    gs_lay_1000d-sel_mode   = 'A'.
    gs_lay_1000d-zebra      = abap_true.
    gs_lay_1000d-grid_title = TEXT-018.

    CALL METHOD ctl_alv_1000d->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_1000d
        is_variant           = gs_var_1000d
        i_default            = space
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_1000d
      CHANGING
        it_fieldcatalog      = it_catalog_1000d
        it_outtab            = it_zmmt0119_alv[].

    CALL METHOD ctl_alv_1000d->refresh_table_display.

  ELSE.
    CALL METHOD ctl_alv_1000d->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_1000->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_1000
      es_row_no   = gs_scroll_row_1000.

  CALL METHOD ctl_alv_1000t->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_1000t
      es_row_no   = gs_scroll_row_1000t.

  CALL METHOD ctl_alv_1000a->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_1000a
      es_row_no   = gs_scroll_row_1000a.

  CALL METHOD ctl_alv_1000c->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_1000c
      es_row_no   = gs_scroll_row_1000c.

  CALL METHOD ctl_alv_1000d->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_1000d
      es_row_no   = gs_scroll_row_1000d.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1000 .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZMMT0073_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_1000.

  LOOP AT it_catalog_1000 ASSIGNING FIELD-SYMBOL(<fs_73>).
    IF <fs_73>-fieldname = 'CD_DEPARTAMENTO'.
      <fs_73>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_1000

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1000 .

  gs_var_1000-report      = sy-repid.
  gs_var_1000-handle      = '1000'.
  gs_var_1000-log_group   = abap_false.
  gs_var_1000-username    = abap_false.
  gs_var_1000-variant     = abap_false.
  gs_var_1000-text        = abap_false.
  gs_var_1000-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0300

*&---------------------------------------------------------------------*
*&      Form  ATRIBUI_ZMMT0043_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atribui_zmmt0043_alv .

  DATA: wa_alv     TYPE zde_zmmt0073_alv.
  DATA: wa_alv_75  TYPE zde_zmmt0075_alv.
  DATA: wa_alv_76  TYPE zde_zmmt0076_alv.
  DATA: wa_alv_77  TYPE zde_zmmt0077_alv.
  DATA: wa_alv_119 TYPE zde_zmmt0119_alv.

  CLEAR: it_zmmt0073_alv[], it_zmmt0073_alv.
  CLEAR: it_zmmt0075_alv[], it_zmmt0075_alv.
  CLEAR: it_zmmt0076_alv[], it_zmmt0076_alv.
  CLEAR: it_zmmt0077_alv[], it_zmmt0077_alv.
  CLEAR: it_zmmt0119_alv[], it_zmmt0119_alv.

  IF it_zmmt0073[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_t023t)
      FROM t023t
       FOR ALL ENTRIES IN @it_zmmt0073
     WHERE spras EQ @sy-langu
       AND matkl EQ @it_zmmt0073-matkl.

    SORT it_t023t BY matkl.

    SELECT * INTO TABLE @DATA(it_klah)
      FROM klah
       FOR ALL ENTRIES IN @it_zmmt0073
     WHERE clint EQ @it_zmmt0073-clint.

    SORT it_klah BY clint.

  ENDIF.

  LOOP AT it_zmmt0073 INTO wa_zmmt0073.
    MOVE-CORRESPONDING wa_zmmt0073 TO wa_alv.

    READ TABLE it_t023t WITH KEY matkl = wa_zmmt0073-matkl INTO DATA(wa_t023t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv-wgbez = wa_t023t-wgbez.
    ENDIF.

    READ TABLE it_klah WITH KEY clint = wa_zmmt0073-clint INTO DATA(wa_klah) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv-klart = wa_klah-klart.
      wa_alv-class = wa_klah-class.
    ENDIF.

    APPEND wa_alv TO it_zmmt0073_alv.
  ENDLOOP.

  IF it_zmmt0075[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_t161t)
      FROM t161t
       FOR ALL ENTRIES IN @it_zmmt0075
     WHERE spras EQ @sy-langu
       AND bsart EQ @it_zmmt0075-bsart
       AND bstyp EQ @it_zmmt0075-bstyp.

    SORT it_t161t BY bstyp bsart.

    SELECT * INTO TABLE @DATA(it_t156ht)
      FROM t156ht
       FOR ALL ENTRIES IN @it_zmmt0075
     WHERE spras EQ @sy-langu
       AND bwart EQ @it_zmmt0075-tipo_movimento.

    SORT it_t156ht BY bwart.

    SELECT * INTO TABLE @DATA(it_t003t)
      FROM t003t
       FOR ALL ENTRIES IN @it_zmmt0075
     WHERE spras EQ @sy-langu
       AND blart EQ @it_zmmt0075-blart.

    SORT it_t003t BY blart.

    SELECT * INTO TABLE @DATA(it_t008t)
      FROM t008t
       FOR ALL ENTRIES IN @it_zmmt0075
     WHERE spras EQ @sy-langu
       AND zahls EQ @it_zmmt0075-zlspr.

    SORT it_t008t BY zahls.

  ENDIF.

  LOOP AT it_zmmt0075 INTO wa_zmmt0075.
    CLEAR: wa_alv_75.

    MOVE-CORRESPONDING wa_zmmt0075 TO wa_alv_75.

    READ TABLE it_t161t WITH KEY bstyp = wa_zmmt0075-bstyp bsart = wa_zmmt0075-bsart INTO DATA(wa_t161t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_75-batxt = wa_t161t-batxt.
    ENDIF.

    READ TABLE it_t156ht WITH KEY bwart = wa_zmmt0075-tipo_movimento INTO DATA(wa_t156ht) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_75-btext = wa_t156ht-btext.
    ENDIF.

    READ TABLE it_t003t WITH KEY blart = wa_zmmt0075-blart INTO DATA(wa_t003t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_75-ltext = wa_t003t-ltext.
    ENDIF.

    READ TABLE it_t008t WITH KEY zahls = wa_zmmt0075-zlspr INTO DATA(wa_t008t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_75-textl = wa_t008t-textl.
    ENDIF.

    APPEND wa_alv_75 TO it_zmmt0075_alv.
  ENDLOOP.

  IF it_zmmt0076[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_lfa1)
      FROM lfa1
       FOR ALL ENTRIES IN @it_zmmt0076
     WHERE lifnr EQ @it_zmmt0076-lifnr.
    SORT it_lfa1 BY lifnr.
  ENDIF.

  LOOP AT it_zmmt0076 INTO wa_zmmt0076.
    MOVE-CORRESPONDING wa_zmmt0076 TO wa_alv_76.
    READ TABLE it_lfa1 WITH KEY lifnr = wa_zmmt0076-lifnr INTO DATA(wa_lfa1) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_76-name1 = wa_lfa1-name1.
    ENDIF.
    APPEND wa_alv_76 TO it_zmmt0076_alv.
  ENDLOOP.

  IF it_zmmt0077[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_j_1bagt)
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0077
     WHERE spras EQ @sy-langu
       AND cfop EQ @it_zmmt0077-cfop.
    SORT it_j_1bagt BY cfop.
  ENDIF.

  LOOP AT it_zmmt0077 INTO wa_zmmt0077.
    MOVE-CORRESPONDING wa_zmmt0077 TO wa_alv_77.
    READ TABLE it_j_1bagt WITH KEY cfop = wa_zmmt0077-cfop INTO DATA(wa_j_1bagt) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_77-cfotxt = wa_j_1bagt-cfotxt.
    ENDIF.
    APPEND wa_alv_77 TO it_zmmt0077_alv.
  ENDLOOP.

  CLEAR: it_j_1bagt[], it_j_1bagt.
  IF it_zmmt0119[] IS NOT INITIAL.

    SELECT * INTO TABLE @it_j_1bagt
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0119
     WHERE spras EQ @sy-langu
       AND cfop EQ @it_zmmt0119-cfop.

    SELECT * APPENDING TABLE @it_j_1bagt
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0119
     WHERE spras EQ @sy-langu
       AND cfop EQ @it_zmmt0119-cfop_armaz.

    SORT it_j_1bagt BY cfop.
  ENDIF.

  LOOP AT it_zmmt0119 INTO wa_zmmt0119.
    MOVE-CORRESPONDING wa_zmmt0119 TO wa_alv_119.

    READ TABLE it_j_1bagt WITH KEY cfop = wa_alv_119-cfop INTO wa_j_1bagt BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_119-cfotxt = wa_j_1bagt-cfotxt.
    ENDIF.

    READ TABLE it_j_1bagt WITH KEY cfop = wa_alv_119-cfop_armaz INTO wa_j_1bagt BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_alv_119-cfotxt_armaz = wa_j_1bagt-cfotxt.
    ENDIF.

    APPEND wa_alv_119 TO it_zmmt0119_alv.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATRIBUI_ZMMT0043_ALV_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atribui_zmmt0043_alv_desc.

  DATA: wa_alv              TYPE zde_zmmt0073_alv,
        it_zmmt0073_alv_aux TYPE TABLE OF zde_zmmt0073_alv WITH HEADER LINE,
        it_zmmt0075_alv_aux TYPE TABLE OF zde_zmmt0075_alv WITH HEADER LINE,
        it_zmmt0076_alv_aux TYPE TABLE OF zde_zmmt0076_alv WITH HEADER LINE,
        it_zmmt0077_alv_aux TYPE TABLE OF zde_zmmt0077_alv WITH HEADER LINE,
        it_zmmt0119_alv_aux TYPE TABLE OF zde_zmmt0119_alv WITH HEADER LINE.

  MOVE it_zmmt0073_alv[] TO it_zmmt0073_alv_aux[].
  MOVE it_zmmt0075_alv[] TO it_zmmt0075_alv_aux[].
  MOVE it_zmmt0076_alv[] TO it_zmmt0076_alv_aux[].
  MOVE it_zmmt0077_alv[] TO it_zmmt0077_alv_aux[].
  MOVE it_zmmt0119_alv[] TO it_zmmt0119_alv_aux[].

  DELETE it_zmmt0073_alv_aux WHERE matkl IS INITIAL.
  DELETE it_zmmt0075_alv_aux WHERE bsart IS INITIAL.
  DELETE it_zmmt0076_alv_aux WHERE lifnr IS INITIAL.
  DELETE it_zmmt0077_alv_aux WHERE cfop IS INITIAL.
  DELETE it_zmmt0119_alv_aux WHERE cfop IS INITIAL.

  IF it_zmmt0073_alv_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_t023t)
      FROM t023t
       FOR ALL ENTRIES IN @it_zmmt0073_alv_aux
     WHERE spras EQ @sy-langu
       AND matkl EQ @it_zmmt0073_alv_aux-matkl.

    SORT it_t023t BY matkl.

    SELECT * INTO TABLE @DATA(it_klah)
      FROM klah
       FOR ALL ENTRIES IN @it_zmmt0073_alv_aux
     WHERE clint EQ @it_zmmt0073_alv_aux-clint.

    SORT it_klah BY clint.

  ENDIF.

  LOOP AT it_zmmt0073_alv ASSIGNING FIELD-SYMBOL(<fs_73>).
    READ TABLE it_t023t WITH KEY matkl = <fs_73>-matkl INTO DATA(wa_t023t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_73>-wgbez = wa_t023t-wgbez.
    ENDIF.

    READ TABLE it_klah WITH KEY clint = <fs_73>-clint INTO DATA(wa_klah) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_73>-klart = wa_klah-klart.
      <fs_73>-class = wa_klah-class.
    ENDIF.

  ENDLOOP.

  IF it_zmmt0075_alv_aux[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_t161t)
      FROM t161t
       FOR ALL ENTRIES IN @it_zmmt0075_alv_aux
     WHERE spras EQ @sy-langu
       AND bstyp EQ @it_zmmt0075_alv_aux-bstyp
       AND bsart EQ @it_zmmt0075_alv_aux-bsart.

    SORT it_t161t BY bstyp bsart.

    SELECT * INTO TABLE @DATA(it_t156ht)
      FROM t156ht
       FOR ALL ENTRIES IN @it_zmmt0075_alv_aux
     WHERE spras EQ @sy-langu
       AND bwart EQ @it_zmmt0075_alv_aux-tipo_movimento.

    SORT it_t156ht BY bwart.

    SELECT * INTO TABLE @DATA(it_t003t)
      FROM t003t
       FOR ALL ENTRIES IN @it_zmmt0075_alv_aux
     WHERE spras EQ @sy-langu
       AND blart EQ @it_zmmt0075_alv_aux-blart.

    SORT it_t003t BY blart.

    SELECT * INTO TABLE @DATA(it_t008t)
      FROM t008t
       FOR ALL ENTRIES IN @it_zmmt0075_alv_aux
     WHERE spras EQ @sy-langu
       AND zahls EQ @it_zmmt0075_alv_aux-zlspr.

    SORT it_t008t BY zahls.

  ENDIF.

  LOOP AT it_zmmt0075_alv ASSIGNING FIELD-SYMBOL(<fs_75>).

    READ TABLE it_t161t WITH KEY bstyp = <fs_75>-bstyp bsart = <fs_75>-bsart INTO DATA(wa_t161t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_75>-batxt = wa_t161t-batxt.
    ENDIF.

    READ TABLE it_t156ht WITH KEY bwart = <fs_75>-tipo_movimento INTO DATA(wa_t156ht) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_75>-btext = wa_t156ht-btext.
    ENDIF.

    READ TABLE it_t003t WITH KEY blart = <fs_75>-blart INTO DATA(wa_t003t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_75>-ltext = wa_t003t-ltext.
    ENDIF.

    READ TABLE it_t008t WITH KEY zahls = <fs_75>-zlspr INTO DATA(wa_t008t) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_75>-textl = wa_t008t-textl.
    ENDIF.

  ENDLOOP.

  IF it_zmmt0076_alv_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_lfa1)
      FROM lfa1
       FOR ALL ENTRIES IN @it_zmmt0076_alv_aux
     WHERE lifnr EQ @it_zmmt0076_alv_aux-lifnr.

    SORT it_lfa1 BY lifnr.
  ENDIF.

  LOOP AT it_zmmt0076_alv ASSIGNING FIELD-SYMBOL(<fs_76>).
    READ TABLE it_lfa1 WITH KEY lifnr = <fs_76>-lifnr INTO DATA(wa_lfa1) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_76>-name1 = wa_lfa1-name1.
    ENDIF.
  ENDLOOP.

  IF it_zmmt0077_alv_aux[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_j_1bagt)
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0077_alv_aux
     WHERE spras EQ @sy-langu
       AND cfop  EQ @it_zmmt0077_alv_aux-cfop.
    SORT it_j_1bagt BY cfop.
  ENDIF.

  LOOP AT it_zmmt0077_alv ASSIGNING FIELD-SYMBOL(<fs_77>).
    READ TABLE it_j_1bagt WITH KEY cfop = <fs_77>-cfop INTO DATA(wa_j_1bagt) BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_77>-cfotxt = wa_j_1bagt-cfotxt.
    ENDIF.
  ENDLOOP.

  CLEAR: it_j_1bagt[].
  IF it_zmmt0119_alv_aux[] IS NOT INITIAL.

    SELECT * INTO TABLE @it_j_1bagt
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0119_alv_aux
     WHERE spras EQ @sy-langu
       AND cfop  EQ @it_zmmt0119_alv_aux-cfop.

    SELECT * APPENDING TABLE @it_j_1bagt
      FROM j_1bagt
       FOR ALL ENTRIES IN @it_zmmt0119_alv_aux
     WHERE spras EQ @sy-langu
       AND cfop  EQ @it_zmmt0119_alv_aux-cfop_armaz.

    SORT it_j_1bagt BY cfop.

  ENDIF.

  LOOP AT it_zmmt0119_alv ASSIGNING FIELD-SYMBOL(<fs_119>).

    READ TABLE it_j_1bagt WITH KEY cfop = <fs_119>-cfop INTO wa_j_1bagt BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_119>-cfotxt = wa_j_1bagt-cfotxt.
    ENDIF.

    READ TABLE it_j_1bagt WITH KEY cfop = <fs_119>-cfop_armaz INTO wa_j_1bagt BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_119>-cfotxt_armaz = wa_j_1bagt-cfotxt.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_documento .

  CLEAR: zde_zmmt0073_alv, ck_informado_grupo, ck_alterou_grupo, zde_zmmt0075_alv.

  CALL SCREEN 1001 STARTING AT 40 05.

  IF ck_informado_grupo EQ abap_true.
    APPEND zde_zmmt0073_alv TO it_zmmt0073_alv.
    PERFORM atualiza_grupos.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO_tipo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_documento_tipo .

  CLEAR: zde_zmmt0075_alv, ck_informado_tipo, ck_alterou_tipo, zde_zmmt0075_alv.

  CALL SCREEN 1002 STARTING AT 40 05.

  IF ck_informado_tipo EQ abap_true.
    APPEND zde_zmmt0075_alv TO it_zmmt0075_alv.
    PERFORM atualiza_tipos.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_GRUPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_grupos .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_1000->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_tipos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_tipos .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_1000t->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.

  SET PF-STATUS 'PF1001'.
  SET TITLEBAR 'TL1001'.

  IF ( zde_zmmt0073_alv-matkl IS NOT INITIAL ) AND ( ck_alterou_grupo EQ abap_true ).

    SELECT SINGLE wgbez INTO zde_zmmt0073_alv-wgbez
      FROM t023t
     WHERE spras EQ sy-langu
       AND matkl EQ zde_zmmt0073_alv-matkl.

    ck_alterou_grupo = abap_false.

  ELSEIF ck_alterou_grupo EQ abap_true.

    CLEAR: zde_zmmt0073_alv-matkl,
           zde_zmmt0073_alv-wgbez.

    ck_alterou_grupo = abap_false.

  ENDIF.

  IF ( zde_zmmt0073_alv-clint IS NOT INITIAL ) AND ( ck_alterou_class EQ abap_true ).

    SELECT SINGLE class INTO zde_zmmt0073_alv-class
      FROM klah
     WHERE clint EQ zde_zmmt0073_alv-clint.

    ck_alterou_class = abap_false.

  ELSEIF ck_alterou_class EQ abap_true.

    CLEAR: zde_zmmt0073_alv-clint,
           zde_zmmt0073_alv-klart,
           zde_zmmt0073_alv-class.

    ck_alterou_class = abap_false.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1002 OUTPUT.

  SET PF-STATUS 'PF1001'.
  SET TITLEBAR 'TL1002'.

  IF ( zde_zmmt0075_alv-bstyp IS NOT INITIAL ) AND ( zde_zmmt0075_alv-bsart IS NOT INITIAL ) AND ( ck_alterou_tipo EQ abap_true ).
    SELECT SINGLE batxt INTO zde_zmmt0075_alv-batxt
      FROM t161t
     WHERE spras EQ sy-langu
       AND bstyp EQ zde_zmmt0075_alv-bstyp
       AND bsart EQ zde_zmmt0075_alv-bsart.
    ck_alterou_tipo = abap_false.
  ELSEIF ( ( zde_zmmt0075_alv-bstyp IS INITIAL ) OR ( zde_zmmt0075_alv-bsart IS INITIAL ) ) AND ( ck_alterou_tipo EQ abap_true ).
    CLEAR: zde_zmmt0075_alv-batxt.
    ck_alterou_tipo = abap_false.
  ENDIF.

  IF ( zde_zmmt0075_alv-tipo_movimento IS NOT INITIAL ) AND ( ck_alterou_tipo_movimento EQ abap_true ).
    SELECT SINGLE btext INTO zde_zmmt0075_alv-btext
      FROM t156ht
     WHERE spras EQ sy-langu
       AND bwart EQ zde_zmmt0075_alv-tipo_movimento.
    ck_alterou_tipo_movimento = abap_false.
  ELSEIF ( zde_zmmt0075_alv-tipo_movimento IS INITIAL ) AND ( ck_alterou_tipo_movimento EQ abap_true ).
    CLEAR: zde_zmmt0075_alv-btext.
    ck_alterou_tipo_movimento = abap_false.
  ENDIF.

  IF ( zde_zmmt0075_alv-blart IS NOT INITIAL ) AND ( ck_alterou_tipo_documento EQ abap_true ).
    SELECT SINGLE ltext INTO zde_zmmt0075_alv-ltext
      FROM t003t
     WHERE spras EQ sy-langu
       AND blart EQ zde_zmmt0075_alv-blart.
    ck_alterou_tipo_documento = abap_false.
  ELSEIF ( zde_zmmt0075_alv-blart IS INITIAL ) AND ( ck_alterou_tipo_documento EQ abap_true ).
    CLEAR: zde_zmmt0075_alv-ltext.
    ck_alterou_tipo_documento = abap_false.
  ENDIF.

  IF ( zde_zmmt0075_alv-zlspr IS NOT INITIAL ) AND ( ck_alterou_form_bloqueio EQ abap_true ).
    SELECT SINGLE textl INTO zde_zmmt0075_alv-textl
      FROM t008t
     WHERE spras EQ sy-langu
       AND zahls EQ zde_zmmt0075_alv-zlspr.
    ck_alterou_form_bloqueio  = abap_false.
  ELSEIF ( zde_zmmt0075_alv-zlspr IS INITIAL ) AND ( ck_alterou_form_bloqueio EQ abap_true ).
    CLEAR: zde_zmmt0075_alv-textl.
    ck_alterou_form_bloqueio  = abap_false.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_documento .
  LOOP AT it_zmmt0073_sel INTO DATA(wa_zmmt0073_sel).
    DELETE it_zmmt0073_alv WHERE matkl EQ wa_zmmt0073_sel-matkl.
  ENDLOOP.
  CLEAR: it_zmmt0073_sel[].
  PERFORM atualiza_grupos.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_TIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_documento_tipo .
  LOOP AT it_zmmt0075_sel INTO DATA(wa_zmmt0075_sel).
    DELETE it_zmmt0075_alv WHERE bsart EQ wa_zmmt0075_sel-bsart AND bstyp EQ wa_zmmt0075_sel-bstyp.
  ENDLOOP.
  CLEAR: it_zmmt0075_sel[].
  PERFORM atualiza_tipos.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.

  CHECK ck_alterou_grupo EQ abap_false.
  CHECK ck_alterou_class EQ abap_false.

  CASE ok_code.
    WHEN ok_gravar.
      ck_informado_grupo = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.

  CHECK ck_alterou_tipo EQ abap_false.
  CHECK ck_alterou_tipo_movimento EQ abap_false.
  CHECK ck_alterou_tipo_documento EQ abap_false.
  CHECK ck_alterou_form_bloqueio EQ abap_false.

  CASE ok_code.
    WHEN ok_gravar.
      ck_informado_tipo = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_GRUPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_grupo INPUT.
  ck_alterou_grupo = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_tipo INPUT.
  ck_alterou_tipo = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ABRIR_DEPARTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM abrir_departamento .

  IF it_zmmt0072_sel[] IS INITIAL.
    MESSAGE s022(znfe_distri).
    RETURN.
  ENDIF.

  READ TABLE it_zmmt0072_sel INDEX 1.

  CALL FUNCTION 'ZNFE_CADASTRO_DEPARTAMENTO_CAD'
    EXPORTING
      i_consulta        = abap_true
      i_cd_departamento = it_zmmt0072_sel-cd_departamento.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_DEPARTAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editar_departamento .

  DATA: i_gravou TYPE char01.

  IF it_zmmt0072_sel[] IS INITIAL.
    MESSAGE s022(znfe_distri).
    RETURN.
  ENDIF.

  READ TABLE it_zmmt0072_sel INDEX 1.

  CALL FUNCTION 'ZNFE_CADASTRO_DEPARTAMENTO_CAD'
    EXPORTING
      i_consulta        = abap_false
      i_cd_departamento = it_zmmt0072_sel-cd_departamento
    IMPORTING
      i_gravou          = i_gravou.

  IF i_gravou EQ abap_true.
    PERFORM atualizar_consulta .
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1000t
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1000t .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZMMT0075_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_1000t.

  LOOP AT it_catalog_1000t ASSIGNING FIELD-SYMBOL(<fs_75>).
    IF <fs_75>-fieldname = 'CD_DEPARTAMENTO'.
      <fs_75>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_1000

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1000t
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1000t .

  gs_var_1000t-report      = sy-repid.
  gs_var_1000t-handle      = '1002'.
  gs_var_1000t-log_group   = abap_false.
  gs_var_1000t-username    = abap_false.
  gs_var_1000t-variant     = abap_false.
  gs_var_1000t-text        = abap_false.
  gs_var_1000t-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0300

*&---------------------------------------------------------------------*
*&      Module  STATUS_1003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1003 OUTPUT.

  SET PF-STATUS 'PF1001'.
  SET TITLEBAR 'TL1003'.

  IF ( zde_zmmt0076_alv-lifnr IS NOT INITIAL ) AND ( ck_alterou_armazem EQ abap_true ).
    SELECT SINGLE name1 INTO zde_zmmt0076_alv-name1
      FROM lfa1
     WHERE lifnr EQ zde_zmmt0076_alv-lifnr.
    ck_alterou_armazem = abap_false.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1003 INPUT.

  CHECK ck_alterou_armazem EQ abap_false.

  CASE ok_code.
    WHEN ok_gravar.
      ck_informado_armazem = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO_ARMAZEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_documento_armazem .

  CLEAR: zde_zmmt0076_alv, ck_informado_armazem, ck_alterou_armazem, zde_zmmt0076_alv.

  CALL SCREEN 1003 STARTING AT 40 05.

  IF ck_informado_armazem EQ abap_true.
    APPEND zde_zmmt0076_alv TO it_zmmt0076_alv.
    PERFORM atualiza_armazens.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_documento_cfop .

  CLEAR: zde_zmmt0077_alv, ck_informado_cfop, ck_alterou_cfop, zde_zmmt0077_alv.

  CALL SCREEN 1004 STARTING AT 40 05.

  IF ck_informado_cfop EQ abap_true.
    APPEND zde_zmmt0077_alv TO it_zmmt0077_alv.
    PERFORM atualiza_cfop.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ARMAZENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_armazens .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_1000a->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_cfop .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_1000c->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_cfop_retorno.

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  CALL METHOD ctl_alv_1000d->refresh_table_display
    EXPORTING
      is_stable      = gs_alv_refres_cond
      i_soft_refresh = abap_true.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOCUMENTO_ARMAZEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_documento_armazem .

  LOOP AT it_zmmt0076_sel INTO DATA(wa_zmmt0076_sel).
    DELETE it_zmmt0076_alv WHERE lifnr EQ wa_zmmt0076_sel-lifnr.
  ENDLOOP.
  CLEAR: it_zmmt0076_sel[].
  PERFORM atualiza_armazens.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOCUMENTO_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_documento_cfop .

  LOOP AT it_zmmt0077_sel INTO DATA(wa_zmmt0077_sel).
    DELETE it_zmmt0077_alv WHERE cfop EQ wa_zmmt0077_sel-cfop.
  ENDLOOP.
  CLEAR: it_zmmt0077_sel[].
  PERFORM atualiza_cfop.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETAR_DOCUMENTO_CFOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deletar_documento_cfop_retorno .

  LOOP AT it_zmmt0119_sel INTO DATA(wa_zmmt0119_sel).
    DELETE it_zmmt0119_alv WHERE cfop EQ wa_zmmt0119_sel-cfop.
  ENDLOOP.
  CLEAR: it_zmmt0119_sel[].
  PERFORM atualiza_cfop_retorno.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1000A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1000a .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZMMT0076_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_1000a.

  LOOP AT it_catalog_1000a ASSIGNING FIELD-SYMBOL(<fs_76>).
    IF <fs_76>-fieldname = 'CD_DEPARTAMENTO'.
      <fs_76>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1000A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1000c .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZMMT0077_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_1000c.

  LOOP AT it_catalog_1000c ASSIGNING FIELD-SYMBOL(<fs_77>).
    IF <fs_77>-fieldname = 'CD_DEPARTAMENTO'.
      <fs_77>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1000A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1000a .

  gs_var_1000a-report      = sy-repid.
  gs_var_1000a-handle      = '1003'.
  gs_var_1000a-log_group   = abap_false.
  gs_var_1000a-username    = abap_false.
  gs_var_1000a-variant     = abap_false.
  gs_var_1000a-text        = abap_false.
  gs_var_1000a-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1004 OUTPUT.

  SET PF-STATUS 'PF1001'.
  SET TITLEBAR 'TL1004'.

  IF ( zde_zmmt0077_alv-cfop IS NOT INITIAL ) AND ( ck_alterou_cfop EQ abap_true ).
    SELECT SINGLE cfotxt INTO zde_zmmt0077_alv-cfotxt
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ zde_zmmt0077_alv-cfop.
    ck_alterou_cfop = abap_false.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CFOP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_cfop INPUT.
  ck_alterou_cfop = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1004 INPUT.


  CHECK ck_alterou_cfop EQ abap_false.

  CASE ok_code.
    WHEN ok_gravar.
      ck_informado_cfop = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1000A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1000c .

  gs_var_1000c-report      = sy-repid.
  gs_var_1000c-handle      = '1004'.
  gs_var_1000c-log_group   = abap_false.
  gs_var_1000c-username    = abap_false.
  gs_var_1000c-variant     = abap_false.
  gs_var_1000c-text        = abap_false.
  gs_var_1000c-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TIPO_MOVIMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_tipo_movimento INPUT.
  ck_alterou_tipo_movimento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CLASS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_class INPUT.
  ck_alterou_class = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_TIPO_DOCUMENTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_tipo_documento INPUT.
  ck_alterou_tipo_documento = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_FORM_BLOQUEIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_form_bloqueio INPUT.
  ck_alterou_form_bloqueio = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_DOCUMENTO_CFOP_RETORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM incluir_documento_cfop_retorno .

  CLEAR: zde_zmmt0119_alv, ck_informado_cfop_retorno, ck_alterou_cfop_retorno, zde_zmmt0119_alv.

  CALL SCREEN 1005 STARTING AT 40 05.

  IF ck_informado_cfop_retorno EQ abap_true.
    APPEND zde_zmmt0119_alv TO it_zmmt0119_alv.
    PERFORM atualiza_cfop_retorno.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_1000D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_1000d .

  gs_var_1000d-report      = sy-repid.
  gs_var_1000d-handle      = '1005'.
  gs_var_1000d-log_group   = abap_false.
  gs_var_1000d-username    = abap_false.
  gs_var_1000d-variant     = abap_false.
  gs_var_1000d-text        = abap_false.
  gs_var_1000d-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_1000D
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_1000d .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZMMT0119_ALV'
    CHANGING
      ct_fieldcat      = it_catalog_1000d.

  LOOP AT it_catalog_1000d ASSIGNING FIELD-SYMBOL(<fs_119>).
    IF <fs_119>-fieldname = 'CD_DEPARTAMENTO'.
      <fs_119>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1005 OUTPUT.

  SET PF-STATUS 'PF1001'.
  SET TITLEBAR 'TL1005'.

  IF ( zde_zmmt0119_alv-cfop IS NOT INITIAL ) AND ( ck_alterou_cfop_retorno EQ abap_true ).
    SELECT SINGLE cfotxt INTO zde_zmmt0119_alv-cfotxt
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ zde_zmmt0119_alv-cfop.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE cfotxt INTO zde_zmmt0119_alv-cfotxt
        FROM j_1bagt
       WHERE cfop  EQ zde_zmmt0119_alv-cfop.
    ENDIF.
  ENDIF.

  IF ( zde_zmmt0119_alv-cfop_armaz IS NOT INITIAL ) AND ( ck_alterou_cfop_retorno EQ abap_true ).
    SELECT SINGLE cfotxt INTO zde_zmmt0119_alv-cfotxt_armaz
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ zde_zmmt0119_alv-cfop_armaz.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE cfotxt INTO zde_zmmt0119_alv-cfotxt_armaz
        FROM j_1bagt
       WHERE cfop EQ zde_zmmt0119_alv-cfop_armaz.
    ENDIF.
  ENDIF.

  ck_alterou_cfop_retorno = abap_false.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1005_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1005 INPUT.

  CHECK ck_alterou_cfop_retorno EQ abap_false.

  CASE ok_code.
    WHEN ok_gravar.

      IF zde_zmmt0119_alv-cfop EQ zde_zmmt0119_alv-cfop_armaz.
        MESSAGE s151 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      ck_informado_cfop_retorno = abap_true.
      LEAVE TO SCREEN 0.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_CFOP_RETORNO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE alterou_cfop_retorno INPUT.
  ck_alterou_cfop_retorno = abap_true.
ENDMODULE.
