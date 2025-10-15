*----------------------------------------------------------------------*
***INCLUDE ZGL023_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.

**---------- Definition -----------------------------------------------*
*CLASS LCL_EVENT_HANDLER DEFINITION.
*  PUBLIC SECTION.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
*ENDCLASS.                    "lcl_event_handler DEFINITION
*
**---------- Inclementação  -------------------------------------------*
*CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
*  METHOD TOP_OF_PAGE.
*    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.
*  ENDMETHOD.
*ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

DATA: gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      picture          TYPE REF TO cl_gui_picture,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      "      EVENT_HANDLER    TYPE REF TO LCL_EVENT_HANDLER,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions,
      it_fieldcatalog  TYPE lvc_t_fcat,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF gf_first_display = 'X'.

*   Create object for container
    CREATE OBJECT ctl_cccontainer
      EXPORTING
        container_name = 'ALV_0001'.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = ctl_cccontainer
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
        container = dg_parent_grid.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 16.

*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_resumo
      EXPORTING
        i_parent = dg_parent_grid.

*   Fill field catalog
    PERFORM fill_it_fieldcatalog.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

    "GS_LAYOUT-SEL_MODE = 'A'.
    gs_layout-zebra      = 'X'.
    "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.
    gs_layout-stylefname = 'CELLTAB'.

*   Create Object for Event Handler
    "    CREATE OBJECT EVENT_HANDLER.
    "    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE FOR CTL_ALV_RESUMO.

*   Send data to ALV grid
    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_resumo_alv[].

    PERFORM cria_html_cab.

*    CALL METHOD CTL_ALV_RESUMO->LIST_PROCESSING_EVENTS
*      EXPORTING
*        I_EVENT_NAME = 'TOP_OF_PAGE'
*        I_DYNDOC_ID  = DG_DYNDOC_ID.

    CLEAR: gf_first_display.
  ENDIF.
  "140938 CS2024000424 ZGL029 - Rel. Notas do Imobilizado PSA
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: wa_fieldcatalog TYPE lvc_s_fcat,
        lc_pos          TYPE lvc_colpos,
        lc_exit_conv    TYPE convexit.

  CLEAR: it_fieldcatalog.

  lc_pos = 0.

  "140938 CS2024000424 ZGL029 - Rel. Notas do Imobilizado PSA
**********************************************************************
  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'RBUKRS'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Empresa'.
  wa_fieldcatalog-outputlen = 7.
  wa_fieldcatalog-convexit  = 'ALPHA'.
  wa_fieldcatalog-just      = 'R'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

**********************************************************************

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'RACCT'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Conta'.
  wa_fieldcatalog-outputlen = 12.
  wa_fieldcatalog-convexit  = 'ALPHA'.
  wa_fieldcatalog-just      = 'R'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'TXT50'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Desc. da Conta'.
  wa_fieldcatalog-outputlen = 40.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  "ADD 1 TO LC_POS.
  "CLEAR: WA_FIELDCATALOG.
  "WA_FIELDCATALOG-FIELDNAME = 'RMVCT'.
  "WA_FIELDCATALOG-COL_POS   = LC_POS.
  "WA_FIELDCATALOG-REPTEXT   = 'Tipo'.
  "WA_FIELDCATALOG-OUTPUTLEN = 4.
  "APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  "ADD 1 TO LC_POS.
  "CLEAR: WA_FIELDCATALOG.
  "WA_FIELDCATALOG-FIELDNAME = 'RMVCT_TEXTO'.
  "WA_FIELDCATALOG-COL_POS   = LC_POS.
  "WA_FIELDCATALOG-REPTEXT   = 'Desc. Tipo Movimento'.
  "WA_FIELDCATALOG-OUTPUTLEN = 25.
  "APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'SALDO_INICIAL'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Saldo Inicial'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  "WA_FIELDCATALOG-EMPHASIZE = 'C300'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'SALDO_ADICAO'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Saldo Adições'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'SALDO_BAIXAS'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Saldo Baixas'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'SALDO_TRANSF'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Saldo Transferência'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
*>> GR
  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'INCORPORACAO'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Incorporação'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
*<< GR
  ADD 1 TO lc_pos.
  CLEAR: wa_fieldcatalog.
  wa_fieldcatalog-fieldname = 'SALDO_FINAL'.
  wa_fieldcatalog-col_pos   = lc_pos.
  wa_fieldcatalog-reptext   = 'Saldo Final'.
  wa_fieldcatalog-outputlen = 20.
  wa_fieldcatalog-do_sum    = 'X'.
  "WA_FIELDCATALOG-EMPHASIZE = 'C300'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " FILL_IT_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML

*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text      TYPE sdydo_text_element
                    p_style     TYPE sdydo_attribute
                    p_size      TYPE sdydo_attribute
                    p_color     TYPE sdydo_attribute
                    p_fix_lines TYPE sdydo_flag.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      fix_lines     = p_fix_lines
      sap_fontstyle = cl_dd_area=>sans_serif.

ENDFORM.                    " ADD_TEXT

*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM event_top_of_page USING dg_dyndoc_id TYPE REF TO cl_dd_document.

  DATA : dl_text(255) TYPE c,
         wa_zglt039   TYPE zglt039,
         lv_conta     TYPE skb1-saknr.  "Text

  dl_text = 'Relatório para Notas do Imobilizado'.
  PERFORM add_text USING dl_text cl_dd_area=>heading cl_dd_area=>extra_large cl_dd_area=>list_heading abap_false.
  CALL METHOD dg_dyndoc_id->new_line.

  dl_text = 'Empresa:'.
  PERFORM add_text USING dl_text '' '' cl_dd_area=>key abap_true.

  dl_text = s_bukrs-low.
  PERFORM add_text USING dl_text '' '' ''  abap_false.

  "  CONCATENATE 'Empresa:' S_BUKRS INTO DL_TEXT SEPARATED BY SPACE.
  "  PERFORM ADD_TEXT USING DL_TEXT '' CL_DD_AREA=>MEDIUM ''  ABAP_FALSE.
  CALL METHOD dg_dyndoc_id->new_line.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = s_conta-low
    IMPORTING
      output = lv_conta.

  CONCATENATE 'Exercício:' s_exerc INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text '' cl_dd_area=>medium ''  abap_false.
  CALL METHOD dg_dyndoc_id->new_line.

  CONCATENATE 'Período:' s_perio INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text '' cl_dd_area=>medium ''  abap_false.
  CALL METHOD dg_dyndoc_id->new_line.

  CONCATENATE 'Moeda:' s_moeda INTO dl_text SEPARATED BY space.
  PERFORM add_text USING dl_text '' cl_dd_area=>medium ''  abap_false.
  CALL METHOD dg_dyndoc_id->new_line.

  IF s_ntcl IS NOT INITIAL.
    SELECT SINGLE * INTO wa_zglt039
      FROM zglt039
     WHERE codigo EQ s_ntcl.
    IF sy-subrc IS INITIAL.
      CONCATENATE 'Class. Balanço:' wa_zglt039-codigo '-' wa_zglt039-descr INTO dl_text SEPARATED BY space.
      PERFORM add_text USING dl_text '' cl_dd_area=>medium ''  abap_false.
      CALL METHOD dg_dyndoc_id->new_line.
    ENDIF.
  ENDIF.

  IF s_nota IS NOT INITIAL.
    SELECT SINGLE * INTO wa_zglt039
      FROM zglt039
     WHERE cod_nota EQ s_nota.
    IF sy-subrc IS INITIAL.
      CONCATENATE 'Class. Nota:' wa_zglt039-cod_nota '-' wa_zglt039-descr_nota INTO dl_text SEPARATED BY space.
      PERFORM add_text USING dl_text '' cl_dd_area=>medium ''  abap_false.
      CALL METHOD dg_dyndoc_id->new_line.
    ENDIF.
  ENDIF.

  PERFORM container_html.

ENDFORM.                    "EVENT_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

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
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        wa_t001                 TYPE t001,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        vcont                   TYPE i,
        vcont2                  TYPE i,
        wa_zglt039              TYPE zglt039.

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

  p_text = 'Notas do Imobilizado'.
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
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  sdydo_text_element = 'Empresa: '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Período: '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Moeda: '.
  APPEND sdydo_text_element TO p_text_table.

  IF s_conta-low IS NOT INITIAL.
    sdydo_text_element = 'Conta Razão: '.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  IF s_ntcl IS NOT INITIAL.
    sdydo_text_element = 'Class. Balanço: '.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  IF s_nota IS NOT INITIAL.
    sdydo_text_element = 'Class. Nota: '.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  sdydo_text_element = 'Data: '.
  APPEND sdydo_text_element TO p_text_table.

  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CLEAR: p_text_table.

  CLEAR wa_t001-butxt.
  CLEAR vcont2.
  LOOP AT s_bukrs.
    ADD 1 TO vcont2.
  ENDLOOP.

  CLEAR vcont.
  IF s_bukrs-high IS INITIAL.
    LOOP AT s_bukrs.
      ADD 1 TO vcont.
      IF vcont = vcont2.
        CONCATENATE wa_t001-butxt s_bukrs-low  INTO wa_t001-butxt.
      ELSE.
        CONCATENATE wa_t001-butxt s_bukrs-low ',' INTO wa_t001-butxt.
      ENDIF.
    ENDLOOP.
  ELSE.
    CONCATENATE s_bukrs-low 'A' s_bukrs-high  INTO wa_t001-butxt SEPARATED BY space.
  ENDIF.

  IF s_bukrs-high IS NOT INITIAL OR vcont GT 1.

  ELSE.
    SELECT SINGLE * INTO wa_t001 FROM t001 WHERE bukrs EQ s_bukrs-low.
  ENDIF.

  sdydo_text_element = wa_t001-butxt.
  APPEND sdydo_text_element TO p_text_table.


  MOVE: s_perio TO vg_mes,
        s_exerc TO vg_ano.
  CONCATENATE vg_mes '/' vg_ano INTO sdydo_text_element.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = s_moeda.
  APPEND sdydo_text_element TO p_text_table.

  IF s_conta-low IS NOT INITIAL.
    sdydo_text_element = s_conta-low.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  IF s_ntcl IS NOT INITIAL.
    SELECT SINGLE * INTO wa_zglt039
      FROM zglt039
     WHERE codigo EQ s_ntcl.
    IF sy-subrc IS INITIAL.
      CONCATENATE wa_zglt039-codigo '-' wa_zglt039-descr INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
  ENDIF.

  IF s_nota IS NOT INITIAL.
    SELECT SINGLE * INTO wa_zglt039
      FROM zglt039
     WHERE cod_nota EQ s_nota.
    IF sy-subrc IS INITIAL.
      CONCATENATE wa_zglt039-cod_nota '-' wa_zglt039-descr_nota INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
  ENDIF.

  WRITE sy-datum TO sdydo_text_element.
  APPEND sdydo_text_element TO p_text_table.

  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB
