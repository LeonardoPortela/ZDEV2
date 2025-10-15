*----------------------------------------------------------------------*
***INCLUDE ZGLT068_0100.
*----------------------------------------------------------------------*

CONSTANTS: ok_calcular    TYPE sy-ucomm VALUE 'CALCULAR',
           ok_excluir     TYPE sy-ucomm VALUE 'EXCLUIR',
           ok_editar      TYPE sy-ucomm VALUE 'EDITAR',
           ok_leitura     TYPE sy-ucomm VALUE 'LEITURA',
           ok_leitura_dia TYPE sy-ucomm VALUE 'LEITURADIA',
           ok_executar    TYPE sy-ucomm VALUE 'EXECUTAR',
           ok_salvar      TYPE sy-ucomm VALUE 'SALVAR'.

DATA: g_custom_container  TYPE REF TO cl_gui_custom_container,
      dg_splitter_1       TYPE REF TO cl_gui_splitter_container,
      dg_parent_1         TYPE REF TO cl_gui_container,
      dg_splitter_2       TYPE REF TO cl_gui_splitter_container,
      dg_parent_2         TYPE REF TO cl_gui_container,
      dg_parent_2a        TYPE REF TO cl_gui_container,
      dg_parent_alv       TYPE REF TO cl_gui_container,
      picture             TYPE REF TO cl_gui_picture,
      gs_layout           TYPE lvc_s_layo,
*      gs_variant          TYPE disvariant,
      it_fieldcatalog     TYPE lvc_t_fcat,
      wa_fieldcatalog     TYPE lvc_s_fcat,
      ctl_alv             TYPE REF TO cl_gui_alv_grid,
      obj_gui_container   TYPE REF TO cl_gui_custom_container, "Classe para chamada do Container.
      gs_scroll_col       TYPE lvc_s_col,
      ls_stable           TYPE lvc_s_stbl,
      gs_scroll_row       TYPE lvc_s_roid,
      it_exclude_fcode    TYPE ui_functions,
      wa_exclude_fcode    LIKE LINE OF it_exclude_fcode,
      dg_dyndoc_id        TYPE REF TO cl_dd_document,
      table_element       TYPE REF TO cl_dd_table_element,
      column              TYPE REF TO cl_dd_area,
      table_element2      TYPE REF TO cl_dd_table_element,
      column_1            TYPE REF TO cl_dd_area,
      column_2            TYPE REF TO cl_dd_area,
      dg_html_cntrl       TYPE REF TO cl_gui_html_viewer,
      it_select           TYPE TABLE OF zglt_dre_04 WITH HEADER LINE,
      ck_gravou           TYPE c LENGTH 1,
      ck_primeira_entrada TYPE c LENGTH 1.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row,
      lc_dia_executar  TYPE datum.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN ok_calcular.
      PERFORM calcular_base.
      CLEAR: ok_code.
    WHEN ok_editar.
      PERFORM editar_base.
      CLEAR: ok_code.
    WHEN ok_excluir.
      PERFORM refazer_base.
      CLEAR: ok_code.
    WHEN ok_leitura_dia.
      PERFORM forcar_leitura_dia.
      CLEAR: ok_code.
    WHEN ok_leitura.
      PERFORM forcar_leitura.
      CLEAR: ok_code.
*  	WHEN OTHERS.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  CLEAR ok_code.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        it_codes                TYPE TABLE OF sy-ucomm.

  IF p_var IS INITIAL.
    CLEAR: gs_variant.
    gs_variant-report   = sy-repid.
    gs_variant-username = sy-uname.
  ENDIF.

  IF vobjeto IS NOT INITIAL.
    CLEAR: it_codes.
    APPEND 'EDITAR'     TO it_codes.
    APPEND 'CALCULAR'   TO it_codes.
    APPEND 'EXCLUIR'    TO it_codes.
    APPEND 'LEITURA'    TO it_codes.
    APPEND 'LEITURADIA' TO it_codes.
  ENDIF.

  DATA: t_return     TYPE TABLE OF bapiret2,
        t_parameters TYPE TABLE OF bapiparam.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username  = sy-uname
    TABLES
      parameter = t_parameters
      return    = t_return.

  READ TABLE t_parameters[] INTO DATA(w_parameters) WITH KEY parid = 'ZGL060_EXEC'.
  IF ( ( sy-subrc = 0 ) AND ( w_parameters-parva IN p_bukrs ) ) OR ( w_parameters-parva = '*' ).
    SET PF-STATUS 'PF0100' EXCLUDING it_codes.
  ELSE.
    SET PF-STATUS 'PF0099' EXCLUDING it_codes.
  ENDIF.

  SET TITLEBAR 'TL0100'.

  gs_variant-report   = sy-repid.
  gs_variant-username = sy-uname.

  IF g_custom_container IS INITIAL.

* create a container for the tree control
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

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_it_fieldcatalog.

*   Fill info for layout variant
*    PERFORM fill_gs_variant.

*    gs_layout-sel_mode   = 'A'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

*    CREATE OBJECT ctl_alv
*      EXPORTING
*        i_parent          = obj_gui_container
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
  ENDIF.

  CALL METHOD ctl_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = gs_layout
      i_save                        = 'A'
      is_variant                    = gs_variant
*     it_toolbar_excluding          = it_exclude_fcode
    CHANGING
      it_fieldcatalog               = it_fieldcatalog
      it_outtab                     = it_zglt_dre_04[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

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









  p_text = TEXT-002.







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
  sdydo_text_element = 'Empresa: '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Exercício: '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Período: '.
  APPEND sdydo_text_element TO p_text_table.



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

  SELECT SINGLE butxt INTO sdydo_text_element
    FROM t001
   WHERE bukrs EQ p_bukrs-low.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = p_gjahr-low.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = p_poper-low.

  APPEND sdydo_text_element TO p_text_table.


  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CALL METHOD dg_dyndoc_id->merge_document.



  CREATE OBJECT dg_html_cntrl
    EXPORTING
      parent = dg_parent_2.

  dg_dyndoc_id->html_control = dg_html_cntrl.






  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_2
    EXCEPTIONS
      html_display_error = 1.

*  ENDIF.

*  CALL METHOD ctl_alv->get_scroll_info_via_id
*    IMPORTING
*      es_col_info = gs_scroll_col
*      es_row_no   = gs_scroll_row.

  CALL METHOD ctl_alv->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

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

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      I_STRUCTURE_NAME = 'ZGLT_DRE_04'
*    CHANGING
*      CT_FIELDCAT      = IT_FIELDCATALOG.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZGLT_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.


  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.

  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

*  GS_VARIANT-REPORT      = SY-REPID.
*  GS_VARIANT-HANDLE      = '0100'.
*  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
*  GS_VARIANT-USERNAME    = ABAP_FALSE.
*  GS_VARIANT-VARIANT     = ABAP_FALSE.
*  GS_VARIANT-TEXT        = ABAP_FALSE.
*  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  CALL METHOD ctl_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.

  CALL METHOD ctl_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR it_select[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_zglt_dre_04 INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING it_zglt_dre_04 TO it_select.
    APPEND it_select.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CALCULAR_BASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcular_base .

  CALL FUNCTION 'Z_01_DRE_AJUSTA_TABELAS'
    EXPORTING
      i_bukrs  = p_bukrs-low
      i_gjahr  = p_gjahr-low
      i_poper  = p_poper-low
    EXCEPTIONS
      erro_sql = 1
      OTHERS   = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE s027.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITAR_BASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editar_base .

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  gs_alv_refres_cond-row = abap_true.
  gs_alv_refres_cond-col = abap_true.

  IF it_select[] IS INITIAL.
    MESSAGE s005.
    RETURN.
  ENDIF.

  CLEAR: ck_gravou.

  LOOP AT it_select.
    MOVE-CORRESPONDING it_select TO zglt_dre_04.
    ck_primeira_entrada = abap_true.
    CALL SCREEN 0002 STARTING AT 05 05.
  ENDLOOP.

  CLEAR: it_select[].

  IF ck_gravou = abap_true.
    PERFORM: limpar_tabelas,
             selecionar_registros.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REFAZER_BASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refazer_base .

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = TEXT-003
      textline1 = TEXT-004
      textline2 = TEXT-005
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN 'J'.
      CALL FUNCTION 'Z_01_DRE_AJUSTA_TABELAS'
        EXPORTING
          i_bukrs      = p_bukrs-low
          i_gjahr      = p_gjahr-low
          i_poper      = p_poper-low
          i_excluir_04 = abap_true
          i_refazer    = abap_true
        EXCEPTIONS
          erro_sql     = 1
          OTHERS       = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORCAR_LEITURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM forcar_leitura .

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = TEXT-003
      textline1 = TEXT-006
      textline2 = TEXT-005
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN 'J'.

      CALL FUNCTION 'Z_01_DRE_AJUSTA_TABELAS'
        EXPORTING
          i_bukrs      = p_bukrs-low
          i_gjahr      = p_gjahr-low
          i_poper      = p_poper-low
          i_excluir_04 = abap_false
          i_refazer    = abap_true
        EXCEPTIONS
          erro_sql     = 1
          OTHERS       = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORCAR_LEITURA_DIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM forcar_leitura_dia .

  DATA: answer     TYPE c LENGTH 1,
        lc_parcial TYPE  char01.

  CLEAR: lc_dia_executar.

  CALL SCREEN 9001 STARTING AT 05 05.

  CHECK lc_dia_executar IS NOT INITIAL.

  MESSAGE s060 WITH lc_dia_executar INTO DATA(lc_textline1).

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = TEXT-003
      textline1 = lc_textline1
      textline2 = TEXT-005
    IMPORTING
      answer    = answer.

  CASE answer.
    WHEN 'J'.

      CALL FUNCTION 'TH_REDISPATCH'.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 0
          text       = lc_textline1.

      lc_parcial = abap_true.

      WHILE lc_parcial EQ abap_true.

        CALL FUNCTION 'Z_01_DRE_PROC_DIARIO'
          EXPORTING
            pdata    = lc_dia_executar
          IMPORTING
            parcial  = lc_parcial
          EXCEPTIONS
            erro_sql = 1
            OTHERS   = 2.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          lc_parcial = abap_false.
        ENDIF.

        CALL FUNCTION 'TH_REDISPATCH'.

      ENDWHILE.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 100
          text       = lc_textline1.

  ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'PF9001'.
  SET TITLEBAR 'TL9001'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.

  IF ok_code EQ ok_executar.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001_exit INPUT.
  CLEAR: lc_dia_executar.
  LEAVE TO SCREEN 0.
ENDMODULE.
