*&---------------------------------------------------------------------*
*& Report  ZMMR137
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr137.

TABLES: makt, asmdt, ausp.

INITIALIZATION.

  TYPES: BEGIN OF ty_saida,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
           atwrt TYPE ausp-atwrt,
           asktx TYPE asmdt-asktx,
           steuc TYPE marc-steuc, "US 156136 // MMSILVA - 22.10.2024
         END OF ty_saida.

  TYPES: BEGIN OF ty_ausp,
           atinn TYPE ausp-atinn,
           klart TYPE ausp-klart,
           objek TYPE makt-matnr,
           atwrt TYPE asmdt-asnum,
         END OF   ty_ausp.

  TYPES: BEGIN OF ty_makt,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
         END OF ty_makt.

  TYPES: BEGIN OF ty_asmdt,
           asnum TYPE asmdt-asnum,
           asktx TYPE asmdt-asktx,
         END OF   ty_asmdt.

"US 156136 // MMSILVA - 22.10.2024 - Inicio
 TYPES: BEGIN OF ty_marc,
           steuc TYPE marc-steuc,
           matnr TYPE marc-matnr,
        END OF   ty_marc.
"US 156136 // MMSILVA - 22.10.2024 - Fim

  DATA: t_saida TYPE TABLE OF ty_saida,
        t_ausp  TYPE TABLE OF ty_ausp,
        t_makt  TYPE TABLE OF ty_makt,
        t_asmdt TYPE TABLE OF ty_asmdt,
        t_marc  TYPE TABLE OF ty_marc, "US 156136 // MMSILVA - 22.10.2024

        w_saida TYPE ty_saida,
        w_ausp  TYPE ty_ausp,
        w_makt  TYPE ty_makt,
        w_asmdt TYPE ty_asmdt,
        w_marc  TYPE ty_marc. "US 156136 // MMSILVA - 22.10.2024


  DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
        dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
        dg_parent_1        TYPE REF TO cl_gui_container,
        dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
        dg_parent_2        TYPE REF TO cl_gui_container,
        dg_parent_2a       TYPE REF TO cl_gui_container,
        dg_parent_alv      TYPE REF TO cl_gui_container,
        picture            TYPE REF TO cl_gui_picture,
        gs_layout          TYPE lvc_s_layo,
        gs_variant         TYPE disvariant,
        ctl_alv            TYPE REF TO cl_gui_alv_grid,
        it_fieldcatalog    TYPE lvc_t_fcat,
        wa_fieldcatalog    TYPE lvc_s_fcat,
        gs_scroll_col      TYPE lvc_s_col,
        gs_scroll_row      TYPE lvc_s_roid,
        it_exclude_fcode   TYPE ui_functions,
        wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
        dg_dyndoc_id       TYPE REF TO cl_dd_document,
        table_element      TYPE REF TO cl_dd_table_element,
        column             TYPE REF TO cl_dd_area,
        table_element2     TYPE REF TO cl_dd_table_element,
        column_1           TYPE REF TO cl_dd_area,
        column_2           TYPE REF TO cl_dd_area,
        dg_html_cntrl      TYPE REF TO cl_gui_html_viewer.




  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: p_matnr FOR  ausp-objek,
                    p_atwrt FOR  ausp-atwrt.
  SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM imprimi_alv.

END-OF-SELECTION.



FORM seleciona_dados.

  IF p_matnr IS INITIAL AND p_atwrt IS INITIAL.

    SELECT  a~atinn  a~klart
            a~objek  a~atwrt
      FROM ausp AS a
      INNER JOIN mara AS b ON b~matnr EQ a~objek
      INTO TABLE t_ausp
     WHERE
      "ATINN EQ 'SERVICO' AND
      a~klart EQ '200'
      AND b~lvorm EQ space
      AND b~mstae EQ space.

  ELSEIF   p_matnr IS NOT INITIAL AND p_atwrt IS INITIAL.

    SELECT  a~atinn  a~klart
            a~objek  a~atwrt
      FROM ausp AS a
      INNER JOIN mara AS b ON b~matnr EQ a~objek
      INTO TABLE t_ausp
   WHERE
      "   ATINN EQ 'SERVICO'   AND
           a~klart EQ '200'
      AND  a~objek IN p_matnr
      AND  b~lvorm EQ space
       AND b~mstae EQ space.

  ELSEIF   p_matnr IS  INITIAL AND p_atwrt IS NOT INITIAL.

    SELECT  a~atinn  a~klart
            a~objek  a~atwrt
      FROM ausp AS a
      INNER JOIN mara AS b ON b~matnr EQ a~objek
      INTO TABLE t_ausp
   WHERE  " ATINN EQ 'SERVICO' AND
           klart EQ '200'
      AND  a~atwrt IN p_atwrt
       AND  b~lvorm EQ space
       AND b~mstae EQ space.

  ELSEIF   p_matnr IS  NOT INITIAL AND p_atwrt IS NOT INITIAL.

    SELECT  a~atinn  a~klart
            a~objek  a~atwrt
      FROM ausp AS a
      INNER JOIN mara AS b ON b~matnr EQ a~objek
      INTO TABLE t_ausp
   WHERE   "ATINN EQ 'SERVICO'  AND
           klart EQ '200'
      AND  a~objek IN p_matnr
      AND  a~atwrt IN p_atwrt
      AND  b~lvorm EQ space
       AND b~mstae EQ space.
  ENDIF.


  IF t_ausp[] IS NOT INITIAL.

    SELECT  matnr
            maktx
      FROM makt
      INTO TABLE t_makt
    FOR ALL ENTRIES IN t_ausp
    WHERE matnr EQ t_ausp-objek.

  ENDIF.

  LOOP AT t_ausp INTO w_ausp.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_ausp-atwrt
      IMPORTING
        output = w_ausp-atwrt.

    READ TABLE t_makt INTO w_makt WITH KEY matnr = w_ausp-objek.
    IF sy-subrc = 0.
      w_saida-matnr =  w_makt-matnr.
      w_saida-maktx =  w_makt-maktx.
    ENDIF.

    SELECT asnum
           asktx
       FROM asmdt
       INTO TABLE t_asmdt
     WHERE asnum EQ w_ausp-atwrt.


    READ TABLE t_asmdt INTO w_asmdt WITH KEY asnum = w_ausp-atwrt.
    IF sy-subrc = 0.
      w_saida-atwrt = w_asmdt-asnum.
      w_saida-asktx = w_asmdt-asktx.
    ENDIF.

    "US 156136 // MMSILVA - 22.10.2024 - Inicio
    SELECT steuc
           matnr
      FROM marc
      INTO TABLE t_marc
      WHERE matnr EQ w_makt-matnr.

    READ TABLE t_marc INTO w_marc WITH KEY matnr = w_makt-matnr.

    IF sy-subrc = 0.
      w_saida-steuc = w_marc-steuc.
    ENDIF.
    "US 156136 // MMSILVA - 22.10.2024 - Fim


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = w_saida-matnr
      IMPORTING
        output = w_saida-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = w_saida-atwrt
      IMPORTING
        output = w_saida-atwrt.

    APPEND w_saida TO t_saida.


    CLEAR: w_saida,
           w_ausp,
           w_makt ,
           w_asmdt,
           w_marc. "US 156136 // MMSILVA - 22.10.2024
  ENDLOOP.

ENDFORM.

FORM imprimi_alv.

  CLEAR: it_fieldcatalog[].
  PERFORM preenche_cat USING:

      'MATNR'    'Material'                    '18'     ''    ''  ''  '',
      'MAKTX'    'Descrição Material'          '40'     ''    ''  ''  '',
      'ATWRT'    'Serviço'                     '18'     ''    ''  ''  '',
      'ASKTX'    'Descrição Serviço'           '40'     ''    ''  ''  '',
      'STEUC'    'Código de Controle'          '15'     ''    ''  ''  ''. "US 156136 // MMSILVA - 22.10.2024

  CALL SCREEN 0100.

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just).


  wa_fieldcatalog-fieldname = p_campo.
  wa_fieldcatalog-coltext   = p_desc.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.

  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-hotspot   = p_hot.
  wa_fieldcatalog-no_zero   = p_zero.
  wa_fieldcatalog-do_sum    = p_sum.
  wa_fieldcatalog-just      = p_just.


  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        data_ini(10)            TYPE c,
        sdydo_text_element(255) TYPE c,
        p_text_table            TYPE sdydo_text_table.

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

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
    PERFORM pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    PERFORM fill_gs_variant.

    gs_layout-sel_mode = 'A'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.


    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = t_saida[].

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

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

*    CONCATENATE 'Período:  ' DATA_INI  ' - '  DATA_FIM INTO  SDYDO_TEXT_ELEMENT SEPARATED BY SPACE.
*    APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

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

    CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO data_ini.


    CONCATENATE 'Data de Processamento: ' data_ini INTO sdydo_text_element SEPARATED BY space.
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

  ENDIF.

  CALL METHOD ctl_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.

FORM fill_gs_variant.
  gs_variant-report     = sy-repid.
  gs_variant-handle     = '0100'.
  gs_variant-log_group  = abap_false.
  gs_variant-username   = abap_false.
  gs_variant-variant    = abap_false.
  gs_variant-text       = abap_false.
  gs_variant-dependvars = abap_false.
ENDFORM.



FORM pega_logo USING nome_logo CHANGING url.

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

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
