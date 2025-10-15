FUNCTION-POOL zles_averbacao_seguro MESSAGE-ID zavseguro.

* INCLUDE LZLES_AVERBACAO_SEGUROD...         " Local class definition

TABLES: zlest0143, zlest0145, zlest0144.

CONSTANTS: ok_gravar   TYPE sy-ucomm VALUE 'GRAVAR',
           ok_cancelar TYPE sy-ucomm VALUE 'CANCELAR'.

DATA: dg_splitter       TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2     TYPE REF TO cl_gui_splitter_container,
      dg_parent_html    TYPE REF TO cl_gui_container,
      dg_parent_html_2  TYPE REF TO cl_gui_container,
      dg_parent_html1   TYPE REF TO cl_gui_container,
      dg_parent_html1_2 TYPE REF TO cl_gui_container,
      dg_parent_html2   TYPE REF TO cl_gui_container,
      picture           TYPE REF TO cl_gui_picture,
      picture_2         TYPE REF TO cl_gui_picture,
      dg_parent_alv     TYPE REF TO cl_gui_container,
      dg_parent_alv_2   TYPE REF TO cl_gui_container,
      g_alv             TYPE REF TO cl_gui_alv_grid,
      g_alv2            TYPE REF TO cl_gui_alv_grid,
      gs_layout         TYPE lvc_s_layo,
      gs_variant        TYPE disvariant,
      gs_variant_s      TYPE disvariant,
      it_fieldcatalog   TYPE lvc_t_fcat,
      it_fieldcatalog_s TYPE lvc_t_fcat,
      dg_dyndoc_id      TYPE REF TO cl_dd_document,
      table_element     TYPE REF TO cl_dd_table_element,
      table_element2    TYPE REF TO cl_dd_table_element,
      dg_dyndoc_id_2    TYPE REF TO cl_dd_document,
      table_element_2   TYPE REF TO cl_dd_table_element,
      table_element2_2  TYPE REF TO cl_dd_table_element,

      column            TYPE REF TO cl_dd_area,
      column_1          TYPE REF TO cl_dd_area,
      column_2          TYPE REF TO cl_dd_area,
      dg_html_cntrl     TYPE REF TO cl_gui_html_viewer,
      column_           TYPE REF TO cl_dd_area,
      column_1_         TYPE REF TO cl_dd_area,
      column_2_         TYPE REF TO cl_dd_area,
      dg_html_cntrl_2   TYPE REF TO cl_gui_html_viewer.

DATA: url(255)                TYPE c,
      p_text                  TYPE sdydo_text_element,
      sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.

DATA: gs_scroll_col TYPE lvc_s_col,
      gs_scroll_row TYPE lvc_s_roid.

DATA: gs_scroll_col_ TYPE lvc_s_col,
      gs_scroll_row_ TYPE lvc_s_roid.

DATA: it_zlest0143     TYPE TABLE OF zde_zlest0143_alv,
      it_zlest0143_sel TYPE TABLE OF zde_zlest0143_alv,
      it_zlest0145     TYPE TABLE OF zlest0145,
      it_zlest0145_sel TYPE TABLE OF zlest0145,
      it_zlest0144     TYPE TABLE OF zlest0144,
      it_zlest0144_sel TYPE TABLE OF zlest0144,
      lc_filtro_43     TYPE zde_zlest0143_filtro,
      lc_filtro_45     TYPE zde_zlest0145_filtro,
      lc_filtro_44     TYPE zde_zlest0144_filtro.

DATA: obj_averbacao    TYPE REF TO zcl_averbacao_seguro,
      obj_token        TYPE REF TO zcl_averbacao_seguro_token,
      obj_uri          TYPE REF TO zcl_averbacao_seguro_uri,
      ck_consulta      TYPE c LENGTH 1,
      ck_gravado       TYPE c LENGTH 1,
      ck_novo_token    TYPE c LENGTH 1,
      ck_alterou_token TYPE c LENGTH 1,
      ck_novo_aveb     TYPE c LENGTH 1,
      ck_alterou_aveb  TYPE c LENGTH 1,
      ck_novo_uri      TYPE c LENGTH 1,
      ck_alterou_uri   TYPE c LENGTH 1,
      wa_zlest0143     TYPE zlest0143,
      wa_zlest0145     TYPE zlest0145,
      wa_zlest0144     TYPE zlest0144,
      it_ucomm         TYPE TABLE OF sy-ucomm,
      i_campo	         TYPE name_feld,
      ok_code          TYPE sy-ucomm.

DATA: ws_zlest0143_alv_sint TYPE zde_zlest0143_alv_sint,
      it_zlest0143_alv_sint TYPE TABLE OF  zde_zlest0143_alv_sint,
      vg_tp_rel             TYPE char01.

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
*&      Form  LIMPAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_variaveis .

ENDFORM.
