*&---------------------------------------------------------------------*
*& Include          ZMMR189_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida_input,
         butxt         TYPE t001-butxt,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         descricao     TYPE zfit0109-descricao,
         st_calc_sdo   TYPE zfit0109-st_calc_sdo,
         valor         TYPE zfit0115-dmbtr,
         field_style   TYPE lvc_t_styl,
         status        TYPE string.
         INCLUDE STRUCTURE zfit0115.
TYPES: END OF ty_saida_input.

TYPES: BEGIN OF ty_saida_list,
         butxt         TYPE t001-butxt,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         descricao     TYPE zfit0109-descricao,
         valor         TYPE zfit0115-dmbtr,
         valor_old     TYPE zfit0115-dmbtr,
         dt_vcto_old   TYPE zfit0115-dt_vcto.
         INCLUDE STRUCTURE zfit0115.
TYPES: END OF ty_saida_list.

TYPES: BEGIN OF ty_materiais,
         maktx   TYPE zfit0202-maktx, "DESCRICAO_MATERIAL
         matnr   TYPE zfit0202-matnr, "COD_MATERIAL
         wgbez60 TYPE zfit0202-wgbez60, "DESCRICAO_GRUPO
       END OF ty_materiais.

DATA: it_materiais TYPE STANDARD TABLE OF ty_materiais INITIAL SIZE 0,
      it_dados_p22 TYPE STANDARD TABLE OF zfit0200 WITH HEADER LINE,
      it_dados_p21 TYPE STANDARD TABLE OF zfit0201 WITH HEADER LINE,
      it_dados_p01 TYPE STANDARD TABLE OF zfit0202 WITH HEADER LINE,
      it_saida_p01 TYPE STANDARD TABLE OF zfit0202 WITH HEADER LINE.

DATA: p01_g_okcode TYPE sy-ucomm.
DATA: p01_lr_column TYPE REF TO cl_salv_column.
DATA p01_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA p01_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: p01_gr_table   TYPE REF TO cl_salv_table.
DATA: p01_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      p01_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.


DATA: p01_lr_functions   TYPE REF TO cl_salv_functions,
      p01_l_text_edit    TYPE string,
      p01_l_icon_edit    TYPE string,
      p01_l_text_save    TYPE string,
      p01_l_icon_save    TYPE string,
      p01_l_text_getitem TYPE string,
      p01_l_icon_getitem TYPE string,
      p01_l_text_del     TYPE string,
      p01_l_icon_del     TYPE string,
      p01_l_text_ref     TYPE string,
      p01_l_icon_ref     TYPE string.

DATA: p21_g_okcode TYPE sy-ucomm.
DATA: p21_lr_column TYPE REF TO cl_salv_column.
DATA p21_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA p21_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: p21_gr_table   TYPE REF TO cl_salv_table.
DATA: p21_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      p21_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.


DATA: p21_lr_functions        TYPE REF TO cl_salv_functions,
      p21_l_text_edit         TYPE string,
      p21_l_icon_edit         TYPE string,
      p21_l_text_del          TYPE string,
      p21_l_icon_del          TYPE string,
      p21_l_text_ref          TYPE string,
      p21_l_icon_ref          TYPE string,
      p21_l_text_save         TYPE string,
      p21_l_icon_save         TYPE string,
      p21_l_text_fat          TYPE string,
      p21_l_icon_fat          TYPE string,
      p21_functional_settings TYPE REF TO cl_salv_functional_settings,
      p21_tooltips            TYPE REF TO cl_salv_tooltips,
      p21_iconvalue           TYPE lvc_value.

DATA: p22_g_okcode TYPE sy-ucomm.
DATA: p22_lr_column TYPE REF TO cl_salv_column.
DATA p22_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA p22_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: p22_gr_table   TYPE REF TO cl_salv_table.
DATA: p22_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      p22_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.


DATA: p22_lr_functions TYPE REF TO cl_salv_functions,
      p22_l_text_ref   TYPE string,
      p22_l_icon_ref   TYPE string,
      p22_l_text_del   TYPE string,
      p22_l_icon_del   TYPE string,
      p22_l_text_edit  TYPE string,
      p22_l_icon_edit  TYPE string,
      p22_l_text_save  TYPE string,
      p22_l_icon_save  TYPE string.

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.


DATA: p21p1_oo1 TYPE salv_s_cell,
      p21p1_oo2 TYPE salv_t_cell,
      p21p1_oo3 TYPE salv_t_column,
      p21p1_oo4 TYPE salv_t_row,
      p21p1_oo5 TYPE i.

DATA: p21p2_oo1 TYPE salv_s_cell,
      p21p2_oo2 TYPE salv_t_cell,
      p21p2_oo3 TYPE salv_t_column,
      p21p2_oo4 TYPE salv_t_row,
      p21p2_oo5 TYPE i.

TYPES: BEGIN OF ty_resultado,
         matnr  TYPE mseg-matnr,
         werks  TYPE mseg-werks,
         bukrs  TYPE mseg-bukrs,
         mjahr  TYPE mseg-mjahr,
         kstar  TYPE mseg-sakto,
         kostl  TYPE mseg-kostl,
         meinb  TYPE mseg-meins,
         mbgbtr TYPE mseg-menge,
         budat  TYPE mseg-budat_mkpf,
         ebtxt  TYPE makt-maktx,
       END OF ty_resultado.

DATA: resultado   TYPE STANDARD TABLE OF ty_resultado INITIAL SIZE 0,
      w_resultado TYPE ty_resultado.

DATA: vlr_indic TYPE zfit0201-cd_indic.
DATA: it_saida TYPE STANDARD TABLE OF zfie0204 WITH HEADER LINE.

DATA: l_budat1 TYPE mseg-budat_mkpf, "budat,
      l_budat2 TYPE mseg-budat_mkpf, "budat,
      l_tcode  TYPE sy-tcode,
      l_kokrs  TYPE string.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE tit_a .

  PARAMETERS: p_ano TYPE gjahr.

SELECTION-SCREEN END OF BLOCK a.

INITIALIZATION.

  tit_a = 'Parâmetros'.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
*       Inicia ALV
*----------------------------------------------------------------------*

START-OF-SELECTION.
*  cl_demo_output=>display( it_saida ).

  PERFORM container_main_p21.
  PERFORM display_grid.
