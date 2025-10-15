*&---------------------------------------------------------------------*
*&  Include           ZTOPO
*&---------------------------------------------------------------------*

REPORT zpmr0040.
TABLES: ztpm_l_amost, equi, sscrfields, zpmt0053.

DATA: lw_ztpm_l_amost TYPE ztpm_l_amost OCCURS 0.

TYPES:
  BEGIN OF ty_ztpm_l_amost,
    id      TYPE ztpm_l_amost-id,
    cliente TYPE ztpm_l_amost-cliente,
  END OF ty_ztpm_l_amost.

TYPES:
  BEGIN OF ty_ztpm_r_amost,
    id_comp       TYPE ztpm_r_amost-id_comp,
    compartimento TYPE ztpm_r_amost-compartimento,
  END OF ty_ztpm_r_amost.

DATA:
  wa_layout       TYPE lvc_s_layo,
  ls_layout       TYPE lvc_s_layo,
  obj_custom_0110 TYPE REF TO cl_gui_custom_container,
  obj_alv_0110    TYPE REF TO cl_gui_alv_grid,
  obj_custom_0200 TYPE REF TO cl_gui_custom_container,
  obj_alv_0200    TYPE REF TO cl_gui_alv_grid,
  gw_exc_button   TYPE ui_functions,
  obj_custom_0201 TYPE REF TO cl_gui_custom_container,
  obj_alv_0201    TYPE REF TO cl_gui_alv_grid,
  gs_exc_button   TYPE ui_functions,
  gt_exc_button   TYPE ui_functions,
  wt_stable       TYPE lvc_s_stbl,
  wa_stable       TYPE lvc_s_stbl,
  ws_stable       TYPE lvc_s_stbl,
  it_fcat_equi    TYPE TABLE OF lvc_s_fcat,
  it_fcat_sint    TYPE TABLE OF lvc_s_fcat,
  it_fcat         TYPE TABLE OF lvc_s_fcat,
  dg_splitter_1   TYPE REF TO cl_gui_splitter_container,
  dg_parent_1     TYPE REF TO cl_gui_container,
  dg_splitter_2   TYPE REF TO cl_gui_splitter_container,
  dg_parent_2     TYPE REF TO cl_gui_container,
  dg_parent_2a    TYPE REF TO cl_gui_container,
  dg_parent_alv   TYPE REF TO cl_gui_container,
  picture         TYPE REF TO cl_gui_picture.

DATA: url(255)                TYPE c,
      p_text                  TYPE sdydo_text_element,
      p_text_2                TYPE sdydo_text_element,
      sdydo_text_element(255),
      vl_cont                 TYPE i,
      p_text_table            TYPE sdydo_text_table,
      p_text_table_2          TYPE sdydo_text_table.

DATA:
  dg_dyndoc_id   TYPE REF TO cl_dd_document,
  table_element  TYPE REF TO cl_dd_table_element,
  column         TYPE REF TO cl_dd_area,
  table_element2 TYPE REF TO cl_dd_table_element,
  column_1       TYPE REF TO cl_dd_area,
  dg_html_cntrl  TYPE REF TO cl_gui_html_viewer.

DATA:
  vl_butxt  TYPE char255,
  vl_dates1 TYPE char10,
  vl_dates2 TYPE char10.

DATA:
  it_exclude_fcode TYPE ui_functions,
  wa_exclude_fcode LIKE LINE OF it_exclude_fcode,
  it_exclude_alv2  TYPE ui_functions,
  wa_exclude_alv2  LIKE LINE OF it_exclude_fcode,
  lst_layout       TYPE lvc_s_layo.

FIELD-SYMBOLS:
  <fs_table> TYPE STANDARD TABLE,
  <wa_fcat>  TYPE lvc_s_fcat,
  <fs_line>  TYPE any,
  <fs_campo> TYPE any.

DATA:
t_new_line   TYPE REF TO data.

DATA :
  document   TYPE REF TO cl_dd_document,
  o_docking  TYPE REF TO cl_gui_docking_container,
  o_splitter TYPE REF TO cl_gui_splitter_container.


DATA :
  o_container_1 TYPE REF TO cl_gui_container,
  o_container_2 TYPE REF TO cl_gui_container,
  o_container_3 TYPE REF TO cl_gui_container,
  o_alv_1       TYPE REF TO cl_gui_alv_grid,
  wa_toolbar    TYPE stb_button,
  o_alv_2       TYPE REF TO cl_gui_alv_grid.


DATA: clicks TYPE sy-tabix.

DATA: ls_ztpm_token_s360 TYPE ztpm_token_s360,
      it_ztpm_r_amost    TYPE TABLE OF ty_ztpm_r_amost,
      it_ztpm_l_amost    TYPE TABLE OF ty_ztpm_l_amost,
      t_zpmt0053         TYPE TABLE OF zpmt0053.

DATA: pdf_tab LIKE tline OCCURS 0 WITH HEADER LINE.

DATA: name  TYPE vrm_id,
      lista TYPE vrm_values,
      value LIKE LINE OF lista.

DATA: name_comp  TYPE vrm_id,
      lista_comp TYPE vrm_values,
      value_comp LIKE LINE OF lista_comp,
      lc_i(1)    VALUE 'I',
      lc_eq(2)   VALUE 'EQ'.

RANGES: r_stat FOR jest-stat.

DATA: p_status TYPE RANGE OF string.

DATA: p_stat TYPE RANGE OF ztpm_l_amost-situacao,
      p_sta  LIKE LINE OF p_stat.

DATA rb6 TYPE boolean   VALUE 'X'.
DATA: def_variant  TYPE disvariant,
      variant      TYPE disvariant,
      gs_variant_c TYPE disvariant,
      variante     LIKE disvariant,
      v_save(1)    TYPE c VALUE 'A'.



*-----------------------------------------------------------------------------*
*  Tela de CheckBox
*-----------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE text-015.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 03.
PARAMETER: rb_res RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(15) text-016 FOR FIELD rb_res.

SELECTION-SCREEN POSITION 10.
SELECTION-SCREEN COMMENT 23(10) text-017 FOR FIELD rb_cad.
PARAMETER: rb_cad RADIOBUTTON GROUP g2 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b5.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE text-009.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 1 .
PARAMETER: p_agu AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(12) text-010 FOR FIELD p_agu.

SELECTION-SCREEN POSITION 20.
PARAMETER: p_fin AS CHECKBOX DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 25(14) text-011 FOR FIELD p_fin.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b4.

*Parametro de seleção.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_equnr FOR ztpm_l_amost-frota,
                p_amost FOR ztpm_l_amost-numeroamostra,
                p_data  FOR equi-erdat,
                p_date  FOR zpmt0053-datacoleta.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 11.
PARAMETER: p_class(10) AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN COMMENT 1(10) text-006 FOR FIELD p_class.

*SELECTION-SCREEN POSITION 50.
*PARAMETER: P_COMPT(10) AS LISTBOX VISIBLE LENGTH 10.
*SELECTION-SCREEN COMMENT 35(15) TEXT-007 FOR FIELD P_COMPT.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.
*----------------------------------------------------------------------

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN POSITION 01.
PARAMETER: rb_anal RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(10) text-005 FOR FIELD rb_anal.

SELECTION-SCREEN POSITION 30.
PARAMETER: rb_sint RADIOBUTTON GROUP g1 .
SELECTION-SCREEN COMMENT 20(10) text-008 FOR FIELD rb_sint.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

*
*SELECTION-SCREEN FUNCTION KEY 1.
*INITIALIZATION.
*  SSCRFIELDS-FUNCTXT_01 = ICON_RELATIONSHIP && ' Executar conexão SAP x ALS'.
**
**AT SELECTION-SCREEN. "PAI
*  CASE SSCRFIELDS-UCOMM. "pushbutton pressed
*    WHEN 'FC01'.
**      MESSAGE 'teste' TYPE 'S'.
*
*      IF SY-SUBRC IS INITIAL.
*        CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
*          EXPORTING
*            ACTION    = 'U'
*            VIEW_NAME = 'ZPMT0021'.
*      ENDIF.
*  ENDCASE.

*
*SELECTION-SCREEN: BEGIN OF BLOCK b7 WITH FRAME TITLE text-000.
*PARAMETER: p_varia TYPE disvariant-variant.
*SELECTION-SCREEN: END OF BLOCK b7.
