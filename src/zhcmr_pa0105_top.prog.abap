*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0105_TOP
*&---------------------------------------------------------------------*
REPORT zhcmr_pa0105.

TABLES: t001, pa0009, pa0001, pa0000.

TYPES: BEGIN OF ty_pa0001,
         pernr  TYPE pa0001-pernr,
         stell  TYPE pa0001-stell,
         bukrs  TYPE pa0001-bukrs,
         werks  TYPE pa0001-werks,
         orgeh  TYPE pa0001-orgeh,
         stat2  TYPE pa0000-stat2,
         endda  TYPE pa0001-endda,
         cpf_nr TYPE pa0465-cpf_nr,
       END OF ty_pa0001,

       BEGIN OF ty_saida,
         cpf_nr     TYPE pa0465-cpf_nr,
         pernr      TYPE pa0001-pernr,
         cname      TYPE pa0002-cname,
         begda      TYPE char10,
         dtadmis    TYPE char10,
         stell      TYPE pa0001-stell,
         bukrs      TYPE pa0001-bukrs,
         werks      TYPE pa0001-werks,
         orgeh      TYPE pa0001-orgeh,
         gestimed   TYPE pa9002-gestimed,
         cnameimed  TYPE pa9002-cnameimed,
         areadep    TYPE char40,
         local      TYPE char40,
         usrid_long TYPE pa0105-usrid_long,
         stat2      TYPE pa0000-stat2,
       END OF ty_saida.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: t_pa001          TYPE TABLE OF ty_pa0001.
DATA: t_pa001_aux      TYPE TABLE OF ty_pa0001.
DATA: t_saida  TYPE TABLE OF ty_saida,
      x_saida  TYPE TABLE OF ty_saida,
      y_saida  TYPE TABLE OF ty_saida,
      wa_saida TYPE ty_saida.

DATA: t_zhcmt_pa_0035  TYPE TABLE OF zhcmt_pa_0035,
      wa_zhcmt_pa_0035 TYPE zhcmt_pa_0035.

DATA: t_tvarvc TYPE TABLE OF tvarvc,
      w_tvarvc TYPE tvarvc.

DATA: r_empresas_ignoradas TYPE RANGE OF pa0001-bukrs,
      r_subty              TYPE RANGE OF pa2001-subty.

DATA: tl_tab  TYPE sy-dynnr.

DATA: v_check_job TYPE char1.
*----------------------------------------------------------------------*
*Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF c_main_tab,
    tab1 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB1',
  END OF c_main_tab.

*----------------------------------------------------------------------*
* Objetos de tela:
*----------------------------------------------------------------------*
DATA:
  obj_container_01 TYPE REF TO cl_gui_custom_container,
  obj_alv_01       TYPE REF TO cl_gui_alv_grid.


"Controles de Tela
CONTROLS: main_tab TYPE TABSTRIP,
          i_tab    TYPE TABLEVIEW USING SCREEN 0101.

****************************************************************
*& Define JSON
****************************************************************
DEFINE add_tag.
  CONCATENATE json_input '"' &1 '":"' &2 '"' &3 INTO json_input.
  REPLACE '"NULL"' INTO json_input WITH 'null'.
END-OF-DEFINITION.
