*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_TOP
*&---------------------------------------------------------------------*

REPORT zlesr0129.

TABLES: zlest0174, lfa1, kna1, zlest0175.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100_l1,
         proc           TYPE c LENGTH 4,
         chave_nf       TYPE zlest0175-chave_nf,
         model          TYPE zlest0175-model,
         nfenum         TYPE zlest0175-nfenum,
         series         TYPE zlest0175-series,
         docdat         TYPE zlest0175-docdat,
         docnum         TYPE zlest0175-docnum,
         peso_declarado TYPE zlest0175-peso_declarado,
         peso_descarga  TYPE zlest0175-peso_descarga.
         INCLUDE STRUCTURE zlest0174.
       TYPES END OF ty_saida_0100_l1.

TYPES: BEGIN OF ty_saida_0100_l2,
         proc           TYPE c LENGTH 4,
         chave_nf       TYPE zlest0178-chave_nf,
         model          TYPE zlest0178-model,
         nfenum         TYPE zlest0178-nfenum,
         series         TYPE zlest0178-series,
         docdat         TYPE zlest0178-docdat,
         docnum         TYPE zlest0178-docnum,
         peso_declarado TYPE zlest0178-peso_declarado,
         peso_carregado TYPE zlest0178-peso_carregado,
         placa_vagao    TYPE c LENGTH 50.
         INCLUDE STRUCTURE zlest0177.
       TYPES END OF ty_saida_0100_l2.

TYPES: BEGIN OF ty_saida_0100_l3,
         proc           TYPE c LENGTH 4,
         chave_nf       TYPE zlest0180-chave_nf,
         model          TYPE zlest0180-model,
         nfenum         TYPE zlest0180-nfenum,
         series         TYPE zlest0180-series,
         docdat         TYPE zlest0180-docdat,
         docnum         TYPE zlest0180-docnum,
         peso_declarado TYPE zlest0180-peso_declarado,
         peso_rateado   TYPE zlest0180-peso_rateado,
         placa_vagao    TYPE c LENGTH 50.
         INCLUDE STRUCTURE zlest0179.
       TYPES END OF ty_saida_0100_l3.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0100 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

*-------------------------------------------------------------------
* Classes
*-------------------------------------------------------------------

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100_l1 TYPE TABLE OF ty_saida_0100_l1,
      wa_saida_0100_l1 TYPE ty_saida_0100_l1,

      it_saida_0100_l2 TYPE TABLE OF ty_saida_0100_l2,
      wa_saida_0100_l2 TYPE ty_saida_0100_l2,

      it_saida_0100_l3 TYPE TABLE OF ty_saida_0100_l3,
      wa_saida_0100_l3 TYPE ty_saida_0100_l3,

      tg_zlest0174     TYPE TABLE OF zlest0174 WITH HEADER LINE,
      tg_zlest0175     TYPE TABLE OF zlest0175 WITH HEADER LINE,

      tg_zlest0177     TYPE TABLE OF zlest0177 WITH HEADER LINE,
      tg_zlest0178     TYPE TABLE OF zlest0178 WITH HEADER LINE,

      tg_zlest0179     TYPE TABLE OF zlest0179 WITH HEADER LINE,
      tg_zlest0180     TYPE TABLE OF zlest0180 WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao    TYPE c LENGTH 20,
      vg_dt_consulta TYPE sy-datum,
      VG_INTER       TYPE zlest0174-srv_integracao,
      var_answer     TYPE c.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_consultar  TYPE c VALUE 'CONSULTAR'   LENGTH 50,
           c_processar  TYPE c VALUE 'PROCESSAR'   LENGTH 50,
           c_view_dacte TYPE c VALUE 'VIEW_DACTE'  LENGTH 50.

*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-005. "Parâmetros Consulta

SELECT-OPTIONS: p_data     FOR zlest0174-dt_chegada OBLIGATORY,
                p_nfenum   FOR zlest0175-nfenum,
                p_docdat   FOR zlest0175-docdat,
                p_srvint   FOR zlest0174-srv_integracao NO INTERVALS NO-EXTENSION . "PBI - 52204

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-001. "Tipo

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: p_l1 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN COMMENT 3(60) text-002 FOR FIELD p_l1.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: p_l2 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN COMMENT 03(60) text-003 FOR FIELD p_l2.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS: p_l3 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN COMMENT 03(60) text-004 FOR FIELD p_l3.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.


START-OF-SELECTION.

  PERFORM: f_selecionar_dados,
           f_processa_dados.

  CALL SCREEN 0100.
