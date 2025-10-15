FUNCTION-POOL znota_importacao MESSAGE-ID zsimetrya.

TYPES: BEGIN OF ty_menu_tree,
         node_pai         TYPE char50,
         node_filho       TYPE char50,
         node_image_pai   TYPE tv_image,
         node_image_filho TYPE tv_image,
         node_filho_key   TYPE lvc_nkey,
         node_pai_key     TYPE lvc_nkey,
       END OF ty_menu_tree.

TYPE-POOLS: vrm, ustyp, slis, f4typ.

*----------------------------------------------------------------------*
*   INCLUDE TLIST_TREE_CONTROL_DEMOCL1                                 *
*----------------------------------------------------------------------*

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_item_double_click
        FOR EVENT item_double_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_di DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_di
      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_di_ad DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_di_ad
      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

"Tabelas
TABLES: znota_import, znota_import_ad, znota_import_ii.

DATA: vg_docnum  TYPE j_1bdocnum,
      vg_itmnum  TYPE j_1bitmnum,
      vg_itdidoc TYPE j_1bitmnum.

"WorkAreas
DATA: wa_znota_import TYPE znota_import.

"Range
DATA: lr_nunm TYPE RANGE OF znota_import-itmnum.

"Tabelas Internas
DATA: it_znota_import     TYPE TABLE OF znota_import WITH HEADER LINE,
      it_znota_import_ad  TYPE TABLE OF znota_import_ad WITH HEADER LINE,
      it_j_1bnflin        TYPE TABLE OF j_1bnflin,
      it_j_1bnflin_aux       TYPE TABLE OF j_1bnflin,
      qtdLines type i,
      vg_item_selecionado TYPE string,
      wa_j_1bnflin        TYPE j_1bnflin,
      it_menu_tree        TYPE TABLE OF ty_menu_tree,
      wa_menu_tree        TYPE ty_menu_tree.

DATA: ok_tabdi1 TYPE c LENGTH 06 VALUE 'TABDI1',
      ok_tabdi2 TYPE c LENGTH 06 VALUE 'TABDI2'.

"Comandos de Tela
DATA: ok_code            TYPE sy-ucomm,
      ok_code_di         TYPE sy-ucomm,
      ok_code_ad         TYPE sy-ucomm,
      ok_nova_di         TYPE sy-ucomm VALUE 'NOVA_DI',
      ok_di_inserir      TYPE sy-ucomm VALUE 'DI_INSERIR',
      ok_di_editar       TYPE sy-ucomm VALUE 'DI_EDITAR',
      ok_di_excluir      TYPE sy-ucomm VALUE 'DI_EXCLUIR',
      ok_di_gravar       TYPE sy-ucomm VALUE 'DI_GRAVAR',
      ok_di_cancela      TYPE sy-ucomm VALUE 'DI_CANCELA',
      ok_di_sair         TYPE sy-ucomm VALUE 'DI_SAIR',
      ok_ii_salvar       TYPE sy-ucomm VALUE 'SALVAR',
      ok_di_altera       TYPE c LENGTH 1,
      wa_fcode_di        TYPE sy-ucomm,
      it_fcode_di        LIKE TABLE OF wa_fcode_di,
      c_0001             TYPE sy-dynnr VALUE 0001,
      c_0002             TYPE sy-dynnr VALUE 0002,
      c_2001             TYPE sy-dynnr VALUE 2001, "Grid Declarações
      c_2002             TYPE sy-dynnr VALUE 2002, "Cadastro d Declarações
      c_2003             TYPE sy-dynnr VALUE 2003, "Grid Adições
      c_0003             TYPE sy-dynnr VALUE 0003, "Cadastro valor base
      imp_dynnr_000      TYPE sy-dynnr,
      imp_dynnr_002      TYPE sy-dynnr.

DATA: it_set_values       TYPE TABLE OF rgsb4.

DATA: gra_cfop_drawback TYPE RANGE OF j_1bnflin-cfop.

CONTROLS: tab_control TYPE TABSTRIP,
          tabdi1      TYPE TABLEVIEW USING SCREEN 0002,
          tabdi2      TYPE TABLEVIEW USING SCREEN 0002.

* Fields on Dynpro 100
DATA: g_event(30),
      g_node_key  TYPE tv_nodekey,
      g_item_name TYPE tv_itmname.

CONSTANTS: c_x TYPE c LENGTH 1 VALUE 'X'.

DATA: cria_dock_tela TYPE c LENGTH 1,
      imp_prim_di    TYPE c LENGTH 1,
      vg_node_key    TYPE tv_nodekey,
      init.

"ALV
DATA: imp_container_di     TYPE REF TO cl_gui_custom_container,
      imp_event_di         TYPE REF TO lcl_event_di,
      imp_alv_di           TYPE REF TO cl_gui_alv_grid,
      imp_gs_layout        TYPE lvc_s_layo,
      it_imp_catalog_di    TYPE lvc_t_fcat,

      imp_prim_di_ad       TYPE c LENGTH 1,
      imp_container_di_ad  TYPE REF TO cl_gui_custom_container,
      imp_event_di_ad      TYPE REF TO lcl_event_di_ad,
      it_imp_catalog_di_ad TYPE lvc_t_fcat,
      imp_alv_di_ad        TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_item_double_click.
    g_event = 'ITEM_DOUBLE_CLICK'.
    PERFORM atualizada_telas_item USING node_key. "item_name.
    imp_dynnr_002         = c_2001.
    tab_control-activetab = ok_tabdi1.
    LEAVE TO SCREEN 0001.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION .
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.
ENDCLASS.                    "lcl_tree_event_receiver definition

DATA: g_application           TYPE REF TO lcl_application,
      obj_tree_event_receiver TYPE REF TO lcl_tree_event_receiver.

TYPES: item_table_type LIKE STANDARD TABLE OF mtreeitm WITH DEFAULT KEY.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_di IMPLEMENTATION.
  METHOD handle_hotspot_di.
    IF e_column_id-fieldname EQ 'NDI'.
      READ TABLE it_znota_import INDEX e_row_id INTO znota_import.
      ok_di_altera = space.
      CALL SCREEN 2002 STARTING AT 20 10 ENDING AT 98 21.
      LEAVE TO SCREEN 0001.
    ENDIF.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_di_ad IMPLEMENTATION.
  METHOD handle_hotspot_di_ad.
    IF e_column_id-fieldname EQ 'NR_ADICAO'.
      READ TABLE it_znota_import_ad INDEX e_row_id INTO znota_import_ad.
      ok_di_altera = space.
      CALL SCREEN 2004 STARTING AT 20 10 ENDING AT 80 21.
      LEAVE TO SCREEN 0001.
    ENDIF.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD  handle_double_click.
    CLEAR vg_node_key.
    READ TABLE it_menu_tree INTO wa_menu_tree WITH KEY node_filho_key = node_key.

    IF sy-subrc = 0.
      MOVE wa_menu_tree-node_filho(6) TO vg_node_key.
      PERFORM atualizada_telas_item USING vg_node_key.
      imp_dynnr_002         = c_2001.
      tab_control-activetab = ok_tabdi1.
      LEAVE TO SCREEN 0001.
    ENDIF.

  ENDMETHOD.                    "handle_double_click

ENDCLASS.                    "LCL_TREE_EVENT_RECEIVER IMPLEMENTATION
