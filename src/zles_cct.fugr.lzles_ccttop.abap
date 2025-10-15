FUNCTION-POOL zles_cct    MESSAGE-ID zcct.


TABLES: zsdt0179, zsdt0180, zsdt0181.

*&--------------------------------------------------------------------&*
*& Tipos                                                              &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_cab_entrega.
*         DS_EMPRESA        TYPE T001-BUTXT,
*         DS_URF_DESPACHO   TYPE ZSDT0167-DS_URF,
*         DS_RA_DESPACHO    TYPE ZSDT0168-DS_RA,
*         DS_URF_EMBARQUE   TYPE ZSDT0167-DS_URF,
*         DS_RA_EMBARQUE    TYPE ZSDT0168-DS_RA,
*         RETIFICAR         TYPE C.
         INCLUDE STRUCTURE zsdt0179.
       TYPES  END OF ty_cab_entrega.

TYPES: BEGIN OF ty_parc,
         parid  TYPE j_1bnfdoc-parid,
         partyp TYPE j_1bnfdoc-partyp.
TYPES  END OF ty_parc.

TYPES: BEGIN OF ty_series,
         serie TYPE j_1bnfdoc-series.
TYPES  END OF ty_series.

TYPES: BEGIN OF ty_branch,
         cnpj    TYPE bapibranch-cgc_number,
         name    TYPE bapibranch-name,
         country TYPE adrc-country,
         region  TYPE adrc-region,
         street  TYPE adrc-street,
         city2   TYPE adrc-city2,
         city1   TYPE adrc-city1.
TYPES  END OF ty_branch.

TYPES: BEGIN OF ty_entrega_default,
         tipo_carga	 TYPE zsdt0181-tipo_carga,
         tipo_granel TYPE zsdt0181-tipo_granel,
         id_due	     TYPE zsdt0181-id_due,
         unid_medida TYPE zsdt0181-unid_medida.
TYPES  END OF ty_entrega_default.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0120,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0180.
       TYPES END OF ty_saida_0120.

TYPES: BEGIN OF ty_saida_0122,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0181.
       TYPES END OF ty_saida_0122.

TYPES: BEGIN OF ty_entrega_control,
         no_edit_documentos TYPE c,
         modo               TYPE c.
TYPES: END OF ty_entrega_control.

TYPES: BEGIN OF ty_inf_parid,
         stcd1    TYPE lfa1-stcd1,
         stcd2    TYPE lfa1-stcd2,
         cnpj_cpf TYPE j_1bstcd1,
         region   TYPE adrc-region,
       END OF ty_inf_parid.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0120 DEFINITION.
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

CLASS lcl_alv_toolbar_0122 DEFINITION.
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


CLASS lcl_event_handler_0120 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

    CLASS-METHODS : refresh_grid
      IMPORTING i_soft         TYPE xfeld  DEFAULT abap_true
                i_set_current  TYPE xfeld  DEFAULT abap_true
                i_set_selected TYPE xfeld  DEFAULT abap_false.


ENDCLASS.


CLASS lcl_event_handler_0122 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

DATA: obj_alv_0120       TYPE REF TO cl_gui_alv_grid,
      obj_container_0120 TYPE REF TO cl_gui_custom_container,
      obj_alv_0122       TYPE REF TO cl_gui_alv_grid,
      obj_container_0122 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0120 TYPE REF TO lcl_alv_toolbar_0120,
      obj_toolbar_0122 TYPE REF TO lcl_alv_toolbar_0122.

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


*&--------------------------------------------------------------------&*
*& Variaveis de Comando                                               &*
*&--------------------------------------------------------------------&*

DATA: ok_code_0100 TYPE syucomm.

*&--------------------------------------------------------------------&*
*& Variaveis Dynpro                                                   &*
*&--------------------------------------------------------------------&*

DATA: entrega_dynnr_000 LIKE sy-dynnr.

*&--------------------------------------------------------------------&*
*& Controles                                                          &*
*&--------------------------------------------------------------------&*

CONTROLS: info_entrega_tab  TYPE TABSTRIP.

*&--------------------------------------------------------------------&*
*& Constantes
*&--------------------------------------------------------------------&*

CONSTANTS: c_entrega_novo   TYPE c VALUE '1'   LENGTH 1,
           c_entrega_change TYPE c VALUE '2'   LENGTH 1,
           c_entrega_view   TYPE c VALUE '3'   LENGTH 1.

CONSTANTS: c_novo      TYPE c VALUE 'NOVO'      LENGTH 4,
           c_del       TYPE c VALUE 'DEL'       LENGTH 4,
           c_save      TYPE c VALUE 'SAVE'      LENGTH 4,
           c_cancel    TYPE c VALUE 'CANCEL'    LENGTH 6,
           c_change    TYPE c VALUE 'CHANGE'    LENGTH 6,
           c_view      TYPE c VALUE 'VIEW'      LENGTH 6,
           c_doc_carga TYPE c VALUE 'DOC_CARGA' LENGTH 10.


"Tabs
CONSTANTS: entrega_tb01 TYPE c LENGTH 12 VALUE 'ENTREGA_TB01',
           entrega_tb02 TYPE c LENGTH 12 VALUE 'ENTREGA_TB02'.

"Screens
CONSTANTS: entrega_0110 LIKE sy-dynnr VALUE '0110',
           entrega_0120 LIKE sy-dynnr VALUE '0120'.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao_0120 TYPE c LENGTH 20,
      var_answer       TYPE c.

*-------------------------------------------------------------------
* Ranges
*-------------------------------------------------------------------

RANGES: rg_serie       FOR j_1bnfdoc-series.

*&--------------------------------------------------------------------&*
*& Internal Table e Work Area
*&--------------------------------------------------------------------&*

DATA: BEGIN OF tg_0180 OCCURS 0.
        INCLUDE TYPE zsdt0180.
      DATA: END OF tg_0180.

DATA: BEGIN OF tg_0181 OCCURS 0.
        INCLUDE TYPE zsdt0181.
      DATA: END OF tg_0181.

DATA: it_saida_0120     TYPE TABLE OF ty_saida_0120,
      wa_saida_0120     TYPE ty_saida_0120,
      "IT_SAIDA_0120_ITM   TYPE TABLE OF TY_SAIDA_0120,
      "WA_SAIDA_0120_ITM   TYPE TY_SAIDA_0120,

      it_saida_0122     TYPE TABLE OF ty_saida_0122,
      wa_saida_0122     TYPE ty_saida_0122,
      it_saida_0122_itm TYPE TABLE OF ty_saida_0122,
      wa_saida_0122_itm TYPE ty_saida_0122,
      it_parc           TYPE TABLE OF ty_parc WITH HEADER LINE,
      it_series         TYPE TABLE OF ty_series WITH HEADER LINE.

DATA: cab_entrega     TYPE ty_cab_entrega,
      entrega_control TYPE ty_entrega_control,
      entrega_default TYPE ty_entrega_default.


* INCLUDE LZLES_CCTD...                      " Local class definition
