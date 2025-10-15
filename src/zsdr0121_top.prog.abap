*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0095.

TABLES: j_1bnfdoc,
        zsdt0246.

TYPES: BEGIN OF ty_vbrk,
         vbeln  TYPE vbrk-vbeln,
         vkorg  TYPE vbrk-vkorg,
         bupla  TYPE vbrk-bupla,
         fkart  TYPE vbrk-fkart,
         fkdat  TYPE vbrk-fkdat,
         refkey TYPE j_1bnflin-refkey,
         vbelv  TYPE vbfa-vbelv,
         ernam  TYPE vbrk-ernam,
         erzet  TYPE vbrk-erzet,
         erdat  TYPE vbrk-erdat,
       END OF ty_vbrk.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         status         TYPE icon-id,
         docnum         TYPE j_1bnfdoc-docnum,
         nfenum         TYPE j_1bnfdoc-nfenum,
         serie          TYPE j_1bnfdoc-series,
         docdat         TYPE j_1bnfdoc-docdat,
         chave_nfe      TYPE c LENGTH 44,
         branch         TYPE j_1bnfdoc-branch,
         ds_filial      TYPE j_1bbranch-name,
         matnr          TYPE j_1bnflin-matnr,
         menge          TYPE j_1bnflin-menge,
         netpr          TYPE j_1bnflin-netpr,
         netwr          TYPE j_1bnflin-netwr,
         charg          TYPE j_1bnflin-charg,
         menge_bol      TYPE j_1bnflin-menge,
         saldo_nf       TYPE j_1bnflin-menge,
         saldo_final    TYPE j_1bnflin-menge,
         qtde_utilizada TYPE zsdt0251-qtde_utilizada,
         id_boletim     TYPE zsdt0246-id_boletim,
*-CS2021000386 - 28.04.2021 - JT - inicio
         categ_soja     TYPE zsdt0246-categ_soja,
         descr_soja     TYPE char30, "zsdt0246-categ_soja,
         lgort_v        TYPE zsdt0251-lgort_v,
         mblnr_mb1b     TYPE zsdt0251-mblnr_mb1b,
         mjahr_mb1b     TYPE zsdt0251-mjahr_mb1b,
         mblnr          TYPE mkpf-mblnr,
         mjahr          TYPE mkpf-mjahr,
         bktxt          TYPE mkpf-bktxt,
         message_mb1b   TYPE zsdt0251-message_mb1b,
*-CS2021000386 - 28.04.2021 - JT - fim
         docdat_nfe_ret TYPE j_1bnfdoc-docdat,
         nfenum_nfe_ret TYPE j_1bnfdoc-nfenum,
         docnum_nfe_ret TYPE j_1bnfdoc-docnum,
         chave_nfe_ret  TYPE c LENGTH 44,
         dias_emissao   TYPE i,
         disp_porto     TYPE c LENGTH 4,
         color          TYPE kkblo_specialcol OCCURS 0.
TYPES END OF ty_saida_0100.

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
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100 TYPE TABLE OF ty_saida_0100,
      wa_saida_0100 TYPE ty_saida_0100.


DATA: tg_vbrk           TYPE TABLE OF ty_vbrk        WITH HEADER LINE,
      tg_vbrp           TYPE TABLE OF vbrp           WITH HEADER LINE,
      tg_likp           TYPE TABLE OF likp           WITH HEADER LINE,
      tg_lips           TYPE TABLE OF lips           WITH HEADER LINE,
      tg_zsdt0023       TYPE TABLE OF zsdt0023       WITH HEADER LINE,
      tg_j_1bnflin      TYPE TABLE OF j_1bnflin      WITH HEADER LINE,
      tg_j_1bnfe_active TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
      tg_vbfa_estorno   TYPE TABLE OF vbfa           WITH HEADER LINE,
      tg_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc      WITH HEADER LINE,
      tg_j_1bbranch     TYPE TABLE OF j_1bbranch     WITH HEADER LINE,
      tg_zfiwrt0008     TYPE TABLE OF zfiwrt0008     WITH HEADER LINE,
      tg_zsdt0246       TYPE TABLE OF zsdt0246       WITH HEADER LINE,
      tg_zsdt0252       TYPE TABLE OF zsdt0252       WITH HEADER LINE,
      tg_zsdt0249       TYPE TABLE OF zsdt0249       WITH HEADER LINE,
      tg_zsdt0251       TYPE TABLE OF zsdt0251       WITH HEADER LINE,
      tg_mkpf           TYPE TABLE OF mkpf           WITH HEADER LINE,
      tg_icon           TYPE TABLE OF icon           WITH HEADER LINE.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao TYPE c LENGTH 20,
      var_answer  TYPE c,
      l_lgort     TYPE lgort_d,
      l_erro      TYPE c,
      l_message   TYPE bapiret2-message.

DATA: wl_header TYPE bapi2017_gm_head_01,
      wl_code   TYPE bapi2017_gm_code,
      wl_mblnr  TYPE zfiwrt0008-mblnr,
      wl_mjahr  TYPE mjahr,
      wl_item   TYPE bapi2017_gm_item_create,
      tl_item   TYPE TABLE OF bapi2017_gm_item_create,
      tl_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------


*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. "Parametros Seleção
SELECT-OPTIONS: p_branch  FOR j_1bnfdoc-branch,
                p_docdat  FOR j_1bnfdoc-docdat OBLIGATORY,
                p_categ   FOR zsdt0246-categ_soja.
"Saldo
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002. "NF Com Saldo

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_sld_s  RADIOBUTTON GROUP r1  MODIF ID v1. "Sim
SELECTION-SCREEN COMMENT 03(04) text-i01 MODIF ID v1.

PARAMETERS: r_sld_n  RADIOBUTTON GROUP r1  MODIF ID v1. "Não
SELECTION-SCREEN COMMENT 10(03) text-i02 MODIF ID v1.

PARAMETERS: r_sld_t  RADIOBUTTON GROUP r1  MODIF ID v1. "Todas
SELECTION-SCREEN COMMENT 16(06) text-i03 MODIF ID v1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.


"Disponivel Porto
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003. "Disponivel no Porto

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_prt_s  RADIOBUTTON GROUP r2  MODIF ID v2. "Sim
SELECTION-SCREEN COMMENT 03(04) text-i01 MODIF ID v2.

PARAMETERS: r_prt_n  RADIOBUTTON GROUP r2  MODIF ID v2. "Não
SELECTION-SCREEN COMMENT 10(03) text-i02 MODIF ID v2.

PARAMETERS: r_prt_t  RADIOBUTTON GROUP r2  MODIF ID v2. "Todas
SELECTION-SCREEN COMMENT 16(06) text-i03 MODIF ID v2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM: f_selecionar_dados,
           f_processa_dados,
           f_show_alv.
