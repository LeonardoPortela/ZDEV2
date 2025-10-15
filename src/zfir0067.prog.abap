*&---------------------------------------------------------------------*
*& Report  ZFIR0067
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir0067.

TYPE-POOLS icon.

*----------------------------------------------------------------------*
***Tabelas
*----------------------------------------------------------------------*
TABLES: t001, bsik, zfit0111, makt.


*---------------------------------------------------------------------*
*  Types
*---------------------------------------------------------------------*

TYPES: BEGIN OF ty_saida,
         bukrs           TYPE zfit0111-bukrs,
         codigo          TYPE string,
         descricao       TYPE zfit0109-descricao,
         clas_flx        TYPE zfit0111-clas_flx,
         tp_prev         TYPE zfit0109-tp_prev,
         st_calc_sdo     TYPE zfit0109-st_calc_sdo,
         processo_esp    TYPE zfit0109-processo_esp,
         sistema_orig    TYPE zfit0109-sistema_orig,
         seq             TYPE zfit0111-seq,
         dt_vcto         TYPE zfit0111-dt_vcto,
         nivel           TYPE c,
         varicacao       TYPE c,
         pgto_bloq       TYPE c,
         txt_vrs1(60)    TYPE c,
         txt_vrs2(60)    TYPE c,
         color           TYPE   kkblo_specialcol OCCURS 0,
         "Versão em Exibção---------------------------------*
         dt_versao       TYPE zfit0111-dt_base_versao,
         versao          TYPE zfit0111-versao,
         hora_versao     TYPE zfit0111-hora_versao,
         "Versão Variação-----------------------------------*
         dt_versao_var   TYPE zfit0111-dt_base_versao,
         versao_var      TYPE zfit0111-versao,
         hora_versao_var TYPE zfit0111-hora_versao,
         "--------------------------------------------------*
         day_01          TYPE zfit0111-dmbtr,
         day_02          TYPE zfit0111-dmbtr,
         day_03          TYPE zfit0111-dmbtr,
         day_04          TYPE zfit0111-dmbtr,
         day_05          TYPE zfit0111-dmbtr,
         day_06          TYPE zfit0111-dmbtr,
         day_07          TYPE zfit0111-dmbtr,
         day_08          TYPE zfit0111-dmbtr,
         day_09          TYPE zfit0111-dmbtr,
         day_10          TYPE zfit0111-dmbtr,
         day_11          TYPE zfit0111-dmbtr,
         day_12          TYPE zfit0111-dmbtr,
         day_13          TYPE zfit0111-dmbtr,
         day_14          TYPE zfit0111-dmbtr,
         day_15          TYPE zfit0111-dmbtr,
         day_16          TYPE zfit0111-dmbtr,
         day_17          TYPE zfit0111-dmbtr,
         day_18          TYPE zfit0111-dmbtr,
         day_19          TYPE zfit0111-dmbtr,
         day_20          TYPE zfit0111-dmbtr,
         day_21          TYPE zfit0111-dmbtr,
         day_22          TYPE zfit0111-dmbtr,
         day_23          TYPE zfit0111-dmbtr,
         day_24          TYPE zfit0111-dmbtr,
         day_25          TYPE zfit0111-dmbtr,
         day_26          TYPE zfit0111-dmbtr,
         day_27          TYPE zfit0111-dmbtr,
         day_28          TYPE zfit0111-dmbtr,
         day_29          TYPE zfit0111-dmbtr,
         day_30          TYPE zfit0111-dmbtr,
         day_31          TYPE zfit0111-dmbtr,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_var,
         desc_flx   LIKE zfit0109-descricao,
         name1      LIKE lfa1-name1,
         txt50      LIKE skat-txt50,
         opr_numero LIKE zfit0119-opr_numero,
         con_codigo LIKE zfit0119-con_codigo,
         mdo_codigo LIKE zfit0119-mdo_codigo,
         par_tipo   LIKE zfit0119-par_tipo,
         mdo_tipo   LIKE zfit0119-mdo_tipo,
         bukrs_opr  LIKE zfit0119-bukrs_opr,
         agente     LIKE zfit0119-agente,
         regra_val  LIKE zfit0119-regra_val.
         INCLUDE STRUCTURE zfit0079.
TYPES  END OF ty_saida_var.

TYPES: BEGIN OF ty_saida_vproc,
         bukrs          TYPE zfit0111-bukrs,
         dt_base_versao TYPE zfit0111-dt_base_versao,
         versao         TYPE zfit0111-versao,
         hora_versao    TYPE zfit0111-hora_versao,
         kursf          TYPE zfit0111-kursf,
       END OF ty_saida_vproc.

TYPES: BEGIN OF ty_saida_bmov.
         INCLUDE STRUCTURE zfit0117.
TYPES: END OF ty_saida_bmov.

TYPES: BEGIN OF ty_days_mov,
         coluna(06) TYPE c,
         dt_vcto    LIKE zfit0079-zfbdt,
       END OF ty_days_mov.

TYPES: BEGIN OF ty_list_vrs,
         dt_base_versao TYPE zfit0111-dt_base_versao,
         bukrs          TYPE zfit0111-bukrs,
         butxt          TYPE t001-butxt,
         versao         TYPE zfit0111-versao,
         hora_versao    TYPE zfit0111-hora_versao,
       END OF ty_list_vrs.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
*Define an event handler method for each event you want to react to.
    METHODS handle_node_double_click
      FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key sender.

ENDCLASS.

CLASS lcl_event_handler_0101 DEFINITION.

  PUBLIC SECTION.                                           "

    CLASS-METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      user_command    FOR EVENT user_command  OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_f4            FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.

CLASS lcl_alv_toolbar_bloq_mov DEFINITION.
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

CLASS lcl_alv_toolbar_mov_ajuste DEFINITION.
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

CLASS lcl_event_handler_0107 DEFINITION.

  PUBLIC SECTION.                                           "

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.

CLASS lcl_event_handler_0103v1 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

CLASS lcl_event_handler_0103v2 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       Definição da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_110 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                  e_interactive.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*       Implementação da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_110 IMPLEMENTATION.

  METHOD handle_toolbar.
    PERFORM elimina_botoes_list_vrs  USING e_object.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Declaração de Instância de Métodos
*&---------------------------------------------------------------------*
DATA: v_event_receiver_110 TYPE REF TO lcl_event_receiver_110.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: vg_no_valid        TYPE c,
      wa_bukrs_versao    TYPE zfit0111-bukrs,
      wa_dt_versao       TYPE zfit0111-dt_base_versao,
      wa_hora_versao     TYPE zfit0111-hora_versao,
      wa_versao          TYPE zfit0111-versao,
      wa_dt_versao_var   TYPE zfit0111-dt_base_versao,
      wa_versao_var      TYPE zfit0111-versao,
      wa_hora_versao_var TYPE zfit0111-hora_versao,
      vg_tx_usd_brl      TYPE ukurs_curr,
      vg_tx_usd_ars      TYPE ukurs_curr,
      vg_tx_eur_brl      TYPE ukurs_curr,
      vg_tx_eur_usd      TYPE ukurs_curr,
      wg_coment_ajuste   TYPE zfit0149,
      vg_moeda_int       TYPE c,
      vg_ok_0110         TYPE sy-ucomm,
      vg_waers           TYPE waers.

*---------------------------------------------------------------------*
* Constantes
*---------------------------------------------------------------------*

CONSTANTS: c_ars VALUE 'ARS' TYPE c LENGTH 3,
           c_brl VALUE 'BRL' TYPE c LENGTH 3,
           c_usd VALUE 'USD' TYPE c LENGTH 3.

*-------------------------------------------------------------------
* Variaveis Colunns
*-------------------------------------------------------------------
DATA: day_01_mov TYPE zfit0111-dt_vcto,
      day_02_mov TYPE zfit0111-dt_vcto,
      day_03_mov TYPE zfit0111-dt_vcto,
      day_04_mov TYPE zfit0111-dt_vcto,
      day_05_mov TYPE zfit0111-dt_vcto,
      day_06_mov TYPE zfit0111-dt_vcto,
      day_07_mov TYPE zfit0111-dt_vcto,
      day_08_mov TYPE zfit0111-dt_vcto,
      day_09_mov TYPE zfit0111-dt_vcto,
      day_10_mov TYPE zfit0111-dt_vcto,
      day_11_mov TYPE zfit0111-dt_vcto,
      day_12_mov TYPE zfit0111-dt_vcto,
      day_13_mov TYPE zfit0111-dt_vcto,
      day_14_mov TYPE zfit0111-dt_vcto,
      day_15_mov TYPE zfit0111-dt_vcto,
      day_16_mov TYPE zfit0111-dt_vcto,
      day_17_mov TYPE zfit0111-dt_vcto,
      day_18_mov TYPE zfit0111-dt_vcto,
      day_19_mov TYPE zfit0111-dt_vcto,
      day_20_mov TYPE zfit0111-dt_vcto,
      day_21_mov TYPE zfit0111-dt_vcto,
      day_22_mov TYPE zfit0111-dt_vcto,
      day_23_mov TYPE zfit0111-dt_vcto,
      day_24_mov TYPE zfit0111-dt_vcto,
      day_25_mov TYPE zfit0111-dt_vcto,
      day_26_mov TYPE zfit0111-dt_vcto,
      day_27_mov TYPE zfit0111-dt_vcto,
      day_28_mov TYPE zfit0111-dt_vcto,
      day_29_mov TYPE zfit0111-dt_vcto,
      day_30_mov TYPE zfit0111-dt_vcto,
      day_31_mov TYPE zfit0111-dt_vcto.

DATA: day_01_ajuste TYPE zfit0111-dt_vcto,
      day_02_ajuste TYPE zfit0111-dt_vcto,
      day_03_ajuste TYPE zfit0111-dt_vcto,
      day_04_ajuste TYPE zfit0111-dt_vcto,
      day_05_ajuste TYPE zfit0111-dt_vcto,
      day_06_ajuste TYPE zfit0111-dt_vcto,
      day_07_ajuste TYPE zfit0111-dt_vcto,
      day_08_ajuste TYPE zfit0111-dt_vcto,
      day_09_ajuste TYPE zfit0111-dt_vcto,
      day_10_ajuste TYPE zfit0111-dt_vcto,
      day_11_ajuste TYPE zfit0111-dt_vcto,
      day_12_ajuste TYPE zfit0111-dt_vcto,
      day_13_ajuste TYPE zfit0111-dt_vcto,
      day_14_ajuste TYPE zfit0111-dt_vcto,
      day_15_ajuste TYPE zfit0111-dt_vcto,
      day_16_ajuste TYPE zfit0111-dt_vcto,
      day_17_ajuste TYPE zfit0111-dt_vcto,
      day_18_ajuste TYPE zfit0111-dt_vcto,
      day_19_ajuste TYPE zfit0111-dt_vcto,
      day_20_ajuste TYPE zfit0111-dt_vcto,
      day_21_ajuste TYPE zfit0111-dt_vcto,
      day_22_ajuste TYPE zfit0111-dt_vcto,
      day_23_ajuste TYPE zfit0111-dt_vcto,
      day_24_ajuste TYPE zfit0111-dt_vcto,
      day_25_ajuste TYPE zfit0111-dt_vcto,
      day_26_ajuste TYPE zfit0111-dt_vcto,
      day_27_ajuste TYPE zfit0111-dt_vcto,
      day_28_ajuste TYPE zfit0111-dt_vcto,
      day_29_ajuste TYPE zfit0111-dt_vcto,
      day_30_ajuste TYPE zfit0111-dt_vcto,
      day_31_ajuste TYPE zfit0111-dt_vcto.

*----------------------------------------------------------------------*
* Tabelas Interna e Work Areas
*----------------------------------------------------------------------*
DATA: tg_0109            TYPE TABLE OF zfit0109 WITH HEADER LINE,
      tg_0111            TYPE TABLE OF zfit0111 WITH HEADER LINE,
      tg_0111_group      TYPE TABLE OF zfit0111 WITH HEADER LINE,
      tg_0117            TYPE TABLE OF zfit0117 WITH HEADER LINE,
      tg_0118            TYPE TABLE OF zfit0118 WITH HEADER LINE,
      tg_0118_group      TYPE TABLE OF zfit0118 WITH HEADER LINE,
      it_saida           TYPE TABLE OF ty_saida,
      wa_saida           TYPE ty_saida,
      it_scalc_saldo     TYPE TABLE OF ty_saida,
      wa_scalc_saldo     TYPE ty_saida,
      it_alv_tree        TYPE TABLE OF ty_saida,
      wa_alv_tree        TYPE ty_saida,
      it_saida_mov_flx   TYPE TABLE OF ty_saida,
      wa_saida_mov_flx   TYPE ty_saida,
      it_saida_var1      TYPE TABLE OF ty_saida_var,
      wa_saida_var1      TYPE ty_saida_var,
      it_saida_var2      TYPE TABLE OF ty_saida_var,
      wa_saida_var2      TYPE ty_saida_var,
      it_saida_vproc     TYPE TABLE OF ty_saida_vproc,
      wa_saida_vproc     TYPE ty_saida_vproc,
      it_saida_bmov      TYPE TABLE OF ty_saida_bmov,
      wa_saida_bmov      TYPE ty_saida_bmov,
      it_sai_ajuste      TYPE TABLE OF ty_saida,
      wa_sai_ajuste      TYPE ty_saida,
      wa_sai_ajuste_copy TYPE ty_saida,
      it_mov_ajuste      TYPE TABLE OF ty_saida,
      wa_mov_ajuste      TYPE ty_saida,
      tg_days_mov_ajuste TYPE TABLE OF ty_days_mov WITH HEADER LINE,
      it_rsparams        TYPE TABLE OF rsparams,
      wa_rsparams        TYPE rsparams.


DATA: BEGIN OF tg_t001 OCCURS 0,
        bukrs TYPE t001-bukrs,
        butxt TYPE t001-butxt,
      END OF tg_t001,

      BEGIN OF tg_last_versao OCCURS 0,
        bukrs          TYPE t001-bukrs,
        dt_base_versao TYPE zfit0111-dt_base_versao,
        versao         TYPE zfit0111-versao,
      END OF tg_last_versao.

DATA: BEGIN OF tg_0079 OCCURS 0.
        INCLUDE STRUCTURE zfit0079.
DATA: END OF tg_0079.

DATA: BEGIN OF tg_0119 OCCURS 0.
        INCLUDE STRUCTURE zfit0119.
DATA: END OF tg_0119.

DATA: BEGIN OF tg_lfa1 OCCURS 0,
        lifnr LIKE lfa1-lifnr,
        name1 LIKE lfa1-name1,
      END OF tg_lfa1.

DATA: BEGIN OF tg_kna1 OCCURS 0,
        kunnr LIKE kna1-kunnr,
        name1 LIKE kna1-name1,
      END OF tg_kna1.

DATA: BEGIN OF tg_skat OCCURS 0.
        INCLUDE STRUCTURE skat.
DATA: END OF tg_skat.

DATA: t_list_vrs TYPE TABLE OF ty_list_vrs.
*----------------------------------------------------------------------*
* Estrutura
*----------------------------------------------------------------------*
DATA: vg_node_key       TYPE lvc_t_nkey,
      it_fcat           TYPE lvc_t_fcat,
      wa_fcat           TYPE lvc_s_fcat,
      h_header          TYPE treev_hhdr,
      wa_variant        TYPE disvariant,
      vg_no_expand_node TYPE c,
      tree              TYPE REF TO cl_gui_alv_tree.

DATA: g_variant       LIKE disvariant,
      g_layout        TYPE lvc_s_layo,
      g_tree          TYPE REF TO cl_gui_alv_tree,
      dg_html_cntrl   TYPE REF TO cl_gui_html_viewer,
      dg_dyndoc_id    TYPE REF TO cl_dd_document,
      dg_splitter     TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2   TYPE REF TO cl_gui_splitter_container,
      dg_parent_tree  TYPE REF TO cl_gui_container,
      dg_parent_html  TYPE REF TO cl_gui_container,
      dg_parent_html1 TYPE REF TO cl_gui_container,
      dg_parent_html2 TYPE REF TO cl_gui_container,
      picture         TYPE REF TO cl_gui_picture,
      container       TYPE REF TO cl_gui_custom_container,
      ok_code         TYPE sy-ucomm.

DATA: obj_alv_mov_flx       TYPE REF TO cl_gui_alv_grid,
      obj_container_mov_flx TYPE REF TO cl_gui_custom_container,
      obj_alv_var_v1        TYPE REF TO cl_gui_alv_grid,
      obj_container_var_v1  TYPE REF TO cl_gui_custom_container,
      obj_alv_var_v2        TYPE REF TO cl_gui_alv_grid,
      obj_container_var_v2  TYPE REF TO cl_gui_custom_container,
      obj_alv_vproc         TYPE REF TO cl_gui_alv_grid,
      obj_container_vproc   TYPE REF TO cl_gui_custom_container,
      obj_alv_bmov          TYPE REF TO cl_gui_alv_grid,
      obj_container_bmov    TYPE REF TO cl_gui_custom_container,
      obj_alv_ajuste        TYPE REF TO cl_gui_alv_grid,
      obj_container_ajuste  TYPE REF TO cl_gui_custom_container.

*----------------------------------------------------------------------*
* Ranges
*----------------------------------------------------------------------*
RANGES: r_tp_prev     FOR zfit0109-tp_prev,
        s_dt_versao   FOR zfit0079-dt_base_versao,
        s_versao      FOR zfit0079-versao,
        s_bukrs_rep   FOR t001-bukrs,
        s_dt_rep      FOR zfit0079-zfbdt.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row,

      it_selectedcell  TYPE lvc_t_cell,
      wa_selectedcell  TYPE lvc_s_cell.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

DATA: obj_toolbar_bmov   TYPE REF TO lcl_alv_toolbar_bloq_mov,
      obj_toolbar_ajuste TYPE REF TO lcl_alv_toolbar_mov_ajuste.

DATA: gt_bdc TYPE TABLE OF bdcdata,
      gw_bdc TYPE bdcdata.

*----------------------------------------------------------------------*
* Group Box
*----------------------------------------------------------------------*
DATA: qd_versao_1(60),
      qd_versao_2(60).

*----------------------------------------------------------------------*
* Grid
*----------------------------------------------------------------------*
DATA: v_grid_110 TYPE REF TO cl_gui_alv_grid.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR t001-bukrs OBLIGATORY,
                  s_zfbdt FOR bsik-zfbdt OBLIGATORY,
                  s_waers FOR bsik-waers NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. 171477 CS2022001075 ZFI0101 PSA
*  PARAMETERS: p_empre RADIOBUTTON GROUP g1,
*              p_forte RADIOBUTTON GROUP g1.
*SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_nacio RADIOBUTTON GROUP g2,
              p_inter RADIOBUTTON GROUP g2.
SELECTION-SCREEN: END OF BLOCK b3.



*=============================================================================*
*Subscreen para listar versões                                                *
*=============================================================================*
SELECTION-SCREEN BEGIN OF SCREEN 109 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF BLOCK b109 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: s_bukvrs FOR t001-bukrs,
                    s_dtvrs FOR zfit0111-dt_base_versao.
  SELECTION-SCREEN: END OF BLOCK b109.
SELECTION-SCREEN END   OF SCREEN 109.

*=============================================================================*
* Initialization
*=============================================================================*
INITIALIZATION.

  REFRESH s_zfbdt.
  s_zfbdt-sign   = 'I'.
  s_zfbdt-option = 'BT'.
  s_zfbdt-low    = sy-datum.
  s_zfbdt-high   = sy-datum + 30.
  APPEND s_zfbdt.


*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  CLEAR: vg_moeda_int.

  SELECT SINGLE *
    FROM t001 INTO @DATA(_wl_t001)
   WHERE bukrs = @s_bukrs-low.

*  IF ( sy-subrc NE 0 ) OR 171477 CS2022001075 ZFI0101 PSA
*     ( _wl_t001-waers IS INITIAL  ).
*    MESSAGE |Informações referente a moeda da empresa: { s_bukrs-low }, não encontrado ou incompleto!| TYPE 'S'.
*    EXIT.
*  ENDIF.

  IF ( _wl_t001-waers IN s_waers ). "171477 CS2022001075 ZFI0101 PSA
    vg_moeda_int = abap_TRUE.
  ELSE.
    vg_moeda_int = abap_false.
  ENDIF.

  vg_waers = s_waers-low.



*  IF p_empre IS NOT INITIAL. "Foi comentado essa automatização para voltar o matchcode 171477 CS2022001075 ZFI0101 PSA
*    vg_moeda_int = 'X'.
*  ENDIF.
*
*  IF p_empre IS NOT INITIAL AND _wl_t001-bukrs NE '0004'.
*    vg_waers = _wl_t001-waers.
*  ELSE.
*    vg_waers = 'USD'.
*  ENDIF.


  vg_no_expand_node = 'X'.

  CALL SCREEN 0100.

  INCLUDE zfir0067_form.
  INCLUDE zfir0067_class.
  INCLUDE zfir0067_pbo.
  INCLUDE zfir0067_pai.

MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN: 'LCTO_AJUS'.
      PERFORM lcto_ajuste.
      IF g_tree IS NOT INITIAL.
        CALL METHOD g_tree->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: g_tree.
      ENDIF.
    WHEN: 'REN'.
      CALL METHOD g_tree->get_expanded_nodes
        CHANGING
          ct_expanded_nodes = vg_node_key.

      IF g_tree IS NOT INITIAL.
        CALL METHOD g_tree->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: g_tree.
      ENDIF.
    WHEN: 'VERIF_VSR'.
      CALL SCREEN 0102 STARTING AT 02 02 ENDING AT 90 10.
    WHEN: 'VRS_PROC'.
      CALL SCREEN 0104 STARTING AT 02 02 ENDING AT 57 10.
    WHEN: 'BLOQ_MOV'.
      PERFORM lctos_bloq_mov.
    WHEN: 'PROC_FLX'.
      CALL SCREEN 0106 STARTING AT 02 02 ENDING AT 65 02.
      IF g_tree IS NOT INITIAL.
        CALL METHOD g_tree->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE: g_tree.
      ENDIF.
    WHEN: 'LIST_VSR'.
      REFRESH: t_list_vrs.
      CALL SCREEN 0110 STARTING AT 02 02 ENDING AT 118 20.
  ENDCASE.

ENDMODULE.
