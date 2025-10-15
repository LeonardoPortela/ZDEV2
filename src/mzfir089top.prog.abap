*&---------------------------------------------------------------------*
*& Include MZDRE0001TOP                                      PoolMóds.        SAPMZDRE0001
*&
*&---------------------------------------------------------------------*

PROGRAM  sapmzfir089 MESSAGE-ID zdre.

TYPE-POOLS: icon.

TABLES: zfit181,
        zfit182,
        zfit183,
        zfit184.

TYPES: BEGIN OF tp_fluxo_caixa.
TYPES:  id TYPE tv_nodekey,
        tx TYPE txt50_skat.
        INCLUDE STRUCTURE zfit183.
        TYPES: END OF tp_fluxo_caixa.

TYPES: BEGIN OF tp_emp_saldo_aplic.
TYPES:  id TYPE tv_nodekey,
        tx TYPE ktext.
        INCLUDE STRUCTURE zfit184.
        TYPES: END OF tp_emp_saldo_aplic.

TYPES: BEGIN OF tp_centro_lucro.
TYPES:  id TYPE tv_nodekey,
        tx TYPE ltext.
        INCLUDE STRUCTURE zgl015_dre_est05.
        TYPES: END OF tp_centro_lucro.

TYPES: BEGIN OF tp_grupo_material.
TYPES:  id TYPE tv_nodekey,
        tx TYPE wgbez60.
        INCLUDE STRUCTURE zgl015_dre_est06.
        TYPES: END OF tp_grupo_material.

TYPES: BEGIN OF tp_nivel_agrupado.
TYPES:  id TYPE tv_nodekey,
        tx TYPE char50.
        INCLUDE STRUCTURE zgl015_dre_est07.
        TYPES: END OF tp_nivel_agrupado.

TYPES: BEGIN OF tp_dre_est02.
TYPES:   nv01      TYPE char05,
         nv02      TYPE char05,
         nv03      TYPE char05,
         nv04      TYPE char05,
         nv05      TYPE char05,
         na01      TYPE char05,
         na02      TYPE char05,
         na03      TYPE char05,
         na04      TYPE char05,
         nitxt_ant TYPE char50.
         INCLUDE STRUCTURE zfit182.
         TYPES: END OF tp_dre_est02.

TYPES: BEGIN OF tp_dre_dados.
TYPES: icost       TYPE char04,
       icoli       TYPE char04,
       irepo       TYPE char04,
       butxt       TYPE butxt,
       vstxt       TYPE vstxt_011t,
       status_prot TYPE c LENGTH 8,
       rowcolor    TYPE char04.
       INCLUDE STRUCTURE zgl020_dre_dados.
       TYPES: END OF tp_dre_dados.

TYPES: BEGIN OF ty_zfit181_alv.
TYPES:   butxt  TYPE butxt,
         editar TYPE char04,
         estrut TYPE char04.
         INCLUDE STRUCTURE zfit181.
         TYPES: END OF ty_zfit181_alv.

CONSTANTS: ok_csta      TYPE sy-ucomm   VALUE 'CSTA',
           ok_cpro      TYPE sy-ucomm   VALUE 'CPRO',
           ok_ccancpro  TYPE sy-ucomm   VALUE 'CCANCPRO',
           ok_cexcel    TYPE sy-ucomm   VALUE 'CEXCEL',
           ok_filtro    TYPE sy-ucomm   VALUE 'TAB_FILTRO',
           ok_result    TYPE sy-ucomm   VALUE 'TAB_RESULT',
           ok_save      TYPE sy-ucomm   VALUE 'SAVE',
           ok_liberar   TYPE sy-ucomm   VALUE 'LIBERAR',
           ok_back      TYPE sy-ucomm   VALUE 'BACK',
           ok_exit      TYPE sy-ucomm   VALUE 'EXIT',
           ok_cancel    TYPE sy-ucomm   VALUE 'CANCEL',
           ok_atualiza  TYPE sy-ucomm   VALUE 'ATUALIZA',
           ok_nova_est  TYPE sy-ucomm   VALUE 'NOVA_EST',
           ok_copi_est  TYPE sy-ucomm   VALUE 'COPI_EST',
           ok_dele_est  TYPE sy-ucomm   VALUE 'DELE_EST',
           ok_cod_fluxo TYPE sy-ucomm   VALUE 'COD_FLUXO',
           ok_cta_custo TYPE sy-ucomm   VALUE 'CTA_CUSTO',
           ok_cta_lucro TYPE sy-ucomm   VALUE 'CTA_LUCRO',
           ok_grp_merca TYPE sy-ucomm   VALUE 'GRP_MERCA',
           ok_agp_nivel TYPE sy-ucomm   VALUE 'AGP_NIVEL',
           ok_erro_cl   TYPE sy-ucomm   VALUE 'ERRO_CL',
           ok_erro_cc   TYPE sy-ucomm   VALUE 'ERRO_CC',
           ok_erro_gm   TYPE sy-ucomm   VALUE 'ERRO_GM',
           ok_erro_all  TYPE sy-ucomm   VALUE 'ERRO_ALL',
           tl_0002      TYPE sydynnr    VALUE '0002',
           tl_1001      TYPE sydynnr    VALUE '1001',
           tl_1002      TYPE sydynnr    VALUE '1002',
           tl_1101      TYPE sydynnr    VALUE '1101',
           tl_1102      TYPE sydynnr    VALUE '1102',
           tl_9001      TYPE sydynnr    VALUE '9001',
           tl_9003      TYPE sydynnr    VALUE '9003',
           c_x          TYPE c LENGTH 1 VALUE 'X',
           c_j          TYPE c LENGTH 1 VALUE 'J',
           c_icost      TYPE c LENGTH 5 VALUE 'ICOST',
           c_icoli      TYPE c LENGTH 5 VALUE 'ICOLI',
           c_irepo      TYPE c LENGTH 5 VALUE 'IREPO',
           c_editar     TYPE c LENGTH 6 VALUE 'EDITAR',
           c_insert     TYPE c LENGTH 6 VALUE 'INSERT',
           c_estrut     TYPE c LENGTH 6 VALUE 'ESTRUT',
           c_excluir    TYPE c LENGTH 7 VALUE 'EXCLUIR',
           c_conf       TYPE c LENGTH 4 VALUE 'CONF'.

CONTROLS: tab_strip  TYPE TABSTRIP,
          tab_filtro TYPE TABLEVIEW USING SCREEN 0001,
          tab_result TYPE TABLEVIEW USING SCREEN 0001.

DATA: ok_code_0001           TYPE sy-ucomm,
      ok_code_1103           TYPE sy-ucomm,
      ok_code_1105           TYPE sy-ucomm,
      ok_code_9000           TYPE sy-ucomm,
      ok_code_9002           TYPE sy-ucomm,
      ok_code_9004           TYPE sy-ucomm,
      ok_code_9005           TYPE sy-ucomm,
      ok_code_9006           TYPE sy-ucomm,
      ok_code_9007           TYPE sy-ucomm,
      ok_code_9008           TYPE sy-ucomm,
      ok_code_9009           TYPE sy-ucomm,
      ok_code_9010           TYPE sy-ucomm,
      ok_code_9012           TYPE sy-ucomm,
      tl_0001                TYPE sydynnr,
      tl_9000                TYPE sydynnr,
      wa_fcode               TYPE sy-ucomm,
      it_fcode               LIKE TABLE OF wa_fcode,
      qt_char_nivel          TYPE i,
      vg_nome_empresa_dest   TYPE butxt,
      vg_nome_estrutura_dest TYPE zfit181-nome_estrutura,
      vg_plano_conta         TYPE ktplt,
      vg_cc_grupo            TYPE ktext,
      vg_centro_lucro        TYPE ltext,
      vg_conta_razao         TYPE txt50_skat,
      vg_grupo_material      TYPE wgbez60,
      vg_gerar_rel           TYPE char01.

DATA: vg_seq1 TYPE zfit0109-seq,
      vg_seq2 TYPE zfit0109-seq,
      vg_seq3 TYPE zfit0109-seq,
      vg_seq4 TYPE zfit0109-seq,
      vg_seq5 TYPE zfit0109-seq.

DATA: vg_seq_desc1 TYPE zfit0109-descricao,
      vg_seq_desc2 TYPE zfit0109-descricao,
      vg_seq_desc3 TYPE zfit0109-descricao,
      vg_seq_desc4 TYPE zfit0109-descricao,
      vg_seq_desc5 TYPE zfit0109-descricao.

DATA: vg_emp1 TYPE t001-bukrs,
      vg_emp2 TYPE t001-bukrs,
      vg_emp3 TYPE t001-bukrs.

DATA: vg_emp_desc1 TYPE t001-butxt,
      vg_emp_desc2 TYPE t001-butxt,
      vg_emp_desc3 TYPE t001-butxt.

*---------- Definition -----------------------------------------------*
CLASS lcl_dre_event_dre DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_dre
                FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_dre_event_dre_est DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_dre_est
                FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.

ENDCLASS.                    "lcl_event_est_handler DEFINITION

DATA: it_dre_dados           TYPE TABLE OF zgl020_dre_dados WITH HEADER LINE,
      it_dre_dados_alv       TYPE TABLE OF tp_dre_dados WITH HEADER LINE,
      wa_dre_dados_alv       TYPE tp_dre_dados,
      wa_scroll_col          TYPE lvc_s_col,
      wa_scroll_row          TYPE lvc_s_roid,
      prim_dre               TYPE char01,
      prim_dre_est           TYPE char01,
      prim_dre_nivel         TYPE char01,
      prim_dre_nivel_re      TYPE char01,
      dre_container          TYPE REF TO cl_gui_custom_container,
      dre_alv                TYPE REF TO cl_gui_alv_grid,
      dre_container_est      TYPE REF TO cl_gui_custom_container,
      dre_alv_est            TYPE REF TO cl_gui_alv_grid,
      dre_gs_layout          TYPE lvc_s_layo,
      dre_catalogo           TYPE lvc_t_fcat,
      dre_sort               TYPE lvc_t_sort,
      dre_except_qinfo       TYPE lvc_t_qinf,
      dre_catalogo_est       TYPE lvc_t_fcat,
      dre_event_dre          TYPE REF TO lcl_dre_event_dre,
      dre_event_dre_est      TYPE REF TO lcl_dre_event_dre_est,
      it_zfit181             TYPE TABLE OF zfit181 WITH HEADER LINE,
      wa_zfit181             TYPE zfit181,
      wa_zfit182             TYPE zfit182,
      it_zfit182             TYPE TABLE OF zfit182 WITH HEADER LINE,

      it_zfit183             TYPE TABLE OF zfit183 WITH HEADER LINE,
      it_zfit183_id          TYPE TABLE OF tp_fluxo_caixa   WITH HEADER LINE,
      wa_zfit183_id          TYPE tp_fluxo_caixa,

      it_zfit184             TYPE TABLE OF zfit184 WITH HEADER LINE,
      it_zfit184_id          TYPE TABLE OF tp_emp_saldo_aplic  WITH HEADER LINE,
      wa_zfit184_id          TYPE tp_emp_saldo_aplic,

*      it_zgl015_dre_est05    TYPE TABLE OF zgl015_dre_est05 WITH HEADER LINE,
*      it_zgl015_dre_est05_id TYPE TABLE OF tp_centro_lucro  WITH HEADER LINE,
*      wa_zgl015_dre_est08    TYPE zgl015_dre_est08,
*      wa_zgl015_dre_est05_id TYPE tp_centro_lucro,
*      it_motra_erro          TYPE TABLE OF ty_motra_erro WITH HEADER LINE,
*
*      it_zgl015_dre_est06    TYPE TABLE OF zgl015_dre_est06 WITH HEADER LINE,
*      it_zgl015_dre_est06_id TYPE TABLE OF tp_grupo_material  WITH HEADER LINE,
*      wa_zgl015_dre_est06_id TYPE tp_grupo_material,
*
*      it_zgl015_dre_est07    TYPE TABLE OF zgl015_dre_est07 WITH HEADER LINE,
*      it_zgl015_dre_est07_id TYPE TABLE OF tp_nivel_agrupado WITH HEADER LINE,
*      wa_zgl015_dre_est07_id TYPE tp_nivel_agrupado,

      it_zfit181_alv         TYPE TABLE OF ty_zfit181_alv WITH HEADER LINE,
      wa_zfit181_alv         TYPE ty_zfit181_alv,
      vg_editar              TYPE c LENGTH 1,
      qt_niveis              TYPE i,
      vg_bukrs_txt           TYPE butxt,
      wa_fluxo_est02         TYPE tp_dre_est02.

*&** Níveis de Estrutura **********************************************
*&*********************************************************************

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
      FOR EVENT node_double_click
                  OF cl_gui_list_tree
        IMPORTING node_key,
      handle_expand_no_children
      FOR EVENT expand_no_children
                  OF cl_gui_list_tree
        IMPORTING node_key,
      handle_item_double_click
      FOR EVENT item_double_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_button_click
      FOR EVENT button_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_link_click
      FOR EVENT link_click
                  OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_checkbox_change
      FOR EVENT checkbox_change
                  OF cl_gui_list_tree
        IMPORTING node_key item_name checked.

ENDCLASS.                    "LCL_APPLICATION DEFINITION

TYPES: item_table_type LIKE STANDARD TABLE OF mtreeitm WITH DEFAULT KEY.

DATA: g_application      TYPE REF TO lcl_application,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_tree             TYPE REF TO cl_gui_list_tree,
      g_event(30),
      g_node_key         TYPE tv_nodekey,
      g_item_name        TYPE tv_itmname.

*&*********************************************************************



*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.

  DATA: wa_catalog TYPE lvc_s_fcat.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  wa_catalog-do_sum      = p_do_sum.
  wa_catalog-icon        = p_icon.
  wa_catalog-just        = p_just.
  wa_catalog-emphasize   = p_emphasize.
  wa_catalog-edit        = p_edit.
  APPEND wa_catalog TO it_catalogo.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*---------- Implementation -------------------------------------------*
CLASS lcl_dre_event_dre IMPLEMENTATION.
  METHOD handle_hotspot_dre.
    READ TABLE it_dre_dados_alv INTO wa_dre_dados_alv INDEX es_row_no-row_id.
    CASE e_column_id-fieldname.
      WHEN c_icost.
*        PERFORM seleciona_dre USING wa_dre_dados_alv.
        LEAVE TO SCREEN 1.
      WHEN c_icoli.
        IF wa_dre_dados_alv-liberado EQ 'X'.

          IF wa_dre_dados_alv-status GT 0.
            CALL FUNCTION 'ZENQUEUE_PROC_DRE'
              EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2
                OTHERS         = 3.
            IF sy-subrc IS  INITIAL.
              CALL FUNCTION 'ZDEQUEUE_PROC_DRE'.
*              PERFORM liberar_dre USING wa_dre_dados_alv.
              LEAVE TO SCREEN 1.

            ELSE.
              MESSAGE s005.

            ENDIF.
          ELSE.
            MESSAGE s005.

          ENDIF.
        ELSE.
*          PERFORM liberar_dre USING wa_dre_dados_alv.
          LEAVE TO SCREEN 1.
        ENDIF.
      WHEN c_irepo.
        IF ( wa_dre_dados_alv-status EQ '3' ) OR ( wa_dre_dados_alv-status EQ '2' ).
*          PERFORM liberar_dre USING wa_dre_dados_alv.
          LEAVE TO SCREEN 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_dre_event_dre_est IMPLEMENTATION.
  METHOD handle_hotspot_dre_est.
    READ TABLE it_zfit181_alv INTO wa_zfit181_alv INDEX es_row_no-row_id.
    CASE e_column_id-fieldname.
      WHEN c_editar.
        vg_editar = 'X'.
        CLEAR: zfit181.
        MOVE-CORRESPONDING wa_zfit181_alv TO zfit181.
        CALL SCREEN 9002 STARTING AT 10 10.
      WHEN c_estrut.
        tl_9000 = tl_9003.
        CLEAR: wa_zfit181.
        MOVE-CORRESPONDING wa_zfit181_alv TO wa_zfit181.
        IF NOT prim_dre_nivel IS INITIAL.
          CALL METHOD g_tree->delete_all_nodes.
          prim_dre_nivel_re = c_x.
        ENDIF.
        LEAVE TO SCREEN 9000.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    g_event = 'NODE_DOUBLE_CLICK'.
    g_node_key = node_key.
  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD  handle_item_double_click.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    g_event = 'ITEM_DOUBLE_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  handle_link_click.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    g_event = 'LINK_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  handle_button_click.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    g_event = 'BUTTON_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD  handle_checkbox_change.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    g_event = 'CHECKBOX_CHANGE'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE

  METHOD handle_expand_no_children.
    DATA: node_table TYPE treev_ntab,
          node       TYPE treev_node,
          item_table TYPE item_table_type,
          item       TYPE mtreeitm.

* show the key of the expanded node in a dynpro field
    g_event = 'EXPAND_NO_CHILDREN'.
    g_node_key = node_key.


    CALL METHOD g_tree->add_nodes_and_items
      EXPORTING
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.
    IF sy-subrc <> 0.
      MESSAGE a000.
    ENDIF.
  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION
