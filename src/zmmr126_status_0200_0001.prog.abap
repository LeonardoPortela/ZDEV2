*----------------------------------------------------------------------*
***INCLUDE ZLESR0105_STATUS_0200.
*----------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION DEFERRED.
CLASS lcl_alv_toolbar   DEFINITION DEFERRED.
CLASS lcl_alv_toolbar_2 DEFINITION DEFERRED.
CLASS lcl_application   DEFINITION DEFERRED.

DATA: dg_splitter          TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer      TYPE REF TO cl_gui_container,
      ctl_cccontainer2     TYPE REF TO cl_gui_container,
      ctl_cccontainer2a    TYPE REF TO cl_gui_container,
      ctl_cccontainer2b    TYPE REF TO cl_gui_container,
      ctl_alv              TYPE REF TO cl_gui_alv_grid,
      ctl_alv2             TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      obg_toolbar_2        TYPE REF TO lcl_alv_toolbar_2,
      obj_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      obj_toolbarmanager_2 TYPE REF TO cl_alv_grid_toolbar_manager,
      dg_splitter_2        TYPE REF TO cl_gui_splitter_container,
      g_application        TYPE REF TO lcl_application.

DATA: it_fieldcatalog   TYPE lvc_t_fcat,
      it_fieldcatalog_2 TYPE lvc_t_fcat,
      wa_fieldcatalog   TYPE lvc_s_fcat,
      it_except_qinfo   TYPE lvc_t_qinf,
      gs_variant        TYPE disvariant,
      gs_variant_2      TYPE disvariant,
      gs_layout         TYPE lvc_s_layo,
      gs_layout_2       TYPE lvc_s_layo,
      it_exclude_fcode  TYPE ui_functions,
      wa_exclude_fcode  LIKE LINE OF it_exclude_fcode,
      gs_scroll_col     TYPE lvc_s_col,
      gs_scroll_row     TYPE lvc_s_roid,
      gs_scroll_col_2   TYPE lvc_s_col,
      gs_scroll_row_2   TYPE lvc_s_roid,
      it_selected_rows  TYPE lvc_t_row,
      wa_selected_rows  TYPE lvc_s_row,
      events            TYPE cntl_simple_events.

DATA: it_retorno_alv          TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      it_retorno_alv_sel      TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      it_romaneio_entrada     TYPE TABLE OF zsdt0001 WITH HEADER LINE,
      it_romaneio_entrada_sel TYPE TABLE OF zsdt0001 WITH HEADER LINE,
      wa_carga_romaneio       TYPE ty_itens_alv.

DATA: ck_mostrar_entrada TYPE char01,
      ck_mostrar_logs    TYPE char01,
      ck_registro_log    TYPE char01,
      qt_rows_splitter   TYPE i.

DATA: event_handler      TYPE REF TO lcl_event_handler.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_2 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor  IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_link_click   FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name,
      handle_double_click FOR EVENT node_double_click OF cl_gui_column_tree IMPORTING node_key.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  DATA: hierarchy_header TYPE treev_hhdr,
        event            TYPE cntl_simple_event.

  SET PF-STATUS 'PF0200'.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      SET TITLEBAR 'TL0200'.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      SET TITLEBAR 'TL0201'.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      SET TITLEBAR 'TL0202'.
  ENDCASE.

  IF qt_rows_splitter IS INITIAL.
    qt_rows_splitter = 1.
  ENDIF.

  IF it_retorno_alv[] IS INITIAL.
    PERFORM carrega_saida.
  ENDIF.

  IF ck_confer_carga EQ abap_true.
    ck_mostrar_entrada = abap_true.
    READ TABLE it_retorno_alv INDEX 1 ASSIGNING FIELD-SYMBOL(<retorno_alv_sel>).
    PERFORM carregar_entrada USING <retorno_alv_sel>.
    qt_rows_splitter = 2.
    CLEAR: ck_confer_carga.
  ENDIF.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = qt_rows_splitter
        columns = 1.

    ctl_cccontainer = dg_splitter->get_container( row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.

    "Hints
    PERFORM fill_it_hints.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

*   Set layout parameters for ALV grid
    "GS_LAYOUT-GRID_TITLE = TEXT-100.
    gs_layout-sel_mode   = 'A'.
    gs_layout-info_fname = 'LINE_COLOR'.
    gs_layout-stylefname = 'STYLE'.
    gs_layout-ctab_fname = 'COLOR_CELL'.
    gs_layout-zebra      = abap_false.

    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = ctl_alv.

    SET HANDLER obg_toolbar->on_toolbar FOR ctl_alv.
    SET HANDLER obg_toolbar->handle_user_command FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
        it_except_qinfo      = it_except_qinfo
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_retorno_alv[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv.

    IF qt_rows_splitter EQ 2 .

      ctl_cccontainer2 = dg_splitter->get_container( row = 2 column = 1 ).

      CREATE OBJECT dg_splitter_2
        EXPORTING
          parent  = ctl_cccontainer2
          rows    = 1
          columns = 2.

      dg_splitter_2->set_column_width( id = 2 width = 30 ).

      ctl_cccontainer2a = dg_splitter_2->get_container( row = 1 column = 1 ).
      ctl_cccontainer2b = dg_splitter_2->get_container( row = 1 column = 2 ).

      "ALV ROMANEIOS DE ENTRADA
      CREATE OBJECT ctl_alv2
        EXPORTING
          i_parent = ctl_cccontainer2a.

      PERFORM fill_it_fieldcatalog_2.
      PERFORM fill_it_hints_2.
      PERFORM fill_gs_variant_2.

      CONCATENATE 'Ticket:'  wa_carga_romaneio-nr_ticket
                  'Placa:'   wa_carga_romaneio-ds_placa_trator
                  'Produto:' wa_carga_romaneio-ds_produto
             INTO gs_layout_2-grid_title SEPARATED BY space.
      gs_layout_2-sel_mode   = 'A'.
      gs_layout_2-info_fname = 'ROWCOLOR'.
      gs_layout_2-stylefname = 'STYLE'.
      gs_layout_2-ctab_fname = 'COLOR_CELL'.
      gs_layout_2-zebra      = abap_false.

      CREATE OBJECT obg_toolbar_2
        EXPORTING
          io_alv_grid = ctl_alv2.

      SET HANDLER obg_toolbar_2->on_toolbar FOR ctl_alv2.
      SET HANDLER obg_toolbar_2->handle_user_command FOR ctl_alv2.

      CALL METHOD ctl_alv2->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout_2
          is_variant      = gs_variant_2
          i_save          = 'A'
        CHANGING
          it_fieldcatalog = it_fieldcatalog_2
          it_outtab       = it_romaneio_entrada[].

      DATA: p_inutil TYPE REF TO cl_gui_docking_container.

      PERFORM cria_alv_documentos USING 1 ctl_cccontainer2b p_inutil.

      PERFORM atualiza_tree USING wa_carga_romaneio-id_carga.

    ENDIF.

  ENDIF.

  IF ( gb_st_carga_est = 'W' OR gb_st_carga = 'W' ) AND ( obg_toolbar_2 IS NOT INITIAL AND  ctl_alv2 IS NOT INITIAL ).
    ctl_alv2->refresh_table_display( ).
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200_exit INPUT.

  PERFORM limpa_tela_0200.
  LEAVE TO SCREEN 0.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_tela_0200 .

  PERFORM time_estorno USING abap_false.
  PERFORM time USING abap_false.

  IF tree IS NOT INITIAL.
    tree->free( ).
  ENDIF.
  CLEAR: tree.

  CLEAR: g_application, obg_toolbar_2, event_handler.

  IF ctl_alv2 IS NOT INITIAL.
    ctl_alv2->free( ).
  ENDIF.
  CLEAR: ctl_alv2.

  IF ctl_cccontainer2b IS NOT INITIAL.
    ctl_cccontainer2b->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer2b.

  IF ctl_cccontainer2a IS NOT INITIAL.
    ctl_cccontainer2a->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer2a.

  IF dg_splitter_2 IS NOT INITIAL.
    dg_splitter_2->free( ).
  ENDIF.
  CLEAR: dg_splitter_2.

  IF ctl_cccontainer2 IS NOT INITIAL.
    ctl_cccontainer2->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer2.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  CLEAR: obg_toolbar.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001CG_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  wa_fieldcatalog-fieldname = 'ICO_CARGA'.
  wa_fieldcatalog-datatype  = 'CHAR'.
  wa_fieldcatalog-inttype   = 'C'.
  wa_fieldcatalog-intlen    = '000004'.
  wa_fieldcatalog-lowercase = 'X'.
  wa_fieldcatalog-domname   = 'CHAR04'.
  wa_fieldcatalog-scrtext_l = text-005.
  wa_fieldcatalog-scrtext_m = text-005.
  wa_fieldcatalog-scrtext_s = text-005.
  APPEND wa_fieldcatalog TO it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.
    <fs_cat>-tabname = 'ZDE_ZSDT0001CG_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'ICO_CARGA'.
        <fs_cat>-hotspot = abap_true.
        <fs_cat>-icon    = abap_true.
        <fs_cat>-just    = 'C'.
        <fs_cat>-col_pos = 1.
    ENDCASE.
*
    IF <fs_cat>-fieldname <> 'ICO_CARGA'.
      <fs_cat>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_hints .

  DATA: it_dd07v        TYPE TABLE OF dd07v WITH HEADER LINE,
        wa_except_qinfo LIKE LINE OF it_except_qinfo,
        lc_tp_status    TYPE zde_status_carga,
        lc_ico_carga    TYPE char04.
*
  CLEAR: it_except_qinfo[].
*
  "Informações Documento
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZDM_STATUS_CARGA'
    TABLES
      values_tab = it_dd07v.

  LOOP AT it_dd07v WHERE domvalue_l IS NOT INITIAL.
    wa_except_qinfo-type  = cl_salv_tooltip=>c_type_symbol.
    lc_tp_status = CONV #( it_dd07v-domvalue_l ).
    PERFORM seta_icone_status USING lc_tp_status CHANGING lc_ico_carga.
    wa_except_qinfo-value = lc_ico_carga.
    wa_except_qinfo-text  = it_dd07v-ddtext.
    wa_except_qinfo-tabname   = 'ZDE_ZSDT0001CG_ALV'.
    wa_except_qinfo-fieldname = 'ICO_CARGA'.
    APPEND wa_except_qinfo TO it_except_qinfo.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0200'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info INPUT.

  IF ctl_alv IS NOT INITIAL.
    CALL METHOD ctl_alv->get_scroll_info_via_id
      IMPORTING
        es_col_info = gs_scroll_col
        es_row_no   = gs_scroll_row.
  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.
    CALL METHOD ctl_alv2->get_scroll_info_via_id
      IMPORTING
        es_col_info = gs_scroll_col_2
        es_row_no   = gs_scroll_row_2.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  IF ctl_alv IS NOT INITIAL.
    CLEAR it_selected_rows.
    CALL METHOD ctl_alv->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    CLEAR: it_retorno_alv_sel[], it_retorno_alv_sel.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_retorno_alv INTO DATA(wa_retorno) INDEX wa_selected_rows-index.
      APPEND wa_retorno TO it_retorno_alv_sel.
    ENDLOOP.
  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.
    CLEAR it_selected_rows.
    CALL METHOD ctl_alv2->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    CLEAR: it_romaneio_entrada_sel[], it_romaneio_entrada_sel.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_romaneio_entrada INTO DATA(wa_romaneio) INDEX wa_selected_rows-index.
      APPEND wa_romaneio TO it_romaneio_entrada_sel.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela USING ck_ajusta_titulo TYPE char01.

  DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->get_rows( IMPORTING result = DATA(qtd_linhas) ).
    IF qtd_linhas NE qt_rows_splitter.
      PERFORM limpa_tela_0200.
      EXIT.
    ENDIF.
  ENDIF.

  IF ctl_alv IS NOT INITIAL.
    gs_alv_refres_cond-row = abap_true.
    gs_alv_refres_cond-col = abap_true.

    CALL METHOD ctl_alv->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.
  ENDIF.

  IF ctl_alv2 IS NOT INITIAL.

    gs_alv_refres_cond-row = abap_true.
    gs_alv_refres_cond-col = abap_true.

    CALL METHOD ctl_alv2->refresh_table_display
      EXPORTING
        is_stable      = gs_alv_refres_cond
        i_soft_refresh = abap_true.

    IF ck_ajusta_titulo EQ abap_true.
      CONCATENATE 'Ticket:'  wa_carga_romaneio-nr_ticket
                  'Placa:'   wa_carga_romaneio-ds_placa_trator
                  'Produto:' wa_carga_romaneio-ds_produto
             INTO gs_layout_2-grid_title SEPARATED BY space.

      gs_layout_2-sel_mode   = 'A'.
      gs_layout_2-info_fname = 'ROWCOLOR'.
      gs_layout_2-stylefname = 'STYLE'.
      gs_layout_2-ctab_fname = 'COLOR_CELL'.
      gs_layout_2-zebra      = abap_false.

      ctl_alv2->set_frontend_layout( is_layout = gs_layout_2 ).
    ENDIF.

  ENDIF.

  PERFORM atualiza_tree USING wa_carga_romaneio-id_carga.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Form  CARREGA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_saida .

  LOOP AT it_retorno INTO DATA(wa_retorno).
    READ TABLE it_retorno_alv ASSIGNING FIELD-SYMBOL(<fs_retorno_alv>) WITH KEY id_carga = wa_retorno-id_carga.
    IF <fs_retorno_alv> IS ASSIGNED.
      MOVE-CORRESPONDING wa_retorno TO <fs_retorno_alv>.
      PERFORM seta_icone_status USING <fs_retorno_alv>-tp_status CHANGING <fs_retorno_alv>-ico_carga.
    ELSE.
      MOVE-CORRESPONDING wa_retorno TO it_retorno_alv.
      PERFORM seta_icone_status USING it_retorno_alv-tp_status CHANGING it_retorno_alv-ico_carga.
      APPEND it_retorno_alv.
    ENDIF.
  ENDLOOP.

ENDFORM.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS lcl_application IMPLEMENTATION.
  METHOD handle_link_click.
    PERFORM  mostra_info_node_item USING node_key item_name.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM mostra_info_node_click USING node_key.
  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_retorno_alv INDEX row_id ASSIGNING FIELD-SYMBOL(<fs_retorno>).

  CHECK ( <fs_retorno> IS ASSIGNED ).

  CASE fieldname.
    WHEN 'ICO_CARGA'.
      PERFORM mostrar_carga USING <fs_retorno>-id_carga CHANGING <fs_retorno>.

      IF ck_mostrar_entrada EQ abap_true.
        PERFORM carregar_entrada USING <fs_retorno>.
        qt_rows_splitter  = 2.
      ENDIF.

      PERFORM atualiza_tela USING abap_true.
      LEAVE TO SCREEN 0200.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  CHECK p_row-index IS NOT INITIAL.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_retorno_alv INDEX p_row-index ASSIGNING FIELD-SYMBOL(<fs_retorno>).
    CHECK ( <fs_retorno> IS ASSIGNED ).

    ck_mostrar_entrada = abap_true.
    PERFORM carregar_entrada USING <fs_retorno>.
    qt_rows_splitter   = 2.
    PERFORM atualiza_tela USING abap_true.
    LEAVE TO SCREEN 0200.

  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    "Marcar Todos os Documentos
*    TY_TOOLBAR-ICON      = ICON_USED_RELATION.
*    TY_TOOLBAR-FUNCTION  = 'ENTRADA'.
*    TY_TOOLBAR-QUICKINFO = TEXT-006.
*    TY_TOOLBAR-TEXT      = TEXT-006.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    "Editar Nota Fiscal
    ty_toolbar-icon      = icon_mapped_relation.
    ty_toolbar-function  = 'SAIDA'.
    ty_toolbar-quickinfo = text-007.
    ty_toolbar-text      = text-007.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

    "042  Nova Carga Granel
    "043  Nova Carga de Algodão

    ty_toolbar-icon      = icon_import_all_requests.
    ty_toolbar-function  = 'GRANEL'.
    ty_toolbar-quickinfo = text-042.
    ty_toolbar-text      = text-042.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

    ty_toolbar-icon      = icon_bw_info_cube_ina.
    ty_toolbar-function  = 'ALGODAO'.
    ty_toolbar-quickinfo = text-043.
    ty_toolbar-text      = text-043.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.


*    CASE PTIPCA.
*      WHEN ZIF_CARGA=>ST_TP_CARGA_ENTRADA_FOB.
*
*        TY_TOOLBAR-ICON      = ICON_SUBMIT.
*        TY_TOOLBAR-FUNCTION  = 'AUTOMATICO'.
*        TY_TOOLBAR-QUICKINFO = TEXT-037.
*        TY_TOOLBAR-TEXT      = TEXT-036.
*        TY_TOOLBAR-BUTN_TYPE = 0.
*        APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*        CLEAR: TY_TOOLBAR.
*
*      WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_OPUS.
*      WHEN ZIF_CARGA=>ST_TP_CARGA_SAIDA_ENT_FOB.
*
*        TY_TOOLBAR-ICON      = ICON_SUBMIT.
*        TY_TOOLBAR-FUNCTION  = 'AUTOMATICO'.
*        TY_TOOLBAR-QUICKINFO = TEXT-037.
*        TY_TOOLBAR-TEXT      = TEXT-036.
*        TY_TOOLBAR-BUTN_TYPE = 0.
*        APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*        CLEAR: TY_TOOLBAR.
*
*    ENDCASE.

    CALL METHOD obj_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.

    CALL METHOD ctl_alv->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    CLEAR: it_retorno_alv_sel[].

    IF et_index_rows[] IS INITIAL.
      READ TABLE it_retorno_alv INTO DATA(wa_retorno_alv) INDEX 1.
      CHECK sy-subrc IS INITIAL.
      APPEND wa_retorno_alv TO it_retorno_alv_sel.
    ELSE.
      LOOP AT et_index_rows INTO DATA(wa_index_rows).
        READ TABLE it_retorno_alv INTO wa_retorno_alv INDEX wa_index_rows-index.
        APPEND wa_retorno_alv TO it_retorno_alv_sel.
      ENDLOOP.
    ENDIF.

    CASE e_ucomm.
      WHEN 'GRANEL'.
*-CS2022000332-#78064-07.06.2022-JT-inicio
        TRY.
            zcl_carga_recebimento_v0001=>zif_carga~get_instance(
              )->set_validar_safra( EXPORTING i_safra = psafra
                                              i_acao  = cl_myevent_handler=>st_action_grao
              ).
          CATCH zcx_carga INTO ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            EXIT.
        ENDTRY.
*-CS2022000332-#78064-07.06.2022-JT-fim

        SUBMIT zmmr126_0001 WITH ptipca   EQ ptipca
                            WITH pck_cad  EQ abap_true
                            WITH psafra   EQ psafra
                            WITH pempre   EQ pempre
                            WITH pfilia   EQ pfilia AND RETURN.

        PERFORM atualiza_tela_0200.

      WHEN 'ALGODAO'.
*-CS2022000332-#78064-07.06.2022-JT-inicio
        TRY.
            zcl_carga_recebimento_v0001=>zif_carga~get_instance(
              )->set_validar_safra( EXPORTING i_safra = psafra
                                              i_acao  = cl_myevent_handler=>st_action_algo
              ).
          CATCH zcx_carga INTO ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            EXIT.
        ENDTRY.
*-CS2022000332-#78064-07.06.2022-JT-fim

        SUBMIT zmmr153 WITH ptipca   EQ ptipca
                       WITH pck_cad  EQ abap_true
                       WITH psafra   EQ psafra
                       WITH pempre   EQ pempre
                       WITH pfilia   EQ pfilia AND RETURN.

        PERFORM atualiza_tela_0200.

      WHEN 'ENTRADA'.

        PERFORM limpa_tela_0200.
        IF it_retorno_alv_sel[] IS INITIAL.
          ck_mostrar_entrada = abap_false.
          qt_rows_splitter   = 1.
        ELSE.
          ck_mostrar_entrada = abap_true.
          READ TABLE it_retorno_alv_sel INDEX 1 INTO DATA(wa_retorno_alv_sel).

          LOOP AT it_retorno_alv ASSIGNING FIELD-SYMBOL(<fs_retorno>).
            CLEAR: <fs_retorno>-line_color.
          ENDLOOP.

          READ TABLE it_retorno_alv ASSIGNING <fs_retorno> WITH KEY id_carga = wa_retorno_alv_sel-id_carga.
          IF <fs_retorno> IS ASSIGNED.
            <fs_retorno>-line_color = cs_line_color_selecionada.
          ENDIF.

          PERFORM carregar_entrada USING wa_retorno_alv_sel.
          qt_rows_splitter   = 2.
        ENDIF.
        LEAVE TO SCREEN 0200.

      WHEN 'SAIDA'.

        PERFORM chamar_saida USING wa_retorno_alv.

      WHEN 'AUTOMATICO'.
        CASE ptipca.
          WHEN zif_carga=>st_tp_carga_entrada_fob.
            PERFORM chamar_saida_automatica USING wa_retorno_alv abap_true.
          WHEN zif_carga=>st_tp_carga_saida_opus.
          WHEN zif_carga=>st_tp_carga_saida_ent_fob.
            PERFORM chamar_saida_automatica USING wa_retorno_alv abap_false.
        ENDCASE.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CARREGAR_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV_SEL  text
*----------------------------------------------------------------------*
FORM carregar_entrada USING p_carga TYPE ty_itens_alv.

  DATA: obj_carga TYPE REF TO zif_carga.

  CLEAR: it_romaneio_entrada[]    , it_romaneio_entrada,
         it_romaneio_entrada_sel[], it_romaneio_entrada_sel.

  obj_carga =
  zcl_factory_carga=>zif_factory_carga~get_instance(
    )->set_factory_objeto( EXPORTING i_tp_carga = p_carga-tp_carga i_tp_produto = p_carga-tp_produto_carga
    )->get_factory_objeto(
    ).

  IF gb_id_carga NE p_carga-id_carga.
    CLEAR: gb_id_carga, gb_st_carga.
    PERFORM time USING abap_false.
  ENDIF.

  IF gb_id_carga_est NE p_carga-id_carga.
    CLEAR: gb_id_carga_est, gb_st_carga_est.
    PERFORM time_estorno USING abap_false.
  ENDIF.

  TRY .
      obj_carga->get_romaneio_entrada( EXPORTING i_id_carga  = p_carga-id_carga IMPORTING e_romaneios = DATA(romaneios_entrada) ).
      obj_carga->get_romaneio_saida( EXPORTING i_id_carga  = p_carga-id_carga IMPORTING e_romaneios = DATA(romaneios_saida) ).
    CATCH zcx_carga.
  ENDTRY.

  LOOP AT romaneios_entrada INTO DATA(wa_entrada).
    APPEND wa_entrada TO it_romaneio_entrada.
  ENDLOOP.

  LOOP AT romaneios_saida INTO wa_entrada.
    APPEND wa_entrada TO it_romaneio_entrada.
  ENDLOOP.

  wa_carga_romaneio = p_carga.

  LOOP AT it_retorno_alv ASSIGNING FIELD-SYMBOL(<fs_retorno>).
    CLEAR: <fs_retorno>-line_color.
  ENDLOOP.

  READ TABLE it_retorno_alv WITH KEY id_carga = p_carga-id_carga ASSIGNING <fs_retorno>.
  IF <fs_retorno> IS ASSIGNED.
    <fs_retorno>-line_color = cs_line_color_selecionada.
  ENDIF.

  CLEAR: obj_carga.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_2 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_2[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0001'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_HINTS_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_hints_2 .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_2 .

  gs_variant_2-report      = sy-repid.
  gs_variant_2-handle      = '0201'.
  gs_variant_2-log_group   = abap_false.
  gs_variant_2-username    = abap_false.
  gs_variant_2-variant     = abap_false.
  gs_variant_2-text        = abap_false.
  gs_variant_2-dependvars  = abap_false.

ENDFORM.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_2 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager_2
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Atualizar
    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'ATUALIZAR'.
    ty_toolbar-quickinfo = text-011.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

    "Mensagens de Interface
    ty_toolbar-icon      = icon_display_note.
    ty_toolbar-function  = 'MSGINTERF'.
    ty_toolbar-quickinfo = text-013.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR: ty_toolbar.

    "Desvincular Romaneio
    AUTHORITY-CHECK OBJECT 'ZMM0127_V2' ID 'ZACAO_0001' FIELD '01'. "Desvincular romaneio Carga

    IF sy-subrc IS INITIAL.

      ty_toolbar-icon      = icon_cut_relation.
      ty_toolbar-function  = 'DESVROMANEIO'.
      ty_toolbar-quickinfo = 'Desvincular Romaneio'.
      ty_toolbar-text = 'Desvincular Romaneio'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR: ty_toolbar.

    ENDIF.

    CALL METHOD obj_toolbarmanager_2->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: lc_st_carga     TYPE char01,
          lc_st_carga_est TYPE char01.

    DATA: et_index_rows	TYPE lvc_t_row.

    CALL METHOD ctl_alv2->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    CLEAR: it_romaneio_entrada_sel[].

    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE it_romaneio_entrada INTO DATA(wa_romaneio) INDEX wa_index_rows-index.
      APPEND wa_romaneio TO it_romaneio_entrada_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'MSGINTERF'.
        PERFORM mostra_msg_interface USING wa_carga_romaneio-id_carga.
      WHEN 'GESTOQUE'.
        IF gb_st_carga EQ 'E'.
          PERFORM mostra_msg_interface USING wa_carga_romaneio-id_carga.
          CLEAR: gb_st_carga.
        ELSE.

          lc_st_carga = gb_st_carga.

          CLEAR: gb_st_carga.
          READ TABLE it_retorno_alv WITH KEY id_carga = wa_carga_romaneio-id_carga ASSIGNING FIELD-SYMBOL(<fs_carga_entrada>).
          PERFORM gerar_entrada_estoque USING wa_carga_romaneio-id_carga CHANGING <fs_carga_entrada>.
          PERFORM carregar_entrada USING wa_carga_romaneio.
          PERFORM atualiza_tela USING abap_false.

          IF lc_st_carga IS INITIAL.
            LEAVE TO SCREEN 0200.
          ELSE.
            CLEAR: gb_st_carga.
          ENDIF.

        ENDIF.
      WHEN 'ATUALIZAR'.
        PERFORM carregar_entrada USING wa_carga_romaneio.
        PERFORM atualiza_tela USING abap_false.
        LEAVE TO SCREEN 0200.
      WHEN 'GESTORNA'.
        IF gb_st_carga_est EQ 'E'.
          PERFORM mostra_msg_interface USING wa_carga_romaneio-id_carga.
          CLEAR: gb_st_carga_est.
        ELSE.

          lc_st_carga_est = gb_st_carga_est.

          READ TABLE it_retorno_alv WITH KEY id_carga = wa_carga_romaneio-id_carga ASSIGNING <fs_carga_entrada>.
          PERFORM estornar_entrada_estoque USING wa_carga_romaneio-id_carga CHANGING <fs_carga_entrada>.
          PERFORM carregar_entrada USING wa_carga_romaneio.
          PERFORM atualiza_tela USING abap_false.
          IF lc_st_carga_est IS INITIAL.
            LEAVE TO SCREEN 0200.
          ELSE.
            CLEAR: gb_st_carga_est.
          ENDIF.

        ENDIF.
      WHEN 'DESVROMANEIO'.
        PERFORM f_desvincular_romaneio USING wa_romaneio.
    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV  text
*----------------------------------------------------------------------*
FORM chamar_saida  USING  p_retorno_alv TYPE ty_itens_alv.

  DATA: rg_datas     TYPE RANGE OF zsdt0001-dt_movimento,
        rg_chaves    TYPE RANGE OF zsdt0001-ch_referencia,
        wa_chaves    LIKE LINE OF rg_chaves,
        e_zsdt0001cg TYPE zsdt0001cg,
        lc_romaneios TYPE zde_zsdt0001_t.

  TRY .
      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto_id( i_id_carga = p_retorno_alv-id_carga
        )->get_factory_objeto(
        )->get_cabecalho_carga( EXPORTING i_id_carga = p_retorno_alv-id_carga IMPORTING e_zsdt0001cg = e_zsdt0001cg
        )->get_romaneio_saida( EXPORTING i_id_carga  = p_retorno_alv-id_carga IMPORTING e_romaneios = lc_romaneios
        ).

    CATCH zcx_carga.
  ENDTRY.

  IF lc_romaneios[] IS INITIAL.
    EXIT.
  ENDIF.

  LOOP AT lc_romaneios INTO DATA(wa_romaneio).
    wa_chaves-sign   = 'I'.
    wa_chaves-option = 'EQ'.
    wa_chaves-low    = wa_romaneio-ch_referencia.
    wa_chaves-high   = wa_romaneio-ch_referencia.
    APPEND wa_chaves TO rg_chaves.
  ENDLOOP.

  CASE e_zsdt0001cg-tp_carga.
    WHEN zif_carga=>st_tp_carga_entrada_fob OR space.

      "CASE E_ZSDT0001CG-IN_TRANSFERENCIA.
      "  WHEN ABAP_FALSE.
      SUBMIT zlesr0102 WITH p_bukrs  EQ wa_romaneio-bukrs
                       WITH p_branch EQ wa_romaneio-branch
                       WITH s_data   IN rg_datas
                       WITH r_cp_01  EQ abap_false
                       WITH r_cp_09  EQ abap_true
                       WITH r_dt_a   EQ abap_false
                       WITH r_dt_t   EQ abap_true
                       WITH s_chave  IN rg_chaves
                       WITH p_inter  EQ zcl_romaneio=>interface_carga_sap
                       AND RETURN.
      " WHEN ABAP_TRUE.
      "   SUBMIT ZLESR0086 WITH P_BUKRS  EQ WA_ROMANEIO-BUKRS
      "                    WITH P_BRANCH EQ WA_ROMANEIO-BRANCH
      "                    WITH S_CHAVE  IN RG_CHAVES
      "                    WITH P_INTER  EQ ZCL_ROMANEIO=>INTERFACE_CARGA_SAP
      "                    AND RETURN.
      "ENDCASE.

    WHEN zif_carga=>st_tp_carga_saida_opus.

      SUBMIT zlesr0102 WITH p_bukrs  EQ wa_romaneio-bukrs
                       WITH p_branch EQ wa_romaneio-branch
                       WITH s_data   IN rg_datas
                       WITH r_cp_01  EQ abap_true
                       WITH r_cp_09  EQ abap_false
                       WITH r_dt_a   EQ abap_false
                       WITH r_dt_t   EQ abap_true
                       WITH s_chave  IN rg_chaves
                       WITH p_inter  EQ zcl_romaneio=>interface_ib_romaneio
                       AND RETURN.

    WHEN zif_carga=>st_tp_carga_saida_ent_fob.

      SUBMIT zlesr0102 WITH p_bukrs  EQ wa_romaneio-bukrs
                       WITH p_branch EQ wa_romaneio-branch
                       WITH s_data   IN rg_datas
                       WITH r_cp_01  EQ abap_true
                       WITH r_cp_09  EQ abap_false
                       WITH r_dt_a   EQ abap_false
                       WITH r_dt_t   EQ abap_true
                       WITH s_chave  IN rg_chaves
                       WITH p_inter  EQ zcl_romaneio=>interface_carga_sap
                       AND RETURN.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_INFO_NODE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*----------------------------------------------------------------------*
FORM mostra_info_node_click USING p_node_key TYPE  tv_nodekey.

  DATA: p_fiscal TYPE j_1bdocnum.

  READ TABLE it_tree_info INTO DATA(wa_info_tre)
    WITH KEY node_key = p_node_key item_name = c_tree-column1 BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    CASE wa_info_tre-tipo.

        "Frete de Entrada
      WHEN 'FAV'. "Avido de Recebimento
      WHEN 'FDT'. "Documento de Transporte
      WHEN 'FP1'. "Pedágio REPOM
      WHEN 'FDC'. "Documento de Custo
      WHEN 'FOT'. "Ordem de Venda Frete
      WHEN 'FFT'. "Fatura do Frete
      WHEN 'FFF'. "Documento Fiscal Frete
        p_fiscal = CONV #( wa_info_tre-doc_part_1 ).
        PERFORM mostrar_monitor_eletronico USING p_fiscal.
        PERFORM carregar_entrada USING wa_carga_romaneio.
        PERFORM atualiza_tela USING abap_false.
        LEAVE TO SCREEN 0200.

        "Entrada
      WHEN 'EPO'. "Pedido de Compra
      WHEN 'EAV'. "Avido de Recebimento
      WHEN 'EDM'. "Documento de Material
      WHEN 'EFT'. "Documento de Faturamento
      WHEN 'EFS'. "Documento Fiscal
        p_fiscal = CONV #( wa_info_tre-doc_part_1 ).
        PERFORM mostrar_monitor_eletronico USING p_fiscal.
        PERFORM carregar_entrada USING wa_carga_romaneio.
        PERFORM atualiza_tela USING abap_false.
        LEAVE TO SCREEN 0200.

        "Saída
      WHEN 'SOV'. "Ordem de Venda Mercadoria
      WHEN 'SRM'. "Remessa
      WHEN 'SVF'. "Fatura Da Mercadoria
      WHEN 'SFS'. "Documento Fiscal Mercadoria
        p_fiscal = CONV #( wa_info_tre-doc_part_1 ).
        PERFORM mostrar_monitor_eletronico USING p_fiscal.
        PERFORM carregar_entrada USING wa_carga_romaneio.
        PERFORM atualiza_tela USING abap_false.
        LEAVE TO SCREEN 0200.
      WHEN 'SDT'. "Documento de Transporte
      WHEN 'SP1'. "Pedágio REPOM
      WHEN 'SDC'. "Documento de Custo
      WHEN 'SOT'. "Ordem de Venda Frete
      WHEN 'SFT'. "Fatura do Frete
      WHEN 'SFF'. "Documento Fiscal Frete
        p_fiscal = CONV #( wa_info_tre-doc_part_1 ).
        PERFORM mostrar_monitor_eletronico USING p_fiscal.
        PERFORM carregar_entrada USING wa_carga_romaneio.
        PERFORM atualiza_tela USING abap_false.
        LEAVE TO SCREEN 0200.
    ENDCASE.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN 'REFRESH_EN'.
      CLEAR: ok_code.
      obg_toolbar_2->handle_user_command( EXPORTING e_ucomm = 'GESTOQUE' ).
    WHEN 'REFRESH_ES'.
      CLEAR: ok_code.
      obg_toolbar_2->handle_user_command( EXPORTING e_ucomm = 'GESTORNA' ).
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CL_GUI_CONTAINER  text
*----------------------------------------------------------------------*
FORM cria_alv_documentos  USING  p_numero    TYPE i
                                 p_container TYPE REF TO cl_gui_container
                                 p_docking   TYPE REF TO cl_gui_docking_container.

  "ALV DOCUMENTOS DO FLUXO
  CLEAR: events.

  " link click
  event-eventid    = cl_gui_column_tree=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " link click
  event-eventid    = cl_gui_column_tree=>eventid_node_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  CREATE OBJECT g_application.
  hierarchy_header-heading = text-010.
  hierarchy_header-width   = 44.

  CASE p_numero.
    WHEN 1.
      CREATE OBJECT tree
        EXPORTING
          parent                = p_container
          node_selection_mode   = tree->node_sel_mode_single
          item_selection        = 'X'
          hierarchy_column_name = c_tree-column1
          hierarchy_header      = hierarchy_header.
    WHEN 2.
      CREATE OBJECT tree
        EXPORTING
          parent                = p_docking
          node_selection_mode   = tree->node_sel_mode_single
          item_selection        = 'X'
          hierarchy_column_name = c_tree-column1
          hierarchy_header      = hierarchy_header.
  ENDCASE.

  tree->set_registered_events( EXPORTING events = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3
      OTHERS                    = 4 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER g_application->handle_link_click FOR tree.
  SET HANDLER g_application->handle_double_click FOR tree.

  tree->add_column(
    EXPORTING
      name                         = c_tree-column2
      alignment                    = cl_gui_column_tree=>align_left
      width                        = 18
      header_image                 = 'ICON_PRINT'
      header_text                  = 'Imprimir'
    EXCEPTIONS
      column_exists                = 1
      illegal_column_name          = 2
      too_many_columns             = 3
      illegal_alignment            = 4
      different_column_types       = 5
      cntl_system_error            = 6
      failed                       = 7
      predecessor_column_not_found = 8
      OTHERS                       = 9 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  tree->add_column(
    EXPORTING
      name                         = c_tree-column3
      alignment                    = cl_gui_column_tree=>align_left
      width                        = 30
      header_image                 = 'ICON_PRINT'
      header_text                  = 'Descrição Documento'
    EXCEPTIONS
      column_exists                = 1
      illegal_column_name          = 2
      too_many_columns             = 3
      illegal_alignment            = 4
      different_column_types       = 5
      cntl_system_error            = 6
      failed                       = 7
      predecessor_column_not_found = 8
      OTHERS                       = 9 ).

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_SAIDA_AUTOMATICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RETORNO_ALV  text
*----------------------------------------------------------------------*
FORM chamar_saida_automatica  USING p_retorno_alv TYPE ty_itens_alv p_entrada TYPE char01.

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Gerar Faturamento em backgorund'
      text_question         = 'Deseja Gerar fatuamento em backgorund?'
      text_button_1         = 'Sim'
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'Não'
      icon_button_2         = 'ICON_INCOMPLETE'
      default_button        = '1'
      display_cancel_button = 'X'
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CASE answer.
    WHEN '1'.

      CASE p_entrada.
        WHEN abap_false.

          TRY .

              DATA: number           TYPE tbtcjob-jobcount,
                    name             TYPE tbtcjob-jobname,
                    print_parameters TYPE pri_params.

              DATA(lc_user_job) = zcl_job=>get_user_job( ).

              CONCATENATE 'JOB_SAIDA_AUTOMATICA' p_retorno_alv-id_carga INTO name SEPARATED BY '_'.

              TRY .
                  zcl_job=>get_job_execucao( i_job_name = name ).
                  EXIT.
                CATCH zcx_job.
              ENDTRY.

              TRY .
                  zcl_job=>get_job_escalonado( i_job_name = name ).
                  EXIT.
                CATCH zcx_job.
              ENDTRY.

              CALL FUNCTION 'JOB_OPEN'
                EXPORTING
                  jobname          = name
                IMPORTING
                  jobcount         = number
                EXCEPTIONS
                  cant_create_job  = 1
                  invalid_job_data = 2
                  jobname_missing  = 3
                  OTHERS           = 4.

              IF sy-subrc IS INITIAL.
                SUBMIT zmmr126_job TO SAP-SPOOL SPOOL PARAMETERS print_parameters
                WITHOUT SPOOL DYNPRO VIA JOB name NUMBER number
                  WITH pidcarga EQ p_retorno_alv-id_carga
                  USER lc_user_job
                   AND RETURN.

                IF sy-subrc IS INITIAL.
                  CALL FUNCTION 'JOB_CLOSE'
                    EXPORTING
                      jobcount             = number
                      jobname              = name
                      strtimmed            = 'X'
                    EXCEPTIONS
                      cant_start_immediate = 1
                      invalid_startdate    = 2
                      jobname_missing      = 3
                      job_close_failed     = 4
                      job_nosteps          = 5
                      job_notex            = 6
                      lock_failed          = 7
                      OTHERS               = 8.

                  IF sy-subrc IS NOT INITIAL.
                    DATA(ck_erro) = abap_true.
                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO DATA(mtext) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                    CALL FUNCTION 'BP_JOB_DELETE'
                      EXPORTING
                        jobcount                 = number
                        jobname                  = name
                      EXCEPTIONS
                        cant_delete_event_entry  = 1
                        cant_delete_job          = 2
                        cant_delete_joblog       = 3
                        cant_delete_steps        = 4
                        cant_delete_time_entry   = 5
                        cant_derelease_successor = 6
                        cant_enq_predecessor     = 7
                        cant_enq_successor       = 8
                        cant_enq_tbtco_entry     = 9
                        cant_update_predecessor  = 10
                        cant_update_successor    = 11
                        commit_failed            = 12
                        jobcount_missing         = 13
                        jobname_missing          = 14
                        job_does_not_exist       = 15
                        job_is_already_running   = 16
                        no_delete_authority      = 17
                        OTHERS                   = 18.
                    IF sy-subrc IS NOT INITIAL.
                      ck_erro = abap_false.
                    ENDIF.
                  ENDIF.
                ELSE.
                  ck_erro = abap_true.
                  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO mtext WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  CALL FUNCTION 'BP_JOB_DELETE'
                    EXPORTING
                      jobcount                 = number
                      jobname                  = name
                    EXCEPTIONS
                      cant_delete_event_entry  = 1
                      cant_delete_job          = 2
                      cant_delete_joblog       = 3
                      cant_delete_steps        = 4
                      cant_delete_time_entry   = 5
                      cant_derelease_successor = 6
                      cant_enq_predecessor     = 7
                      cant_enq_successor       = 8
                      cant_enq_tbtco_entry     = 9
                      cant_update_predecessor  = 10
                      cant_update_successor    = 11
                      commit_failed            = 12
                      jobcount_missing         = 13
                      jobname_missing          = 14
                      job_does_not_exist       = 15
                      job_is_already_running   = 16
                      no_delete_authority      = 17
                      OTHERS                   = 18.
                  IF sy-subrc IS NOT INITIAL.
                    ck_erro = abap_false.
                  ENDIF.
                ENDIF.
              ENDIF.
            CATCH zcx_job INTO ex_job.
              ex_job->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        WHEN abap_true.

          TRY .

              zcl_factory_carga=>zif_factory_carga~get_instance(
                )->set_factory_objeto_id( i_id_carga =  p_retorno_alv-id_carga
                )->get_factory_objeto(
                )->set_registro( EXPORTING i_id_carga = p_retorno_alv-id_carga
                )->set_emitir_doc_saidas(
                )->free(
                ).

            CATCH zcx_carga INTO ex_carga.
              ex_carga->published_erro( EXPORTING i_msgty = 'S'  i_msgty_display = 'E' ).
            CATCH zcx_job INTO ex_job.
              ex_job->published_erro( EXPORTING i_msgty = 'S'  i_msgty_display = 'E' ).
          ENDTRY.

      ENDCASE.

    WHEN '2'.

      CHECK p_entrada EQ abap_false.

      TRY .

          zcl_factory_carga=>zif_factory_carga~get_instance(
            )->set_factory_objeto( EXPORTING i_tp_carga = zif_carga=>st_tp_carga_saida_ent_fob i_tp_produto = p_retorno_alv-tp_produto_carga
            )->get_factory_objeto(
            )->set_registro( EXPORTING i_id_carga   = p_retorno_alv-id_carga
            )->set_emitir_doc_saidas(
            ).

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_tela_0200 .

  PERFORM limpa_tela_0200.

  ck_confer_carga = abap_false.
  qt_rows_splitter = 0.

  CLEAR: lc_filtro.
  lc_filtro-inrsafra = VALUE #( option = 'EQ' sign = 'I' ( low = psafra high = psafra ) ).
  lc_filtro-iidbukrs = VALUE #( option = 'EQ' sign = 'I' ( low = pempre high = pempre ) ).
  lc_filtro-iidbranc = VALUE #( option = 'EQ' sign = 'I' ( low = pfilia high = pfilia ) ).
  lc_filtro-idtabert = VALUE #( option = 'EQ' sign = 'I' ( low = sy-datlo high = sy-datlo ) ).

  TRY .
      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto( EXPORTING i_tp_carga = ptipca i_tp_produto = zif_carga=>st_tp_produto_carga_granel
        )->get_factory_objeto(
        )->pesquisar( EXPORTING i_filtros = lc_filtro IMPORTING e_registros = it_retorno
        ).
    CATCH zcx_carga.
  ENDTRY.

  PERFORM carrega_saida.

  LEAVE TO SCREEN 0200.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DESVINCULAR_ROMANEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CARGA_ROMANEIO  text
*----------------------------------------------------------------------*
FORM f_desvincular_romaneio  USING p_romaneio TYPE zsdt0001.

  DATA: wa_zsdt0001rd TYPE zsdt0001rd,
        l_answer      TYPE c.

  AUTHORITY-CHECK OBJECT 'ZMM0127_V2'
           ID 'ZACAO_0001' FIELD '01'.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE s000(z_mm) WITH 'Sem autorização para Desvincular romaneio!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "Permitido desvincular apenas Romaneio de Saída
  IF p_romaneio-tp_movimento <> 'S'.
    MESSAGE s000(z_mm) WITH 'Só é permitido esta ação para romaneios de Saida!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Cargas (Romaneios)
  SELECT SINGLE id_carga, tp_status FROM zsdt0001cg
    INTO @DATA(w_zsdt0001cg)
    WHERE id_carga = @p_romaneio-id_carga.

  IF w_zsdt0001cg-tp_status <> 'CO'.
    MESSAGE s000(z_mm) WITH 'Só é permitido desvincular carga conferida!'
    DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente desvincular o romaneio ?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_answer = 1.

  TRY .

      zcl_carga_recebimento_v0001=>zif_carga~get_instance(
        )->desvincular_romaneio_carga( EXPORTING i_id_carga = p_romaneio-id_carga i_ch_referencia = p_romaneio-ch_referencia
        ).

      " Verifica chave de referência
      SELECT * INTO TABLE @DATA(it_zsdt0001_r) FROM zsdt0001 WHERE ch_referencia EQ @p_romaneio-ch_referencia.

      IF sy-subrc IS INITIAL.

        READ TABLE it_zsdt0001_r ASSIGNING FIELD-SYMBOL(<fs_zsdt0001_r>)
                                                                   WITH KEY id_carga =  p_romaneio-id_carga.
        IF <fs_zsdt0001_r> IS ASSIGNED.

          "Grava tabela de Log de retirada do Romaneio
          CLEAR  wa_zsdt0001rd.
          wa_zsdt0001rd-id_carga      = <fs_zsdt0001_r>-id_carga.
          wa_zsdt0001rd-ch_referencia = <fs_zsdt0001_r>-ch_referencia.
          wa_zsdt0001rd-us_registro   = sy-uname.
          wa_zsdt0001rd-dt_registro   = sy-datum.
          wa_zsdt0001rd-hr_registro   = sy-uzeit.
          MODIFY zsdt0001rd FROM  wa_zsdt0001rd.

          "Limpa o ID CARGA e Atualiza tabela de referÊncia x romaneio
          CLEAR <fs_zsdt0001_r>-id_carga.
          MODIFY zsdt0001 FROM TABLE it_zsdt0001_r.
          REFRESH: it_zsdt0001_r.

          COMMIT WORK.

          CALL METHOD ctl_alv2->refresh_table_display.
          MESSAGE s000(z_mm) WITH 'Romaneio ' p_romaneio-id_carga  ' desvinculado com sucesso!'.

        ENDIF.

      ENDIF.

    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.
