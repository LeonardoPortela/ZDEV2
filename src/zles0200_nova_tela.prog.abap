*&---------------------------------------------------------------------*
*& Include          ZLES0200_NOVA_TELA
*&---------------------------------------------------------------------*

TABLES: zib_nfe_dist_ter,
        zib_nfe_dist_itm,
        zde_nfe_dist_alv,
        zib_nfe_dist_lot,
        zde_nfe_inbound_alv,
        zde_nfe_dist_itm_alv.

TYPES: BEGIN OF ty_info_forne.
TYPES: texto TYPE char50,   "Fornecedor
       bankl TYPE char03, "Banco
       banka TYPE banka,  "Nome do Banco
       bankn TYPE bankn,  "Conta Corrente
       agenc TYPE char15. "Agencia
TYPES: END OF ty_info_forne.

TYPES: BEGIN OF ty_itens_alv.
         INCLUDE STRUCTURE zde_nfe_dist_itm_alv.
TYPES:   maktx TYPE makt-maktx.
TYPES:   precos TYPE char04.
TYPES:   erro TYPE char01.
TYPES: line_color(4) TYPE c, "Used to store row color attributes
       color_cell    TYPE lvc_t_scol,  " Cell color
       style         TYPE lvc_t_styl,
       END OF ty_itens_alv.

TYPES: BEGIN OF ty_itens_lotes_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lot.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         indea(4),
       END OF ty_itens_lotes_alv.

TYPES: BEGIN OF ty_itens_carac_alv.
         INCLUDE STRUCTURE zib_nfe_dist_lca.
TYPES:   line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
       END OF ty_itens_carac_alv.

CLASS lcl_alv_toolbar_1603 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_1601 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor         IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

DATA: lc_frete_remessa_trans TYPE REF TO zcl_frete_remessa_trans,
      obg_toolbar_1601       TYPE REF TO lcl_alv_toolbar_1601,
      toolbarmanager_1601    TYPE REF TO cl_alv_grid_toolbar_manager,
      it_itens_alv_sel       TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      tl_tlines              LIKE tline OCCURS 0 WITH HEADER LINE,
      longtext_tab           TYPE zde_tdline_t,
      longtext               TYPE zde_tdline,
      editor                 TYPE REF TO cl_gui_textedit,
      container              TYPE REF TO cl_gui_custom_container,
      ex_nfe_inbound         TYPE REF TO zcx_nfe_inbound_exception,
      ex_pedido_compra       TYPE REF TO zcx_pedido_compra_exception,
      ex_cadastro            TYPE REF TO zcx_cadastro,
      ex_charg               TYPE REF TO zcx_charg_exception,
      ex_miro                TYPE REF TO zcx_miro_exception,
      ck_informado_preco     TYPE char01,
      ck_nrosol              TYPE zde_nro_sol, " RIM CS1029457 ANB 30.09.2022
      ck_seq                 TYPE numc3.

DATA: lbl_item_sel TYPE c LENGTH 259.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1601 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_data_changed_1601 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS handle_double_click_1601 FOR EVENT double_click   OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.

ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1603 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click_1603 FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
    METHODS handle_hotspot_click_1603 FOR EVENT  hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id.
ENDCLASS.

CLASS lcl_event_receiver_1602 DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data TYPE c.
    METHODS handle_data_changed_1602 FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
  PRIVATE SECTION.
    METHODS: perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.

DATA: event_receiver_1601 TYPE REF TO lcl_event_receiver_1601.
DATA: event_receiver_1602 TYPE REF TO lcl_event_receiver_1602.
DATA: event_receiver_1603 TYPE REF TO lcl_event_receiver_1603.

DATA: cl_grid_1601     TYPE REF TO cl_gui_alv_grid,
      container_1601   TYPE REF TO cl_gui_custom_container,
      it_function_1601 TYPE ui_functions,
      wa_layout_1601   TYPE lvc_s_layo,
      it_fieldcat_1601 TYPE lvc_t_fcat,
      wa_fieldcat_1601 TYPE lvc_s_fcat,
      it_sort_1601     TYPE lvc_t_sort,
      wa_variant_1601  TYPE disvariant,
      wa_stable_1601   TYPE lvc_s_stbl,
      it_f4_1601       TYPE lvc_t_f4,
      wa_f4_1601       TYPE lvc_s_f4.

DATA: cl_grid_1602     TYPE REF TO cl_gui_alv_grid,
      container_1602   TYPE REF TO cl_gui_custom_container,
      it_function_1602 TYPE ui_functions,
      wa_layout_1602   TYPE lvc_s_layo,
      it_fieldcat_1602 TYPE lvc_t_fcat,
      it_sort_1602     TYPE lvc_t_sort,
      wa_variant_1602  TYPE disvariant,
      wa_stable_1602   TYPE lvc_s_stbl,
      it_f4_1602       TYPE lvc_t_f4,
      wa_f4_1602       TYPE lvc_s_f4.

DATA: cl_grid_1603        TYPE REF TO cl_gui_alv_grid,
      container_1603      TYPE REF TO cl_gui_custom_container,
      it_function_1603    TYPE ui_functions,
      wa_layout_1603      TYPE lvc_s_layo,
      it_fieldcat_1603    TYPE lvc_t_fcat,
      it_sort_1603        TYPE lvc_t_sort,
      wa_variant_1603     TYPE disvariant,
      wa_stable_1603      TYPE lvc_s_stbl,
      it_f4_1603          TYPE lvc_t_f4,
      wa_f4_1603          TYPE lvc_s_f4,
      obg_toolbar_1603    TYPE REF TO lcl_alv_toolbar_1603,
      toolbarmanager_1603 TYPE REF TO cl_alv_grid_toolbar_manager,
      it_selected_1601    TYPE lvc_t_row,
      it_selected_1603    TYPE lvc_t_row,
      wa_selected_1601    TYPE lvc_s_row,
      wa_selected_1603    TYPE lvc_s_row,
      t_bdcdata           TYPE TABLE OF bdcdata.

CONSTANTS: ok_gravar    TYPE sy-ucomm VALUE 'GRAVAR',
           ok_cancelar  TYPE sy-ucomm VALUE 'CANCELAR',
           ok_aviso_act TYPE sy-ucomm VALUE 'AVISO_FIS'.

CONSTANTS: cs_line_color_selecionada TYPE c LENGTH 4 VALUE 'C710'.

DATA: wa_nfe_inbound        TYPE znfe_inbound,
      obj_departamento      TYPE REF TO zcl_mm_departamento,
      ck_alterou_lote       TYPE c LENGTH 1,
      ck_alterou_lote_info  TYPE c LENGTH 1,
      ck_consulta           TYPE c LENGTH 1,
      cl_informado_new_item TYPE c LENGTH 1,
      ck_gravado            TYPE c LENGTH 1,
      i_campo	              TYPE name_feld,
      gb_texto_obs          TYPE c LENGTH 40.

DATA: ck_alterou_transportador TYPE c LENGTH 1,
      it_ucomm                 TYPE TABLE OF sy-ucomm,
      wa_itens                 TYPE zib_nfe_dist_itm,
      it_itens                 TYPE zib_nfe_dist_itm_t,
      it_lotes                 TYPE zib_nfe_dist_lot_t,
      it_lotes_c               TYPE zib_nfe_dist_lca_t,
      it_itens_alv             TYPE TABLE OF ty_itens_alv WITH HEADER LINE,
      wa_itens_alv             TYPE ty_itens_alv,
      wa_itens_sel_lote        TYPE ty_itens_alv,
      wa_style                 TYPE lvc_s_styl,
      it_lotes_alv_t           TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_u           TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_lotes_alv_sel         TYPE TABLE OF ty_itens_lotes_alv WITH HEADER LINE,
      it_carac_alv             TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      it_carac_alv_u           TYPE TABLE OF ty_itens_carac_alv WITH HEADER LINE,
      lc_info_forne            TYPE ty_info_forne.

CONSTANTS: tl_tela_1501 TYPE sy-dynnr VALUE '1501',
           tl_tela_1502 TYPE sy-dynnr VALUE '1502',
           tl_tela_1503 TYPE sy-dynnr VALUE '1503',
           tl_tela_1504 TYPE sy-dynnr VALUE '1504',
           tl_tela_1601 TYPE sy-dynnr VALUE '1601',
           tl_tela_1602 TYPE sy-dynnr VALUE '1602'.

DATA: sb_tela_1501 TYPE sy-dynnr,
      sb_tela_1502 TYPE sy-dynnr,
      sb_tela_1503 TYPE sy-dynnr,
      sb_tela_1504 TYPE sy-dynnr,
      sb_tela_1600 TYPE sy-dynnr,
      sb_tela_1601 TYPE sy-dynnr.

DATA: lc_txt_butxt               TYPE t001-butxt,
      ck_alterou_ck_possui_frete TYPE char01.

DATA: lc_transportador_cnpj  TYPE zde_arma_cnpj,
      lc_transportador_ie    TYPE zde_arma_insc_es,
      lc_transportador_razao TYPE zde_arma_razao,
      lc_transportador_regio TYPE regio,
      lc_fornecedor_regio    TYPE regio,
      lc_destinatario_regio  TYPE regio,
      lc_armazem_regio       TYPE regio.

CLASS lcl_alv_toolbar_1603 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1603
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    DATA(ck_permissao) = abap_false.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = ck_permissao.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1603->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CLEAR: it_lotes_alv_sel, it_lotes_alv_sel[].

    CALL METHOD cl_grid_1603->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1603.

    LOOP AT it_selected_1603 INTO wa_selected_1603.
      READ TABLE it_lotes_alv_u INTO DATA(lc_lote_alv) INDEX wa_selected_1603-index.
      APPEND lc_lote_alv TO it_lotes_alv_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_lote.
      WHEN 'DEL'.
        PERFORM deletar_lote.
    ENDCASE.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

CLASS lcl_alv_toolbar_1601 IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT toolbarmanager_1601
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

    FREE: e_object->mt_toolbar.

*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = 'ADD'.
    ty_toolbar-quickinfo = TEXT-005.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = abap_off.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = 'DEL'.
    ty_toolbar-quickinfo = TEXT-006.
    ty_toolbar-butn_type = 0.
    ty_toolbar-disabled  = abap_off.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD toolbarmanager_1601->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: gs_alv_refres_cond TYPE lvc_s_stbl.

    gs_alv_refres_cond-row = abap_true.
    gs_alv_refres_cond-col = abap_true.

    CALL METHOD cl_grid_1601->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_1601.

    CASE e_ucomm.
      WHEN 'ADD'.
        PERFORM incluir_pedido.
      WHEN 'DEL'.
        PERFORM deletar_pedido.
    ENDCASE.

    PERFORM atualiza_tela_1602.
    LEAVE TO SCREEN 1600.

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1601 IMPLEMENTATION.
  METHOD handle_double_click_1601.
    PERFORM double_click_1601 USING e_row e_column es_row_no.
  ENDMETHOD.

  METHOD handle_data_changed_1601.
    PERFORM data_changed_1601 USING er_data_changed.
  ENDMETHOD.

ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver_1603 IMPLEMENTATION.
  METHOD handle_double_click_1603.
    PERFORM double_click_1603 USING e_row e_column es_row_no.
  ENDMETHOD.

  METHOD  handle_hotspot_click_1603.
    DATA line_id TYPE zmmt0102-line_id.
    READ TABLE it_lotes_alv_u INDEX e_row_id-index INTO DATA(wa_lotes_alv_u).
*    LINE_ID = SY-TABIX.
    line_id = wa_lotes_alv_u-prod_item.
    IF wa_lotes_alv_u-charg  = zib_nfe_dist_lot-charg.
      READ TABLE it_itens_alv ASSIGNING FIELD-SYMBOL(<fs_item>) WITH KEY chave_nfe = wa_lotes_alv_u-chave_nfe
                                                                         prod_item = wa_lotes_alv_u-prod_item.
      SELECT SINGLE *
        FROM zib_nfe_dist_ter
        INTO @DATA(wzib_nfe_dist_ter)
        WHERE chave_nfe = @wa_lotes_alv_u-chave_nfe.

      CALL FUNCTION 'Z_MM_INDEA_LOTE'
        EXPORTING
          i_ebeln   = <fs_item>-ebeln
          i_ebelp   = <fs_item>-ebelp
          i_matnr   = <fs_item>-matnr
          i_line_id = line_id
          i_mblnr   = wzib_nfe_dist_ter-mblnr
          i_mjahr   = wzib_nfe_dist_ter-mjahr
          i_charg   = wa_lotes_alv_u-charg
          i_menge   = wa_lotes_alv_u-menge
          i_btn     = 'X'
          i_tcode   = sy-tcode.
    ENDIF.
    "
  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver_1602 IMPLEMENTATION.
  METHOD handle_data_changed_1602.
    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data EQ abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ELSE.
      PERFORM data_changed_1602 USING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed.
    ENDIF.
  ENDMETHOD.

  METHOD perform_semantic_checks.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(ls_good) WHERE fieldname EQ 'ATWRT'.

      IF ls_good-value IS NOT INITIAL.
        READ TABLE it_carac_alv_u INTO DATA(wa_carac_alv_u) INDEX ls_good-row_id.
        TRY.
            zcl_charg=>valida_valor_caracteristica( i_class     = zib_nfe_dist_lot-class
                                                    i_classtype = zib_nfe_dist_lot-klart
                                                    i_atinn     = wa_carac_alv_u-atinn
                                                    i_atwrt     = CONV #( ls_good-value ) ).
          CATCH zcx_charg_exception INTO ex_charg.
            error_in_data = abap_true.
            CALL METHOD pr_data_changed->add_protocol_entry
              EXPORTING
                i_msgid     = ex_charg->if_t100_message~t100key-msgid
                i_msgno     = ex_charg->if_t100_message~t100key-msgno
                i_msgty     = ex_charg->msgty
                i_msgv1     = ex_charg->msgv1
                i_msgv2     = ex_charg->msgv2
                i_msgv3     = ex_charg->msgv3
                i_msgv4     = ex_charg->msgv4
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
        ENDTRY.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_VARIAVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpar_variaveis .
  CLEAR: ck_alterou_transportador,
         ck_alterou_lote,
         ck_alterou_lote_info,
         cl_informado_new_item,
         zib_nfe_dist_ter,
         zib_nfe_dist_lot,
         zde_nfe_inbound_alv,
         zde_nfe_dist_alv,
         zde_nfe_dist_itm_alv,
         it_itens[],
         it_itens,
         it_itens_alv[],
         it_itens_alv_sel,
         it_itens_alv_sel[],
         it_itens_alv,
         it_lotes_alv_t,
         it_lotes_alv_u,
         it_carac_alv,
         it_carac_alv_u,
         it_lotes_alv_t[],
         it_lotes_alv_u[],
         it_carac_alv[],
         it_carac_alv_u[],
         wa_nfe_inbound,
         wa_itens_sel_lote,
         lc_fornecedor_regio,
         lc_destinatario_regio ,
         lc_transportador_regio,
         lc_armazem_regio.

  CLEAR: event_receiver_1601.
  CLEAR: event_receiver_1602.
  CLEAR: event_receiver_1603.

  CLEAR: toolbarmanager_1603, obg_toolbar_1603.

  IF cl_grid_1602 IS NOT INITIAL.
    cl_grid_1602->free( ).
  ENDIF.
  CLEAR: cl_grid_1602.

  IF cl_grid_1603 IS NOT INITIAL.
    cl_grid_1603->free( ).
  ENDIF.
  CLEAR: cl_grid_1603.

  IF cl_grid_1601 IS NOT INITIAL.
    cl_grid_1601->free( ).
  ENDIF.
  CLEAR: cl_grid_1601.

  IF container_1603 IS NOT INITIAL.
    container_1603->free( ).
  ENDIF.
  CLEAR: container_1603.

  IF container_1602 IS NOT INITIAL.
    container_1602->free( ).
  ENDIF.
  CLEAR: container_1602.

  IF container_1601 IS NOT INITIAL.
    container_1601->free( ).
  ENDIF.
  CLEAR: container_1601.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_tela .

  wa_nfe_inbound = lc_frete_remessa_trans->get_info_nota( ).
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_base TO zib_nfe_dist_ter.
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_alv  TO zde_nfe_inbound_alv.
  MOVE-CORRESPONDING wa_nfe_inbound-nfe_alv  TO zde_nfe_dist_alv.
  MOVE wa_nfe_inbound-nfe_base-itens   TO it_itens.
  MOVE wa_nfe_inbound-nfe_base-lotes   TO it_lotes.
  MOVE wa_nfe_inbound-nfe_base-lotes_c TO it_lotes_c.

  READ TABLE it_itens INTO DATA(w_itens) INDEX 1.

  LOOP AT it_lotes INTO DATA(w_lotes).
    w_lotes-menge = COND #( WHEN sy-tabix = 1 THEN w_itens-menge
                                              ELSE 0 ).
    MODIFY it_lotes FROM w_lotes INDEX sy-tabix TRANSPORTING menge.
  ENDLOOP.

  LOOP AT it_lotes INTO DATA(wa_lotes).
    READ TABLE it_lotes_alv_t INTO DATA(w_lotet) WITH KEY chave_nfe    = wa_lotes-chave_nfe
                                                          cd_lote_item = wa_lotes-cd_lote_item.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING wa_lotes TO it_lotes_alv_t.
      APPEND it_lotes_alv_t.
    ENDIF.
  ENDLOOP.

  LOOP AT it_lotes_c INTO DATA(wa_lotes_c).
    MOVE-CORRESPONDING wa_lotes_c TO it_carac_alv.
    APPEND it_carac_alv.
  ENDLOOP.

ENDFORM.
