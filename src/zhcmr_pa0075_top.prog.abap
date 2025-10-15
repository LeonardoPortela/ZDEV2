*&---------------------------------------------------------------------*
*&  Include           ZHCMR_PA0075_TOP
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Tabelas                                                            *
*--------------------------------------------------------------------*
TABLES: pa0000, pa0001.

*--------------------------------------------------------------------*
* Pool de Tipos                                                      *
*--------------------------------------------------------------------*
TYPE-POOLS: abap, col, icon.

TYPES: BEGIN OF ty_saida,
         datint TYPE zmmt_intech_002-datint,
         horint TYPE zmmt_intech_002-horint,
         seqmsg TYPE zmmt_intech_002-seqmsg,
         tipcar TYPE zmmt_intech_002-tipcar,
         tcode  TYPE zmmt_intech_002-tcode,
         usuint TYPE zmmt_intech_002-usuint,
         tipmsg TYPE char11,
         status TYPE char16,
         solic  TYPE zmmt_intech_002-solic,
         matnr  TYPE zmmt_intech_002-matnr,
         txtmsg TYPE zmmt_intech_002-txtmsg,
       END OF ty_saida.

CONSTANTS: serv_cargo TYPE string VALUE 'asas'.
DATA: vl_servico TYPE string.
*--------------------------------------------------------------------*
* Tabelas internas                                                   *
*--------------------------------------------------------------------*
DATA:
  gt_saida    TYPE zcl_hcm_app_desenv=>tyt_saida,
  gt_saidaorg TYPE zcl_hcm_app_desenv=>tyt_saidaorg,
  gt_saidalv  TYPE zcl_hcm_app_desenv=>tyt_saidalv,
  gw_saidalv  TYPE zcl_hcm_app_desenv=>ty_saidalv,
  gt_saidax   TYPE zcl_hcm_app_desenv=>tyt_saida,
  gw_saida    TYPE zcl_hcm_app_desenv=>ty_saida,
  gv_error    TYPE c.

DATA: lo_oref   TYPE REF TO cx_sy_arithmetic_error,
      go_sap_pp TYPE REF TO zcl_hcm_app_desenv,
      gr_bukrs  TYPE RANGE OF bukrs,
      gr_kostl  TYPE RANGE OF kostl,
      gr_pernr  TYPE RANGE OF p_pernr.

*--------------------------------------------------------------------*
* Tipos                                                              *
*--------------------------------------------------------------------*
DATA: go_alv       TYPE REF TO cl_salv_table,
      go_func      TYPE REF TO cl_salv_table,
      go_functions TYPE REF TO cl_salv_functions_list,
      go_sort      TYPE REF TO cl_salv_sorts,
      go_display   TYPE REF TO cl_salv_display_settings,
      go_content   TYPE REF TO cl_salv_form_element,
      go_t_columns TYPE REF TO cl_salv_columns_table,
      go_events    TYPE REF TO cl_salv_events_table,
      go_columns   TYPE REF TO cl_salv_column.

*--------------------------------------------------------------------*
* Tabelas                                                            *
*--------------------------------------------------------------------*
DATA: gt_rows TYPE salv_t_row,
      gt_cols TYPE salv_t_column.

*--------------------------------------------------------------------*
* Estruturas                                                         *
*--------------------------------------------------------------------*
DATA: gw_cell TYPE salv_s_cell,
      gw_row  TYPE i,
      gw_col  TYPE lvc_fname.

*--------------------------------------------------------------------*
*       CLASS cl_report DEFINITION                                   *
*--------------------------------------------------------------------*
##CLASS_FINAL CLASS cl_report DEFINITION.

  PUBLIC SECTION.

    METHODS:
      set_field_cat
        IMPORTING iv_container_name TYPE any DEFAULT abap_false
        CHANGING  co_report         TYPE REF TO cl_report OPTIONAL
                  co_alv            TYPE REF TO cl_salv_table
                  ct_table          TYPE ANY TABLE,

      get_selections
        CHANGING co_alv TYPE REF TO cl_salv_table,

      set_pf_status
        IMPORTING p_ok   TYPE c DEFAULT abap_true
        CHANGING  co_alv TYPE REF TO cl_salv_table,

      set_screen_status
        IMPORTING p_pfstatus TYPE sypfkey
        CHANGING  co_alv     TYPE REF TO cl_salv_table,

      set_layout
        IMPORTING p_set     TYPE xfeld DEFAULT abap_true
                  p_variant TYPE slis_vari
        CHANGING  co_alv    TYPE REF TO cl_salv_table,

      add_button
        IMPORTING p_function TYPE any
                  p_icon     TYPE any OPTIONAL
                  p_text     TYPE any OPTIONAL
                  p_tooltip  TYPE any
        CHANGING  co_alv     TYPE REF TO cl_salv_table,

      set_text
        IMPORTING p_column      TYPE any
                  p_long_text   TYPE any
                  p_medium_text TYPE any
                  p_short_text  TYPE any,

      set_sorts
        IMPORTING p_colum    TYPE lvc_fname
                  p_subtotal TYPE char01 OPTIONAL
        CHANGING  co_alv     TYPE REF TO cl_salv_table,

      set_total
        IMPORTING p_colum TYPE lvc_fname
        CHANGING  co_alv  TYPE REF TO cl_salv_table,

      set_colors
        IMPORTING p_colum TYPE lvc_fname
                  p_color TYPE c
                  p_int   TYPE lvc_int
                  p_inv   TYPE lvc_inv
        CHANGING  co_alv  TYPE REF TO cl_salv_table,

      set_hotspot
        IMPORTING p_colum   TYPE lvc_fname
        CHANGING  co_alv    TYPE REF TO cl_salv_table
                  co_report TYPE REF TO cl_report OPTIONAL,

      set_selection_mode
        IMPORTING p_value TYPE i
        CHANGING  co_alv  TYPE REF TO cl_salv_table,

      set_selected_rows
        IMPORTING p_row  TYPE i
        CHANGING  co_alv TYPE REF TO cl_salv_table,

      on_link_click
                  FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column,

      double_click
                  FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,

      refresh
        CHANGING co_alv TYPE REF TO cl_salv_table,

      refresh2
        CHANGING co_alv TYPE REF TO cl_salv_table,

      set_checkbox
        IMPORTING p_colum TYPE lvc_fname,

      set_size
        IMPORTING p_colum TYPE lvc_fname
                  p_size  TYPE any,

      set_type
        IMPORTING p_colum TYPE lvc_fname
                  p_type  TYPE salv_de_celltype,

      set_visible
        IMPORTING p_colum TYPE lvc_fname
                  p_value TYPE sap_bool DEFAULT abap_false,

      set_alignment
        IMPORTING p_colum TYPE lvc_fname
                  p_value TYPE salv_de_alignment
                    DEFAULT if_salv_c_alignment=>centered,

      set_technical
        IMPORTING p_colum TYPE lvc_fname
                  p_value TYPE sap_bool DEFAULT abap_true.

ENDCLASS.                    "cl_report DEFINITION

*--------------------------------------------------------------------*
* Tipos                                                              *
*--------------------------------------------------------------------*
DATA:
      go_report   TYPE REF TO cl_report.

*--------------------------------------------------------------------*
*       CLASS cl_report IMPLEMENTATION                               *
*--------------------------------------------------------------------*
CLASS cl_report IMPLEMENTATION.

*--------------------------------------------------------------------*
* Método set_visible                                                 *
*--------------------------------------------------------------------*
  METHOD set_visible.

    CHECK go_t_columns IS BOUND.

    TRY.
        go_columns = go_t_columns->get_column( p_colum ).
      CATCH cx_salv_not_found.
    ENDTRY.

    go_columns->set_visible( p_value ).

  ENDMETHOD.                    "set_visible

*--------------------------------------------------------------------*
* Método set_field_cat                                               *
*--------------------------------------------------------------------*
  METHOD set_field_cat.

    DATA: lt_func_list TYPE salv_t_ui_func,
          lw_func_list TYPE salv_s_ui_func,
          lo_container TYPE REF TO cl_gui_custom_container.

    DATA: lx_msg TYPE REF TO cx_salv_msg.

    IF co_alv IS BOUND.
      CLEAR co_alv.
    ENDIF.

    IF NOT iv_container_name IS INITIAL AND cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false.

      CREATE OBJECT lo_container
        EXPORTING
          container_name = iv_container_name.

* Monta estrutura do fieldcat a partir da tabela interna
* Com container
      TRY.
          CALL METHOD cl_salv_table=>factory
            EXPORTING
              r_container    = lo_container
              container_name = iv_container_name
            IMPORTING
              r_salv_table   = co_alv
            CHANGING
              t_table        = ct_table.

        CATCH cx_salv_msg INTO lx_msg.                  "#EC NO_HANDLER

      ENDTRY.

    ELSE.

* Sem container
      TRY.
          CALL METHOD cl_salv_table=>factory
            IMPORTING
              r_salv_table = co_alv
            CHANGING
              t_table      = ct_table.

        CATCH cx_salv_msg INTO lx_msg.                  "#EC NO_HANDLER
      ENDTRY.

    ENDIF.

    go_functions = co_alv->get_functions( ).
    go_functions->set_all( ).

    lt_func_list = go_functions->get_functions( ).
    LOOP AT lt_func_list INTO lw_func_list.
      IF lw_func_list-r_function->get_name( ) = '&VEXCEL'
      OR lw_func_list-r_function->get_name( ) = '&GRAPH'.
        lw_func_list-r_function->set_visible( ' ' ).
      ENDIF.
    ENDLOOP.

    go_display = co_alv->get_display_settings( ).

*** Zebra
    go_display->set_striped_pattern( abap_true ).

*** Retorna Colunas do Fieldcat
    go_t_columns = co_alv->get_columns( ).

*** Ajustar colunas de acordo com conteúdo
    go_t_columns->set_optimize( abap_true ).

    me->set_selection_mode(
      EXPORTING p_value = if_salv_c_selection_mode=>single
       CHANGING co_alv  = co_alv ).

*** Selecionar primeira linha da tabela
    me->set_selected_rows( EXPORTING p_row  = 1
                           CHANGING  co_alv = co_alv ).

*** Adicionando um layout default para ativar botões para salvar Layout
    me->set_layout( EXPORTING: p_set = abap_true p_variant = 'DEFAULT'
                    CHANGING  co_alv = co_alv ).

  ENDMETHOD.                    "set_field_cat

*--------------------------------------------------------------------*
* Método get_selections                                              *
*--------------------------------------------------------------------*
  METHOD get_selections.

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    CLEAR: gt_rows, gt_cols, gw_cell.

    CALL METHOD co_alv->get_metadata.

    lo_selections = co_alv->get_selections( ).
    gt_rows       = lo_selections->get_selected_rows( ).
    gt_cols       = lo_selections->get_selected_columns( ).
    gw_cell       = lo_selections->get_current_cell( ).

  ENDMETHOD.                    "get_selections

*--------------------------------------------------------------------*
* Método set_pf_status                                               *
*--------------------------------------------------------------------*
  METHOD set_pf_status.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.

    lo_functions = co_alv->get_functions( ).
    lo_functions->set_default( p_ok ).

  ENDMETHOD.                    "set_pf_status

*--------------------------------------------------------------------*
* Método set_screen_status                                           *
*--------------------------------------------------------------------*
  METHOD set_screen_status.

    co_alv->set_screen_status(
      pfstatus      =  p_pfstatus
      report        =  sy-repid
      set_functions = co_alv->c_functions_all ).

  ENDMETHOD.                    "set_screen_status

*--------------------------------------------------------------------*
* Método set_layout                                                  *
*--------------------------------------------------------------------*
  METHOD set_layout.

    DATA: lo_layout  TYPE REF TO cl_salv_layout,
          lf_variant TYPE slis_vari,
          ls_key     TYPE salv_s_layout_key.

    lo_layout = co_alv->get_layout( ).

    IF NOT p_set IS INITIAL.
      ls_key-report = sy-repid.
    ENDIF.

    lo_layout->set_key( ls_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lf_variant = p_variant.
    lo_layout->set_initial_layout( lf_variant ).

  ENDMETHOD.                    "set_layout

*--------------------------------------------------------------------*
* Método add_button                                                  *
*--------------------------------------------------------------------*
  METHOD add_button.

    DATA: l_name    TYPE salv_de_function,
          l_icon    TYPE string,
          l_text    TYPE string,
          l_tooltip	TYPE string.

    go_functions = co_alv->get_functions( ).

    l_name    = p_function .
    l_icon    = p_icon .
    l_text    = p_text .
    l_tooltip = p_tooltip.

    TRY.
        go_functions->add_function(
          name     = l_name
          icon     = l_icon
          text     = l_text
          tooltip  = l_tooltip
          position = if_salv_c_function_position=>right_of_salv_functions ).
      CATCH cx_salv_wrong_call cx_salv_existing.
    ENDTRY.

  ENDMETHOD.                    "add_button

*--------------------------------------------------------------------*
* Método set_sorts                                                   *
*--------------------------------------------------------------------*
  METHOD set_sorts.

    DATA: lo_sort TYPE REF TO cl_salv_sorts.

    lo_sort = co_alv->get_sorts( ).

    TRY.
        CALL METHOD lo_sort->add_sort
          EXPORTING
            columnname = p_colum
            subtotal   = p_subtotal.

      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_sorts
  METHOD set_text.
    DATA: l_long_text   TYPE scrtext_l,
          l_medium_text TYPE scrtext_m,
          l_short_text  TYPE scrtext_s,
          l_column      TYPE char30.

    l_column      = p_column.
    l_long_text   = p_long_text.
    l_medium_text = p_medium_text.
    l_short_text  = p_short_text.

    TRY.
        go_columns = go_t_columns->get_column( l_column ).
      CATCH cx_salv_not_found.
    ENDTRY.

    go_columns->set_long_text( l_long_text   ).
    go_columns->set_medium_text( l_medium_text ).
    go_columns->set_short_text( l_short_text  ).

  ENDMETHOD.
*--------------------------------------------------------------------*
* Método set_total                                                   *
*--------------------------------------------------------------------*
  METHOD set_total.

    DATA: lo_aggrs   TYPE REF TO cl_salv_aggregations.

    lo_aggrs = co_alv->get_aggregations( ).

    TRY.
        CALL METHOD lo_aggrs->add_aggregation
          EXPORTING
            columnname  = p_colum
            aggregation = if_salv_c_aggregation=>total.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "total

*--------------------------------------------------------------------*
* Método set_colors                                                  *
*--------------------------------------------------------------------*
  METHOD set_colors.

    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
          lo_col_tab  TYPE REF TO cl_salv_column_table.
    DATA: ls_color    TYPE lvc_s_colo.

    lo_cols_tab = co_alv->get_columns( ).

    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( p_colum ).
        ls_color-col = p_color.
        ls_color-int = p_int.
        ls_color-inv = p_inv.
        lo_col_tab->set_color( ls_color ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_colors

*--------------------------------------------------------------------*
* Método double_click                                                *
*--------------------------------------------------------------------*
  METHOD double_click.
*
*
  ENDMETHOD.                    "double_click

*--------------------------------------------------------------------*
* Método set_hotspot                                                 *
*--------------------------------------------------------------------*
  METHOD set_hotspot.

    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
          lo_col_tab  TYPE REF TO cl_salv_column_table.

    lo_cols_tab = co_alv->get_columns( ).

    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( p_colum ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        CALL METHOD lo_col_tab->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_hotspot

*--------------------------------------------------------------------*
* Método set_selection_mod                                           *
*--------------------------------------------------------------------*
  METHOD set_selection_mode.

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = co_alv->get_selections( ).
    lo_selections->set_selection_mode( p_value  ).

  ENDMETHOD.                    "set_selection_mode

*--------------------------------------------------------------------*
* Método set_selected_rows                                           *
*--------------------------------------------------------------------*
  METHOD set_selected_rows.

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    CHECK co_alv IS BOUND.

    lo_selections = co_alv->get_selections( ).

    REFRESH gt_rows.

    APPEND p_row TO gt_rows.
    lo_selections->set_selected_rows( gt_rows ).

  ENDMETHOD.                    "set_selected_rows.

*--------------------------------------------------------------------*
* Método on_link_click                                               *
*--------------------------------------------------------------------*
  METHOD on_link_click.
*
*
  ENDMETHOD.                    "on_link_click

*--------------------------------------------------------------------*
* Método refresh                                                     *
*--------------------------------------------------------------------*
  METHOD refresh.

    CHECK co_alv IS BOUND.

    go_t_columns = co_alv->get_columns( ).
    go_t_columns->set_optimize( abap_true ).
    co_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    co_alv->close_screen( ).

  ENDMETHOD.                    "refresh

*--------------------------------------------------------------------*
* Método refresh2                                                    *
*--------------------------------------------------------------------*
  METHOD refresh2.

    CHECK co_alv IS BOUND.

    co_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    co_alv->close_screen( ).

  ENDMETHOD.                    "refresh

*--------------------------------------------------------------------*
* Método set_checkbox                                                *
*--------------------------------------------------------------------*
  METHOD set_checkbox.

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    TRY.
        lo_column ?= go_t_columns->get_column( p_colum ).
        lo_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
        lo_column->set_output_length( 10 ).
        lo_column->set_alignment( value = if_salv_c_alignment=>centered ).

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_checkbox

*--------------------------------------------------------------------*
* Método set_size                                                    *
*--------------------------------------------------------------------*
  METHOD set_size.

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    TRY.
        lo_column ?= go_t_columns->get_column( p_colum ).
        lo_column->set_output_length( p_size ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_checkbox

*--------------------------------------------------------------------*
* Método set_type                                                    *
*--------------------------------------------------------------------*
  METHOD set_type.

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    TRY.
        lo_column ?= go_t_columns->get_column( p_colum ).
        lo_column->set_cell_type( p_type ).

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_checkbox

*--------------------------------------------------------------------*
* Método set_alignment                                               *
*--------------------------------------------------------------------*
  METHOD set_alignment.

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    TRY.
        lo_column ?= go_t_columns->get_column( p_colum ).
        lo_column->set_alignment( value = p_value ).

      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_checkbox

*--------------------------------------------------------------------*
* Método set_technical                                               *
*--------------------------------------------------------------------*
  METHOD set_technical.

    DATA: lo_column TYPE REF TO cl_salv_column_list.

    TRY.
        lo_column ?= go_t_columns->get_column( p_colum ).
        lo_column->set_technical( value = p_value ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "set_checkbox

ENDCLASS.                    "cl_report IMPLEMENTATION

*--------------------------------------------------------------------*
*	CLASS cl_click DEFINITION                                          *
*--------------------------------------------------------------------*
##class_final CLASS cl_click DEFINITION.

  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

ENDCLASS.                    "cl_click DEFINITION

*--------------------------------------------------------------------*
*	CLASS cl_click IMPLEMENTATION                                      *
*--------------------------------------------------------------------*
CLASS cl_click IMPLEMENTATION.

  METHOD on_user_command.

    go_report->get_selections( CHANGING co_alv = go_alv ).
    CASE e_salv_function.
      WHEN '&API'.
        FREE gw_saida.

        LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
          READ TABLE gt_saida INTO DATA(wa_saida) INDEX <fs_rows>.

          MOVE wa_saida TO gw_saida.
          APPEND gw_saida TO gt_saidax.
        ENDLOOP.

        go_sap_pp->set_api( EXPORTING it_saida = gt_saidax
                            IMPORTING ev_stat  = DATA(lv_stat)
                                      ev_error = DATA(lv_error)
                                      ev_code  = DATA(lv_code)
                                      ev_text  = DATA(lv_text) ).


        DATA(lv_l_tb) = lines( gt_rows ).

        IF lv_code EQ lv_l_tb.

          LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<fs_rows1>).
            READ TABLE gt_saida INTO DATA(wa_saidax) INDEX <fs_rows1>.

            gw_saidalv = CORRESPONDING #( wa_saidax ).
            gw_saidalv-icon = icon_checked.
            MODIFY gt_saidalv FROM gw_saidalv INDEX <fs_rows1>.

          ENDLOOP.

        ELSEIF lv_text EQ '&ERROR&.' OR lv_error EQ abap_true.

          LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<fs_rows2>).
            READ TABLE gt_saida INTO DATA(wa_saidaz) INDEX <fs_rows2>.

            gw_saidalv = CORRESPONDING #( wa_saidaz ).
            gw_saidalv-icon = icon_led_yellow.
            MODIFY gt_saidalv FROM gw_saidalv INDEX <fs_rows2>.
          ENDLOOP.
        ENDIF.
        IF lv_error IS NOT INITIAL.
          MOVE lv_error TO gv_error.
        ENDIF.

        go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).

        FREE: wa_saida, gw_saida, gt_saidax.



        IF sy-batch IS INITIAL.
          IF gv_error IS INITIAL.
*              Integração realizada com sucesso!
            MESSAGE 'Integração realizada com sucesso!'(i03) TYPE 'I' DISPLAY LIKE 'I'.
          ELSE.
*              Integração realizada com erro(s), analisar!
            MESSAGE 'Integração realizada com erro(s), analisar!'(i04) TYPE 'I' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "cl_click IMPLEMENTATION
