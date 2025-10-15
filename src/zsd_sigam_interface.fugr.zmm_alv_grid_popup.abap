FUNCTION zmm_alv_grid_popup.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_START_COLUMN) TYPE  I DEFAULT 25
*"     REFERENCE(I_START_LINE) TYPE  I DEFAULT 6
*"     REFERENCE(I_END_COLUMN) TYPE  I DEFAULT 100
*"     REFERENCE(I_END_LINE) TYPE  I DEFAULT 10
*"     REFERENCE(I_TITLE) TYPE  STRING DEFAULT 'Exibição'
*"     REFERENCE(I_POPUP) TYPE  FLAG DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_ROWS) TYPE  SALV_T_ROW
*"  TABLES
*"      IT_ALV TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
  DATA lv_title TYPE lvc_title.
  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_handle_events_popup.
  DATA lo_column  TYPE REF TO cl_salv_column_table.

  TRY.

      FREE go_alv.

      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = it_alv[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = go_alv->get_functions( ).
  "lr_functions->set_all( 'X' ).

  go_alv->set_screen_status( pfstatus = '9100'
                             report = 'SAPLZSD_SIGAM_INTERFACE'
                             set_functions = go_alv->c_functions_default
                              ).
  "lr_functions->set_function

  lo_events = go_alv->get_event( ).

  DATA: lr_selections TYPE REF TO cl_salv_selections.

  lr_selections = go_alv->get_selections( ).

  lr_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

  CREATE OBJECT lo_handle.
  SET HANDLER lo_handle->on_user_command FOR lo_events.

  DATA lo_columns TYPE REF TO cl_salv_columns_table.

  lo_columns = go_alv->get_columns( ).

  lo_columns->set_optimize( abap_true ).

  TRY .
      lo_column ?= lo_columns->get_column( 'FILIAL' ).
      lo_column->set_short_text('Filial').
      lo_column->set_medium_text('Filial').
      lo_column->set_long_text('Filial').

      lo_column ?= lo_columns->get_column( 'PRODUTOR' ).
      lo_column->set_short_text('Produtor').
      lo_column->set_medium_text('Produtor').
      lo_column->set_long_text('Produtor').

      lo_column ?= lo_columns->get_column( 'NAME1' ).
      lo_column->set_short_text('NomeProd').
      lo_column->set_medium_text('Nome Produtor').
      lo_column->set_long_text('Nome Produtor').

  ENDTRY.

  IF go_alv IS BOUND.
    IF i_popup = 'X'.

      DATA(lo_display) = go_alv->get_display_settings( ).

      lv_title = i_title.

      lo_display->set_list_header( lv_title  ).

      go_alv->set_screen_popup(
        start_column = i_start_column
        end_column  = i_end_column
        start_line  = i_start_line
        end_line    = i_end_line
        ).
    ENDIF.

    go_alv->display( ).

  ENDIF.

  et_rows = gt_rows.

ENDFUNCTION.
