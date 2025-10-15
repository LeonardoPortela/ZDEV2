FUNCTION zsdmf_sel_perguntas_popup.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_CHECKID) TYPE  ZSDCHECKID OPTIONAL
*"     REFERENCE(IT_0380) TYPE  ZSDC0380 OPTIONAL
*"     REFERENCE(IV_SO_RELAC) TYPE  FLAG DEFAULT SPACE
*"     REFERENCE(IV_POPUP) TYPE  FLAG DEFAULT ABAP_TRUE
*"     REFERENCE(IV_TITLE) TYPE  SYTITLE DEFAULT SY-TITLE
*"     REFERENCE(IV_START_COLUMN) TYPE  I DEFAULT 10
*"     REFERENCE(IV_START_LINE) TYPE  I DEFAULT 1
*"     REFERENCE(IV_END_COLUMN) TYPE  I DEFAULT 100
*"     REFERENCE(IV_END_LINE) TYPE  I DEFAULT 10
*"  EXPORTING
*"     REFERENCE(EV_CANC) TYPE  CHAR1
*"     REFERENCE(ET_PERGUNTAS) TYPE  ZSDC380
*"  CHANGING
*"     REFERENCE(CT_RELACAO) TYPE  ZSDC378_R OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_title TYPE lvc_title.
  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_handle_events.

  DATA lo_columns TYPE REF TO cl_salv_columns_table.
  DATA lo_column  TYPE REF TO cl_salv_column_table.
  DATA lt_rows       TYPE salv_t_row.

  DATA lv_seq TYPE int4.

  CLEAR gt_0378[].

  CLEAR gt_perguntas[].

  IF iv_so_relac <> abap_true.

    SELECT * FROM zsdt0378
      INTO TABLE gt_0378
        WHERE inativar = abap_false
          AND checkid <> iv_checkid
        ORDER BY checkid ASCENDING.

    gv_checkid = iv_checkid.

    CHECK gt_0378[] IS NOT INITIAL.

    LOOP AT ct_relacao ASSIGNING FIELD-SYMBOL(<fs_relacao>).

      READ TABLE gt_0378 ASSIGNING FIELD-SYMBOL(<fs_0378>)
        WITH KEY checkid = <fs_relacao>-checkid_r.

      CHECK sy-subrc EQ 0.

      APPEND sy-tabix TO lt_rows.

    ENDLOOP.

    IF it_0380 IS NOT INITIAL.

      LOOP AT gt_0378 ASSIGNING <fs_0378>.

        DATA(lv_index) = sy-tabix.

        READ TABLE it_0380 TRANSPORTING NO FIELDS
          WITH KEY checkid = <fs_0378>-checkid.

        IF sy-subrc EQ 0.
          DELETE gt_0378 INDEX lv_index.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ELSE.

    LOOP AT ct_relacao ASSIGNING <fs_relacao>.

      APPEND INITIAL LINE TO gt_0378 ASSIGNING <fs_0378>.

      READ TABLE it_0380 ASSIGNING FIELD-SYMBOL(<fs_0380>)
        WITH KEY checkid = <fs_relacao>-checkid_r.

      CHECK sy-subrc EQ 0.

      MOVE-CORRESPONDING <fs_0380> TO <fs_0378>.

    ENDLOOP.

  ENDIF.

  SORT gt_0378 BY checkid ASCENDING.

  IF gt_0378 IS NOT INITIAL.

    SELECT * FROM zsdt0378_r
      INTO TABLE gt_0378_r
        FOR ALL ENTRIES IN gt_0378
          WHERE checkid = gt_0378-checkid.

******    " retira quem tem vinculo
******    LOOP AT gt_0378_r ASSIGNING FIELD-SYMBOL(<fs_0378_r>).
******      DELETE gt_0378 WHERE checkid = <fs_0378_r>-checkid_r.
******    ENDLOOP.


  ENDIF.

  DELETE gt_0378 WHERE checkid = iv_checkid.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_0378[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = go_alv->get_functions( ).

*  lr_functions->


  go_alv->set_screen_status( pfstatus = 'STD_POPUP'
                             report = 'SAPLZGFSD006'
                             set_functions = go_alv->c_functions_default
                              ).

  lo_events = go_alv->get_event( ).

  DATA: lr_selections TYPE REF TO cl_salv_selections.

  lr_selections = go_alv->get_selections( ).

  IF iv_so_relac <> abap_true.
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).
  ELSE.
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>none ).
  ENDIF.

  CREATE OBJECT lo_handle.
  SET HANDLER lo_handle->on_user_command FOR lo_events.

  lr_selections->set_selected_rows( lt_rows ).

  lo_columns = go_alv->get_columns( ).

  TRY .

      lo_column ?= lo_columns->get_column( 'CHECKID' ).lo_column->set_optimized( ).
      lo_column ?= lo_columns->get_column( 'PERGUNTA' )."lo_column->set_optimized( ).

      lo_column->set_output_length( 100 ).

      lo_column ?= lo_columns->get_column( 'MANDT' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'TPCHECK' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'TPCOND' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'TPINCONF' ).lo_column->set_visible( abap_false ).

      "lo_column ?= lo_columns->get_column( 'SIM_NAO_INCONFOR' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'INATIVAR' ).lo_column->set_visible( abap_false ).

      lo_column ?= lo_columns->get_column( 'USER_CREATE' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'DATE_CREATE' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'TIME_CREATE' ).lo_column->set_visible( abap_false ).

      lo_column ?= lo_columns->get_column( 'USER_CHANGE' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'DATE_CHANGE' ).lo_column->set_visible( abap_false ).
      lo_column ?= lo_columns->get_column( 'TIME_CHANGE' ).lo_column->set_visible( abap_false ).

      lo_column ?= lo_columns->get_column( 'DELETED' ).lo_column->set_visible( abap_false ).

  ENDTRY.

  IF go_alv IS BOUND.

    IF iv_popup = 'X'.

      DATA(lo_display) = go_alv->get_display_settings( ).

      lv_title = iv_title.

      lo_display->set_list_header( lv_title  ).

      go_alv->set_screen_popup(
        start_column = iv_start_column
        end_column  = iv_end_column
        start_line  = iv_start_line
        end_line    = iv_end_line ).
    ENDIF.


    go_alv->display( ).

  ENDIF.

  IF gv_popup_processa IS INITIAL.
    ev_canc = abap_true.
  ENDIF.

  "CLEAR ct_relacao[].

  LOOP AT ct_relacao ASSIGNING <fs_relacao> .
    <fs_relacao>-deleted = abap_true.
  ENDLOOP.

  LOOP AT gt_perguntas ASSIGNING FIELD-SYMBOL(<fs_perguntas>).

    READ TABLE ct_relacao ASSIGNING <fs_relacao>
      WITH KEY checkid   = iv_checkid
               checkid_r = <fs_perguntas>-checkid.

    IF sy-subrc EQ 0.
      <fs_relacao>-deleted = abap_false.
    ELSE.

      APPEND INITIAL LINE TO ct_relacao ASSIGNING <fs_relacao>.

      <fs_relacao>-checkid = iv_checkid.
      <fs_relacao>-checkid_r = <fs_perguntas>-checkid.

    ENDIF.

  ENDLOOP.

  et_perguntas[] = gt_perguntas[].

ENDFUNCTION.
